open Tast
open X86_64


(* NOTE Quelques constantes *)
let reg_bytes = 8
let reg_max_args = 6  (* RDI, RSI, RDX, RCX, R8, R9 *)
let context_bytes = 16


(* NOTE Redéfinition de label: Gain de lisibilité *)
let label s = nop ++ label s

let new_label =
  let r = ref 0 in fun () -> incr r; "L_" ^ string_of_int !r


(* NOTE Création des contextes *)
module Stack = struct
  module M = Map.Make(struct type t = int let compare = compare end)

  let add = M.add
  let create = M.empty

  let elem = M.mem
  let get = M.find
  let search = M.find_opt

  let size m =
    let f _ (size, _) acc = acc + size
    in M.fold f m 0

  let find f m =
    let filter k v = function
      | Some (k, v) -> Some (k, v)
      | None -> if f k v then Some (k, v) else None
    in M.fold filter m None
end

type env = {
  params: (int * int) Stack.M.t;
  locals: (int * int) Stack.M.t;
  exit_label: string;
}


(* NOTE Constantes destinées à être dans .data *)
let constants = Hashtbl.create 32

let alloc_constant =
  let r = ref 0 in
  fun constant ->
    match Hashtbl.find_opt constants constant with
    | Some label -> label
    | None ->
      incr r;
      let label = "S_" ^ string_of_int !r in
      Hashtbl.add constants constant label;
      label



let malloc n = movq (imm n) (reg rdi) ++ call "malloc"


let sizeof = Typing.sizeof



(* NOTE Allocation d'une frame de stack *)
let rec stack_frame ?(register=rbp) ?(align=true) asm =
  pushq !%register ++
  movq !%rsp !%register ++
  ( if align then andq (imm (-16)) !%rsp else nop ) ++
  asm ++
  movq !%register !%rsp ++
  popq register


(* NOTE Initialisation de la mémoire à 0 *)
let memset size register =
  match size with
  | 0 -> nop
  | 8 -> movq (imm 0) (ind register)
  | _ -> movq !%register !%rdi ++
         movq (imm 0) !%rsi ++
         movq (imm size) !%rdx ++
         stack_frame ( call "memset" )


(* NOTE Déplacement de la pile, et initialisation à 0 *)
let allocz ?(unstack=true) offset asm =
  match offset with
  | 0 -> asm
  | _ -> subq (imm offset) !%rsp ++
         memset offset rsp ++
         asm ++
         if unstack then
           addq (imm offset) !%rsp
         else
           nop


(* NOTE Récupération de l'offset entre rbp et la variable *)
let get_offset env id =
  match Stack.search id env.params with
  | Some (_, offset) -> offset + context_bytes
  | None -> - (snd (Stack.get id env.locals))


(* NOTE Récupération du TEident d'un TEdot *)
let get_ident_from_dot e =
  let rec get_ident_from_dot e offset =
    match e.expr_desc with
    | TEident var -> (offset, var, e.expr_typ)
    | TEdot (e, { f_ofs }) -> get_ident_from_dot e (offset + f_ofs)
    | _ -> assert false
  in get_ident_from_dot e 0


(* NOTE Bloc assembleur if/else *)
let asm_if_else ?(cmp=imm 0) ?(jump=je) expr if_block else_block =
  let l_else = new_label () and l_end = new_label ()
  in expr ++
     cmpq cmp !%rdi ++
     jump l_else ++
     if_block ++
     jmp l_end ++
     label l_else ++
     else_block ++
     label l_end


let rec expr env e =
  match e.expr_desc with
  | TEskip -> nop

  | TEconstant (Cbool true) -> movq (imm 1) !%rdi

  | TEconstant (Cbool false) -> movq (imm 0) !%rdi

  | TEconstant (Cint x) -> movq (imm64 x) !%rdi

  | TEconstant (Cstring s) -> movq (ilab (alloc_constant s)) !%rdi

  | TEnil -> xorq (reg rdi) (reg rdi)

  | TEbinop (Bor, e1, e2) -> asm_if_else (expr env e1) nop (expr env e2)

  | TEbinop (Band, e1, e2) -> asm_if_else (expr env e1) (expr env e2) nop

  | TEbinop (op, e1, e2) ->
    let asm_binop =
      let assembly_cmp jump =
        asm_if_else ~cmp:!%rsi ~jump:jump nop (movq (imm 0) !%rdi) (movq (imm 1) !%rdi) in

      match op with
      | Badd -> addq !%rsi !%rdi (* NOTE Opération commutative *)
      | Bsub -> movq !%rdi !%rax ++ movq !%rsi !%rdi ++ subq !%rax !%rdi
      | Bmul -> imulq !%rsi !%rdi (* NOTE Opération commutative *)
      | Bdiv -> xorq !%rdx !%rdx ++ movq !%rsi !%rax ++ idivq !%rdi ++ movq !%rax !%rdi
      | Bmod -> xorq !%rdx !%rdx ++ movq !%rsi !%rax ++ idivq !%rdi ++ movq !%rdx !%rdi
      | Beq -> assembly_cmp je
      | Bne -> assembly_cmp jne
      | Blt -> assembly_cmp jg
      | Ble -> assembly_cmp jge
      | Bgt -> assembly_cmp jl
      | Bge -> assembly_cmp jle
      | Band | Bor -> assert false

    in ( match e1.expr_desc with
        | TEcall _ -> expr env e1
        | _ -> expr env e1 ++ pushq !%rdi ) ++

       ( match e2.expr_desc with
         | TEcall _ -> expr env e2 ++ popq rdi
         | _ -> expr env e2 ) ++

       popq rsi ++
       asm_binop

  | TEunop (Uneg, e1) ->
    (* TODO code pour negation ints *) assert false
  | TEunop (Unot, e1) ->
    (* TODO code pour negation bool *) assert false

  | TEunop (Uamp, e1) -> expr env e1 ++ movq !%rsi !%rdi

  | TEunop (Ustar, e1) -> expr env e1 ++ movq (ind rdi) !%rdi

  (* TODO print for TEcall *)
  | TEprint el ->
    (* NOTE Outils de génération du format *)
    let rec create_fmt ?(recurse=true) e =
      let create_fmt_structure s =
        let offset, var, typ = get_ident_from_dot e in
        let ident = { expr_desc = TEident var; expr_typ = typ } in
        let create_fmt_field ({ f_ofs; f_typ } as field) =
          let field = { field with f_ofs = f_ofs + offset } in
          create_fmt ~recurse:false { expr_desc = TEdot (ident, field); expr_typ = f_typ } in

        let fmt, _, exprs =
          let generate_fmt (fmt_acc, prefix, exprs) (_, fmt, expr) =
            (fmt_acc ^ prefix ^ fmt, " ", exprs @ expr) in
          List.fold_left generate_fmt ("", "", [])
            (List.map create_fmt_field
               (List.sort (fun { f_ofs = a } { f_ofs = b } -> a - b)
                  (List.of_seq (Hashtbl.to_seq_values s.s_fields)))) in
        (fmt, exprs) in

      let asm_expr =
        match e.expr_desc with
        | TEcall _ -> expr env e ++ popq rdi
        | _ -> expr env e in

      match e.expr_typ with
      | Tstruct s ->
        let fmt, exprs = create_fmt_structure s
        in (true, "{" ^ fmt ^ "}", exprs)

      | Tptr (Tstruct s) when recurse ->
        let fmt, exprs = create_fmt_structure s
        in (true, "&{" ^ fmt ^ "}", exprs)

      | Tnil -> (true, "<nil>", [])

      (* TODO: .data taille d'un pointeur afin de mettre <nil> *)
      | Tptr _ ->
        let nil_ptr = alloc_constant "<nil>" and buffer = alloc_constant "0xffffffffffffffff" in
        (true, "%s", [asm_if_else asm_expr (movq (ilab buffer) !%rdi) (movq (ilab nil_ptr) !%rdi)])

      | Tint -> (true, "%ld", [asm_expr])

      | Tbool ->
        let s_true = alloc_constant "true" and s_false = alloc_constant "false" in
        (true, "%s", [asm_if_else asm_expr (movq (ilab s_true) !%rdi) (movq (ilab s_false) !%rdi)])

      | Tstring ->
        let s_empty = alloc_constant "" in
        (false, "%s", [asm_if_else asm_expr nop (movq (ilab s_empty) !%rdi)])

      | Tvoid
      | Tmany _ -> assert false in


    (* NOTE Génération effective du format *)
    let fmt_label, el =
      let fmt, _, exprs =
        let generate_fmt (fmt_acc, prev_prefix, exprs) (has_prefix, fmt, expr) =
          let generate_prefix = function
            | true, true -> " "
            | _ -> ""
          in (fmt_acc ^ generate_prefix (prev_prefix, has_prefix) ^ fmt,
              has_prefix,
              exprs @ expr)
        in List.fold_left generate_fmt ("", false, []) (List.map create_fmt el)
      in alloc_constant fmt, exprs in

    (* NOTE Génération du code *)
    let nb_args = List.length el + 1 in
    let offset = max 0 (reg_max_args - nb_args) in

    let push_into_stack expr acc = acc ++ expr ++ pushq !%rdi in

    stack_frame ~register:rbx (
      ( if nb_args > 6 && nb_args mod 2 == 1 then
          subq (imm 8) !%rsp else nop ) ++

      subq (imm (reg_bytes * offset)) !%rsp ++
      List.fold_right push_into_stack el nop ++
      pushq (ilab fmt_label) ++
      call "print"
    )

  | TEident { v_id; v_typ } ->
    let offset = get_offset env v_id in

    leaq (ind ~ofs:offset rbp) rsi ++
    ( match v_typ with
      | Tstruct s -> nop
      | _ -> movq (ind ~ofs:offset rbp) !%rdi )

  | TEassign (lvalues, rvalues) ->
    let pre, exprs, post =
      match rvalues with
      | [{ expr_desc = TEcall ({ fn_typ }, _)} as e] ->
        let typs = Typing.unfold_typ [ fn_typ ] in
        let _, offset_typs =
          List.fold_left_map (fun offset typ -> (offset + sizeof typ, offset)) 0 typs
        in (expr env e ++ movq !%rsp !%rsi,
            List.map (fun offset -> pushq (ind ~ofs:offset rsi)) offset_typs,
            addq (imm (sizeof fn_typ)) !%rsp)

      | _ -> (nop, List.map (fun e -> expr env e ++ pushq !%rdi) rvalues, nop) in

    let assign lvalue =
      match lvalue.expr_desc with
      | TEident { v_typ = Tvoid } -> popq rdi

      | _ -> expr env lvalue ++
             popq rdi ++
             movq !%rdi (ind rsi)

    in pre ++
       List.fold_left (fun acc x -> x ++ acc) nop exprs ++
       List.fold_left (++) nop (List.map assign lvalues) ++
       post


  | TEblock el ->
    (* NOTE Séparation entre variables et séquences *)
    let vars, seq_exprs =
      let expand_assign acc expr =
        match expr.expr_desc with
        | TEblock [{ expr_desc = TEvars _ } as e1; { expr_desc = TEassign _ } as e2]
        | TEblock [{ expr_desc = TEvars _ } as e1; { expr_desc = TEcall _ } as e2] ->
          acc @ [ e1; e2 ]
        | _ -> acc @ [ expr ] in
      let filter_block_expr expr =
        match expr.expr_desc with
        | TEvars var -> Either.Left var
        | _ -> Either.Right expr
      in List.partition_map filter_block_expr
        (List.fold_left expand_assign [] el) in

    (* NOTE Ajout des variables dans la stack des variables *)
    let locals, offset =
      let add_to_env (locals, offset) var =
        let size = sizeof var.v_typ in
        (Stack.add var.v_id (sizeof var.v_typ, offset + size) locals, offset + size)
      in List.fold_left (List.fold_left add_to_env) (env.locals, Stack.size env.locals) vars in

    (* TODO Unstack non-assign call return variables *)

    (* NOTE Génération du code *)
    allocz offset (
      List.fold_left (++) nop
        (List.map (expr { env with locals = locals }) seq_exprs)
    )

  | TEif (e1, e2, e3) -> asm_if_else (expr env e1) (expr env e2) (expr env e3)

  | TEfor (e1, e2) ->
    let for_head = new_label () and for_cond = new_label () in

    jmp for_cond ++
    label for_head ++
    expr env e2 ++
    label for_cond ++
    asm_if_else (expr env e1) (jmp for_head) (nop)

  | TEnew ty -> stack_frame (
      movq (imm 1) !%rdi ++
      movq (imm (sizeof ty)) !%rsi ++
      call "calloc"
    )

  | TEcall (f, el) ->
    (* NOTE Décalage afin d'acceuillir les résultats + arguments *)
    let sizeof_args =
      List.fold_left (+) 0
        (List.map (fun { v_typ } -> sizeof v_typ) f.fn_params) in

    (* NOTE Déplacement des arguments *)
    let push_into_stack e acc = acc ++ (expr env e) ++ pushq !%rdi in

    (* NOTE Génération du code *)
    allocz (sizeof f.fn_typ) ~unstack:false (
      List.fold_right push_into_stack el nop ++
      call ("F_" ^ f.fn_name) ++
      addq (imm sizeof_args) !%rsp
    )

  | TEdot (e, { f_ofs }) ->
    let offset, { v_id; v_typ }, _ = get_ident_from_dot e in
    let rbp_offset = get_offset env v_id in

    ( match v_typ with
      | Tptr _ -> movq (ind ~ofs:rbp_offset rbp) !%rdi ++
                  leaq (ind ~ofs:(offset + f_ofs) rdi) rsi ++
                  movq (ind ~ofs:(offset + f_ofs) rdi) !%rdi

      | _ -> leaq (ind ~ofs:(rbp_offset + offset + f_ofs) rbp) rsi ++
             movq (ind ~ofs:(rbp_offset + offset + f_ofs) rbp) !%rdi )

  (* NOTE Ne devrait jamais arriver: TEvars est traité dans TEblock *)
  | TEvars _ -> assert false

  | TEreturn el ->
    let offset_rbp = Stack.size env.params + context_bytes in
    let return_args (acc, offset) e =
      (acc ++
       expr env e ++
       movq !%rdi (ind ~ofs:(offset_rbp + offset) rbp), sizeof e.expr_typ) in

    fst (List.fold_left return_args (nop, 0) el) ++
    jmp env.exit_label

  | TEincdec (e, op) ->
    expr env e ++
    match op with
    | Inc -> incq (ind rsi)
    | Dec -> decq (ind rsi)


let function_ (f, e) =
  let params, _ =
    let add_to_stack (stack, offset) params =
      let size = sizeof params.v_typ in
      (Stack.add params.v_id (size, offset) stack, offset + size)
    in List.fold_left add_to_stack (Stack.create, 0) f.fn_params in

  let empty_env = {
    params = params;
    locals = Stack.create;
    exit_label = new_label ();
  } in

  label ("F_" ^ f.fn_name) ++
  stack_frame ~align:false (expr empty_env e ++ label empty_env.exit_label) ++
  ret


(* NOTE Fonction spécifique à print *)
let print =
  label "print" ++
  List.fold_left (++) nop (List.map popq [r12; rdi; rsi; rdx; rcx; r8; r9]) ++
  xorl !%eax !%eax ++
  call "printf" ++
  pushq !%r12 ++
  ret


let file dl =
  (* NOTE En-tête du binaire *)
  let init_text =
    globl "main" ++ label "main" ++
    call "F_main" ++
    xorq (reg rax) (reg rax) ++
    ret in

  (* NOTE Génération du code pour les fonctions *)
  let functions =
    let is_function = function
      | TDfunction (func, expr) -> Some (func, expr)
      | TDstruct _ -> None
    in List.fold_left (++) nop (List.map function_ (List.filter_map is_function dl)) in

  (* NOTE Génération du code pour les constantes *)
  let data = Hashtbl.fold (fun constant l d -> label l ++ string constant ++ d) constants nop in

  (* NOTE Sortie du programme *)
  { text = init_text ++ functions ++ print; data = data }
