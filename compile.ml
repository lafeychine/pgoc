open Tast
open X86_64


(* NOTE Quelques constantes *)
let reg_bytes = 8
let reg_max_args = 6  (* RDI, RSI, RDX, RCX, R8, R9 *)
let context_bytes = 16


(* NOTE Redéfinition de label: Gain de lisibilité *)
let label s = nop ++ label s


(* NOTE Création des contextes *)
module Stack = struct
  module M = Map.Make(struct type t = int let compare = compare end)

  let add = M.add
  let create = M.empty

  let elem = M.mem
  let get = M.find
  let search = M.find_opt

  let size m =
    let f _ (_, v) acc = acc + v
    in M.fold f m 0

  let find f m =
    let filter k v = function
      | Some (k, v) -> Some (k, v)
      | None -> if f k v then Some (k, v) else None
    in M.fold filter m None
end

type env = {
  arguments: (int * int) Stack.M.t;
  locals: (int * int) Stack.M.t;
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

let new_label =
  let r = ref 0 in fun () -> incr r; "L_" ^ string_of_int !r


(* NOTE Allocation d'une frame de stack *)
let stack_frame ?(register=rbp) ?(align=true) asm =
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
  match Stack.search id env.arguments with
  | Some (_, offset) -> offset + context_bytes
  | None -> - (snd (Stack.get id env.locals))


(* NOTE Récupération du TEident d'un TEdot *)
let rec get_ident_from_dot e =
  match e.expr_desc with
  | TEident var -> (var, e.expr_typ)
  | TEdot (e, _) -> get_ident_from_dot e
  | _ -> assert false


let rec expr env e =
  match e.expr_desc with
  | TEskip -> nop

  | TEconstant (Cbool true) -> movq (imm 1) !%rdi

  | TEconstant (Cbool false) -> movq (imm 0) !%rdi

  | TEconstant (Cint x) -> movq (imm64 x) !%rdi

  | TEconstant (Cstring s) -> movq (ilab (alloc_constant s)) !%rdi

  | TEnil -> xorq (reg rdi) (reg rdi)

  | TEbinop (Bor, e1, e2) ->
    let l_end = new_label () in

    expr env e1 ++
    cmpq (imm 1) !%rdi ++
    je l_end ++
    expr env e2 ++
    label l_end

  | TEbinop (Band, e1, e2) ->
    let l_end = new_label () in

    expr env e1 ++
    cmpq (imm 0) !%rdi ++
    je l_end ++
    expr env e2 ++
    label l_end

  | TEbinop (op, e1, e2) ->
    let asm_binop =
      let assembly_cmp instruction =
        let l_true = new_label () and l_end = new_label () in

        cmpq (ind rsp) !%rdi ++
        instruction l_true ++
        movq (imm 0) !%rdi ++
        jmp l_end ++
        label l_true ++
        movq (imm 1) !%rdi ++
        label l_end in

      match op with
      | Badd -> addq (ind rsp) !%rdi (* NOTE Opération commutative *)
      | Bsub -> movq !%rdi !%rax ++ movq (ind rsp) !%rdi ++ subq !%rax !%rdi
      | Bmul -> imulq (ind rsp) !%rdi (* NOTE Opération commutative *)
      | Bdiv -> xorq !%rdx !%rdx ++ movq (ind rsp) !%rax ++ idivq !%rdi ++ movq !%rax !%rdi
      | Bmod -> xorq !%rdx !%rdx ++ movq (ind rsp) !%rax ++ idivq !%rdi ++ movq !%rdx !%rdi
      | Beq -> assembly_cmp je
      | Bne -> assembly_cmp jne
      | Blt -> assembly_cmp jg
      | Ble -> assembly_cmp jge
      | Bgt -> assembly_cmp jl
      | Bge -> assembly_cmp jle
      | Band | Bor -> assert false

    in expr env e1 ++
       pushq !%rdi ++
       expr env e2 ++
       asm_binop ++
       addq (imm 8) !%rsp

  | TEunop (Uneg, e1) ->
    (* TODO code pour negation ints *) assert false
  | TEunop (Unot, e1) ->
    (* TODO code pour negation bool *) assert false
  | TEunop (Uamp, e1) ->
    (* TODO code pour & *) assert false
  | TEunop (Ustar, e1) ->
    (* TODO code pour * *) assert false

  | TEprint el ->
    (* NOTE Génération du format pour printf *)
    let fmt_label, el =
      let rec create_fmt e =
        match e.expr_typ with
        | Tnil -> ("<nil>", [])

        (* TODO: .data taille d'un pointeur afin de mettre <nil> *)
        | Tptr _ ->
          let nil_ptr = alloc_constant "<nil>" and buffer = alloc_constant "0xffffffffffffffff" in
          let l_true = new_label () and l_end = new_label () in

          ("%s",
           [expr env e ++
            cmpq (imm 0) !%rdi ++
            jne l_true ++
            movq (ilab nil_ptr) !%rdi ++
            jmp l_end ++
            label l_true ++
            movq (ilab buffer) !%rdi ++
            label l_end])

        | Tint -> ("%ld", [expr env e])

        | Tbool ->
          let s_true = alloc_constant "true" and s_false = alloc_constant "false" in
          let l_true = new_label () and l_end = new_label () in

          ("%s", [expr env e ++
                  cmpq (imm 0) !%rdi ++
                  jne l_true ++
                  movq (ilab s_false) !%rdi ++
                  jmp l_end ++
                  label l_true ++
                  movq (ilab s_true) !%rdi ++
                  label l_end])

        | Tstring ->
          let s_empty = alloc_constant "" in
          let l_true = new_label () in

          ("%s", [expr env e ++
                  cmpq (imm 0) !%rdi ++
                  jne l_true ++
                  movq (ilab s_empty) !%rdi ++
                  label l_true])

        | Tstruct s ->
          let var, typ = get_ident_from_dot e in
          let ident = { expr_desc = TEident var; expr_typ = typ } in
          let create_fmt_field field =
            create_fmt { expr_desc = TEdot (ident, field); expr_typ = field.f_typ } in

          let fmt, _, exprs =
            let generate_fmt (fmt_acc, prefix, exprs) (fmt, expr) =
              (fmt_acc ^ prefix ^ fmt, " ", exprs @ expr)
            in
            List.fold_left generate_fmt ("", "", [])
              (List.map create_fmt_field
                 (List.sort (fun { f_ofs = a } { f_ofs = b } -> a - b)
                    (List.of_seq (Hashtbl.to_seq_values s.s_fields))))
          in ("{" ^ fmt ^ "}", exprs)

        | Tvoid
        | Tmany _ -> assert false in

      let fmt, _, exprs =
        let generate_fmt (fmt_acc, prefix, exprs) (fmt, expr) =
          let generate_prefix = function
            | " ", "%ld" | " ", "%p" | " ", "<nil>" -> " "
            | _ -> ""
          in (fmt_acc ^ generate_prefix (prefix, fmt) ^ fmt,
              generate_prefix (" ", fmt),
              exprs @ expr)
        in List.fold_left generate_fmt ("", "", []) (List.map create_fmt el)
      in alloc_constant fmt, exprs in

    (* NOTE Génération du code *)
    let nb_args = List.length el + 1 in
    let offset = max 0 (reg_max_args - nb_args) in

    let push_into_stack acc expr = expr ++ acc ++ pushq !%rdi in

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

    ( match v_typ with
      | Tstruct s -> leaq (ind ~ofs:offset rbp) rdi
      | _ -> movq (ind ~ofs:offset rbp) !%rdi )

  | TEassign (lvalues, rvalues) ->
    let pre, exprs, post =
      match rvalues with
      | [{ expr_desc = TEcall ({ fn_typ }, _)} as call] ->
        let offset, exprs =
          let analyse_typ offset typ =
            (offset + sizeof typ, (movq (ind ~ofs:offset rsp) !%rdi, typ))
          in List.fold_left_map analyse_typ 0 (Typing.unfold_typ [ fn_typ ])
        in (expr env call, exprs, addq (imm (sizeof fn_typ)) !%rsp)

      | _ -> (nop, List.map (fun e -> (expr env e, e.expr_typ)) rvalues, nop) in

    let assign acc lvalue (expr, typ) =
      acc ++ expr ++

      match lvalue.expr_desc with
      | TEident { v_id } ->
        let offset = get_offset env v_id in
        ( match typ with
          | Tstruct s -> nop
          | _ -> movq !%rdi (ind ~ofs:offset rbp) )

      | TEdot (e, { f_ofs }) ->
        let { v_id }, _ = get_ident_from_dot e in
        let offset = get_offset env v_id in

        leaq (ind ~ofs:offset rbp) rax ++
        ( match typ with
          | Tstruct s -> nop
          | _ -> movq !%rdi (ind ~ofs:f_ofs rax) )

    in pre ++ List.fold_left2 assign nop lvalues exprs ++ post


  | TEassign ([{ expr_desc = TEunop (Ustar, { expr_desc = TEident x }) }], [e]) ->
    (* let offset = (\* TODO *\) env.offset_stack - reg_bytes * (x.v_id - Option.get env.first_var_id) in *)
    (* expr env e *)
    (* TODO *)
    assert false


  (* NOTE Rien à faire ici: Cas particulier d'assignation *)
  | TEblock [{ expr_desc = TEvars _ }; { expr_desc = TEcall _ }]
  | TEblock [{ expr_desc = TEvars _ }; { expr_desc = TEassign _ }] ->
    nop

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
      in List.fold_left (List.fold_left add_to_env) (env.locals, 0) vars in

    (* TODO Unstack non-assign call return variables *)

    (* NOTE Génération du code *)
    allocz offset (
      List.fold_left (++) nop
        (List.map (expr { env with locals = locals }) seq_exprs)
    )


  | TEif (e1, e2, e3) ->
    let l_else = new_label () and l_end = new_label () in
    expr env e1 ++
    cmpq (imm 0) !%rdi ++
    je l_else ++
    expr env e2 ++
    jmp l_end ++
    label l_else ++
    expr env e3 ++
    label l_end


  | TEfor (e1, e2) ->
    (* TODO code pour for *) assert false
  | TEnew ty ->
    (* TODO code pour new S *) assert false

  | TEcall (f, el) ->
    (* NOTE Décalage afin d'acceuillir les résultats + arguments *)
    let sizeof_args =
      List.fold_left (+) 0
        (List.map (fun { v_typ } -> sizeof v_typ) f.fn_params) in
    let offset = sizeof f.fn_typ + sizeof_args in

    (* NOTE Déplacement des arguments *)
    let push_into_stack e acc = acc ++ (expr env e) ++ pushq !%rdi in

    (* NOTE Génération du code *)
    allocz offset ~unstack:false (
      List.fold_right push_into_stack el nop ++
      call ("F_" ^ f.fn_name) ++
      addq (imm sizeof_args) !%rsp
    )

  | TEdot (e, { f_ofs }) ->
    let { v_id }, _ = get_ident_from_dot e in
    let offset = get_offset env v_id in

    movq (ind ~ofs:(offset + f_ofs) rbp) !%rdi

  (* NOTE Ne devrait jamais arriver: TEvars est traité dans TEblock *)
  | TEvars _ -> assert false

  | TEreturn el ->
    let offset_rbp = Stack.size env.arguments + context_bytes in
    let toto (acc, offset) e =
      (acc ++
       expr env e ++
       movq !%rdi (ind ~ofs:(offset_rbp + offset) rbp), sizeof e.expr_typ) in

    fst (List.fold_left toto (nop, 0) el)

  | TEincdec (e1, op) ->
    (* TODO code pour return e++, e-- *) assert false


let function_ (f, e) =
  let arguments, _ =
    let add_to_stack (stack, offset) params =
      let size = sizeof params.v_typ in
      (Stack.add params.v_id (size, offset) stack, offset + size)
    in List.fold_left add_to_stack (Stack.create, 0) f.fn_params in

  let empty_env = {
    arguments = arguments;
    locals = Stack.create;
  } in

  label ("F_" ^ f.fn_name) ++
  stack_frame ~align:false (expr empty_env e) ++
  ret


let print =
  label "print" ++
  List.fold_left (++) nop (List.map popq [r13; rdi; rsi; rdx; rcx; r8; r9]) ++
  xorl !%eax !%eax ++
  call "printf" ++
  pushq !%r13 ++
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
    in

    List.fold_left (++) nop
      (List.map function_ (List.filter_map is_function dl)) in

  (* TODO calcul offset champs *)
  { text =
      init_text ++
      functions ++
      print;
    (* TODO print pour d'autres valeurs *)
    (* TODO appel malloc de stdlib *)
    data =
      (Hashtbl.fold (fun constant l d -> label l ++ string constant ++ d) constants nop)
  ;
  }
