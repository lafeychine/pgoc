(* étiquettes
     F_function      entrée fonction
     E_function      sortie fonction
     L_xxx           sauts
     S_xxx           chaîne

   expression calculée avec la pile si besoin, résultat final dans %rdi

   fonction : arguments sur la pile, résultat dans %rax ou sur la pile

            res k
            ...
            res 1
            arg n
            ...
            arg 1
            adr. retour
   rbp ---> ancien rbp
            ...
            var locales
            ...
            calculs
   rsp ---> ...

*)

open Format
open Ast
open Tast
open X86_64


(* NOTE Quelques constantes *)
let reg_bytes = 8
let reg_max_args = 6  (* RDI, RSI, RDX, RCX, R8, R9 *)


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

  let find f m =
    let filter k v = function
      | Some (k, v) -> Some (k, v)
      | None -> if f k v then Some (k, v) else None
    in M.fold filter m None
end


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

let memset size register =
  match size with
  | 0 -> nop
  | 8 -> movq (imm 0) (ind register)
  | _ -> movq !%register !%rdi ++
         movq (imm size) !%rsi ++
         call "init_memory"


let sizeof = Typing.sizeof

let new_label =
  let r = ref 0 in fun () -> incr r; "L_" ^ string_of_int !r


(* NOTE Environnement courant *)
type env = {
  arguments: (int * int) Stack.M.t;
  locals: (int * int) Stack.M.t;
}


let get_offset env id =
  match Stack.search id env.arguments with
  | Some (_, offset) -> offset + 16
  | None -> - (snd (Stack.get id env.locals))

let rec expr env e =
  match e.expr_desc with
  | TEskip -> nop

  | TEconstant (Cbool true) -> movq (imm 1) !%rdi

  | TEconstant (Cbool false) -> movq (imm 0) !%rdi

  | TEconstant (Cint x) -> movq (imm64 x) !%rdi

  | TEconstant (Cstring s) -> movq (ilab (alloc_constant s)) !%rdi

  | TEnil -> xorq (reg rdi) (reg rdi)

  | TEbinop (Band, e1, e2) ->
    (* TODO code pour ET logique lazy *) assert false
  | TEbinop (Bor, e1, e2) ->
    (* TODO code pour OU logique lazy *) assert false
  | TEbinop (Blt | Ble | Bgt | Bge as op, e1, e2) ->
    (* TODO code pour comparaison ints *) assert false

  | TEbinop (Badd | Bsub | Bmul | Bdiv | Bmod as op, e1, e2) ->
    let instruction = (match op with
        | Badd -> addq (ind rsp) !%rdi (* NOTE Opération commutative *)
        | Bsub -> movq !%rdi !%rax ++ movq (ind rsp) !%rdi ++ subq !%rax !%rdi
        | Bmul -> imulq (ind rsp) !%rdi (* NOTE Opération commutative *)
        | Bdiv -> xorq !%rdx !%rdx ++ movq (ind rsp) !%rax ++ idivq !%rdi ++ movq !%rax !%rdi
        | Bmod -> xorq !%rdx !%rdx ++ movq (ind rsp) !%rax ++ idivq !%rdi ++ movq !%rdx !%rdi )
    in expr env e1 ++
       pushq !%rdi ++
       expr env e2 ++
       instruction ++
       addq (imm 8) !%rsp

  | TEbinop (Beq | Bne as op, e1, e2) ->
    (* TODO code pour egalite toute valeur *) assert false
  | TEunop (Uneg, e1) ->
    (* TODO code pour negation ints *) assert false
  | TEunop (Unot, e1) ->
    (* TODO code pour negation bool *) assert false
  | TEunop (Uamp, e1) ->
    (* TODO code pour & *) assert false
  | TEunop (Ustar, e1) ->
    (* TODO code pour * *) assert false

  | TEprint el ->
    let nb_args = List.length el + 1 in
    let offset = max 0 (reg_max_args - nb_args) in

    (* NOTE Génération du format pour printf *)
    let fmt_label, el =
      let rec create_fmt ((fmt_acc, prefix), expr_acc) e =
        let asm_expr = expr env e in

        match e.expr_typ with
        | Tnil
        | Tptr _ -> ((fmt_acc ^ prefix ^ "%p", " "), expr_acc @ [asm_expr])

        | Tint -> ((fmt_acc ^ prefix ^ "%ld", " "), expr_acc @ [asm_expr])

        | Tbool ->
          let s_true = alloc_constant "true" and s_false = alloc_constant "false" in
          let l_true = new_label () and l_end = new_label () in

          let asm_expr =
            asm_expr ++
            cmpq (imm 0) !%rdi ++
            jne l_true ++
            movq (ilab s_false) !%rdi ++
            jmp l_end ++
            label l_true ++
            movq (ilab s_true) !%rdi ++
            label l_end in

          ((fmt_acc ^ "%s", ""), expr_acc @ [asm_expr])

        | Tstring ->
          let s_empty = alloc_constant "" in
          let l_true = new_label () in

          let asm_expr =
            asm_expr ++
            cmpq (imm 0) !%rdi ++
            jne l_true ++
            movq (ilab s_empty) !%rdi ++
            label l_true in

          ((fmt_acc ^ "%s", ""), expr_acc @ [asm_expr])

        | Tstruct s ->
          let field = Hashtbl.find s.s_fields "x" in
          let asm_expr = expr env { expr_desc = TEdot (e, field); expr_typ = field.f_typ } in

          ((fmt_acc ^ "{" ^ "}", " "), expr_acc @ [asm_expr])

        | Tvoid
        | Tmany _ -> assert false in

      let (fmt, _), el = List.fold_left create_fmt (("", ""), []) el
      in alloc_constant fmt, el in

    (* NOTE Génération du code *)
    let push_into_stack acc expr = expr ++ acc ++ pushq !%rdi in

    movq !%rsp !%r12 ++

    andq (imm (-16)) !%rsp ++
    ( if nb_args > 6 && nb_args mod 2 == 1 then
        subq (imm 8) !%rsp else nop ) ++

    subq (imm (reg_bytes * offset)) !%rsp ++
    List.fold_right push_into_stack el nop ++
    pushq (ilab fmt_label) ++
    call "print" ++

    movq !%r12 !%rsp


  | TEident { v_id; v_typ } ->
    let offset = get_offset env v_id in

    ( match v_typ with
      | Tstruct s -> leaq (ind ~ofs:offset rbp) rdi
      | _ -> movq (ind ~ofs:offset rbp) !%rdi )

  | TEassign ([{ expr_desc = TEident { v_id; v_typ } }], [ expr_assign ]) ->
    let offset = get_offset env v_id in

    expr env expr_assign ++
    ( match v_typ with
      | Tstruct s -> nop
      | _ -> movq !%rdi (ind ~ofs:offset rbp) )

  | TEassign ([{ expr_desc = TEdot ( e, { f_ofs } )}], [ expr_assign ]) ->
    let offset =
      let rec get_var_id { expr_desc } =
        match expr_desc with
        | TEident { v_id } -> v_id
        | TEdot (e, _) -> get_var_id e
        | _ -> assert false
      in get_offset env (get_var_id e) in

    leaq (ind ~ofs:offset rbp) rdi ++
    pushq !%rdi ++
    expr env expr_assign ++
    popq rax ++
    ( match expr_assign.expr_typ with
      | Tstruct s -> nop
      | _ -> movq !%rdi (ind ~ofs:f_ofs rax) )

  | TEassign ([{ expr_desc = TEunop (Ustar, { expr_desc = TEident x }) }], [e]) ->
    (* let offset = (\* TODO *\) env.offset_stack - reg_bytes * (x.v_id - Option.get env.first_var_id) in *)
    (* expr env e *)
    (* TODO *)
    assert false

  | TEassign (_, _) -> assert false

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

    (* NOTE Génération du code *)
    subq (imm offset) !%rsp ++
    memset offset rsp ++
    List.fold_left (++) nop (List.map (expr { env with locals = locals }) seq_exprs) ++
    addq (imm offset) !%rsp

  | TEif (e1, e2, e3) ->
    (* TODO code pour if *) assert false
  | TEfor (e1, e2) ->
    (* TODO code pour for *) assert false
  | TEnew ty ->
    (* TODO code pour new S *) assert false

  | TEcall (f, el) ->
    let sizeof_args =
      List.fold_left (+) 0
        (List.map (fun { v_typ } -> sizeof v_typ) f.fn_params) in

    (* NOTE Décalage afin d'acceuillir les résultats + arguments *)
    subq (imm (sizeof f.fn_typ + sizeof_args)) !%rsp ++

    (* NOTE Déplacement des arguments *)
    let push_into_stack e acc = acc ++ (expr env e) ++ pushq !%rdi in

    List.fold_right push_into_stack el nop ++

    (* NOTE Génération du code *)
    List.fold_right (fun e acc -> expr env e ++ acc) el nop ++
    call ("F_" ^ f.fn_name) ++
    popq rdi


  | TEdot (e, { f_ofs }) ->
    let offset =
      let rec get_var_id { expr_desc } =
        match expr_desc with
        | TEident { v_id } -> v_id
        | TEdot (e, _) -> get_var_id e
        | _ -> assert false
      in get_offset env (get_var_id e) in

    movq (ind ~ofs:(offset + f_ofs) rbp) !%rdi

  (* NOTE Ne devrait jamais arriver: TEvars est traité dans TEblock *)
  | TEvars _ -> assert false

  | TEreturn [] -> nop

  | TEreturn [e] ->
    (* let offset = 2 * reg_bytes + env.offset_stack in *)

    (* expr env e ++ *)
    (* movq !%rdi (ind ~ofs:(offset) rsp) *)
    assert false

  | TEreturn _ -> assert false

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
  pushq !%rbp ++
  movq !%rsp !%rbp ++

  expr empty_env e ++

  movq !%rbp !%rsp ++
  popq rbp ++
  ret


let libraries =
  let print =
    List.fold_left (++) nop (List.map popq [r13; rdi; rsi; rdx; rcx; r8; r9]) ++
    xorl !%eax !%eax ++
    call "printf" ++
    subq (imm 48) !%rsp ++
    pushq !%r13
  in

  let init_memory =
    cmpq (imm 0) !%rsi ++
    je "init_memory_end" ++
    decq !%rsi ++
    movb (imm 0) (ind ~index:rsi rdi) ++
    jmp "init_memory" ++
    label "init_memory_end"
  in

  label "print" ++ print ++ ret ++
  label "init_memory" ++ init_memory ++ ret


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
      libraries;
    (* TODO print pour d'autres valeurs *)
    (* TODO appel malloc de stdlib *)
    data =
      (Hashtbl.fold (fun constant l d -> label l ++ string constant ++ d) constants nop)
  ;
  }
