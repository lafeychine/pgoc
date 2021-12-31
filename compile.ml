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

let debug = ref false


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
let allocz n = movq (imm n) (reg rdi) ++ call "allocz"

let sizeof = Typing.sizeof

let new_label =
  let r = ref 0 in fun () -> incr r; "L_" ^ string_of_int !r


(* NOTE Environnement courant *)
type env = {
  offset_stack: int;      (* De combien la stack à été déplacée depuis le stockage de %rbp *)
  first_id_local: int;    (* Quelle est la première variable locale *)
  number_arguments: int;  (* Combien la fonction prend d'arguments *)
}

let mk_bool d = { expr_desc = d; expr_typ = Tbool }

(* f reçoit le label correspondant à ``renvoyer vrai'' *)
let compile_bool f =
  let l_true = new_label () and l_end = new_label () in
  f l_true ++
  movq (imm 0) (reg rdi) ++ jmp l_end ++
  label l_true ++ movq (imm 1) (reg rdi) ++ label l_end

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
       expr { env with offset_stack = env.offset_stack + reg_bytes } e2 ++
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
      let create_fmt ((fmt_acc, prefix), offset, expr_acc) e =
        let offset_stack = env.offset_stack + reg_bytes * offset in
        let asm_expr = expr { env with offset_stack = offset_stack } e in
        ( match e.expr_typ with
          | Tnil -> ((fmt_acc ^ prefix ^ "<nil>", " "), offset - 2, expr_acc)
          | Tint -> ((fmt_acc ^ prefix ^ "%ld", " "), offset - 2, expr_acc @ [asm_expr])

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
            ((fmt_acc ^ "%s", ""), offset - 2, expr_acc @ [asm_expr])

          | Tstring -> ((fmt_acc ^ "%s", ""), offset - 2, expr_acc @ [asm_expr])

          | Tptr _ -> ((fmt_acc ^ prefix ^ "%p", ""), offset - 2, expr_acc @ [asm_expr])

          | _ -> (* TODO *) assert false ) in

      let (fmt, _), _, el = List.fold_left create_fmt (("", ""), (* TODO *)offset + 1, []) el
      in alloc_constant fmt, el in

    (* NOTE Génération du code *)
    let push_into_stack acc expr = expr ++ acc ++ pushq !%rdi in

    subq (imm (reg_bytes * offset)) !%rsp ++
    List.fold_right push_into_stack el nop ++
    pushq (ilab fmt_label) ++
    call "print" ++
    addq (imm (reg_bytes * (max nb_args reg_max_args))) !%rsp


  | TEident x ->
    let offset = env.offset_stack - reg_bytes * (x.v_id - env.first_id_local) in
    movq (ind ~ofs:offset rsp) !%rdi

  | TEassign ([{ expr_desc = TEident x }], [e]) ->
    let offset = env.offset_stack - reg_bytes * (x.v_id - env.first_id_local) in
    expr env e ++
    movq !%rdi (ind ~ofs:offset rsp)

  | TEassign ([lv], [e1]) ->
    (* TODO code pour x1,... := e1,... *) assert false
  | TEassign (_, _) ->
    assert false

  | TEblock [{ expr_desc = TEvars _ }; { expr_desc = TEassign _ }] ->
    (* NOTE Rien à faire ici: Cas particulier d'assignation *)
    nop

  | TEblock el ->
    (* NOTE Séparation entre variables et séquences *)
    let var_exprs, seq_exprs =
      let expand_assign acc expr =
        match expr.expr_desc with
        | TEblock [{ expr_desc = TEvars _ } as e1; { expr_desc = TEassign _ } as e2] ->
          acc @ [ e1; e2 ]
        | _ -> acc @ [ expr ] in
      let filter_block_expr expr =
        match expr.expr_desc with
        | TEvars _ -> Either.Left expr
        | _ -> Either.Right expr
      in List.partition_map filter_block_expr
        (List.fold_left expand_assign [] el) in

    (* NOTE Création du nouvel environnement *)
    let locals_block_number = List.length var_exprs in
    let offset_var = reg_bytes * locals_block_number in
    let env = {
      env with
      offset_stack = env.offset_stack + offset_var;
    } in

    (* NOTE Génération du code *)
    subq (imm offset_var) !%rsp ++
    List.fold_left (++) nop (List.map (expr env) seq_exprs) ++
    addq (imm offset_var) !%rsp

  | TEif (e1, e2, e3) ->
    (* TODO code pour if *) assert false
  | TEfor (e1, e2) ->
    (* TODO code pour for *) assert false
  | TEnew ty ->
    (* TODO code pour new S *) assert false
  | TEcall (f, el) ->
    subq (imm (sizeof f.fn_typ)) !%rsp ++
    call ("F_" ^ f.fn_name) ++
    popq rdi

  | TEdot (e1, {f_ofs=ofs}) ->
    (* TODO code pour e.f *) assert false
  | TEvars _ ->
    assert false (* fait dans block *)

  | TEreturn [] ->
    (* TODO code pour return e *) assert false

  | TEreturn [e] ->
    let offset = 2 * reg_bytes + env.offset_stack in

    expr env e ++
    movq !%rdi (ind ~ofs:(offset) rsp)

  | TEreturn _ -> assert false

  | TEincdec (e1, op) ->
    (* TODO code pour return e++, e-- *) assert false


let function_ (f, e) =
  let empty_env = { 
    offset_stack = 0;
    first_id_local = 0;
    number_arguments = List.length f.fn_params
  } in

  label ("F_" ^ f.fn_name) ++
  pushq !%rbp ++
  movq !%rsp !%rbp ++

  expr empty_env e ++

  movq !%rbp !%rsp ++
  popq rbp ++
  ret


let print =
  label "print" ++
  List.fold_left (++) nop (List.map popq [r12; rdi; rsi; rdx; rcx; r8; r9]) ++
  xorl !%eax !%eax ++
  call "printf" ++
  subq (imm 48) !%rsp ++
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
