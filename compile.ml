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

type env = {
  exit_label: string;
  ofs_this: int;
  nb_locals: int ref; (* maximum *)
  next_local: int; (* 0, 1, ... *)
}

let empty_env =
  { exit_label = ""; ofs_this = -1; nb_locals = ref 0; next_local = 0 }

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
    (* TODO code pour arithmetique ints *) assert false 

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
    (* NOTE Génération du format pour printf *)
    let fmt_label, el =
      let create_fmt ((fmt_acc, prefix), expr_acc) e =
        let asm_expr = expr env e in
        ( match e.expr_typ with
          | Tnil -> ((fmt_acc ^ prefix ^ "<nil>", " "), expr_acc)
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

          | Tstring -> ((fmt_acc ^ "%s", ""), expr_acc @ [asm_expr])

          | Tptr _ -> ((fmt_acc ^ prefix ^ "%p", ""), expr_acc @ [asm_expr])

          | _ -> (* TODO *) assert false ) in

      let (fmt, _), el = List.fold_left create_fmt (("", ""), []) el
      in alloc_constant fmt, el in


    (* NOTE Génération du code *)
    let nb_args = (List.length el + 1) in
    let push_into_stack acc expr = expr ++ acc ++ pushq !%rdi in

    subq (imm (reg_bytes * (max 0 (reg_max_args - nb_args)))) !%rsp ++
    List.fold_right push_into_stack el nop ++
    pushq (ilab fmt_label) ++
    call "print" ++
    addq (imm (reg_bytes * (max nb_args reg_max_args))) !%rsp


  | TEident x ->
    (* TODO code pour x *) assert false
  | TEassign ([{expr_desc=TEident x}], [e1]) ->
    (* TODO code pour x := e *) assert false
  | TEassign ([lv], [e1]) ->
    (* TODO code pour x1,... := e1,... *) assert false
  | TEassign (_, _) ->
    assert false

  | TEblock el ->
    (* TODO *)
    List.fold_left (++) nop (List.map (expr env) el)

  | TEif (e1, e2, e3) ->
    (* TODO code pour if *) assert false
  | TEfor (e1, e2) ->
    (* TODO code pour for *) assert false
  | TEnew ty ->
    (* TODO code pour new S *) assert false
  | TEcall (f, el) ->
    (* TODO code pour appel fonction *) assert false
  | TEdot (e1, {f_ofs=ofs}) ->
    (* TODO code pour e.f *) assert false
  | TEvars _ ->
    assert false (* fait dans block *)
  | TEreturn [] ->
    (* TODO code pour return e *) assert false
  | TEreturn [e1] ->
    (* TODO code pour return e1,... *) assert false
  | TEreturn _ ->
    assert false
  | TEincdec (e1, op) ->
    (* TODO code pour return e++, e-- *) assert false


let function_ (f, e) =
  label ("F_" ^ f.fn_name) ++
  expr empty_env e ++
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
