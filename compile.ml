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

let strings = Hashtbl.create 32
let alloc_string =
  let r = ref 0 in
  fun s ->
    incr r;
    let l = "S_" ^ string_of_int !r in
    Hashtbl.add strings l s;
    l

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

  | TEconstant (Cstring s) -> movq (ilab (alloc_string s)) !%rdi

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
    let fmt_label =
      let create_fmt (acc, prefix) expr =
        ( match expr.expr_typ with
          | Tint -> (acc ^ prefix ^ "%ld", " ")
          | Tstring -> (acc ^ prefix ^ "%s", "")
          | _ -> (* TODO *) assert false ) in

      let fmt, _ = List.fold_left create_fmt ("", "") el
      in alloc_string fmt in

    (* NOTE Génération du code *)
    let eval_and_push acc expr = expr ++ acc ++ pushq !%rdi in
    let nb_args = (List.length el + 1) in

    nop ++ nop ++ nop ++
    subq (imm (reg_bytes * (max 0 (reg_max_args - nb_args)))) !%rsp ++
    List.fold_right eval_and_push (List.map (expr env) el) nop ++
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
      (Hashtbl.fold (fun l s d -> label l ++ string s ++ d) strings nop)
  ;
  }
