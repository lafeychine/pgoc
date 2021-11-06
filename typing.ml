
open Format
open Lib
open Ast
open Tast

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

exception Error of Ast.location * string

let error loc e = raise (Error (loc, e))

(* NOTE Créations des contextes *)
let contexte_structures: (string, structure) Hashtbl.t = Hashtbl.create 0
let contexte_functions: (string, function_) Hashtbl.t = Hashtbl.create 0


(* NOTE Génération du nom d'un type *)
let rec get_type_name = function
  | PTident { id } -> id
  | PTptr ptyp -> "*" ^ get_type_name ptyp


(* NOTE Récupération, si existant, du type correspondant à la chaîne de caractère *)
let rec type_opt = function
  | PTident { id = "int" } -> Some Tint
  | PTident { id = "bool" } -> Some Tbool
  | PTident { id = "string" } -> Some Tstring

  | PTident { id } -> ( match Hashtbl.find_opt contexte_structures id with
      | Some struct_type -> Some (Tstruct struct_type)
      | None             -> None )

  | PTptr ty -> ( match type_opt ty with
      | Some sub_type -> Some (Tptr sub_type)
      | None         -> None )


let rec eq_type ty1 ty2 = match ty1, ty2 with
  | Tint, Tint | Tbool, Tbool | Tstring, Tstring -> true
  | Tstruct s1, Tstruct s2 -> s1 == s2
  | Tptr ty1, Tptr ty2 -> eq_type ty1 ty2
  | _ -> false
(* TODO autres types *)

let evar v = { expr_desc = TEident v; expr_typ = v.v_typ }

(* NOTE Création d'une nouvelle variable *)
let new_var =
  let id = ref 0 in
  fun name loc typ ?(used=false) ->
    incr id;
    { v_name = name; v_id = !id; v_loc = loc; v_typ = typ; v_used = used; v_addr = false }


module Env = struct
  module M = Map.Make(String)
  type t = var M.t
  let empty = M.empty
  let find = M.find
  let add env v = M.add v.v_name v env

  let all_vars = ref []
  let check_unused () =
    let check v =
      if v.v_name <> "_" && (* TODO used *) true then error v.v_loc "unused variable" in
    List.iter check !all_vars


  let var x loc ?used ty env =
    let v = new_var x loc ty ?used in
    all_vars := v :: !all_vars;
    add env v, v

  (* TODO type () et vecteur de types *)
end

let tvoid = Tmany []
let make d ty = { expr_desc = d; expr_typ = ty }
let stmt d = make d tvoid

let rec expr env e =
  let e, ty, rt = expr_desc env e.pexpr_loc e.pexpr_desc in
  { expr_desc = e; expr_typ = ty }, rt

and expr_desc env loc = function
  | PEskip ->
    TEskip, tvoid, false
  | PEconstant c ->
    (* TODO *) TEconstant c, tvoid, false
  | PEbinop (op, e1, e2) ->
    (* TODO *) assert false
  | PEunop (Uamp, e1) ->
    (* TODO *) assert false
  | PEunop (Uneg | Unot | Ustar as op, e1) ->
    (* TODO *) assert false
  | PEcall ({id = "fmt.Print"}, el) ->
    (* TODO *) TEprint [], tvoid, false
  | PEcall ({id="new"}, [{pexpr_desc=PEident {id}}]) ->
    let ty = match id with
      | "int" -> Tint | "bool" -> Tbool | "string" -> Tstring
      | _ -> (* TODO *) error loc ("no such type " ^ id) in
    TEnew ty, Tptr ty, false
  | PEcall ({id="new"}, _) ->
    error loc "new expects a type"
  | PEcall (id, el) ->
    (* TODO *) assert false
  | PEfor (e, b) ->
    (* TODO *) assert false
  | PEif (e1, e2, e3) ->
    (* TODO *) assert false
  | PEnil ->
    (* TODO *) assert false
  | PEident {id=id} ->
    (* TODO *) (try let v = Env.find id env in TEident v, v.v_typ, false
                with Not_found -> error loc ("unbound variable " ^ id))
  | PEdot (e, id) ->
    (* TODO *) assert false
  | PEassign (lvl, el) ->
    (* TODO *) TEassign ([], []), tvoid, false 
  | PEreturn el ->
    (* TODO *) TEreturn [], tvoid, true
  | PEblock el ->
    (* TODO *) TEblock [], tvoid, false
  | PEincdec (e, op) ->
    (* TODO *) assert false
  | PEvars _ ->
    (* TODO *) assert false 


let found_main = ref false
let fmt_used = ref false


(* 1. declare structures *)
let phase1 = function
  | PDstruct { ps_name = { id; loc } } ->
    (* NOTE Vérification de l'unicité des noms de structures *)
    if Hashtbl.find_opt contexte_structures id <> None then
      error loc (Printf.sprintf "structure %s redeclared" id);

    (* NOTE Ajout des structures dans le contexte de typage sans les champs *)
    Hashtbl.add contexte_structures id { s_name = id; s_fields = Hashtbl.create 5 }

  | _ -> ()


(* 2. declare functions and type fields *)
let phase2 = function
  | PDfunction { pf_name = { id; loc }; pf_params = pl; pf_typ = tyl } ->
    (* NOTE Vérification de la fonction main sans paramètres et sans type de retour *)
    if id = "main" then (
      if pl <> [] || tyl <> [] then
        error loc "func main must have no parameters and no return values";
      found_main := true );

    (* NOTE Vérification de l'unicité des noms de fonctions *)
    if Hashtbl.find_opt contexte_functions id <> None then
      error loc (Printf.sprintf "function %s redeclared" id);

    (* NOTE Vérification de l'unicité des noms des paramètres *)
    ( let is_same_identifier ({ id = id_x }, _) ({ id = id_y }, _) = id_x = id_y
      in match Lib.find_opt_duplicate_item is_same_identifier pl with
      | Some ({ id = id_param; loc = loc_param }, _) ->
        error loc_param (Printf.sprintf "duplicate parameter %s in function %s" id_param id)
      | None -> () );

    (* NOTE Vérification de la bonne formation de chacun des paramètres *)
    let fn_params =
      let param_to_var ({ id = id_param; loc = loc_param }, type_param) =
        match type_opt type_param with
        | Some typ -> new_var id_param loc_param typ ~used:false
        | None -> error loc_param (Printf.sprintf "undefined type %s of parameter %s in function %s" (get_type_name type_param) id_param id)
      in List.map param_to_var pl in

    (* NOTE Vérification de la bonne formation de chacun des valeurs de retours *)
    let fn_typ =
      let get_type_return_value type_return_value =
        match type_opt type_return_value with
        | Some value -> value
        | None -> error loc (Printf.sprintf "undefined type %s of one of return values in function %s" (get_type_name type_return_value) id)
      in List.map get_type_return_value tyl in

    Hashtbl.add contexte_functions id { fn_name = id; fn_params; fn_typ }

  | PDstruct { ps_name = { id }; ps_fields = fl } ->
    let { s_fields } as structure = Hashtbl.find contexte_structures id in

    let process_struct_field ({ id = id_field; loc = loc_field }, type_field) =
      (* NOTE Vérification de l'unicité des champs de la structure *)
      if Hashtbl.find_opt s_fields id_field <> None then
        error loc_field (Printf.sprintf "duplicate field %s in structure %s" id_field id);

      (* NOTE Vérification de la bonne formation du type *)
      let f_typ = match type_opt type_field with
        | Some f_typ -> f_typ;
        | None -> error loc_field (Printf.sprintf "undefined type %s of field %s in structure %s" (get_type_name type_field) id_field id) in

      Hashtbl.add s_fields id_field { f_name = id_field; f_typ; f_ofs = 0 }

    in List.iter process_struct_field fl


(* 3. type check function bodies *)
let rec sizeof = function
  | Tint | Tbool | Tstring | Tptr _ -> 8

  | Tstruct { s_fields } -> Seq.fold_left (fun acc { f_typ } -> acc + sizeof f_typ) 0 (Hashtbl.to_seq_values s_fields)

  | _ -> (* TODO *) assert false


let decl = function
  | PDfunction { pf_name = {id; loc}; pf_body = e; pf_typ = tyl } ->
    (* TODO check name and type *)
    let f = { fn_name = id; fn_params = []; fn_typ = [] } in
    let e, rt = expr Env.empty e in
    TDfunction (f, e)

  | PDstruct { ps_name = { id; loc } } ->
    let structure = Hashtbl.find contexte_structures id in

    ( let rec get_recursive_struct_field { s_fields } acc =
        Seq.fold_left (find_recursive_struct_field acc) None (Hashtbl.to_seq_values s_fields)
      and find_recursive_struct_field acc inv field =
        let { f_name; f_typ } = field in
        match f_typ with
        | Tstruct structure ->
          if List.mem f_name acc then Some field
          else get_recursive_struct_field structure (f_name :: acc)
        | _ -> inv in

      match get_recursive_struct_field structure [ id ] with
      | Some { f_name } -> error loc (Printf.sprintf "recursive field %s in the struct %s" f_name id)
      | None -> ());

    TDstruct structure


let file (imp, dl) =
  List.iter phase1 dl;
  List.iter phase2 dl;

  if not !found_main then error dummy_loc "missing method main";

  let dl = List.map decl dl in

  Env.check_unused (); (* TODO variables non utilisees *)

  if imp && not !fmt_used then error dummy_loc "fmt imported but not used";

  dl
