open Ast
open Lib
open Printf
open Tast

exception Error of Ast.location option * string

let error loc e = raise (Error (loc, e))


(* NOTE Créations des contextes *)
module Context = struct
  module M = Map.Make(String)

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


let found_main = ref false
let fmt_used = ref false


(* NOTE Génération du nom d'un type *)
let get_binop_name = function
  | Badd -> "+"
  | Bsub -> "-"
  | Bmul -> "*"
  | Bdiv -> "/"
  | Bmod -> "%"
  | Beq -> "=="
  | Bne -> "!="
  | Blt -> "<"
  | Ble -> "<="
  | Bgt -> ">"
  | Bge -> ">="
  | Band -> "&&"
  | Bor -> "||"

let get_incdec_name = function
  | Inc -> "++"
  | Dec -> "--"

let rec get_ast_type_name = function
  | PTident { id } -> id
  | PTptr ptyp -> "*" ^ get_ast_type_name ptyp

let rec get_tast_type_name = function
  | Tvoid -> "void"
  | Tnil -> "nil"
  | Tint -> "int"
  | Tbool -> "bool"
  | Tstring -> "string"
  | Tstruct { s_name } -> s_name
  | Tptr typ -> "*" ^ get_tast_type_name typ
  | Tmany typ -> String.concat ", " (List.map get_tast_type_name typ)


(* NOTE Récupération, si existant, du type correspondant à la chaîne de caractère *)
let rec type_opt structures = function
  | PTident { id = "int" } -> Some Tint
  | PTident { id = "bool" } -> Some Tbool
  | PTident { id = "string" } -> Some Tstring

  | PTident { id } -> ( match Context.search id structures with
      | Some struct_type -> Some (Tstruct struct_type)
      | None             -> None )

  | PTptr ty -> ( match type_opt structures ty with
      | Some sub_type -> Some (Tptr sub_type)
      | None          -> None )


(* NOTE Comparaison de deux types *)
let rec eq_type ty1 ty2 =
  match ty1, ty2 with
  | Tvoid, Tvoid | Tnil, Tnil | Tint, Tint | Tbool, Tbool | Tstring, Tstring -> true

  | Tnil, Tptr _ | Tptr _, Tnil -> true

  | Tstruct s1, Tstruct s2 -> s1 == s2

  | Tptr ty1, Tptr ty2 -> eq_type ty1 ty2

  | Tmany ty1, Tmany ty2 -> List.for_all2 eq_type ty1 ty2

  | _ -> false


(* NOTE Récupération du type d'une liste *)
let list_type = function
  | [] -> Tvoid
  | [ return_value ] -> return_value
  | return_values -> Tmany return_values


(* NOTE Création d'une nouvelle variable *)
let new_var =
  let id = ref (-1) in
  fun name loc typ used ->
    incr id;
    { v_name = name; v_id = !id; v_loc = loc; v_typ = typ; v_used = used; v_addr = false }

let underscore_var =
  let dummy_pos = (Lexing.dummy_pos, Lexing.dummy_pos)
  in { expr_desc = TEident (new_var "_" dummy_pos Tvoid true); expr_typ = Tvoid }


(* NOTE Vérifie si un bloc contient un return, et plus rien après *)
let rec check_unreachable_expr_list loc expr_list =
  ( List.map (fun (expr, _) -> expr ) expr_list,
    List.fold_left (check_unreachable_expr loc) false expr_list )
and check_unreachable_expr loc rt (_, expr_rt) =
  match expr_rt, rt with
  | true,  false -> true
  | false, false -> false
  | _, true -> error loc "block contains unreachable code"


(* NOTE Dépliage du type Tmany *)
let unfold_expr_typ expr_list =
  let unfold acc { expr_typ } =
    match expr_typ with
    | Tmany typs -> acc @ typs
    | _ -> acc @ [expr_typ]
  in List.fold_left unfold [] expr_list


(* NOTE Vérifie si un élément est une l-value *)
let rec is_lvalue env expr =
  if expr = underscore_var then true
  else
    match expr.expr_desc with
    | TEident { v_name } -> Context.elem v_name env
    | TEunop (Ustar, { expr_desc = TEnil }) -> false
    | TEunop (Ustar, _) -> true
    | TEdot (expr, _) -> is_lvalue env expr
    | _ -> false


(* NOTE Création d'un expr à partir d'un expr_desc *)
let new_expr desc typ = { expr_desc = desc; expr_typ = typ }
let new_stmt desc = new_expr desc Tvoid


(* NOTE Transformation de chaque élément d'AST en TAST *)
let rec expr structures functions env { pexpr_loc; pexpr_desc } =
  expr_desc structures functions env pexpr_loc pexpr_desc

and expr_no_return structures functions env pexpr =
  let expr, _ = expr structures functions env pexpr in expr

and expr_desc structures functions env loc pexpr_desc =
  let expr = expr structures functions in
  let expr_no_return = expr_no_return structures functions in

  match pexpr_desc with
  | PEskip -> new_stmt TEskip, false

  | PEconstant c ->
    let constant = match c with
      | Cbool _ -> Tbool
      | Cint _ -> Tint
      | Cstring _ -> Tstring
    in new_expr (TEconstant c) constant, false

  | PEbinop (op, pexpr1, pexpr2) ->
    let expr1 = expr_no_return env pexpr1 in
    let expr2 = expr_no_return env pexpr2 in

    let check_type { pexpr_loc } { expr_typ } typ =
      if not (eq_type expr_typ typ) then
        error (Some pexpr_loc)
          (sprintf "invalid operation: %s must be used on %s, not on %s"
             (get_binop_name op)
             (get_tast_type_name typ)
             (get_tast_type_name expr_typ)) in

    ( match op with
      | Badd | Bsub | Bmul | Bdiv | Bmod ->
        check_type pexpr1 expr1 Tint;
        check_type pexpr2 expr2 Tint;
        new_expr (TEbinop (op, expr1, expr2)) Tint, false

      | Blt | Ble | Bgt | Bge ->
        check_type pexpr1 expr1 Tint;
        check_type pexpr2 expr2 Tint;
        new_expr (TEbinop (op, expr1, expr2)) Tbool, false

      | Band | Bor ->
        check_type pexpr1 expr1 Tbool;
        check_type pexpr2 expr2 Tbool;
        new_expr (TEbinop (op, expr1, expr2)) Tint, false

      | Beq | Bne ->
        if not (eq_type expr1.expr_typ expr2.expr_typ) then
          error (Some pexpr1.pexpr_loc)
            (sprintf "invalid operation: %s must compare same type, have %s and %s"
               (get_binop_name op)
               (get_tast_type_name expr1.expr_typ)
               (get_tast_type_name expr2.expr_typ));

        new_expr (TEbinop (op, expr1, expr2)) Tbool, false )

  | PEunop (op, pexpr) ->
    let expr = expr_no_return env pexpr in

    ( match op with
      | Uneg ->
        if not (eq_type expr.expr_typ Tint) then
          error (Some pexpr.pexpr_loc)
            (sprintf "invalid operation: - must be used on int, not on %s"
               (get_tast_type_name expr.expr_typ));
        new_expr (TEunop (op, expr)) Tint, false

      | Unot ->
        if not (eq_type expr.expr_typ Tbool) then
          error (Some pexpr.pexpr_loc)
            (sprintf "invalid operation: ! must be used on bool, not on %s"
               (get_tast_type_name expr.expr_typ));
        new_expr (TEunop (op, expr)) Tbool, false

      | Uamp ->
        if not (is_lvalue env expr) then
          error (Some pexpr.pexpr_loc) "cannot take the address of non-left value";
        new_expr (TEunop (op, expr)) (Tptr expr.expr_typ), false

      | Ustar ->
        ( match expr.expr_desc with
          | TEnil -> error (Some pexpr.pexpr_loc) "cannot dereference explicit nil"
          | _ -> () );

        ( match expr.expr_typ with
          | Tptr typ -> new_expr (TEunop (op, expr)) typ, false
          | _ -> error (Some pexpr.pexpr_loc)
                   (sprintf "cannot dereference non-pointer %s"
                      (get_tast_type_name expr.expr_typ))))

  | PEcall ({ id = "fmt.Print" }, args) ->
    fmt_used := true;
    new_stmt (TEprint (List.map (expr_no_return env) args)), false

  | PEcall ({ id = "new" }, [{ pexpr_desc = PEident { id; loc } }]) ->
    let typ =
      match type_opt structures (PTident { id; loc }) with
      | Some typ -> typ
      | None -> error (Some loc) (sprintf "cannot allocate an unknown type %s" id)
    in new_expr (TEnew typ) (Tptr typ), false

  | PEcall ({ id = "new" }, [{ pexpr_desc = PEnil }]) -> error (Some loc) "cannot allocate on nil"
  | PEcall ({ id = "new" }, _) -> error (Some loc) "new expects exactly one type"

  | PEcall ({ id; loc }, el) ->
    (* NOTE Vérification de l'existence de la fonction *)
    let func =
      match Context.search id functions with
      | Some func -> func
      | None -> error (Some loc) (sprintf "call to unknown function %s" id) in

    (* NOTE Construction de chacune des expressions *)
    let expr_list = List.map (expr_no_return env) el in

    (* NOTE Vérification entre le nombre de paramètres et d'arguments de la fonction *)
    if List.length (unfold_expr_typ expr_list) <> List.length func.fn_params then
      error (Some loc)
        (sprintf "incorrect number of arguments in call to %s: %d arguments but %d parameters"
           (func.fn_name) (List.length (unfold_expr_typ expr_list)) (List.length func.fn_params));

    (* NOTE Vérification du type entre les paramètres et les arguments *)
    let check_type { v_typ } typ =
      if not (eq_type v_typ typ) then
        error (Some loc)
          (sprintf "cannot use type %s as type %s in argument to %s"
             (get_tast_type_name typ) (get_tast_type_name v_typ) id)
    in List.iter2 check_type func.fn_params (unfold_expr_typ expr_list);

    new_expr (TEcall (func, expr_list)) func.fn_typ, false


  | PEfor (cond, b) ->
    let cond_expr = expr_no_return env cond in
    let block_expr = expr_no_return env b in

    (* NOTE Vérification de la condition *)
    if not (eq_type cond_expr.expr_typ Tbool) then
      error (Some cond.pexpr_loc)
        (sprintf "cannot use non-bool type %s as for condition"
           (get_tast_type_name cond_expr.expr_typ));

    new_stmt (TEfor (cond_expr, block_expr)), false

  | PEif (pexpr1, pexpr2, pexpr3) ->
    let expr1 = expr_no_return env pexpr1 in
    let expr2, rt_if = expr env pexpr2 in
    let expr3, rt_else = expr env pexpr3 in

    (* NOTE Vérification de la condition *)
    if not (eq_type expr1.expr_typ Tbool) then
      error (Some pexpr1.pexpr_loc)
        (sprintf "cannot use non-bool type %s as if condition"
           (get_tast_type_name expr1.expr_typ));

    let if_stmt = new_stmt (TEif (expr1, expr2, expr3))
    in ( match expr3.expr_desc with
        | TEskip -> if_stmt, false
        | _      -> if_stmt, rt_if && rt_else )

  | PEnil -> new_expr TEnil Tnil, false

  | PEident { id; loc } ->
    if id = "_" then
      error (Some loc) "cannot use _ as value";

    ( match Context.search id env with
      | Some v -> ( v.v_used <- true;
                    new_expr (TEident v) v.v_typ, false )
      | None -> error (Some loc) ("unbound variable " ^ id) )

  | PEdot (e, { id; loc }) ->
    let structure_expr, rt = expr env e in
    let field =
      match structure_expr.expr_desc with
      | TEdot (_, field) -> field

      | TEident { v_typ } -> (
          match v_typ with
          | Tptr (Tstruct { s_name })
          | Tstruct { s_name } ->
            let structure = Context.get s_name structures in
            ( match Hashtbl.find_opt structure.s_fields id with
              | Some field -> field
              | None -> error (Some loc)
                          (sprintf "type %s has no field %s" (get_tast_type_name v_typ) id))

          | _ -> error (Some loc)
                   (sprintf "type %s is not a structure" (get_tast_type_name v_typ)))

      | TEnil -> error (Some loc) "use of untyped nil"
      | _ -> error (Some loc) "use of dot syntax on a non-identifier" in

    new_expr (TEdot (structure_expr, field)) field.f_typ, rt

  | PEassign (lvl, el) ->
    (* NOTE Récupération de l'expression, si son nom n'est pas "_" *)
    let maybe_expr pexpr_left =
      match pexpr_left.pexpr_desc with
      | PEident { id = "_" } -> underscore_var
      | _ -> expr_no_return env pexpr_left in

    let left = List.map maybe_expr lvl  in
    let right = List.map (expr_no_return env) el in

    (* NOTE Vérification de la taille des assignements *)
    if List.length lvl <> List.length (unfold_expr_typ right) then
      error (Some loc)
        (sprintf "assignment mismatch: %d variables but %d values"
           (List.length lvl) (List.length (unfold_expr_typ right)));

    (* NOTE Vérification des types lors de l'assignement *)
    let check_type expr typ =
      if expr <> underscore_var && not (eq_type expr.expr_typ typ) then
        error (Some loc)
          (sprintf "cannot use type %s as type %s in assignment"
             (get_tast_type_name typ) (get_tast_type_name expr.expr_typ))
    in List.iter2 check_type left (unfold_expr_typ right);

    (* NOTE Vérification des l-values *)
    ( let check_lvalue expr =
        if not (is_lvalue env expr) then
          error (Some loc) "cannot assign to a non-left value"
      in List.iter check_lvalue left);

    new_stmt (TEassign (left, right)), false

  | PEreturn el -> new_stmt (TEreturn (List.map (expr_no_return env) el)), true

  | PEblock el ->
    let expr_propagate_env env el = (* TODO *)
      let add_var_to_env env v = Context.add v.v_name v env in
      let tast_expr, rt = expr env el in
      let env = match el.pexpr_desc, tast_expr.expr_desc with
        | PEvars _, TEblock ({ expr_desc = TEvars var_list} :: _)
        | PEvars _, TEvars var_list -> List.fold_left add_var_to_env env var_list
        | _ -> env
      in env, (tast_expr, rt) in

    let env, expr_list = List.fold_left_map expr_propagate_env env el in
    let expr_list, rt = check_unreachable_expr_list (Some loc) expr_list in

    (* NOTE Vérification des variables non utilisées *)
    ( match Context.find (fun _ { v_used } -> not v_used) env with
      | Some (_, { v_name; v_loc }) ->
        error (Some v_loc) (sprintf "%s declared but not used" v_name)
      | None -> () );

    new_stmt (TEblock expr_list), rt

  | PEincdec (pexpr, op) ->
    let expr = expr_no_return env pexpr in

    if not (eq_type expr.expr_typ Tint) then
      error (Some pexpr.pexpr_loc)
        (sprintf "invalid operation: %s must be used on int, not on %s"
           (get_incdec_name op) (get_tast_type_name expr.expr_typ));

    if not (is_lvalue env expr) then
      error (Some pexpr.pexpr_loc)
        (sprintf "cannot assign to a non-left value");

    new_expr (TEincdec (expr, op)) Tint, false

  | PEvars (idents, ptyp, pexprs) ->
    (* NOTE Création d'une variable, si son nom n'est pas "_" *)
    let create_var ptyp acc { id; loc } =
      match id with
      | "_" -> acc
      | _   -> acc @ [ new_var id loc ptyp false ] in

    (* NOTE Récupération du type si existant *)
    let get_typ ptyp =
      match type_opt structures ptyp with
      | Some ptyp -> ptyp
      | None -> error (Some loc)
                  (sprintf "undefined type %s of variable declaration" (get_ast_type_name ptyp)) in

    (* NOTE Vérification des types lors de l'assignement *)
    let check_type v_typ { expr_typ } =
      if not (eq_type v_typ expr_typ) then
        error (Some loc)
          (sprintf "cannot use type %s as type %s in assignment"
             (get_tast_type_name expr_typ) (get_tast_type_name v_typ)) in

    (* NOTE Transformation en TEvars *)
    let var_expr =
      match ptyp, pexprs with
      | Some ptyp, [] ->
        let typ = get_typ ptyp
        in TEvars(List.fold_left (create_var typ) [] idents)

      | Some ptyp, pexprs ->
        let typ = get_typ ptyp in

        if List.length idents <> List.length pexprs then
          error (Some loc)
            (sprintf "assignment mismatch: %d variables but %d values"
               (List.length idents) (List.length pexprs));

        let exprs = List.map (expr_no_return env) pexprs in
        List.iter (check_type typ) exprs;

        let vars = List.fold_left (create_var typ) [] idents in
        let idents = List.map (fun x -> new_stmt (TEident x)) vars in
        TEblock([new_stmt (TEvars(vars)); new_stmt (TEassign(idents, exprs))])

      | None, xs ->
        if List.length idents <> List.length pexprs then
          error (Some loc)
            (sprintf "assignment mismatch: %d variables but %d values"
               (List.length idents) (List.length pexprs));

        let exprs = List.map (expr_no_return env) pexprs in
        let vars = List.fold_left2 (fun acc { expr_typ } -> create_var expr_typ acc) [] exprs idents in
        let idents = List.map (fun x -> new_stmt (TEident x)) vars in
        TEblock([new_stmt (TEvars(vars)); new_stmt (TEassign(idents, exprs))])

    in new_stmt var_expr, false


(* 1. declare structures *)
let phase1 structures = function
  | PDstruct { ps_name = { id; loc } } ->
    (* NOTE Vérification de l'unicité des noms de structures *)
    if Context.elem id structures then
      error (Some loc) (sprintf "structure %s redeclared" id);

    (* NOTE Ajout des structures dans le contexte de typage sans les champs *)
    Context.add id { s_name = id; s_fields = Hashtbl.create 5 } structures

  | _ -> structures


(* 2. declare functions and type fields *)
let phase2 structures functions = function
  | PDfunction { pf_name = { id; loc }; pf_params = pl; pf_typ = tyl } ->
    (* NOTE Vérification de la fonction main sans paramètres et sans type de retour *)
    if id = "main" then (
      if pl <> [] || tyl <> [] then
        error (Some loc) "func main must have no parameters and no return values";
      found_main := true );

    (* NOTE Vérification de l'unicité des noms de fonctions *)
    if Context.elem id functions then
      error (Some loc) (sprintf "function %s redeclared" id);

    (* NOTE Vérification de l'unicité des noms des paramètres *)
    ( let is_same_identifier ({ id = id_x }, _) ({ id = id_y }, _) = id_x = id_y
      in match Lib.find_opt_duplicate_item is_same_identifier pl with
      | Some ({ id = id_param; loc = loc_param }, _) ->
        error (Some loc_param) (sprintf "duplicate parameter %s in function %s" id_param id)
      | None -> () );

    (* NOTE Vérification de la bonne formation de chacun des paramètres *)
    let fn_params =
      let param_to_var ({ id = id_param; loc = loc_param }, type_param) =
        match type_opt structures type_param with
        | Some typ -> new_var id_param loc_param typ false
        | None -> error (Some loc_param)
                    (sprintf "undefined type %s of parameter %s in function %s"
                       (get_ast_type_name type_param) id_param id)
      in List.map param_to_var pl in

    (* NOTE Vérification de la bonne formation de chacun des valeurs de retours *)
    let fn_typ =
      let get_type_return_value type_return_value =
        match type_opt structures type_return_value with
        | Some value -> value
        | None -> error (Some loc)
                    (sprintf "undefined type %s of one of return values in function %s"
                       (get_ast_type_name type_return_value) id)
      in list_type (List.map get_type_return_value tyl) in

    Context.add id { fn_name = id; fn_params; fn_typ } functions

  | PDstruct { ps_name = { id }; ps_fields = fl } ->
    let { s_fields } as structure = Context.get id structures in

    let process_struct_field ({ id = id_field; loc = loc_field }, type_field) =
      (* NOTE Vérification de l'unicité des champs de la structure *)
      if Hashtbl.mem s_fields id_field then
        error (Some loc_field) (sprintf "duplicate field %s in structure %s" id_field id);

      (* NOTE Vérification de la bonne formation du type *)
      let f_typ = match type_opt structures type_field with
        | Some f_typ -> f_typ;
        | None -> error (Some loc_field)
                    (sprintf "undefined type %s of field %s in structure %s"
                       (get_ast_type_name type_field) id_field id) in

      Hashtbl.add s_fields id_field { f_name = id_field; f_typ; f_ofs = 0 } in

    List.iter process_struct_field fl;
    functions


(* 3. type check function bodies *)
let rec sizeof = function
  | Tint | Tbool | Tstring | Tptr _ -> 8

  | Tstruct { s_fields } -> Seq.fold_left (fun acc { f_typ } -> acc + sizeof f_typ) 0 (Hashtbl.to_seq_values s_fields)

  | _ -> (* TODO *) assert false


let decl structures functions = function
  | PDfunction { pf_name = { id; loc }; pf_body = e; pf_typ = tyl } ->
    let fn = Context.get id functions in

    let context =
      let add_param_to_context context param = Context.add param.v_name param context
      in List.fold_left add_param_to_context Context.create fn.fn_params in

    let e, rt = expr structures functions context e in

    (* NOTE Vérification de chacun des return *)
    ( let rec iter_return_stmt f { expr_desc } =
        match expr_desc with
        | TEblock expr_list -> List.iter (iter_return_stmt f) expr_list
        | TEif (_, expr, _) -> iter_return_stmt f expr
        | TEfor (_, expr) -> iter_return_stmt f expr
        | TEreturn return_values -> f return_values
        | _ -> () in

      let check_return_type { fn_name; fn_typ } return_values =
        let return_type = list_type (List.map (fun { expr_typ } -> expr_typ) return_values) in
        if not (eq_type fn_typ return_type) then
          error (Some loc)
            (sprintf "bad return type %s, expected %s in function %s"
               (get_tast_type_name return_type) (get_tast_type_name fn_typ) fn_name)

      in iter_return_stmt (check_return_type fn) e );

    (* NOTE Vérification du branche du flot d'exécution *)
    ( match fn.fn_typ, rt with
      | Tvoid, _ -> ()
      | _, false -> error (Some loc) (sprintf "missing return at end of function %s" fn.fn_name)
      | _, _ -> () );

    TDfunction (fn, e)

  | PDstruct { ps_name = { id; loc } } ->
    let structure = Context.get id structures in

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
      | Some { f_name } ->
        error (Some loc) (sprintf "recursive field %s in the struct %s" f_name id)
      | None -> ());

    TDstruct structure


let file (imp, dl) =
  let structures = List.fold_left phase1 Context.create dl in
  let functions = List.fold_left (phase2 structures) Context.create dl in

  if not !found_main then error None "missing method main";

  let dl = List.map (decl structures functions) dl in

  if imp && not !fmt_used then error None "fmt imported but not used";
  if !fmt_used && not imp then error None "fmt used but not imported";

  dl
