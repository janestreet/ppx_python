open Base
open Ppxlib
open Ast_builder.Default

let default =
  Attribute.declare
    "python.default"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
    (fun x -> x)
;;

let option =
  Attribute.declare
    "python.option"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    (fun x -> x)
;;

let disallow_extra_fields =
  Attribute.declare
    "python.disallow_extra_fields"
    Attribute.Context.type_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let lident ~loc str = Loc.make ~loc (Lident str)

let fresh_label =
  let counter = ref 0 in
  fun ~loc ->
    Int.incr counter;
    let label = Printf.sprintf "_lbl_%d" !counter in
    ppat_var (Loc.make ~loc label) ~loc, pexp_ident (lident ~loc label) ~loc
;;

let raise_errorf ~loc fmt = Location.raise_errorf ~loc (Stdlib.( ^^ ) "ppx_python: " fmt)

(* Generated function names. *)
let python_of tname = "python_of_" ^ tname
let of_python tname = tname ^ "_of_python"

(* For parameterized types, use these function names as arguments. *)
let python_of_arg tname = "__python_of_" ^ tname
let of_python_arg tname = "__" ^ tname ^ "_of_python"

let app_list ~loc (func : expression) (args : expression list) =
  [%expr [%e func] [%e elist ~loc args]]
;;

let curry_app_list ~loc (func : expression) (args : expression list) =
  List.fold_left args ~init:func ~f:(fun acc arg -> [%expr [%e acc] [%e arg]])
;;

let fun_multi ~loc (args : label list) (body : expression) =
  List.fold_right args ~init:body ~f:(fun arg acc ->
    pexp_fun Nolabel ~loc None (ppat_var (Loc.make ~loc arg) ~loc) acc)
;;

let closure_of_fn (fn : expression -> expression) ~loc : expression =
  let loc = { loc with loc_ghost = true } in
  let arg_pat, arg_expr = fresh_label ~loc in
  pexp_fun Nolabel ~loc None arg_pat (fn arg_expr)
;;

module Signature : sig
  val gen
    :  [ `to_ | `of_ | `both ]
    -> (signature_item list, rec_flag * type_declaration list) Deriving.Generator.t
end = struct
  let of_td ~kind td : signature_item list =
    let { Location.loc; txt = tname } = td.ptype_name in
    let to_python_type =
      List.fold_left
        (List.rev td.ptype_params)
        ~init:[%type: [%t Ppxlib.core_type_of_type_declaration td] -> Pytypes.pyobject]
        ~f:(fun acc (tvar, _variance) ->
          [%type: ([%t tvar] -> Pytypes.pyobject) -> [%t acc]])
    in
    let of_python_type =
      List.fold_left
        (List.rev td.ptype_params)
        ~init:[%type: Pytypes.pyobject -> [%t Ppxlib.core_type_of_type_declaration td]]
        ~f:(fun acc (tvar, _variance) ->
          [%type: (Pytypes.pyobject -> [%t tvar]) -> [%t acc]])
    in
    let psig_value ~name ~type_ =
      psig_value ~loc (value_description ~loc ~name:(Loc.make name ~loc) ~type_ ~prim:[])
    in
    match kind with
    | `both ->
      [ psig_value ~name:(python_of tname) ~type_:to_python_type
      ; psig_value ~name:(of_python tname) ~type_:of_python_type
      ]
    | `to_ -> [ psig_value ~name:(python_of tname) ~type_:to_python_type ]
    | `of_ -> [ psig_value ~name:(of_python tname) ~type_:of_python_type ]
  ;;

  let gen kind =
    Deriving.Generator.make_noarg (fun ~loc:_ ~path:_ (_rec_flag, tds) ->
      List.concat_map tds ~f:(of_td ~kind))
  ;;
end

module Structure : sig
  val of_python_ty : core_type -> expression -> expression
  val to_python_ty : core_type -> expression -> expression

  val gen
    :  [ `to_ | `of_ | `both ]
    -> (structure, rec_flag * type_declaration list) Deriving.Generator.t
end = struct
  let change_lidloc_suffix ~f lid =
    Located.map
      (function
        | Lident str -> Lident (f str)
        | Ldot (m, str) -> Ldot (m, f str)
        | Lapply _ -> raise_errorf ~loc:lid.loc "lapply not supported")
      lid
  ;;

  let rec handle_core_type ~tuple ~var ~constr ~polymorphic_variant ct v =
    let loc = { ct.ptyp_loc with loc_ghost = true } in
    match Ppxlib_jane.Shim.Core_type_desc.of_parsetree ct.ptyp_desc with
    | Ptyp_tuple labeled_core_types ->
      (match Ppxlib_jane.as_unlabeled_tuple labeled_core_types with
       | Some core_types -> tuple ~loc core_types v
       | None -> raise_errorf ~loc "labeled tuples not supported")
    | Ptyp_var (tv, _) -> [%expr [%e pexp_ident ~loc (lident (var tv) ~loc)] [%e v]]
    | Ptyp_constr (longident_loc, args) ->
      let lid_loc = change_lidloc_suffix ~f:constr longident_loc in
      let args =
        List.map args ~f:(fun arg ->
          let arg_fn = handle_core_type ~tuple ~var ~constr ~polymorphic_variant arg in
          closure_of_fn ~loc arg_fn)
        @ [ v ]
      in
      curry_app_list (pexp_ident lid_loc ~loc) args ~loc
    | Ptyp_alias (alias, _, _) ->
      handle_core_type ~tuple ~var ~constr ~polymorphic_variant alias v
    | Ptyp_variant (row_fields, Closed, None) -> polymorphic_variant row_fields ~loc v
    | Ptyp_variant (_, _, _) ->
      raise_errorf
        ~loc
        "'%a' not supported, only closed variants with no labels are supported"
        Pprintast.core_type
        ct
    | _ -> raise_errorf ~loc "'%a' not supported" Pprintast.core_type ct
  ;;

  let rec of_python_ty core_type v =
    handle_core_type
      ~tuple:(of_python_tuple ~wrap:Fn.id)
      ~var:of_python_arg
      ~constr:of_python
      ~polymorphic_variant:of_python_polymorphic_variant
      core_type
      v

  and of_python_tuple ~wrap ~loc core_types v =
    let list =
      List.mapi core_types ~f:(fun i core_type ->
        [%expr
          let t = Py.Tuple.get_item [%e v] [%e eint i ~loc] in
          [%e of_python_ty core_type [%expr t]]])
    in
    let tuple_len = eint (List.length core_types) ~loc in
    [%expr
      if not (Py.Tuple.check [%e v])
      then Printf.sprintf "not a python tuple %s" (Py.Object.to_string [%e v]) |> failwith;
      let p_len = Py.Tuple.size [%e v] in
      if p_len <> [%e tuple_len]
      then Printf.sprintf "tuple size mismatch %d <> %d" [%e tuple_len] p_len |> failwith;
      [%e wrap (pexp_tuple ~loc list)]]

  and of_python_polymorphic_variant row_fields ~loc v =
    let match_cases ~args =
      List.map row_fields ~f:(fun { prf_desc; prf_loc = loc; prf_attributes = _ } ->
        match prf_desc with
        | Rinherit _ -> raise_errorf ~loc "inherited polymorphic variant not supported"
        | Rtag (label, has_constant_constructor, ctors) ->
          let rhs args = pexp_variant ~loc label.txt args in
          let rhs =
            match ctors, has_constant_constructor with
            | [], _ -> rhs None
            | [ core_type ], false -> rhs (Some (of_python_ty core_type args))
            | [ _ ], true ->
              raise_errorf ~loc "cannot have both a constant and non-constant constructor"
            | _, _ -> raise_errorf ~loc "multiple constructors are not supported"
          in
          case
            ~lhs:(ppat_constant ~loc (Pconst_string (label.txt, loc, None)))
            ~guard:None
            ~rhs)
      @ [ case
            ~lhs:[%pat? cstor]
            ~guard:None
            ~rhs:[%expr failwith (Printf.sprintf "unexpected constructor %s" cstor)]
        ]
    in
    [%expr
      if not (Py.Tuple.check [%e v])
      then Printf.sprintf "not a python tuple %s" (Py.Object.to_string [%e v]) |> failwith;
      let p_len = Py.Tuple.size [%e v] in
      if p_len <> 2
      then Printf.sprintf "not a python pair %s" (Py.Object.to_string [%e v]) |> failwith;
      let cstor, _args = Py.Tuple.to_pair [%e v] in
      let cstor = Py.String.to_string cstor in
      [%e pexp_match ~loc [%expr cstor] (match_cases ~args:[%expr _args])]]
  ;;

  let of_python_fields fields ~wrap ~loc ~allow_extra_fields v =
    let record_fields =
      List.map fields ~f:(fun field ->
        let name_as_string = estring ~loc field.pld_name.txt in
        let default_branch =
          match Attribute.get default field with
          | Some default -> default
          | None ->
            (match Attribute.get option field with
             | Some _ -> [%expr None]
             | None ->
               [%expr
                 Printf.sprintf "cannot find field %s in dict" [%e name_as_string]
                 |> failwith])
        in
        let expr =
          [%expr
            match Ppx_python_runtime.Dict_str_keys.find [%e v] [%e name_as_string] with
            | exception (Stdlib.Not_found | Not_found_s _) -> [%e default_branch]
            | v ->
              __pyocaml_field_read := !__pyocaml_field_read + 1;
              [%e of_python_ty field.pld_type [%expr v]]]
        in
        lident field.pld_name.txt ~loc, expr)
    in
    let check_extra_fields =
      if allow_extra_fields
      then [%expr ()]
      else (
        (* The [fail_on_extra_fields] bit is slow but this is ok as it's only used
           when an error is generated so outside of the path that we're optimizing
           for. *)
        let field_names =
          List.map fields ~f:(fun field -> estring ~loc field.pld_name.txt) |> elist ~loc
        in
        [%expr
          if !__pyocaml_field_read <> Py.Dict.size [%e v]
          then
            Ppx_python_runtime.Dict_str_keys.fail_on_extra_fields
              [%e v]
              ~expected_field_names:[%e field_names]])
    in
    [%expr
      if not (Py.Dict.check [%e v])
      then Printf.sprintf "not a python dict %s" (Py.Object.to_string [%e v]) |> failwith;
      let __pyocaml_field_read = ref 0 in
      let __pyocaml_res = [%e wrap (pexp_record record_fields ~loc None)] in
      [%e check_extra_fields];
      __pyocaml_res]
  ;;

  let of_python_variant variant ~loc ~allow_extra_fields v =
    let match_cases ~args =
      List.map variant ~f:(fun variant ->
        let rhs args = pexp_construct ~loc (lident ~loc variant.pcd_name.txt) args in
        let rhs =
          match variant.pcd_args with
          | Pcstr_tuple [] -> rhs None
          | Pcstr_tuple tuple_args ->
            let core_types =
              List.map tuple_args ~f:Ppxlib_jane.Shim.Pcstr_tuple_arg.to_core_type
            in
            of_python_tuple core_types args ~loc ~wrap:(fun v -> rhs (Some v))
          | Pcstr_record fields ->
            of_python_fields
              fields
              ~loc
              args
              ~wrap:(fun record -> rhs (Some record))
              ~allow_extra_fields
        in
        case
          ~lhs:(ppat_constant ~loc (Pconst_string (variant.pcd_name.txt, loc, None)))
          ~guard:None
          ~rhs)
      @ [ case
            ~lhs:[%pat? cstor]
            ~guard:None
            ~rhs:[%expr failwith (Printf.sprintf "unexpected constructor %s" cstor)]
        ]
    in
    [%expr
      if not (Py.Tuple.check [%e v])
      then Printf.sprintf "not a python tuple %s" (Py.Object.to_string [%e v]) |> failwith;
      let p_len = Py.Tuple.size [%e v] in
      if p_len <> 2
      then Printf.sprintf "not a python pair %s" (Py.Object.to_string [%e v]) |> failwith;
      let cstor, _args = Py.Tuple.to_pair [%e v] in
      let cstor = Py.String.to_string cstor in
      [%e pexp_match ~loc [%expr cstor] (match_cases ~args:[%expr _args])]]
  ;;

  let rec to_python_ty core_type v =
    let tuple ~loc core_types v =
      let pat, expr = to_python_tuple ~loc core_types in
      pexp_let ~loc Nonrecursive [ value_binding ~loc ~pat ~expr:v ] expr
    in
    handle_core_type
      ~tuple
      ~var:python_of_arg
      ~constr:python_of
      ~polymorphic_variant:to_polymorphic_variant
      core_type
      v

  and to_python_tuple ~loc core_types =
    let var_name i = "t" ^ Int.to_string i in
    let pat =
      List.mapi core_types ~f:(fun i _ -> ppat_var ~loc (Loc.make (var_name i) ~loc))
      |> ppat_tuple ~loc
    in
    let list =
      List.mapi core_types ~f:(fun i core_type ->
        to_python_ty core_type (pexp_ident (lident (var_name i) ~loc) ~loc))
    in
    pat, app_list [%expr Py.Tuple.of_list] ~loc list

  and to_polymorphic_variant row_fields ~loc v =
    let match_cases =
      List.map row_fields ~f:(fun { prf_desc; prf_loc = loc; prf_attributes = _ } ->
        match prf_desc with
        | Rinherit _ -> raise_errorf ~loc "inherited polymorphic variant not supported"
        | Rtag (label, has_constant_constructor, ctors) ->
          let constructor = estring ~loc label.txt in
          let args_lhs, args_rhs =
            match ctors, has_constant_constructor with
            | [], _ -> None, [%expr Py.none]
            | [ core_type ], false -> Some [%pat? t], to_python_ty core_type [%expr t]
            | [ _ ], true ->
              raise_errorf ~loc "cannot have both a constant and non-constant constructor"
            | _, _ -> raise_errorf ~loc "multiple constructors are not supported"
          in
          case
            ~lhs:(ppat_variant ~loc label.txt args_lhs)
            ~guard:None
            ~rhs:
              [%expr
                Py.Tuple.of_pair (Py.String.of_string [%e constructor], [%e args_rhs])])
    in
    pexp_match ~loc v match_cases
  ;;

  let to_python_fields fields ~loc v =
    let mandatory_fields, optional_fields =
      List.partition_tf fields ~f:(fun field ->
        Attribute.get option field |> Option.is_none)
    in
    let mandatory_fields =
      List.map mandatory_fields ~f:(fun field ->
        let name_as_string = estring ~loc field.pld_name.txt in
        let value = pexp_field v (lident ~loc field.pld_name.txt) ~loc in
        [%expr [%e name_as_string], [%e to_python_ty field.pld_type value]])
    in
    let mandatory_dict =
      app_list ~loc [%expr Ppx_python_runtime.Dict_str_keys.create] mandatory_fields
    in
    if List.is_empty optional_fields
    then mandatory_dict
    else (
      let optional_setters =
        List.map optional_fields ~f:(fun field ->
          let name_as_string = estring ~loc field.pld_name.txt in
          let value = pexp_field v (lident ~loc field.pld_name.txt) ~loc in
          let pat_ident = lident ~loc "pat_value" |> pexp_ident ~loc in
          [%expr
            match [%e value] with
            | None -> ()
            | Some _ as pat_value ->
              Ppx_python_runtime.Dict_str_keys.set
                dict
                [%e name_as_string]
                [%e to_python_ty field.pld_type pat_ident]])
      in
      [%expr
        let dict = [%e mandatory_dict] in
        [%e esequence ~loc optional_setters];
        dict])
  ;;

  let to_python_variant variant ~loc v =
    let match_cases =
      List.map variant ~f:(fun variant ->
        let constructor = estring ~loc variant.pcd_name.txt in
        let args_lhs, args_rhs =
          match variant.pcd_args with
          | Pcstr_tuple [] -> None, [%expr Py.none]
          | Pcstr_tuple args ->
            let core_types =
              List.map args ~f:Ppxlib_jane.Shim.Pcstr_tuple_arg.to_core_type
            in
            let pat, expr = to_python_tuple ~loc core_types in
            Some pat, expr
          | Pcstr_record fields -> Some [%pat? t], to_python_fields fields ~loc [%expr t]
        in
        case
          ~lhs:(ppat_construct ~loc (lident ~loc variant.pcd_name.txt) args_lhs)
          ~guard:None
          ~rhs:
            [%expr Py.Tuple.of_pair (Py.String.of_string [%e constructor], [%e args_rhs])])
    in
    pexp_match ~loc v match_cases
  ;;

  let expr_of_td ~tvar_wrapper ~type_expr ~variant ~record td =
    let { Location.loc; txt = _ } = td.ptype_name in
    let tvars =
      List.map td.ptype_params ~f:(fun (te, _variance) ->
        match Ppxlib_jane.Shim.Core_type_desc.of_parsetree te.ptyp_desc with
        | Ptyp_var (lbl, _) -> tvar_wrapper lbl
        | _ ->
          (* we've called [name_type_params_in_td] *)
          assert false)
    in
    let expr arg_t =
      match td.ptype_kind with
      | Ptype_abstract ->
        (match td.ptype_manifest with
         | None -> raise_errorf ~loc "abstract types not yet supported"
         | Some ty -> type_expr ty arg_t)
      | Ptype_variant cstrs -> variant cstrs ~loc arg_t
      | Ptype_record fields -> record fields ~loc arg_t
      | Ptype_open -> raise_errorf ~loc "open types not yet supported"
    in
    fun_multi ~loc tvars (closure_of_fn expr ~loc)
  ;;

  let gen kind =
    let attributes =
      match kind with
      | `both | `of_ ->
        [ Attribute.T default; Attribute.T option; Attribute.T disallow_extra_fields ]
      | `to_ -> []
    in
    Deriving.Generator.make_noarg ~attributes (fun ~loc ~path:_ (rec_flag, tds) ->
      let tds = List.map tds ~f:name_type_params_in_td in
      let of_python_bindings () =
        List.map tds ~f:(fun td ->
          let pat =
            let { Location.loc; txt = tname } = td.ptype_name in
            let name = of_python tname in
            ppat_var ~loc (Loc.make name ~loc)
          in
          let expr =
            let allow_extra_fields =
              Option.is_none (Attribute.get disallow_extra_fields td)
            in
            expr_of_td
              ~tvar_wrapper:of_python_arg
              ~type_expr:of_python_ty
              ~variant:(of_python_variant ~allow_extra_fields)
              ~record:(of_python_fields ~wrap:Fn.id ~allow_extra_fields)
              td
          in
          value_binding ~loc ~pat ~expr)
      in
      let to_python_bindings () =
        List.map tds ~f:(fun td ->
          let pat =
            let { Location.loc; txt = tname } = td.ptype_name in
            let name = python_of tname in
            ppat_var ~loc (Loc.make name ~loc)
          in
          let expr =
            expr_of_td
              ~tvar_wrapper:python_of_arg
              ~type_expr:to_python_ty
              ~variant:to_python_variant
              ~record:to_python_fields
              td
          in
          value_binding ~loc ~pat ~expr)
      in
      let bindings =
        match kind with
        | `both -> to_python_bindings () @ of_python_bindings ()
        | `to_ -> to_python_bindings ()
        | `of_ -> of_python_bindings ()
      in
      [ pstr_value ~loc (really_recursive rec_flag tds) bindings ])
  ;;
end

let python =
  Deriving.add
    "python"
    ~str_type_decl:(Structure.gen `both)
    ~sig_type_decl:(Signature.gen `both)
;;

module Python_of = struct
  let name = "python_of"
  let extension ~loc ~path:_ ctyp = closure_of_fn (Structure.to_python_ty ctyp) ~loc

  let deriver =
    Deriving.add
      name
      ~str_type_decl:(Structure.gen `to_)
      ~sig_type_decl:(Signature.gen `to_)
      ~extension
  ;;
end

module Of_python = struct
  let name = "of_python"
  let extension ~loc ~path:_ ctyp = closure_of_fn (Structure.of_python_ty ctyp) ~loc

  let deriver =
    Deriving.add
      name
      ~str_type_decl:(Structure.gen `of_)
      ~sig_type_decl:(Signature.gen `of_)
      ~extension
  ;;
end

(* [py_string_expressions] maps from the string literal to the expression binding *)
let py_string_expressions = Hashtbl.create (module String)

let expand ~expr_loc ~string_loc ~string =
  let loc = { string_loc with loc_ghost = true } in
  let expr_var, _expr =
    Hashtbl.find_or_add py_string_expressions string ~default:(fun () ->
      let expr = [%expr lazy (Py.String.of_string [%e estring string ~loc])] in
      let len = Hashtbl.length py_string_expressions in
      let expr_var = Printf.sprintf "py_string_%d" len in
      expr_var, expr)
  in
  [%expr Lazy.force [%e evar expr_var ~loc:expr_loc]]
;;

let () =
  Ppxlib.Driver.register_transformation
    "ppx_python_conv"
    ~rules:
      [ Context_free.Rule.extension
          (Extension.declare
             "ppx_python_conv.py_string"
             Extension.Context.expression
             Ast_pattern.(
               pstr (pstr_eval (pexp_constant (pconst_string __ __ drop)) nil ^:: nil))
             (* [delimiter] can be things like "\n". This comes up if we use a multi-line
                string. *)
               (fun ~loc:expr_loc ~path:_ string string_loc ->
               Merlin_helpers.hide_expression (expand ~expr_loc ~string_loc ~string)))
      ]
    ~impl:(fun structure ->
      let loc = Location.none in
      let header =
        Hashtbl.data py_string_expressions
        |> List.map ~f:(fun (key, value) -> [%stri let [%p pvar key ~loc] = [%e value]])
      in
      Hashtbl.clear py_string_expressions;
      header @ structure)
;;
