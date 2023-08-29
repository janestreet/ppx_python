open Core
open Ppx_python_runtime

module type S = sig
  type _t = { field_a : int } [@@deriving python]
end

type t =
  { field_a : int
  ; field_b : string
  }
[@@deriving python, sexp]

type u =
  { foo : int * int
  ; bar : t
  }
[@@deriving python]

type v =
  | A
  | B of string
  | C of int
  | D of t * string
  | E of
      { x : int
      ; y : string
      }
[@@deriving python, sexp]

let%expect_test "t" =
  if not (Py.is_initialized ()) then Py.initialize ~version:3 ();
  let t = { field_a = 42; field_b = "foobar" } in
  let pyobject = python_of_t t in
  let items =
    Py.Dict.to_bindings_string pyobject |> List.sort ~compare:Stdlib.compare
  in
  List.iter items ~f:(fun (key, value) ->
    printf "%s: %s\n%!" key (Py.Object.to_string value));
  [%expect {|
    field_a: 42
    field_b: foobar |}];
  let t = t_of_python pyobject in
  printf !"%{Sexp}\n%!" (sexp_of_t t);
  [%expect {| ((field_a 42)(field_b foobar)) |}]
;;

let%expect_test "v" =
  if not (Py.is_initialized ()) then Py.initialize ~version:3 ();
  let v = D ({ field_a = 42; field_b = "foobar" }, "pi") in
  let pyobject = python_of_v v in
  let v = v_of_python pyobject in
  printf !"%{Sexp}\n%!" (sexp_of_v v);
  [%expect {|
    (D((field_a 42)(field_b foobar))pi) |}];
  let v = E { x = 42; y = "foobar" } in
  let pyobject = python_of_v v in
  let v = v_of_python pyobject in
  printf !"%{Sexp}\n%!" (sexp_of_v v);
  [%expect {| (E(x 42)(y foobar)) |}]
;;

module M : sig
  type t = int [@@deriving python, sexp]

  type 'a u =
    | A of int
    | B of 'a
  [@@deriving python, sexp]
end = struct
  type t = int [@@deriving python, sexp]

  type 'a u =
    | A of int
    | B of 'a
  [@@deriving python, sexp]
end

let%expect_test "M.u" =
  if not (Py.is_initialized ()) then Py.initialize ~version:3 ();
  let v = M.B 12 in
  let pyobject = M.python_of_u python_of_int v in
  let v = M.u_of_python int_of_python pyobject in
  printf !"%{sexp:int M.u}\n%!" v;
  [%expect {|
  (B 12) |}]
;;

type 'a w =
  | One of 'a
  | Multi of 'a list
[@@deriving python, sexp]

let%expect_test "w" =
  if not (Py.is_initialized ()) then Py.initialize ~version:3 ();
  let v = Multi [ 1; 2; 3; 4 ] in
  let pyobject = python_of_w python_of_int v in
  let v = w_of_python int_of_python pyobject in
  printf !"%{sexp:int w}\n%!" v;
  [%expect {|
    (Multi (1 2 3 4)) |}]
;;

type 'a tree =
  | Leaf of 'a
  | Node of 'a tree * 'a tree
[@@deriving python, sexp]

let%expect_test "tree" =
  if not (Py.is_initialized ()) then Py.initialize ~version:3 ();
  let v = Node (Leaf "test", Node (Leaf "foo", Leaf "bar")) in
  let pyobject = python_of_tree python_of_string v in
  let v = tree_of_python string_of_python pyobject in
  printf !"%{sexp:string tree}\n%!" v;
  [%expect {|
    (Node (Leaf test) (Node (Leaf foo) (Leaf bar))) |}]
;;

(* Check that unused type variables are not an issue. *)
type 'a z1 = int [@@deriving python]

(* Check that underscores are not an issue neither. *)
type _ z2 = int [@@deriving python]

let%expect_test "type-var" =
  if not (Py.is_initialized ()) then Py.initialize ~version:3 ();
  let round_trip1 v =
    let pyobject = python_of_z1 (fun _ -> assert false) v in
    z1_of_python (fun _ -> assert false) pyobject
  in
  let round_trip2 v =
    let pyobject = python_of_z2 (fun _ -> assert false) v in
    z2_of_python (fun _ -> assert false) pyobject
  in
  printf !"%d %d\n%!" (round_trip1 42) (round_trip2 42);
  [%expect {|
    42 42 |}]
;;

module type Test = sig
  type 'a t1 = int [@@deriving python]
  type _ t2 = int [@@deriving python]
  type u1 = int t1 [@@deriving python]
  type u2 = int t2 [@@deriving python]
end

type runtime_types =
  { bool : bool
  ; int : int
  ; float : float
  ; string : string
  ; array : (float * string) array
  ; list : (string list * bool) list
  ; option : int option option
  }
[@@deriving python, sexp]

let%expect_test "runtime-types" =
  if not (Py.is_initialized ()) then Py.initialize ~version:3 ();
  List.iter
    ~f:(fun v ->
      printf
        !"%{sexp:runtime_types}\n%!"
        (python_of_runtime_types v |> runtime_types_of_python))
    [ { bool = true
      ; int = 42
      ; float = 3.1415
      ; string = "foobar"
      ; array = [| 1., "one" |]
      ; list = []
      ; option = None
      }
    ; { bool = true
      ; int = 1337
      ; float = 2.71828
      ; string = "another-string"
      ; array = [||]
      ; list = [ [ "a"; "b" ], true; [], false ]
      ; option = Some None
      }
    ];
  [%expect
    {|
    ((bool true) (int 42) (float 3.1415) (string foobar) (array ((1 one)))
     (list ()) (option ()))
    ((bool true) (int 1337) (float 2.71828) (string another-string) (array ())
     (list (((a b) true) (() false))) (option ())) |}]
;;

let%expect_test "of-python-errors" =
  if not (Py.is_initialized ()) then Py.initialize ~version:3 ();
  let expect_exn f =
    let success =
      try
        f ();
        true
      with
      | exn ->
        printf !"ocaml exn: %{Exn}" exn;
        false
    in
    if success then failwith "an exception was expected"
  in
  expect_exn (fun () -> ignore (t_of_python (Py.String.of_string "test") : t));
  [%expect {| ocaml exn: (Failure "not a python dict test") |}];
  expect_exn (fun () -> ignore (t_of_python (python_of_v A)));
  [%expect {| ocaml exn: (Failure "not a python dict ('A', None)") |}];
  expect_exn (fun () ->
    ignore
      (t_of_python (python_of_u { foo = 1, 2; bar = { field_a = 1; field_b = "test" } })));
  [%expect {| ocaml exn: (Failure "cannot find field field_b in dict") |}]
;;

let%expect_test "python_of-of_python" =
  if not (Py.is_initialized ()) then Py.initialize ~version:3 ();
  let pyobject = [%python_of: int * float] (42, 1.337) in
  print_endline (Py.Object.to_string pyobject);
  [%expect {| (42, 1.337) |}];
  let forty_two, pi = [%of_python: int * float] pyobject in
  printf "%d %.3f\n%!" forty_two pi;
  [%expect {| 42 1.337 |}];
  let pyobject =
    [%python_of: int list * t] ([ 3; 1; 4; 1; 5 ], { field_a = 42; field_b = "foo" })
  in
  print_endline (Py.Object.to_string pyobject);
  [%expect {| ([3, 1, 4, 1, 5], {'field_a': 42, 'field_b': 'foo'}) |}];
  let list, t = [%of_python: int list * t] pyobject in
  printf !"%{sexp:int list} %{sexp:t}\n%!" list t;
  [%expect {| (3 1 4 1 5) ((field_a 42) (field_b foo)) |}]
;;

type t_with_default =
  { field_a : int
  ; field_b : string [@python.default "foo"]
  }
[@@deriving python, sexp]

let%expect_test "default" =
  if not (Py.is_initialized ()) then Py.initialize ~version:3 ();
  let t_with_default = { field_a = 42; field_b = "bar" } in
  let pyobject = python_of_t_with_default t_with_default in
  print_endline (Py.Object.to_string pyobject);
  [%expect {| {'field_a': 42, 'field_b': 'bar'} |}];
  let t_with_default = t_with_default_of_python pyobject in
  printf !"%{sexp:t_with_default}\n%!" t_with_default;
  [%expect {| ((field_a 42) (field_b bar)) |}];
  let pyobject = Py.Dict.create () in
  Py.Dict.set_item_string pyobject "field_a" (python_of_int 1337);
  let t_with_default = t_with_default_of_python pyobject in
  printf !"%{sexp:t_with_default}\n%!" t_with_default;
  [%expect {| ((field_a 1337) (field_b foo)) |}]
;;

type t_with_option =
  { field_a : int
  ; field_b : (string * float) option [@python.option]
  ; field_c : int option [@python.option]
  }
[@@deriving python, sexp]

let%expect_test "option" =
  if not (Py.is_initialized ()) then Py.initialize ~version:3 ();
  let t_with_option = { field_a = 42; field_b = Some ("foo", 3.14); field_c = None } in
  let pyobject = python_of_t_with_option t_with_option in
  print_endline (Py.Object.to_string pyobject);
  [%expect {| {'field_a': 42, 'field_b': ('foo', 3.14)} |}];
  let t_with_option = t_with_option_of_python pyobject in
  printf !"%{sexp:t_with_option}\n%!" t_with_option;
  [%expect {| ((field_a 42) (field_b ((foo 3.14))) (field_c ())) |}];
  let pyobject = Py.Dict.create () in
  Py.Dict.set_item_string pyobject "field_a" (python_of_int 1337);
  Py.Dict.set_item_string pyobject "field_c" (python_of_int 42);
  let t_with_option = t_with_option_of_python pyobject in
  printf !"%{sexp:t_with_option}\n%!" t_with_option;
  [%expect {| ((field_a 1337) (field_b ()) (field_c (42))) |}]
;;

type t_python_of =
  { foo : int
  ; bar : float option
  }
[@@deriving python_of]

let%expect_test "python-of" =
  if not (Py.is_initialized ()) then Py.initialize ~version:3 ();
  let t = { foo = 42; bar = Some 3.1415 } in
  let pyobject = python_of_t_python_of t in
  print_endline (Py.Object.to_string pyobject);
  [%expect {| {'foo': 42, 'bar': 3.1415} |}]
;;

type t_of_python =
  { foo : int
  ; bar : float option
  }
[@@deriving of_python, sexp]

let%expect_test "python-of" =
  if not (Py.is_initialized ()) then Py.initialize ~version:3 ();
  let pyobject = Py.Dict.create () in
  Py.Dict.set_item_string pyobject "foo" (python_of_int 1337);
  Py.Dict.set_item_string pyobject "bar" (python_of_float 2.71828182846);
  let t = t_of_python_of_python pyobject in
  printf !"%{sexp:t_of_python}\n%!" t;
  [%expect {| ((foo 1337) (bar (2.71828182846))) |}]
;;

module _ : sig
  (* Export the type to check the mli generation too. *)
  type 'a l =
    | Empty
    | Cons of 'a * 'a l
  [@@deriving python]

  type int_tree =
    | Leaf of int
    | Node of int tree * int tree
  [@@deriving python]
end = struct
  type 'a l =
    | Empty
    | Cons of 'a * 'a l
  [@@deriving python, sexp]

  let%expect_test "rec" =
    if not (Py.is_initialized ()) then Py.initialize ~version:3 ();
    List.iter
      [ Empty; Cons ("foo", Empty); Cons ("foo", Cons ("bar", Empty)) ]
      ~f:(fun l ->
        printf !"%{sexp:string l}\n%!" l;
        let pyobject = python_of_l python_of_string l in
        print_endline (Py.Object.to_string pyobject);
        printf !"%{sexp:string l}\n%!" (l_of_python string_of_python pyobject));
    [%expect
      {|
      Empty
      ('Empty', None)
      Empty
      (Cons foo Empty)
      ('Cons', ('foo', ('Empty', None)))
      (Cons foo Empty)
      (Cons foo (Cons bar Empty))
      ('Cons', ('foo', ('Cons', ('bar', ('Empty', None)))))
      (Cons foo (Cons bar Empty)) |}]
  ;;

  type int_tree =
    | Leaf of int
    | Node of int tree * int tree
  [@@deriving python, sexp]

  let%expect_test "rec" =
    if not (Py.is_initialized ()) then Py.initialize ~version:3 ();
    List.iter
      [ Leaf 42
      ; Node (Leaf 1, Leaf 2)
      ; Node (Node (Leaf 1, Node (Leaf 2, Leaf 3)), Leaf 4)
      ]
      ~f:(fun tree ->
        printf !"%{sexp:int_tree}\n%!" tree;
        let pyobject = python_of_int_tree tree in
        print_endline (Py.Object.to_string pyobject);
        printf !"%{sexp:int_tree}\n%!" (int_tree_of_python pyobject));
    [%expect
      {|
      (Leaf 42)
      ('Leaf', (42,))
      (Leaf 42)
      (Node (Leaf 1) (Leaf 2))
      ('Node', (('Leaf', (1,)), ('Leaf', (2,))))
      (Node (Leaf 1) (Leaf 2))
      (Node (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))) (Leaf 4))
      ('Node', (('Node', (('Leaf', (1,)), ('Node', (('Leaf', (2,)), ('Leaf', (3,)))))), ('Leaf', (4,))))
      (Node (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))) (Leaf 4)) |}]
  ;;
end

module _ : sig
  type t =
    | Base of int
    | App of t * u

  and u = Lam of t [@@deriving python, sexp]
end = struct
  type t =
    | Base of int
    | App of t * u

  and u = Lam of t [@@deriving python, sexp]

  let%expect_test "mut-rec" =
    if not (Py.is_initialized ()) then Py.initialize ~version:3 ();
    let t = App (Base 42, Lam (App (Base 299792458, Lam (Base 1337)))) in
    printf !"%{sexp:t}\n%!" t;
    let pyobject = python_of_t t in
    print_endline (Py.Object.to_string pyobject);
    printf !"%{sexp:t}\n%!" (t_of_python pyobject);
    [%expect
      {|
      (App (Base 42) (Lam (App (Base 299792458) (Lam (Base 1337)))))
      ('App', (('Base', (42,)), ('Lam', (('App', (('Base', (299792458,)), ('Lam', (('Base', (1337,)),)))),))))
      (App (Base 42) (Lam (App (Base 299792458) (Lam (Base 1337))))) |}]
  ;;
end

module _ : sig
  type tree = [ `Node of int * tree list ] [@@deriving python]
end = struct
  type t =
    [ `A
    | `B of int
    | `C of int * string * string
    | `D
    ]
  [@@deriving python, sexp]

  let%expect_test "polymorphic-variant" =
    if not (Py.is_initialized ()) then Py.initialize ~version:3 ();
    let all = [ `A; `B 42; `C (1337, "alan", "turing"); `D ] in
    List.iter all ~f:(fun t ->
      printf !"%{sexp:t}\n%!" t;
      let pyobject = python_of_t t in
      print_endline (Py.Object.to_string pyobject);
      printf !"%{sexp:t}\n%!" (t_of_python pyobject));
    [%expect
      {|
      A
      ('A', None)
      A
      (B 42)
      ('B', 42)
      (B 42)
      (C (1337 alan turing))
      ('C', (1337, 'alan', 'turing'))
      (C (1337 alan turing))
      D
      ('D', None)
      D |}]
  ;;

  type u =
    { foo : t
    ; bar : [ `c | `A | `d of [ `c | `d of string ] ]
    }
  [@@deriving python, sexp]

  let%expect_test "polymorphic-variant-2" =
    if not (Py.is_initialized ()) then Py.initialize ~version:3 ();
    let all = [ { foo = `A; bar = `c }; { foo = `B 42; bar = `d (`d "foobar") } ] in
    List.iter all ~f:(fun u ->
      printf !"%{sexp:u}\n%!" u;
      let pyobject = python_of_u u in
      print_endline (Py.Object.to_string pyobject);
      printf !"%{sexp:u}\n%!" (u_of_python pyobject));
    [%expect
      {|
      ((foo A) (bar c))
      {'foo': ('A', None), 'bar': ('c', None)}
      ((foo A) (bar c))
      ((foo (B 42)) (bar (d (d foobar))))
      {'foo': ('B', 42), 'bar': ('d', ('d', 'foobar'))}
      ((foo (B 42)) (bar (d (d foobar)))) |}]
  ;;

  type tree = [ `Node of int * tree list ] [@@deriving python, sexp]

  let%expect_test "polymorphic-variant-tree" =
    if not (Py.is_initialized ()) then Py.initialize ~version:3 ();
    let t : tree =
      `Node (1, [ `Node (2, []); `Node (3, []); `Node (4, [ `Node (5, []) ]) ])
    in
    printf !"%{sexp:tree}\n%!" t;
    let pyobject = python_of_tree t in
    print_endline (Py.Object.to_string pyobject);
    printf !"%{sexp:tree}\n%!" (tree_of_python pyobject);
    [%expect
      {|
      (Node (1 ((Node (2 ())) (Node (3 ())) (Node (4 ((Node (5 ()))))))))
      ('Node', (1, [('Node', (2, [])), ('Node', (3, [])), ('Node', (4, [('Node', (5, []))]))]))
      (Node (1 ((Node (2 ())) (Node (3 ())) (Node (4 ((Node (5 ())))))))) |}];
    let t2 = `Node (42, [ t; t; t; t ]) in
    let t = `Node (1337, [ t; t2; t ]) in
    printf !"%d" (Stdlib.compare (tree_of_python (python_of_tree t)) t);
    [%expect {| 0 |}]
  ;;
end

module _ = struct
  type t =
    { field_a : int
    ; field_b : string
    ; field_c : float
    }
  [@@deriving python, sexp] [@@python.disallow_extra_fields]

  type t_allow =
    { field_a : int
    ; field_b : string
    }
  [@@deriving python, sexp]

  type t_disallow =
    { field_a : int
    ; field_b : string
    }
  [@@deriving python, sexp] [@@python.disallow_extra_fields]

  let%expect_test "extra-field-test" =
    if not (Py.is_initialized ()) then Py.initialize ~version:3 ();
    let t = { field_a = 42; field_b = "aturing"; field_c = 3.141592653589 } in
    let pyobject = python_of_t t in
    let try_extract f = Or_error.try_with (fun () -> f pyobject) in
    printf !"%{sexp:t_allow Or_error.t}\n%!" (try_extract t_allow_of_python);
    printf !"%{sexp:t_disallow Or_error.t}\n%!" (try_extract t_disallow_of_python);
    Py.Dict.set_item_string pyobject "another_extra_field" Py.none;
    printf !"%{sexp:t_allow Or_error.t}\n%!" (try_extract t_allow_of_python);
    printf !"%{sexp:t_disallow Or_error.t}\n%!" (try_extract t_disallow_of_python);
    Py.Dict.del_item_string pyobject "another_extra_field";
    Py.Dict.del_item_string pyobject "field_c";
    printf !"%{sexp:t_allow Or_error.t}\n%!" (try_extract t_allow_of_python);
    printf !"%{sexp:t_disallow Or_error.t}\n%!" (try_extract t_disallow_of_python);
    Py.Dict.del_item_string pyobject "field_b";
    printf !"%{sexp:t_allow Or_error.t}\n%!" (try_extract t_allow_of_python);
    printf !"%{sexp:t_disallow Or_error.t}\n%!" (try_extract t_disallow_of_python);
    [%expect
      {|
      (Ok ((field_a 42) (field_b aturing)))
      (Error (Failure "unexpected extra field names 'field_c'"))
      (Ok ((field_a 42) (field_b aturing)))
      (Error
       (Failure "unexpected extra field names 'field_c','another_extra_field'"))
      (Ok ((field_a 42) (field_b aturing)))
      (Ok ((field_a 42) (field_b aturing)))
      (Error (Failure "cannot find field field_b in dict"))
      (Error (Failure "cannot find field field_b in dict")) |}]
  ;;

  type t_with_default =
    { f_a : int
    ; f_b : string [@python.default "foobar"]
    ; f_c : float
    }
  [@@deriving python, sexp] [@@python.disallow_extra_fields]

  let%expect_test "extra-field-with-default-test" =
    if not (Py.is_initialized ()) then Py.initialize ~version:3 ();
    let extract_and_print bindings =
      let pyobject = Py.Dict.of_bindings_string bindings in
      printf
        !"%{sexp:t_with_default Or_error.t}\n%!"
        (Or_error.try_with (fun () -> t_with_default_of_python pyobject))
    in
    extract_and_print
      [ "f_a", python_of_int 1
      ; "f_b", python_of_string "barfoo"
      ; "f_c", python_of_float 3.141592
      ];
    extract_and_print
      [ "f_a", python_of_int 1
      ; "f_bb", python_of_string "barfoo"
      ; "f_b", python_of_string "barfoo"
      ; "f_c", python_of_float 3.141592
      ];
    extract_and_print
      [ "f_a", python_of_int 1
      ; "f_bb", python_of_string "barfoo"
      ; "f_c", python_of_float 3.141592
      ];
    extract_and_print [ "f_a", python_of_int 1; "f_c", python_of_float 3.141592 ];
    [%expect
      {|
      (Ok ((f_a 1) (f_b barfoo) (f_c 3.141592)))
      (Error (Failure "unexpected extra field names 'f_bb'"))
      (Error (Failure "unexpected extra field names 'f_bb'"))
      (Ok ((f_a 1) (f_b foobar) (f_c 3.141592))) |}]
  ;;
end

module _ : sig
  type ('a, 'b, 'c) template =
    | A of 'a
    | B of 'b
    | C of 'c
  [@@deriving python]
end = struct
  type ('a, 'b, 'c) template =
    | A of 'a
    | B of 'b
    | C of 'c
  [@@deriving python, sexp]

  module Custom = struct
    type t = int [@@deriving python, sexp]
  end

  type int_template = (int, int, int) template [@@deriving python, sexp]
  type float_template = (float, float, float) template [@@deriving python, sexp]
  type bool_template = (bool, bool, bool) template [@@deriving python, sexp]

  let%expect_test "multi-polymorphic-type" =
    (* Test python_of_t conversions *)
    let i = python_of_int_template (A 1) in
    let f = python_of_float_template (B 1.) in
    let b = python_of_bool_template (C false) in
    let custom : (Custom.t, float, bool) template = A 5 in
    let custom_python =
      python_of_template Custom.python_of_t python_of_float python_of_bool custom
    in
    print_endline (Py.Object.to_string i);
    print_endline (Py.Object.to_string f);
    print_endline (Py.Object.to_string b);
    print_endline (Py.Object.to_string custom_python);
    [%expect
      {|
        ('A', (1,))
        ('B', (1.0,))
        ('C', (False,))
        ('A', (5,))
    |}];
    (* Test t_of_python conversions *)
    let i = int_template_of_python i in
    let f = float_template_of_python f in
    let b = bool_template_of_python b in
    let custom =
      template_of_python Custom.t_of_python float_of_python bool_of_python custom_python
    in
    printf !"%{Sexp}\n%!" (sexp_of_int_template i);
    printf !"%{Sexp}\n%!" (sexp_of_float_template f);
    printf !"%{Sexp}\n%!" (sexp_of_bool_template b);
    printf
      !"%{Sexp}\n%!"
      (sexp_of_template Custom.sexp_of_t sexp_of_float sexp_of_bool custom);
    [%expect {|
        (A 1)
        (B 1)
        (C false)
        (A 5)
    |}]
  ;;
end

let%expect_test "py_string literal tests" =
  if not (Py.is_initialized ()) then Py.initialize ();
  let (some_python_string : Py.Object.t) = [%py_string "python_string!\n"] in
  let (_ : Py.Object.t) = [%py_string "another python string!\n"] in
  let sys = Py.Import.import_module "sys" in
  let sys_stdout = Py.Object.get_attr_string sys "stdout" |> Option.value_exn in
  let python_stdout_write =
    Py.Object.get_attr_string sys_stdout "write" |> Option.value_exn
  in
  let python_stdout_flush =
    Py.Object.get_attr_string sys_stdout "flush" |> Option.value_exn
  in
  let _none = Py.Callable.to_function python_stdout_write [| some_python_string |] in
  let _none = Py.Callable.to_function python_stdout_flush [||] in
  (* let's print the second python string using the lazy cached value from the py_string
     ppx extension *)
  let _none =
    Py.Callable.to_function python_stdout_write [| Lazy.force py_string_1 |]
  in
  let _none = Py.Callable.to_function python_stdout_flush [||] in
  [%expect {|
    python_string!
    another python string! |}]
;;
