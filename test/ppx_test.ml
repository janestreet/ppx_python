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
    Py.Dict.to_bindings_string pyobject |> List.sort ~compare:Caml.compare
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
      (t_of_python
         (python_of_u { foo = 1, 2; bar = { field_a = 1; field_b = "test" } })));
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
