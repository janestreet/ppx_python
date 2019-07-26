open! Base


let python_of_bool = Py.Bool.of_bool
let bool_of_python = Py.Bool.to_bool
let python_of_int = Py.Int.of_int
let int_of_python = Py.Int.to_int
let python_of_float = Py.Float.of_float
let float_of_python = Py.Float.to_float
let python_of_string = Py.String.of_string
let string_of_python = Py.String.to_string
let python_of_array = Py.List.of_array_map
let array_of_python = Py.List.to_array_map
let python_of_list = Py.List.of_list_map
let list_of_python = Py.List.to_list_map

let python_of_option f = function
  | None -> Py.none
  | Some v -> f v
;;

let option_of_python f pyobject =
  if Caml.( = ) pyobject Py.none then None else Some (f pyobject)
;;
