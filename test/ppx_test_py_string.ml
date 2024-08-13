let _py_string () = [%py_string "single-line-py-string"]

let _multi_line_py_string () =
  [%py_string
    {|

  fizz

  buzz

  15


|}]
;;

let _py_string_dup () = [%py_string "single-line-py-string"]
