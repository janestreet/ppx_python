  $ cd $TESTDIR
  $ cat ppx_test_py_string.ml.pp
  let py_string_1 =
    lazy (Pyml.Py.String.of_string "\n\n  fizz\n\n  buzz\n\n  15\n\n\n")
  let py_string_0 = lazy (Pyml.Py.String.of_string "single-line-py-string")
  let () =
    Ppx_bench_lib.Benchmark_accumulator.Current_libname.set "ppx_python_test"
  let () =
    Expect_test_collector.Current_file.set
      ~absolute_filename:"ppx/ppx_python/test/ppx_test_py_string.ml"
  let () =
    Ppx_inline_test_lib.set_lib_and_partition "ppx_python_test"
      "ppx_test_py_string"
  let _py_string () = ((Lazy.force py_string_0)[@merlin.hide ])
  let _multi_line_py_string () = ((Lazy.force py_string_1)[@merlin.hide ])
  let _py_string_dup () = ((Lazy.force py_string_0)[@merlin.hide ])
  let () = Ppx_inline_test_lib.unset_lib "ppx_python_test"
  let () = Expect_test_collector.Current_file.unset ()
  let () = Ppx_bench_lib.Benchmark_accumulator.Current_libname.unset ()
