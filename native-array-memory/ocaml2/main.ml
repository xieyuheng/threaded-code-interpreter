define_primitive_function "end"
  (fun () ->
   return_stack_pop ();
   next ())


define_primitive_function "bye"
  (fun () ->
   print_string "bye bye ^-^/";
   print_string "\n";
   flush_all ())


define_primitive_function "dup"
  (fun () ->
   let a = argument_stack_pop () in
   argument_stack_push a;
   argument_stack_push a;
   next ())


define_primitive_function "mul"
  (fun () ->
   let b = argument_stack_pop () in
   let a = argument_stack_pop () in
   argument_stack_push (a * b);
   next ())


define_primitive_function "simple-wirte"
  (fun () ->
   let a = argument_stack_pop () in
   print_int a;
   print_string "\n";
   flush_all ();
   next ())


define_variable "little-test-number" 4

define_function "square"
[ "dup"
; "mul"
; "end" ]


define_function "little-test"
[ "little-test-number"
; "square"
; "simple-wirte"
; "bye" ]


define_function "first-function"
[ "little-test"
; "end" ]


let function_body_for_little_test =
  (Hashtbl.find in_host_name_record "first-function")
  + 1


let begin_to_interpret_threaded_code
: unit -> unit
= fun () ->
  return_stack_push function_body_for_little_test;
  next ();


begin_to_interpret_threaded_code ()
