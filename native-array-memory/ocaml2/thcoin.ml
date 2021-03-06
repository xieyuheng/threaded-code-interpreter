let cell_area_size = 1024 * 1024 ;;
let cell_area
= Array.make cell_area_size 0
;;

let cell_area_get
= fun address ->
  Array.get cell_area address
;;

let cell_area_set
= fun address value ->
  Array.set cell_area address value
;;

let cell_area_pointer = ref 0 ;;

let allocate_cell_area
= fun size ->
  let return_address = !cell_area_pointer in
  cell_area_pointer := !cell_area_pointer + size;
  return_address
;;

let empty_argument_point = 0 ;;

let argument_stack_size = 1024 ;;
let argument_stack
= Array.make
    argument_stack_size
    empty_argument_point
;;

let argument_stack_pointer = ref 0 ;;

let argument_stack_push
= fun value ->
  Array.set
    argument_stack
    !argument_stack_pointer
    value;
  argument_stack_pointer :=
    !argument_stack_pointer + 1;
;;

let argument_stack_pop
= fun () ->
  argument_stack_pointer :=
    !argument_stack_pointer - 1;
  Array.get
    argument_stack
    !argument_stack_pointer;
;;

type return_point = int ;;

let empty_return_point = 0 ;;

let return_stack_size = 1024 ;;
let return_stack
: return_point array
= Array.make
    return_stack_size
    empty_return_point
;;

let return_stack_pointer = ref 0 ;;

let return_stack_push
: return_point -> unit
= fun value ->
  Array.set
    return_stack
    !return_stack_pointer
    value;
  return_stack_pointer :=
    !return_stack_pointer + 1;
;;

let return_stack_pop
: unit -> return_point
= fun () ->
  return_stack_pointer :=
    !return_stack_pointer - 1;
  Array.get
    return_stack
    !return_stack_pointer;
;;

let primitive_function_record_size = 1024 ;;

type primitive_function = unit -> unit

let primitive_function_record_place_holder
: primitive_function
= fun () -> ()
;;

let primitive_function_record
= Array.make
    primitive_function_record_size
    primitive_function_record_place_holder
;;

let primitive_function_counter = ref 0 ;;

let primitive_function_record_get
: int -> primitive_function
= fun index ->
  Array.get primitive_function_record index
;;

let primitive_function_record_set
: int -> primitive_function -> unit
= fun index primitive_function ->
  Array.set
    primitive_function_record
    index
    primitive_function
;;

let create_primitive_function
: primitive_function -> int
= fun primitive_function ->
  let return_value = !primitive_function_counter in
  primitive_function_record_set
    !primitive_function_counter
    primitive_function;
  primitive_function_counter
    := !primitive_function_counter + 1;
  return_value
;;

let next_explainer_argument = ref 0 ;;

let next
: unit -> unit
= fun () ->
  let jojo = return_stack_pop () in
  let jo = cell_area_get jojo in
  let explainer = cell_area_get jo in
  return_stack_push (jojo + 1);
  next_explainer_argument := jo + 1;
  (primitive_function_record_get explainer) ();
;;

let in_host_name_record = Hashtbl.create 1024 ;;

let data
= fun value ->
    cell_area_set !cell_area_pointer value;
    cell_area_pointer :=
      !cell_area_pointer + 1;
;;

let mark
: string -> unit
= fun name_string ->
  Hashtbl.add in_host_name_record
    name_string !cell_area_pointer
;;

let link = ref 0 ;;

let define_header
: string -> int -> unit
= fun name_string explainer ->
  data !link;
  link := !cell_area_pointer - 1;
  mark name_string;
  data explainer;
;;

let primitive_function_explainer
: int
= create_primitive_function
   (fun () ->
     (primitive_function_record_get
       (cell_area_get !next_explainer_argument)) ())
;;

let define_primitive_function
: string -> primitive_function -> unit
= fun name_string primitive_function ->
  let function_index =
    create_primitive_function primitive_function in
  define_header name_string primitive_function_explainer;
  data function_index;
;;

let function_explainer
: int
= create_primitive_function
   (fun () ->
     return_stack_push !next_explainer_argument;
     next ())
;;

let define_function
: string -> string list -> unit
= fun name_string name_string_list ->
  define_header name_string function_explainer;
  List.iter
    (fun name_string ->
       data (Hashtbl.find in_host_name_record name_string))
    name_string_list;
;;

let variable_explainer
: int
= create_primitive_function
   (fun () ->
     argument_stack_push (cell_area_get !next_explainer_argument);
     next ())
;;

let define_variable
= fun name_string value ->
  define_header name_string variable_explainer;
  data value;
;;

define_primitive_function "end"
  (fun () ->
   return_stack_pop ();
   next ())
;;

define_primitive_function "bye"
  (fun () ->
   print_string "bye bye ^-^/";
   print_string "\n";
   flush_all ())
;;

define_primitive_function "dup"
  (fun () ->
   let a = argument_stack_pop () in
   argument_stack_push a;
   argument_stack_push a;
   next ())
;;

define_primitive_function "mul"
  (fun () ->
   let b = argument_stack_pop () in
   let a = argument_stack_pop () in
   argument_stack_push (a * b);
   next ())
;;

define_primitive_function "simple-wirte"
  (fun () ->
   let a = argument_stack_pop () in
   print_int a;
   print_string "\n";
   flush_all ();
   next ())
;;

define_variable "little-test-number" 4 ;;

define_function "square"
[ "dup"
; "mul"
; "end" ]
;;

define_function "little-test"
[ "little-test-number"
; "square"
; "simple-wirte"
; "bye" ]
;;

define_function "first-function"
[ "little-test"
; "end" ]
;;

let function_body_for_little_test =
  (Hashtbl.find in_host_name_record "first-function")
  + 1
;;

let begin_to_interpret_threaded_code
: unit -> unit
= fun () ->
  return_stack_push function_body_for_little_test;
  next ();
;;

begin_to_interpret_threaded_code () ;;
