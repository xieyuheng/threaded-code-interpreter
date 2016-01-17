type address = int ;;
type byte = int ;;
type value = int ;;

let cell = 4 ;;
let memory_size = 1024 * 1024 ;;
let memory = Bytes.make memory_size (Char.chr 0) ;;

let memory_get_byte
: address -> byte
= fun address ->
  Char.code (Bytes.get memory address)
;;

let memory_set_byte
: address -> byte -> unit
= fun address byte ->
  Bytes.set memory address (Char.chr byte)
;;

(* memory_set_byte 0 6 ;;
 * memory_get_byte 0 ;; *)

(* little endian *)
let memory_get
: address -> value
= fun address ->
  let result = ref 0 in
  for i = 0 to cell - 1 do
    result := !result
        + ((Char.code (Bytes.get memory (address + i)))
             lsl (8 * i));
  done;
  !result
;;

let memory_set
: address -> value -> unit
= fun address value ->
  let rest_value = ref value in
  let byte = ref (!rest_value land 255) in
  for i = 0 to cell - 1 do
    Bytes.set memory (address + i) (Char.chr !byte);
    rest_value := (!rest_value lsr 8);
    byte := (!rest_value land 255);
  done;
;;

(* memory_set 0 666 ;;
 * memory_get 0 ;; *)

let memory_pointer = ref 0 ;;

let allocate_memory
: value -> address
= fun size ->
  let return_address = !memory_pointer in
  memory_pointer := !memory_pointer + size;
  return_address
;;

(* allocate_memory 16 ;; *)
(* allocate_memory 16 ;; *)

allocate_memory (cell * 64) ;; (* underflow *)

let argument_stack_address = allocate_memory (cell * 1024) ;;
let argument_stack_pointer = ref argument_stack_address ;;

let argument_stack_push
: value -> unit
= fun value ->
  memory_set !argument_stack_pointer value;
  argument_stack_pointer := !argument_stack_pointer + cell;
;;

let argument_stack_pop
: unit -> value
= fun () ->
  argument_stack_pointer :=
    !argument_stack_pointer - cell;
  memory_get !argument_stack_pointer;
;;

(* argument_stack_push 123 ;;
 * argument_stack_pop () ;; *)

allocate_memory (cell * 64) ;; (* underflow *)

let return_stack_address = allocate_memory (cell * 1024) ;;
let return_stack_pointer = ref return_stack_address ;;

let return_stack_push
: value -> unit
= fun value ->
  memory_set !return_stack_pointer value;
  return_stack_pointer :=
    !return_stack_pointer + cell;
;;

let return_stack_pop
: unit -> value
= fun () ->
  return_stack_pointer :=
    !return_stack_pointer - cell;
  memory_get !return_stack_pointer;
;;

(* return_stack_push 123 ;;
 * return_stack_pop () ;; *)

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
  Array.set primitive_function_record index primitive_function
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
  let function_body = return_stack_pop () in
  let explainer = memory_get (memory_get function_body) in
  return_stack_push (function_body + cell);
  next_explainer_argument :=
    (memory_get function_body) + cell;
  (primitive_function_record_get explainer) ();
;;

let in_host_name_record = Hashtbl.create 1024 ;;

let data
: value -> unit
= fun value ->
    memory_set !memory_pointer value;
    memory_pointer :=
      !memory_pointer + cell;
;;

let mark
: string -> unit
= fun name_string ->
  Hashtbl.add in_host_name_record
    name_string !memory_pointer
;;

let link = ref 0 ;;

let define_header
: string -> unit
= fun name_string ->
  data !link;
  link := !memory_pointer - cell;
  mark name_string;
;;

let primitive_function_explainer
: int
= create_primitive_function
   (fun () ->
     (primitive_function_record_get
       (memory_get !next_explainer_argument)) ())
;;

let define_primitive_function
: string -> primitive_function -> unit
= fun name_string primitive_function ->
  let function_index =
    create_primitive_function primitive_function in
  define_header name_string;
  data primitive_function_explainer;
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
  define_header name_string;
  data function_explainer;
  List.iter
    (fun name_string ->
       data (Hashtbl.find in_host_name_record name_string))
    name_string_list;
;;

let variable_explainer
: int
= create_primitive_function
   (fun () ->
     argument_stack_push (memory_get !next_explainer_argument);
     next ())
;;

let define_variable
: string -> value -> unit
= fun name_string value ->
  define_header name_string;
  data variable_explainer;
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
  + cell
;;

let begin_to_interpret_threaded_code
: unit -> unit
= fun () ->
  return_stack_push function_body_for_little_test;
  next ();
;;

begin_to_interpret_threaded_code () ;;
