val cell_area : int array
val cell_area_get : int -> int
val cell_area_set : int -> int -> unit
val allocate_cell_area : int -> int
val argument_stack : int array
val argument_stack_push : int -> unit
val argument_stack_pop : unit -> int
val return_stack : int array
val return_stack_push : int -> unit
val return_stack_pop : unit -> int
type primitive_function = unit -> unit
val primitive_function_record_place_holder : primitive_function
val primitive_function_record_get : int -> primitive_function
val primitive_function_record_set : int -> primitive_function -> unit
val create_primitive_function : primitive_function -> int
val next : unit -> unit
val data : int -> unit
val mark : string -> unit
val define_header : string -> int -> unit
val primitive_function_explainer : int
val define_primitive_function : string -> primitive_function -> unit
val function_explainer : int
val define_function : string -> string list -> unit
val variable_explainer : int
val define_variable : string -> int -> unit
