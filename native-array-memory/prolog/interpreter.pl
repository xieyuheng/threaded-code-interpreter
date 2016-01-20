%% author: 謝宇恆 / XIE Yuheng
%% github: https://github.com/xieyuheng

%% threaded code interpreter embeded in prolog

%% :- module(interpreter,
%%           [
%%            begin_to_interpret_threaded_code/0]).

:- use_module(array).

:- use_module(buffer).

%% any value can be stored in the cell_area
:- make_buffer(cell_area, 1024, 0).
data(Value) :-
  %: + value
  buffer_get_cursor(cell_area, Cursor),
  buffer_set(cell_area, Cursor, Value),
  buffer_add_cursor(cell_area, 1).

:- use_module(stack).

:- make_stack(argument_stack, 1024, 0).
:- make_stack(return_stack, 1024, 0).

%% no clean namespace of primitive_function

:- nb_setval(next_explainer_argument, 0).
next() :-
  stack_pop(return_stack, JOJO),
  buffer_get(cell_area, JOJO, JO),
  buffer_get(cell_area, JO, EXPLAINER),
  NEW_JOJO is JOJO + 1,
  stack_push(return_stack, NEW_JOJO),
  EXPLAINER_ARGUMENT is JO + 1,
  nb_setval(next_explainer_argument, EXPLAINER_ARGUMENT),
  call(EXPLAINER).

:- use_module(table).
:- make_table(name_jo_table, 1024).

primitive_function_explainer :-
  nb_getval(next_explainer_argument, EXPLAINER_ARGUMENT),
  buffer_get(cell_area, EXPLAINER_ARGUMENT, PRIMITIVE_FUNCTION),
  call(PRIMITIVE_FUNCTION).

declare_primitive_function(Name) :-
  buffer_get_cursor(cell_area, Cursor),
  table_set(name_jo_table, Name, Cursor),
  data(primitive_function_explainer),
  data(Name).


function_explainer :-
  nb_getval(next_explainer_argument, EXPLAINER_ARGUMENT),
  stack_push(return_stack, EXPLAINER_ARGUMENT),
  next.

define_function(Name, NameList) :-
  buffer_get_cursor(cell_area, Cursor),
  table_set(name_jo_table, Name, Cursor),
  data(function_explainer),
  define_body(NameList).

define_body([]).
define_body([Name|Rest]) :-
  table_find(name_jo_table, Name, JO, _),
  data(JO),
  define_body(Rest).


variable_explainer :-
  nb_getval(next_explainer_argument, EXPLAINER_ARGUMENT),
  buffer_get(cell_area, EXPLAINER_ARGUMENT, Value),
  stack_push(argument_stack, Value),
  next.

define_variable(Name, Value) :-
  buffer_get_cursor(cell_area, Cursor),
  table_set(name_jo_table, Name, Cursor),
  data(variable_explainer),
  data(Value).


push(Value) :-
  %: +value
  stack_push(argument_stack, Value).

pop(Value) :-
  %: value
  stack_pop(argument_stack, Value).


:- declare_primitive_function(end).
end :-
  stack_pop(return_stack, _),
  next.

:- declare_primitive_function(bye).
bye :-
  stack_pop(return_stack, _),
  writeln('bye bye ^-^/').

:- declare_primitive_function(dup).
dup :-
  pop(A),
  push(A),
  push(A),
  next.

:- declare_primitive_function(mul).
mul :-
  pop(B),
  pop(A),
  N is A * B,
  push(N),
  next.

:- declare_primitive_function(simple_wirte).
simple_wirte :-
  pop(A),
  writeln(A),
  next.

:- define_variable(little_test_number, 4).

:- define_function(
       square,
       [ dup,
         mul,
         end ]).

:- define_function(
       little_test,
       [ little_test_number,
         square,
         simple_wirte,
         bye ]).

:- define_function(
       first_function,
       [ little_test,
         end ]).


begin_to_interpret_threaded_code :-
  table_find(name_jo_table, first_function, JO, _),
  FUNCTION_BODY_FOR_LITTLE_TEST is JO + 1,
  stack_push(return_stack, FUNCTION_BODY_FOR_LITTLE_TEST),
  next.

:- begin_to_interpret_threaded_code.

:- halt.
