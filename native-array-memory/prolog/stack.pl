%% author: 謝宇恆 / XIE Yuheng
%% github: https://github.com/xieyuheng

%% imperative global named stack
%% a stack is a buffer with speical interface

:- module(stack,
          [make_stack/3,
           stack_push/2,
           stack_pop/2,
           stack_tos/2]).

:- use_module(buffer).

make_stack(Name, Size, InitValue) :-
  %: +atom +number +value
  make_buffer(Name, Size, InitValue).

stack_push(Name, Value) :-
  %: +atom +value
  buffer_get_cursor(Name, Cursor),
  buffer_set(Name, Cursor, Value),
  buffer_add_cursor(Name, 1).

stack_pop(Name, Value) :-
  %: +atom value
  buffer_sub_cursor(Name, 1),
  buffer_get_cursor(Name, Cursor),
  buffer_get(Name, Cursor, Value).

stack_tos(Name, Value) :-
  %: +atom value
  buffer_get_cursor(Name, Cursor),
  OldCursor is Cursor - 1,
  buffer_get(Name, OldCursor, Value).

/*
stack_push(argument_stack, 666).
stack_push(argument_stack, k1).
stack_push(argument_stack, k2).
stack_pop(argument_stack, _k2).
stack_pop(argument_stack, _k1).
stack_pop(argument_stack, _666).
*/
