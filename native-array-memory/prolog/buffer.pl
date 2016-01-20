%% author: 謝宇恆 / XIE Yuheng
%% github: https://github.com/xieyuheng

%% imperative global named buffer
%% a buffer is an array with a cursor
%% index 0 of the array is used as cursor

:- module(buffer,
          [make_buffer/3,
           buffer_set/3,
           buffer_get/3,
           buffer_set_cursor/2,
           buffer_get_cursor/2,
           buffer_add_cursor/2,
           buffer_sub_cursor/2,
           buffer_allocate/3]).

:- use_module(array).

make_buffer(Name, Size, InitValue) :-
  %: +atom +number +value
  RealSize is Size + 1,
  make_array(Name, RealSize, InitValue),
  array_set(Name, 0, 0).

buffer_set(Name, Address, Value) :-
  %: +number +number
  RealAddress is Address + 1,
  array_set(Name, RealAddress, Value).

buffer_get(Name, Address, Value) :-
  %: +number number
  RealAddress is Address + 1,
  array_get(Name, RealAddress, Value).

buffer_set_cursor(Name, Cursor) :-
  %: +atom +number
  array_set(Name, 0, Cursor).

buffer_get_cursor(Name, Cursor) :-
  %: +atom number
  array_get(Name, 0, Cursor).

buffer_add_cursor(Name, Value) :-
  %: +atom +number number
  buffer_get_cursor(Name, Cursor),
  NewCursor is Cursor + Value,
  buffer_set_cursor(Name, NewCursor).

buffer_sub_cursor(Name, Value) :-
  %: +atom +number number
  buffer_get_cursor(Name, Cursor),
  NewCursor is Cursor - Value,
  buffer_set_cursor(Name, NewCursor).

buffer_allocate(Name, Size, Address) :-
  %: +atom +number -number
  buffer_get_cursor(Name, Cursor),
  Address is Cursor,
  buffer_add_cursor(Name, Size).

/*
buffer_get(cell_area, 0, Value),
Value is 0.
buffer_set(cell_area, 0, 666),
buffer_get(cell_area, 0, Value),
Value is 666.
*/

/*
buffer_get_cursor(cell_area, Cursor),
buffer_allocate(cell_area, 16, Address1),
buffer_allocate(cell_area, 32, Address2),
Address1 is Cursor,
16 is Address2 - Address1.
*/
