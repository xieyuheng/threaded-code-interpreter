%% author: 謝宇恆 / XIE Yuheng
%% github: https://github.com/xieyuheng

%% imperative global named table
%% implemented by buffer of key value pair

:- module(table,
          [make_table/2,
           table_set/3,
           table_find/4]).

:- use_module(buffer).

make_table(Name, Size) :-
  %: +atom +number
  make_buffer(Name, Size, 0).

table_set(Name, Key, Value) :-
  %: +atom +value +value
  buffer_get_cursor(Name, Cursor),
  buffer_set(Name, Cursor, [Key|Value]),
  buffer_add_cursor(Name, 1).

table_find(Name, Key, Value, SUCCESS_OR_FAIL) :-
  %: +atom +value value ('success or 'fail)
  buffer_get_cursor(Name, Cursor),
  table_find_loop(Name, Key, Value, SUCCESS_OR_FAIL, Cursor).

table_find_loop(_Name, _Key, _Value, fail, 0).
table_find_loop(Name, Key, Value, SUCCESS_OR_FAIL, Cursor) :-
  Next is Cursor - 1,
  (buffer_get(Name, Next, [Key|Value]),
   SUCCESS_OR_FAIL = success, !;
   table_find_loop(Name, Key, Value, SUCCESS_OR_FAIL, Next), !).

/*
table_set(name_jo_table, k1, k(1)).
table_find(name_jo_table, k1, V1, SUCCESS_OR_FAIL).
table_set(name_jo_table, k2, k(2)).
table_find(name_jo_table, k1, V1, SUCCESS_OR_FAIL),
table_find(name_jo_table, k2, V2, SUCCESS_OR_FAIL).
*/
