%% author: 謝宇恆 / XIE Yuheng
%% github: https://github.com/xieyuheng

%% imperative global named array
%% implemented by abusing functor/3

:- module(array,
          [make_array/3,
           array_set/3,
           array_get/3]).

%% this function must be called in :- initialization
make_array(Name, Size, InitValue) :-
  %: +atom +number +value
  functor(Array, array, Size),
  nb_setval(Name, Array),
  make_array_acc(Name, Size, InitValue, 0).

make_array_acc(_Name, Size, _InitValue, Size).
make_array_acc(Name, Size, InitValue, Counter) :-
  array_set(Name, Counter, InitValue),
  NextCounter is Counter + 1,
  make_array_acc(Name, Size, InitValue, NextCounter).

array_set(Name, Address, Value) :-
  %: +atom +number +value
  nb_getval(Name, Array),
  OneBasedAddress is Address + 1,
  nb_setarg(OneBasedAddress, Array, Value).

array_get(Name, Address, Value) :-
  %: +atom +number value
  nb_getval(Name, Array),
  OneBasedAddress is Address + 1,
  arg(OneBasedAddress, Array, Value).
