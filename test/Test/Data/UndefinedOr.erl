-module(test_data_undefinedOr@foreign).

-export([undefined/0, defined/1, eqUndefinedOrImpl/3, compareUndefinedOrImpl/6]).

undefined() -> undefined.


defined(X) -> X.

eqUndefinedOrImpl(_Eq,undefined, undefined) -> true;
eqUndefinedOrImpl(Eq, A, B) -> (Eq(A))(B).

compareUndefinedOrImpl(LT,EQ,GT,Compare,A,B) ->
  case {A,B} of
    {undefined,undefined} -> EQ;
    {undefined,_} -> LT;
    {_,undefined} -> GT;
    _ -> (Compare(A))(B)
  end.
