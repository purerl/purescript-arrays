-module(data_array_nonEmpty_internal@foreign).
-export([fold1Impl/2, traverse1Impl/4]).

fold1Impl1(F, A, Acc, I, N) when I < N ->
    fold1Impl1(F, A, (F(Acc))(array:get(I, A)), I+1, N);
fold1Impl1(_,_,Acc,_,_) -> Acc.

fold1Impl(F, A) ->
    Length = array:size(A),
    fold1Impl1(F, A, array:get(0, A), 1, Length).

traverse1Impl(Apply, Map, F, A) ->
    Len = array:size(A),
    Last = array:get(Len-1, A),
    Cons = fun (X) -> fun (XS) -> [X|XS] end end,
    Acc = (Map(fun (X) -> [X] end))(Last),
    BuildFrom = fun (_, X, YS) ->
        (Apply( (Map(Cons))(F(X)) ))(YS)
      end,
    ResultList = array:foldr(BuildFrom, Acc, array:resize(Len-1, A)),
        (Map(fun array:from_list/1))(ResultList).
