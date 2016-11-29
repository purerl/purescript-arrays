-module(data_array@foreign).
-export([range/2,replicate/2,fromFoldableImpl/2,length/1,cons/2,snoc/2,'uncons\''/3,indexImpl/4,findIndexImpl/4,findLastIndexImpl/4,'_insertAt'/5,'_deleteAt'/4,'_updateAt'/5,reverse/1,concat/1,filter/2,partition/2,sortImpl/2,slice/3,take/2,drop/2,zipWith/3,unsafeIndexImpl/2]).

% ------------------------------------------------------------------------------
%  Array creation --------------------------------------------------------------
% ------------------------------------------------------------------------------

range(Start, End) -> array:from_list(lists:seq(Start, End, case End < Start of true -> -1; false -> 1 end)).

replicate(N, X) -> array:new(max(N,0), {default, X}).

fromFoldableImpl(Foldr,Xs) -> array:from_list( ((Foldr(fun (H) -> fun (T) -> [H|T] end end))([]))(Xs) ).


% ------------------------------------------------------------------------------
%  Array size ------------------------------------------------------------------
% ------------------------------------------------------------------------------

length(Xs) -> array:size(Xs).

% ------------------------------------------------------------------------------
%  Extending arrays ------------------------------------------------------------
% ------------------------------------------------------------------------------

cons(E,L) -> array:from_list([E|array:to_list(L)]).

snoc(L,E) -> array:set(array:size(L), E, L).

% ------------------------------------------------------------------------------
%  Non-indexed reads -----------------------------------------------------------
% ------------------------------------------------------------------------------

'uncons\''(Empty, Next, Xs) ->
  case array:size(Xs) of
    0 -> Empty(unit);
    _ -> (Next(array:get(0, Xs)))(array:from_list(tl(array:to_list(Xs))))
  end.

% ------------------------------------------------------------------------------
%  Indexed operations ----------------------------------------------------------
% ------------------------------------------------------------------------------

indexImpl(Just,Nothing,Xs,I) ->
  case I < 0 orelse I >= array:size(Xs) of
    true -> Nothing;
    false -> Just(array:get(I,Xs))
  end.

findIndexImpl(Just,Nothing,F,Xs) ->
  begin
    N = array:size(Xs) - 1,
    Find = fun
      Find(I) when I>N -> Nothing;
      Find(I) -> case F(array:get(I,Xs)) of
        true -> Just(I);
        false -> Find(I+1)
      end
    end,
    Find(0)
  end.

findLastIndexImpl(Just,Nothing,F,Xs) ->
  begin
    Find = fun
      Find(I) when I < 0 -> Nothing;
      Find(I) -> case F(array:get(I,Xs)) of
        true -> Just(I);
        false -> Find(I-1)
      end
    end,
    Find(array:size(Xs) - 1)
  end.

'_insertAt'(Just,Nothing,I,A,L) ->
  Size = array:size(L),
  InsertF = fun
    InsertF(LL, J) when J > Size -> LL;
    InsertF(LL, J) when J =:= I -> InsertF(array:set(J, A, LL), J+1);
    InsertF(LL, J) when J > I -> InsertF(array:set(J, array:get(J-1, L), LL), J+1)
  end,
  case I < 0 orelse I > Size of
    true -> Nothing;
    false -> Just(InsertF(L, I))
  end.

'_deleteAt'(Just,Nothing,I,L) ->
  Size = array:size(L),
  DeleteF = fun
    DeleteF(LL, J) when J-1 > Size -> LL;
    DeleteF(LL, J) when J >= I -> DeleteF(array:set(J, array:get(J+1, L), LL), J+1)
  end,
  case I < 0 orelse I > Size - 1 of
    true -> Nothing;
    false -> Just(array:resize(Size-1, DeleteF(L, I)))
  end.

'_updateAt'(Just,Nothing,I,A,L) ->
  case I < 0 orelse I >= array:size(L) of
    true -> Nothing;
    false -> Just(array:set(I, A, L))
  end.

%%------------------------------------------------------------------------------
%% Transformations -------------------------------------------------------------
%%------------------------------------------------------------------------------

reverse(L) -> array:from_list(lists:reverse(array:to_list(L))).

concat(Ls) -> array:from_list(lists:append(array:to_list(array:map(fun (_, X) -> array:to_list(X) end, Ls)))).

filter(F,L) -> array:from_list([X || X <- array:to_list(L), F(X)]).

partition(F,A) -> begin
  {Yes, No} = lists:partition(F, array:to_list(A)),
  #{yes => array:from_list(Yes), no => array:from_list(No)}
end.

% ------------------------------------------------------------------------------
%  Sorting ---------------------------------------------------------------------
% ------------------------------------------------------------------------------

sortImpl(F,A) -> array:from_list(lists:sort(fun (X, Y) -> case (F(X))(Y) of 1 -> false; _ -> true end end, array:to_list(A))).

% ------------------------------------------------------------------------------
%  Subarrays -------------------------------------------------------------------
% ------------------------------------------------------------------------------

slice(S,E,L) -> array:from_list(lists:sublist(array:to_list(L), S+1, E-S)).

take(N,L) -> case {N < 1, N > array:size(L)} of
  {true, _} -> array:from_list([]);
  {_, true} -> L;
  {false,_} -> begin
    {Head, _} = lists:split(N,array:to_list(L)),
    array:from_list(Head)
  end
end.

drop(N,L) -> case {N < 1, N > array:size(L)} of
  {true,_} -> L;
  {_, true} -> array:from_list([]);
  {false,_} -> begin
    {_, Tail} = lists:split(N,array:to_list(L)),
    array:from_list(Tail)
  end
end.

% ------------------------------------------------------------------------------
%  Zipping ---------------------------------------------------------------------
% ------------------------------------------------------------------------------

zipWith(F,Xs,Ys) -> array:from_list(lists:zipwith(fun (X, Y) -> (F(X))(Y) end, array:to_list(Xs), array:to_list(Ys))).

unsafeIndexImpl(A,N) -> array:get(N,A).
