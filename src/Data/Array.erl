-module(data_array@foreign).
-export([range/2,replicate/2,fromFoldableImpl/2,length/1,cons/2,snoc/2,'uncons\''/3,indexImpl/4,findIndexImpl/4,findLastIndexImpl/4,'_insertAt'/5,'_deleteAt'/4,'_updateAt'/5,reverse/1,concat/1,filter/2,partition/2,sortImpl/2,slice/3,take/2,drop/2,zipWith/3,unsafeIndexImpl/2]).

% ------------------------------------------------------------------------------
%  Array creation --------------------------------------------------------------
% ------------------------------------------------------------------------------

range(Start, End) = array:from_list(list:seq(Start, End)).

replicate(N, X) = array:new(N, {default, X}).

fromFoldableImpl(Foldr,Xs) =
  array:from_list( ((Foldr(fun (H) -> fun (T) -> [H|T]))([]))(Xs) ).


% ------------------------------------------------------------------------------
%  Array size ------------------------------------------------------------------
% ------------------------------------------------------------------------------

length(Xs) = array:size(Xs).

% ------------------------------------------------------------------------------
%  Extending arrays ------------------------------------------------------------
% ------------------------------------------------------------------------------

cons(E,L) = array:from_list([E|array:to_list(L)]).

cons(L,E) = array:set(array:size(L), E, L).

% ------------------------------------------------------------------------------
%  Non-indexed reads -----------------------------------------------------------
% ------------------------------------------------------------------------------

'uncons\''(Empty, Next, Xs) =
  case size(Xs) of
    0 -> Empty(unit),
    _ -> (Next(array:get(0, Xs)))(array:from_list(tl(array:to_list(Xs))))
  end.

% ------------------------------------------------------------------------------
%  Indexed operations ----------------------------------------------------------
% ------------------------------------------------------------------------------

indexImpl(Just,Nothing,Xs,I) =
  case i < 0 orelse i >= size(Xs) of
    true -> Nothing
    false -> Just(array:get(I,Xs))
  end.

findIndexImpl(Just,Nothing,F,Xs) =
  begin
    N = size(Xs),
    Find = fun (I) when I>=N -> Nothing;
      (I) when F(I) -> Just(I);
      Find(I) -> Find(I+1)
    end,
    Find(0)
  end.

findLastIndexImpl(Just,Nothing,F,Xs) =
  begin
    Find = fun (I) when I<=0 -> Nothing;
      (I) when F(I) -> Just(I);
      Find(I) -> Find(I-1)
    end,
    Find(size(Xs) - 1)
  end.

% exports._insertAt = function (just) {
%   return function (nothing) {
%     return function (i) {
%       return function (a) {
%         return function (l) {
%           if (i < 0 || i > l.length) return nothing;
%           var l1 = l.slice();
%           l1.splice(i, 0, a);
%           return just(l1);
%         };
%       };
%     };
%   };
% };
%
% exports._deleteAt = function (just) {
%   return function (nothing) {
%     return function (i) {
%       return function (l) {
%         if (i < 0 || i >= l.length) return nothing;
%         var l1 = l.slice();
%         l1.splice(i, 1);
%         return just(l1);
%       };
%     };
%   };
% };

'_updateAt'(Just,Nothing,I,A,L) ->
  case I < 0 orelse I >= Size(L) of
    true -> Nothing
    false -> Just(array:set(I, A, L))
  end.

//------------------------------------------------------------------------------
// Transformations -------------------------------------------------------------
//------------------------------------------------------------------------------

reverse(L) = array:from_list(lists:reverse(array:to_list(L))).


%
% exports.concat = function (xss) {
%   var result = [];
%   for (var i = 0, l = xss.length; i < l; i++) {
%     var xs = xss[i];
%     for (var j = 0, m = xs.length; j < m; j++) {
%       result.push(xs[j]);
%     }
%   }
%   return result;
% };
%
% exports.filter = function (f) {
%   return function (xs) {
%     return xs.filter(f);
%   };
% };
%
% exports.partition = function (f) {
%   return function (xs) {
%     var yes = [];
%     var no  = [];
%     for (var i = 0; i < xs.length; i++) {
%       var x = xs[i];
%       if (f(x))
%         yes.push(x);
%       else
%         no.push(x);
%     }
%     return { yes: yes, no: no };
%   };
% };
%
% //------------------------------------------------------------------------------
% // Sorting ---------------------------------------------------------------------
% //------------------------------------------------------------------------------
%
% exports.sortImpl = function (f) {
%   return function (l) {
%     // jshint maxparams: 2
%     return l.slice().sort(function (x, y) {
%       return f(x)(y);
%     });
%   };
% };
%
% //------------------------------------------------------------------------------
% // Subarrays -------------------------------------------------------------------
% //------------------------------------------------------------------------------
%
% exports.slice = function (s) {
%   return function (e) {
%     return function (l) {
%       return l.slice(s, e);
%     };
%   };
% };
%
% exports.take = function (n) {
%   return function (l) {
%     return n < 1 ? [] : l.slice(0, n);
%   };
% };
%
% exports.drop = function (n) {
%   return function (l) {
%     return n < 1 ? l : l.slice(n);
%   };
% };
%
% //------------------------------------------------------------------------------
% // Zipping ---------------------------------------------------------------------
% //------------------------------------------------------------------------------
%
% exports.zipWith = function (f) {
%   return function (xs) {
%     return function (ys) {
%       var l = xs.length < ys.length ? xs.length : ys.length;
%       var result = new Array(l);
%       for (var i = 0; i < l; i++) {
%         result[i] = f(xs[i])(ys[i]);
%       }
%       return result;
%     };
%   };
% };

unsafeIndexImpl(A,N) -> array:get(N,A).
