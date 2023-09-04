/* YOUR CODE HERE (Problem 1, delete the following line) */
reverseL(X,RevSol) :- my_rev(X,RevSol,[]).
my_rev([],RevSol,RevSol) :- !.
my_rev([X|Xs],RevSol,Acc) :- my_rev(Xs,RevSol,[X|Acc]).

/* YOUR CODE HERE (Problem 2, delete the following line) */
removeElement(_, [], []) :- !.
removeElement(X, [X|X1], Y) :- !, removeElement(X, X1, Y).
removeElement(X, [Tail|X1], Y) :- !, removeElement(X, X1, Y2), append([Tail], Y2, Y).

remove_duplicates([], []).
remove_duplicates([X|Y], Z):- 
  removeElement(X, Y, Y1), remove_duplicates(Y1, Z1), append([X], Z1, Z).

/* Your CODE HERE (Problem 3, delete the following line) */
count([],X,0).
count([X|Tail],X,Y):- count(Tail,X,Z), Y is 1+Z.
count([X1|Tail],X,Z):- X1\=X,count(Tail,X,Z).

assoc_list([],[]).
assoc_list([A],[A-1]).
assoc_list([X|Y],Z) :- 
  count([X|Y],X,C), removeElement(X,Y,Y1), assoc_list(Y1,Z1), append([X-C], Z1, Z).

/* YOUR CODE HERE (Problem 4, delete the following line) */
intersectionL([], _, []):- !. 
intersectionL(_, [], []):- !.
intersectionL([X|Tail1], List2, [X|Tail3]):- member(X, List2), intersectionL(Tail1, List2, Tail3), !.
intersectionL([_|Tail1], List2, List3):- intersectionL(Tail1, List2, List3).

/* YOUR CODE HERE (Problem 8, delete the following line) */
divide([], [], []).
divide([A], [A], []).
divide([A, B | R], [A | RRA], [B | RRB]) :-  divide(R, RRA, RRB).

my_merge(A, [], A).
my_merge([], B, B).
my_merge([A | RRA], [B | RRB], [A | Median]) :-
  A =< B,
  my_merge(RRA, [B | RRB], Median).
my_merge([A | RRA], [B | RRB], [B | Median]) :-
  A > B,
  my_merge([A | RRA], RRB, Median).

mergesort([], []). 
mergesort([A], [A]).
mergesort([A, B | Rest], S) :-
  divide([A, B | Rest], L1, L2),
  mergesort(L1, S1),
  mergesort(L2, S2),
  my_merge(S1, S2, S).

/* YOUR CODE HERE (Problem 5, delete the following line) */
list_sum([], 0).
list_sum([Head|Tail], Sum):-
    list_sum(Tail, Sum1),
    Sum is Head + Sum1.

n_largest(L, N, R) :-
    mergesort(L, LS),
    length(R, N),
    append(_, R, LS).

maxL3([],X).
maxL3(L,X) :-
  length(L,N), 
  (N >= 3 -> 
    n_largest(L,3,Y),
    list_sum(Y,X)
  ; false).

/* YOUR CODE HERE (Problem 6, delete the following line) */
partition([],[],[]).
partition([H],[H],[]). 
partition(L, Prefix, Suffix) :-
    length(L, N),
    PrefixLen is N div 2,
    length(Prefix, PrefixLen),
    append(Prefix, Suffix, L).

/* YOUR CODE HERE (Problem 7, delete the following line) */
merge([], [], []):- !. 
merge(List1, [], List1):- !. 
merge([], List2, List2):- !. 
merge([X|List1], [Y|List2], Z):- X<Y, merge(List1, [Y|List2], Z1), append([X], Z1, Z), !.
merge(List1, [Y|List2], Z):- merge(List1, List2, Z1), append([Y], Z1, Z).



