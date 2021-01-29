gcd(_, 1, 1).
gcd(X, 0, X).
gcd(X, Y, R) :- X #< Y, gcd(Y, X, R), !.
gcd(X, Y, R) :- X #>= Y, G #= X mod Y, gcd(Y, G, R), !.

access([], 0, nil).
access([H|_], 0, H).
access([_|T], N, E) :- access(T, M, E), N #= M + 1, N #> 0.

randint(N, I) :- random(R), floor(R * N, I).

any([], nil).
any([H], H).
any(Es, E) :-   length(Es, Length),
                randint(Length, I),
                access(Es, I, E).

slice(Es, To, Slice) :- make_slice(Es, Slice, 1, 1, To).
slice(Es, From, To, Slice) :- make_slice(Es, Slice, From, 1, To).

make_slice([], [], _, _, _).
make_slice([E | Rest], Slice, From, Cur, To) :- From =< Cur,
                                                Cur =< To,
                                                append([E], Small, Slice),
                                                Next #= Cur + 1,
                                                make_slice(Rest, Small, From, Next, To),
                                                !.
make_slice([_ | Rest], Slice, From, Cur, To) :- (Cur < From ; To < Cur),
                                                Next #= Cur + 1,
                                                make_slice(Rest, Slice, From, Next, To),
                                                !.

absval(0, 0).
absval(Positive, Positive) :- Positive #> 0.
absval(Negative, Absolute) :- Negative #< 0, Absolute #= Negative * -1.

n_mod_2(N, R) :- R #= N mod 2.

odd(N) :- n_mod_2(N, 1).
even(N) :- n_mod_2(N, 0).

classify(X) :- integer(X), odd(X), write('odd'), !.
classify(X) :- integer(X), even(X), write('even'), !.
classify(X) :- float(X), write('not an integer'), !.
classify(X) :- \+ number(X), write('not a number at all'), !.

my_cut :- !.

fact(1).
fact(2).
cuttest0(X) :- fact(X), !.
cuttest1(X) :- fact(X), my_cut.

f(X) :- g(X), \+ (h(X), !).

g(1).
g(2).
h(3).
h(4).

non_integer(X) :- integer(X), !, fail.
non_integer(_).

typewriter :- repeat, get0(C), put(C), (C = 10 ; C = 13), !.

mirror([], []).
mirror([Head|Tail], MirroredList) :- mirror(Tail, ReversedTail), append(ReversedTail, [Head], MirroredList).

head([], _) :- fail.
head([H|_], H).

flatten([], []).
flatten([[H|T]|Rest], F) :- flatten(Rest, R), append([H|T], R, F).

last([H|T], E) :- T = [], E = H.
last([_|T], E) :- last(T, E).

unique(X, Es) :- tfilter(=(X), Es, [_]).

% unique(E, Es) :- freq(E, Es, 1).

freq(_, [], 0).
freq(E, [H|T], F) :- H #= E, F #= P + 1, freq(E, T, P), !.
freq(E, [H|T], F) :- H #\= E, freq(E, T, F), !.

contains([H|_], H).
contains([_|L], E) :- contains(L, E).

member(X, [X|_]).
member(X, [_|L]) :- member(X, L).

subset_rec([], At, Len, []) :- At #= Len.
subset_rec([_|Y], At, Len, Acc) :- At #< Len, Succ #= At + 1, subset_rec(Y, Succ, Len, Acc).
subset_rec([E|Y], At, Len, [H|Acc]) :- At #< Len, E = H, Succ #= At + 1, subset_rec(Y, Succ, Len, Acc).

subset(X, Y) :- length(Y, Len), subset_rec(Y, 0, Len, X).

intersection([], _, []).
intersection([X|A], B, [X|I]) :- member(X, B), !, intersection(A, B, I).
intersection([_|A], B, I) :- intersection(A, B, I).

power(X, P) :- findall(T, subset(T, X), P).
