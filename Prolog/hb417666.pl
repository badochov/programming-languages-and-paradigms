% Hubert Badocha

% dfa(FunkcjaPrzejścia, StanPoczątkowy, ZbiórStanówAkceptujących),
% FunkcjaPrzejścia jest listą termów postaci fp(S1, C, S2) oznaczających,
% że: δ(S1, C) = S2,
% StanPoczątkowy jest początkowym stanem automatu,
% ZbiórStanówAkceptujących jest listą (bez powtórzeń) wszystkich stanów akceptujących danego automatu.

use_module(library(lists)).

% correct(+Automat, -Reprezentacja)
correct(+dfa(Tf, Ss, As), -R) :- 
    transform_tf(Tf, Tf_), 
    validate_tf(Tf_), 
    sort(As, Sas), 
    validate_states(Tf_, Ss, Sas).

transform_tf(+Tf, -Res) :- transform_tf(Tf, [], Res).
transform_tf(+[], +Res, -Res).
transform_tf(+[fp(S1, C, S2)|T], +Cur, -Res) :-
    process_fp(S1, C, S2, Cur, [], P),
    transform_tf(T, P, Res).


process_fp(+S1, +C, +S2, +[ts(S,Trans)|T], +Pre, -P) :- 
    ( S1 == S ->
        Ntrans is [on(C, S2)|Trans],
        append(Pre, [ts(S,NTrans)|T], P)
    ;
        process_fp(S1, C, S2, H, [ts(S,Trans)|Pre], P)
        ).

process_fp(+S1, +C, +S2, +[], +Pre, -P) :- P is [(ts(S1,[on(C, S2)]))|Pre].

validate_tf(+[ts(_,Trans)|T]) :- 
    get_alphabet(Trans, Alphabet),
    Alphabet \= [],
    sort(Alphabet, SAlphabet),
    validate_tf(SAlphabet, T).

validate_tf(+Alphabet, +ts(_,Trans)) :-
    get_alphabet(Trans, CAlphabet),
    sort(CAlphabet, SCAlphabet),
    SCAlphabet = Alphabet.

get_alphabet(+Trans, -Alphabet) :- get_alphabet(Trans, [], Alphabet). 
get_alphabet(+[], +Alphabet, -Alphabet).
get_alphabet(+[(on(Letter, _))|T], +R, -Alphabet) :- get_alphabet(T, [Letter|T], Alphabet).

validate_states(+Tf, +Ss, +Sas) :- 
    get_states(Tf,States), 
    sort(States, SStates),
    member(Ss, SStates), 
    segment(SStates, Sas).

get_states(+Tf, -S) :- get_states(Tf, [], S).
get_states(+[], -S, +S).
get_states(+[ts(St,_)|T], +R, -S) :- get_states(T, [St|R], S).


% accept(+Automat, ?Słowo)

% empty(+Automat)

% equal(+Automat1, +Automat2)
equal(+A1, +A2) :- correct(A1, C1), correct(A2, C2), subsetEq_(C1, C2), subsetEq_(C2, C1).

% subsetEq(+Automat1, +Automat2)
subsetEq(+A1, +A2) :- correct(A1, C1), correct(A2, C2), subsetEq_(C1, C2).

subsetEq_(+A1, +A2) :- fail.
% Recall some closure properties of regular languages, namely that they are closed under complementation and intersection. Now, L(A)⊆L(B) whenever L(A)∩L(B)¯¯¯¯¯¯¯¯¯¯¯ is empty. So you need to construct the automaton that accepts the intersection of A and the complement of B – these are standard constructions – and test whether the resulting automaton accepts the empty language. This can be checked by inspecting the automaton to see whether any accepting states are reachable from the initial state. If not, then the language accepted is empty.



% example(IdentyfikatorAutomatu, Automat)
example(a11, dfa([fp(1,a,1),fp(1,b,2),fp(2,a,2),fp(2,b,1)], 1, [2,1])).
example(a12, dfa([fp(x,a,y),fp(x,b,x),fp(y,a,x),fp(y,b,x)], x, [x,y])).
example(a2, dfa([fp(1,a,2),fp(2,b,1),fp(1,b,3),fp(2,a,3),fp(3,b,3),fp(3,a,3)], 1, [1])).
example(a3, dfa([fp(0,a,1),fp(1,a,0)], 0, [0])).
example(a4, dfa([fp(x,a,y),fp(y,a,z),fp(z,a,x)], x, [x])).
example(a5, dfa([fp(x,a,y),fp(y,a,z),fp(z,a,zz),fp(zz,a,x)], x, [x])).
example(a6, dfa([fp(1,a,1),fp(1,b,2),fp(2,a,2),fp(2,b,1)], 1, [])).
example(a7, dfa([fp(1,a,1),fp(1,b,2),fp(2,a,2),fp(2,b,1), fp(3,b,3),fp(3,a,3)], 1, [3])).

% bad ones
example(b1, dfa([fp(1,a,1),fp(1,a,1)], 1, [])).
example(b2, dfa([fp(1,a,1),fp(1,a,2)], 1, [])).
example(b3, dfa([fp(1,a,2)], 1, [])).
example(b4, dfa([fp(1,a,1)], 2, [])).
example(b4, dfa([fp(1,a,1)], 1, [1,2])).
example(b5, dfa([], [], [])).


% Success
example(a11, A), example(a12, B), equal(A, B).
example(a2, A), example(a1, B), subsetEq(A, B).
example(a5, A), example(a3, B), subsetEq(A, B).
example(a6, A), empty(A).
example(a7, A), empty(A).
example(a2, A), accept(A, []).
example(a2, A), accept(A, [a,b]).
example(a2, A), accept(A, [a,b,a,b]).

% Failure
example(b1, A), correct(A, _).
example(b2, A), correct(A, _).
example(b3, A), correct(A, _).
example(b4, A), correct(A, _).
example(b5, A), correct(A, _).
example(a2, A), empty(A).
example(a3, A), example(a4, B), equal(A, B).
example(a4, A), example(a3, B), subsetEq(A, B).
example(a2, A), accept(A, [a]).