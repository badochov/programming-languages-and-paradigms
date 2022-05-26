% Hubert Badocha

% use_module(library(lists))


%%%%%%%%%%%%%%%%% BST %%%%%%%%%%%%%%%%%

ins(kv(El, V), nil, bst(kv(El, V), nil, nil)).
ins(kv(El, V), bst(kv(X, Vx), L, R), bst(kv(X2, Vx2), L2, R2)) :-
    ( El @< X -> 
        ins(kv(El, V), L, Nl),
        (X, Vx, Nl, R) = (X2, Vx2, L2, R2)
    ;
        ( El @> X -> 
            ins(kv(El, V), R, Nr),
            (X, Vx, L, Nr) = (X2, Vx2, L2, R2)
        ;
            (X, V, L, R) = (X2, Vx2, L2, R2)
        )
    ).


lookup(El, bst(kv(El, Kv), _, _), Kv).
lookup(El, bst(kv(K, _), L, _), V) :- El @< K, lookup(El, L, V).
lookup(El, bst(kv(K, _), _, R), V) :- El @> K, lookup(El, R, V).

lookup_default(El, T, Def, Res) :- 
    ( lookup(El, T, Res) -> 
        Res = Res
    ; 
        Def = Res
    ).

has(El, T) :- lookup(El, T, _).

keys(T, K) :- keys(T, [], K).
keys(nil, Res, Res).
keys(bst(kv(K,_), L, R), Tmp, Res) :-
    keys(R, Tmp, Kr),
    keys(L, [K|Kr], Res).

values(T, V) :- values(T, [], V).
values(nil, Res, Res).
values(bst(kv(_,V), L, R), Tmp, Res) :-
    values(R, Tmp, Vr),
    values(L, [V|Vr], Res).

keys_in([], _).
keys_in([H|T], Tr) :-
    has(H, Tr),
    keys_in(T, Tr).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% correct(+Automat, -Reprezentacja)
correct(dfa(Tf, Ss, As), R) :- 
    transform_tf(Tf, Tf_), 
    validate_tf(Tf_), 
    validate_states(Tf_, Tf, Ss, As),
    R = odfa(Tf_, Ss, As).


transform_tf(Tf, Res) :- transform_tf(Tf, nil, Res).
transform_tf([], Res, Res).
transform_tf([fp(S1, C, S2)|T], Cur, Res) :-
    lookup_default(S1, Cur, nil, V), 
    ins(kv(C, S2), V, Nv),
    ins(kv(S1, Nv), Cur, P), 
    transform_tf(T, P, Res).


validate_tf(bst(kv(_, V), L, R)) :- 
    keys(V, Alphabet),
    Alphabet \= [],
    validate_tf(Alphabet, L),
    validate_tf(Alphabet, R).

validate_tf(_, nil).
validate_tf(Alphabet, bst(kv(_, V), L, R)) :-
    keys(V, CAlphabet),
    CAlphabet = Alphabet,
    validate_tf(Alphabet, L),
    validate_tf(Alphabet, R).

validate_states(Tf, TfOrg, Ss, As) :- 
    has(Ss, Tf),
    keys_in(As, Tf),
    get_image(TfOrg, Image),
    keys_in(Image, Tf).

get_image(Tf, Image) :- get_image(Tf, [], Image).
get_image([], Image, Image).
get_image([fp(_, _, X)|T], Res, Image) :- get_image(T, [X|Res], Image).

% accept(+Automat, ?Słowo)
accept(A, W) :- 
    correct(A, odfa(Tf, Ss, As)),
    accept_(Tf, Ss, As, W).
accept_(_, S, As, []) :- member(S, As).
accept_(Tf, S, As, [H | T]) :-
 	lookup(S, Tf, Trs),
    apply_transformation(Trs, H, Ns),
    accept_(Tf, Ns, As, T).

get_state_transformations(Tf, S, Trs) :- lookup(S, Tf, Trs).

apply_transformation(Trs, L, Ns) :- lookup(L, Trs, Ns).

subset(A, B) :-
    sort(A, Sa),
    sort(B, Sb),
    segment(Sa, Sb).

segment([H | T], [Hs | Ts]) :-
    ( H = Hs ->
        prefix(T, Ts)
    ;
        segment(T, [Hs | Ts])
    ).

% empty(+Automat)
empty(A) :- 
    correct(A, odfa(Tf, Ss, As)),
    reachable_states(Tf, Ss, Rs),
    \+ overlap(Rs, As).

get_dest_states(Ts, States) :- values(Ts, States).

get_states_reachable_from(Tf,Ss, States) :-
    get_state_transformations(Tf, Ss, Ts),
    get_dest_states(Ts, States).

reachable_states(Tf, Ss, Rs) :- reachable_states(Tf, Ss, [], Rs).
reachable_states(Tf, Ss, Res, Rs) :-
    get_states_reachable_from(Tf, Ss, States),
    ( subset(Res, States) ->
        Rs = Res
    ;   
        add_states(Tf, States, Res, Rs)
        ).

add_states(Tf, [H|T], Res, Rs) :-
    (member(H, Res) -> 
        add_states(Tf, T, Res, Rs)
    ;   
        reachable_states(Tf, H, [H|Res], R_),add_states(Tf, T, R_, Rs)
    ).

overlap(L1, [H|_]) :- member(H, L1).
overlap(L1, [_|T]) :- overlap(L1, T).

% equal(+Automat1, +Automat2)
equal(A1, A2) :- 
    subsetEq(A1, A2), 
    subsetEq(A2, A1).

% subsetEq(+Automat1, +Automat2)
subsetEq(A1, A2) :- 
    correct(A1, C1), 
    correct(A2, C2), 
    subsetEq_(C1, C2).

subsetEq_(A1, A2) :- fail.
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


% % Success
% example(a11, A), example(a12, B), equal(A, B).
% example(a2, A), example(a1, B), subsetEq(A, B).
% example(a5, A), example(a3, B), subsetEq(A, B).
% example(a6, A), empty(A).
% example(a7, A), empty(A).
% example(a2, A), accept(A, []).
% example(a2, A), accept(A, [a,b]).
% example(a2, A), accept(A, [a,b,a,b]).

% % Failure
% example(b1, A), correct(A, _).
% example(b2, A), correct(A, _).
% example(b3, A), correct(A, _).
% example(b4, A), correct(A, _).
% example(b5, A), correct(A, _).
% example(a2, A), empty(A).
% example(a3, A), example(a4, B), equal(A, B).
% example(a4, A), example(a3, B), subsetEq(A, B).
% example(a2, A), accept(A, [a]).