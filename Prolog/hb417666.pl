% Hubert Badocha

% Opis reprezentacji:
% reprezentacja to sześć wartości:
% 1. Funckja przejścia w formie:
%       mapa ze stanów w mape z elementu alfabetu do stanu.
% 2 Zredukowana funkcja przejścia bez stanów 
%       uniemożliwiających zaakceptowanie słowa.
% 3. Stan początkowy
% 4. Zbiór stanów akceptujących
% 5. Lista stanów
% 6. Alfabet

:- use_module(library(lists)).

%%%%%%%%%%%%%%%%% BST %%%%%%%%%%%%%%%%%

% ins(+KeyValuePair, +Tree, +Res)
ins(kv(El, V), nil, bst(kv(El, V), nil, nil)).
ins(kv(El, V), bst(kv(X, Vx), L, R), bst(kv(X2, Vx2), L2, R2)) :-
    ( El @< X -> % Check if el should be in left subtree.
        ins(kv(El, V), L, Nl), % Insert to left subtree.
        (X, Vx, Nl, R) = (X2, Vx2, L2, R2)
    ;
        ( El @> X -> % Check if el should be in right subtree.
            ins(kv(El, V), R, Nr), % Insert to right subtree.
            (X, Vx, L, Nr) = (X2, Vx2, L2, R2)
        ;
            (X, V, L, R) = (X2, Vx2, L2, R2) % El should replace root.
        )
    ).


% lookup(El+, +Tree, -Value)
lookup(El, bst(kv(El, Kv), _, _), Kv).
lookup(El, bst(kv(K, _), L, _), V) :- El @< K, lookup(El, L, V).
lookup(El, bst(kv(K, _), _, R), V) :- El @> K, lookup(El, R, V).

% lookup_default(+El, +Tree, +Default, -Res)
lookup_default(El, T, _, Res) :- lookup(El, T, Res), !.
lookup_default(_, _, Res, Res).

% has(+EL, -Tree)
has(El, T) :- lookup(El, T, _).

% keys(+Tree, ?Keys)
keys(T, K) :- keys(T, [], K).
% keys(+Tree, +TmpRes, ?Keys)
keys(nil, Res, Res).
keys(bst(kv(K,_), L, R), Tmp, Res) :-
    keys(R, Tmp, Kr),
    keys(L, [K|Kr], Res).

% values(+Tree, ?Values)
values(T, V) :- values(T, [], V).
% values(+Tree, +TmpRes, ?Values)
values(nil, Res, Res).
values(bst(kv(_,V), L, R), Tmp, Res) :-
    values(R, Tmp, Vr),
    values(L, [V|Vr], Res).

% in_keys(+List, +Tree)
in_keys([], _).
in_keys([H|T], Tr) :-
    has(H, Tr), % Check if head is in the tree.
    in_keys(T, Tr). % Check if the rest is in the tree.

% to_set(+List, -Set)
to_set(L, S) :- to_set(L, nil, S).
% to_set(+List, +Tmp, -Set)
to_set([], S, S).
to_set([H|T], Tmp, S) :- 
    ins(kv(H, nil), Tmp, Nt),
    to_set(T, Nt, S).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% correct(+Automat, -Reprezentacja)
correct(dfa(Tf, Ss, As), odfa(Tf_, TfClean, Ss, Sas, States, Alpha)) :- 
    transform_tf(Tf, [], Tf_), % Transform transition function to map of maps
    validate_tf(Tf_, Alpha), % Validate that transition function is correct
    validate_states(Tf_, Tf, Ss, As), % Validate that provided states correct.
    to_set(As, Sas), % Convert accepting states to set.
    get_states(Tf_, States), % Get states from dfa.
    get_dead_states(Tf_, States, Sas, DeadStates), % Get dead states.
    transform_tf(Tf, DeadStates, TfClean). % Redo transition function without dead states.


% get_not_in(+TransitionFunction, -State)
get_states(Fp, States) :- keys(Fp, States).

% get_dead_states(+TransformationFunction, +AcceptingStates, -DeadStates)
get_dead_states(Tf, States, As, DeadStates) :-get_dead_states(Tf, As, States, [], DeadStates).
% get_dead_states(+TransformationFunction, +AcceptingStates, +States, +Tmp, -DeadStates)
get_dead_states(_, _, [], DeadStates, DeadStates).
get_dead_states(Tf, As, [H|T], Tmp, DeadStates) :-
    ( alive_state(Tf, As, H) ->
        get_dead_states(Tf, As, T, Tmp, DeadStates)
    ;
        get_dead_states(Tf, As, T, [H|Tmp], DeadStates)
    ).
% dead_state(+TransitionFunction, +AcceptingState, +State)
alive_state(Tf, As, S) :-
    reachable_states(Tf, S, Rs), % Get reachable states.
    ins(kv(S, nil), Rs, Nrs),
    overlap(Nrs, As). % Check if any of the reachable states is accepting.

% transform_tf(+TransitionFunctionOrg, +Blocks, -TransitionFunctionMaps)
transform_tf(Tf, Blocks, Res) :- 
    to_set(Blocks, Bs),
    transform_tf(Tf, Bs, nil, Res).

% blocke(+State1, +State2, +Blockset)
blocked(S1, _, Bs) :- has(S1, Bs).
blocked(_, S2, Bs) :- has(S2, Bs).

% transform_tf(+TransitionFunctionOrg, +BlockSet, +Tmp, -TransitionFunctionMaps)
transform_tf([], _, Res, Res).
transform_tf([fp(S1, C, S2)|T], Blockset, Cur, Res) :-
    ( blocked(S1, S2, Blockset) ->  
      	transform_tf(T, Blockset, Cur, Res)    % Convert rest of the transitions.
    ;   
        lookup_default(S1, Cur, nil, V),    % Get transitions from current state.
        \+ has(C, V),               % Assert that transition wasn't added before.
        ins(kv(C, S2), V, Nv),      % Insert new transtion from the state.     
        ins(kv(S1, Nv), Cur, P),    % Update transition for current state.
        transform_tf(T, Blockset, P, Res)    % Convert rest of the transitions.
    ).
    

% validate_tf(+TransitionFunction, ?Alphabet)
validate_tf(nil, Alphabet) :- nonvar(Alphabet).
validate_tf(bst(kv(_, V), L, R), Alphabet) :- 
    keys(V, Alphabet),  % Get alphabet.
    Alphabet \= [],     % Assume alphabet is sane.
    validate_tf(L, Alphabet),   % Check that transitions in left subtree 
                                % have the same alphabet.
    validate_tf(R, Alphabet).   % Check that transitions in right subtree 
                                % have the same alphabet.

% validate_states(
%                   +TranstionFunction, 
%                   +TranstionFunctionOrg, 
%                   +StartingState
%                   +AcceptingState
%         )
validate_states(Tf, TfOrg, Ss, As) :- 
    has(Ss, Tf),     % Check starting state belongs to transition function.
    in_keys(As, Tf), % Check accepting states belong to transition function.
    get_image(TfOrg, Image), % Get all states used in transition function.
    in_keys(Image, Tf). % Check all states have transitions.

% get_image(+TransitionFunction, -Image)
get_image(Tf, Image) :- get_image(Tf, [], Image).
% get_image(+TransitionFunction, +Tmp, -Image)
get_image([], Image, Image).
get_image([fp(_, _, X)|T], Res, Image) :- get_image(T, [X|Res], Image).

% accept(+DFA, ?Word)
accept(A, W) :- 
    correct(A, Oa), % Check DFA is correct and get representation.
    accept_h(Oa, W).
    %(acceptt(Oa, [], W); 
    %accept_(Oa, 1, [], W)). 

accept_h(Oa, W) :-
   ( var(W) ->  
   		accept_h(Oa, 0, cw([], 0), W)
   ;   
   		accept_bind(Oa, W)
   ).


accept_h(Oa, L, Cw, W) :- 
    has_len(Oa, 0, L), 
    ( 
    	accept_up_to(Oa, L, Cw, W) 
    ; 
    	L2 is L + 1, accept_h(Oa, L2, Cw, W)
    ).

% accept_bind(+ODFA, +Word)
accept_bind(odfa(_, _, S, As, _, _), []) :- has(S, As). 
                                % If word is empty check if state accepts.
accept_bind(odfa(Tf, Ctf, S, As, States, A), [H|T]) :-
    get_state_transformations(Ctf, S, Trs),
                                     % Get transitions from current state.
    in_alphabet(H, A),          % Check if head is from alphabet.
    apply_transformation(Trs, H, Ns),
    accept_bind(odfa(Tf, Ctf, Ns, As, States, A), T). % Check if tail can accept.

% has_len(+odfa, +TmpLen, +DesiredLen).
has_len(_, L, L) :- !.
has_len(odfa(Tf, Ctf, S, As, States, A), Cl, L) :- 
    get_state_transformations(Ctf, S, Trs),
                                     % Get transitions from current state.
    in_alphabet(H, A),          % Check if head is from alphabet.
    apply_transformation(Trs, H, Ns),
    Cl2 is Cl + 1,
    has_len(odfa(Tf, Ctf, Ns, As, States, A), Cl2, L), !.

% accept_up_to(+odfa, +Length, +CurrentWord, - Word).
accept_up_to(odfa(_, _, S, As, _, _), L, cw(Cw, L), W) :- 
    has(S, As),
    reverse(Cw, W).
accept_up_to(odfa(Tf, Ctf, S, As, States, A), L, cw(Cw, Cwl), W) :- 
    Cwl2 is Cwl + 1,
    L @>= Cwl2,
    get_state_transformations(Ctf, S, Trs),
                                     % Get transitions from current state.
    in_alphabet(H, A),          % Check if head is from alphabet.
    apply_transformation(Trs, H, Ns),
    accept_up_to(odfa(Tf, Ctf, Ns, As, States, A), L, cw([H|Cw], Cwl2), W).

% in_alphabet(+Letter, +Alphabet).
in_alphabet(H, [_|T]) :- in_alphabet(H, T).
in_alphabet(H, [H|_]).

% get_state_transformations(+TransitionFunction, +State, -Transitions).
get_state_transformations(Tf, S, Trs) :- lookup(S, Tf, Trs).

% apply_transformation(+Transitions, +Letter, -NewState).
apply_transformation(Trs, L, Ns) :- lookup(L, Trs, Ns).

% all_in(+L, +S)
% Check if all elements of L are in S
all_in([], _).
all_in([H|T], S) :-
    has(H, S),
    all_in(T, S).

% empty(+DFA)
empty(A) :- 
    correct(A, E),
    empty_(E).

% empty(+ODFA)
empty_(odfa(Tf, _, Ss, As, _, _)) :-
    reachable_states(Tf, Ss, Rs), % get states reachable from staring state.
    \+ overlap(Rs, As). % check if any of reachable states is accepting.

% get_dest_states(+LetterToState, -States)
get_dest_states(Ts, States) :- values(Ts, States).

% get_states_reachable_from(+TransitionFunction, +StartingState, -States)
get_states_reachable_from(Tf,Ss, States) :-
    get_state_transformations(Tf, Ss, Ts),
    get_dest_states(Ts, States).

% reachable_state(+TransitionFunction, +StartingState, -ReachableStates)
reachable_states(Tf, Ss, Rs) :- reachable_states(Tf, Ss, nil, Rs).
% reachable_state(+TransitionFunction, +StartingState, +Tmp, -ReachableStates)
reachable_states(Tf, Ss, Res, Rs) :-
    get_states_reachable_from(Tf, Ss, States),
    ( all_in(States, Res) ->
        Rs = Res
    ;   
        add_states(Tf, States, Res, Rs)
    ).

% add_states(+TransitionFunction, +StatesToAdd, +Tmp, -ReachableStates)
add_states(_, [], Res, Res).
add_states(Tf, [H|T], Res, Rs) :-
    (has(H, Res) -> 
        add_states(Tf, T, Res, Rs)
    ;   
        ins(kv(H, nil), Res, Nr),
        reachable_states(Tf, H, Nr, R_),
        add_states(Tf, T, R_, Rs)
    ).

% overlap(+Set, +Set2)
overlap(bst(kv(K, _), _, _), S) :- has(K, S), !.
overlap(bst(_, L, _), S) :- overlap(L, S), !.
overlap(bst(_, _, R), S) :- overlap(R, S), !.

% equal(+DFA, +DFA)
equal(A1, A2) :- 
    correct(A1, C1), 
    correct(A2, C2), 
    subsetEq_(C1, C2), 
    subsetEq_(C2, C1).

% subsetEq(+Dfa, +Dfa2)
subsetEq(A1, A2) :- 
    correct(A1, C1), 
    correct(A2, C2), 
    subsetEq_(C1, C2).

% subsetEq(+ODFA, +ODFA2)
subsetEq_(A1, A2) :- 
    complement(A2, C2),
    intersection(A1, C2, I),
    empty_(I).

% combine_tf(
%    +TranstionFunction, 
%    +TransitionFunction2, 
%    -CombinedTransitionFunction
% )
combine_tf(Tf, Tf2, Tfc) :- combine_tf(Tf, Tf2, nil, Tfc).

% combine_tf(
%    +TranstionFunction, 
%    +TransitionFunction2, 
%    +Tmp,
%    -CombinedTransitionFunction
% )
combine_tf(nil, _, Tf, Tf).
combine_tf(bst(Kv, L, R), Tf, Res, Tfc) :-
    combine_kv(Kv, Tf, Res, TfKv), % Combine transtions from root state.
    combine_tf(R, Tf, TfKv, TfR), % Combine transitions in left tree.
    combine_tf(L, Tf, TfR, Tfc).  % Combine transitions in right tree.

% combine_tf(
%    +StateAndTranistionFunction, 
%    +TransitionFunction, 
%    +Tmp,
%    -CombinedTransitionFunction
% )
combine_kv(_, nil, Tfc, Tfc).
combine_kv(kv(K, V), bst(kv(TK, TV), L, R), Res, Tfc) :-
    combine_v(V, TV, nil, Vc), % Combine transtions from root.
    ins(kv((K,TK), Vc), Res, Nr), % Insert combined transition to output.
    combine_kv(kv(K, V), L, Nr, Nlr), % Combine transitions in left tree.
    combine_kv(kv(K, V), R, Nlr, Tfc). % Combine transitions in right tree.

% combine_v(
%    +TransitionsFromState, 
%    +TransitionsFromState2, 
%    +Tmp, 
%    -CombinedTransitionsFromStates
% )
combine_v(nil, _, Vc, Vc).
combine_v(bst(kv(K,S), L, R), Tv, Re, Vc) :- 
    lookup(K, Tv, Sl), % Get transitions from current letter.
    ins(kv(K, (S, Sl)), Re, Res), % Insert combined transition from 
                                  % current letter.
    combine_v(L, Tv, Res, Tl), % Combine left subtree.
    combine_v(R, Tv, Tl, Vc).  % Combine right subtree.
    

% combine_ss(+Ss, +Ss2, -CombinedSS)
combine_ss(Ss, Ss2, (Ss, Ss2)).

% combine_as(+As1, +As2, -CombinedAs)
combine_as(As, As2, Res) :- cartesian_prod(As, As2, nil, Res).

% cartesian_prod(+S1, +S2, +Tmp, -Res)
% Calculate cartesian product of two sets.
cartesian_prod(nil, _, Prod, Prod).
cartesian_prod(bst(kv(H,_), L, R), S, P, Prod) :- 
    cartesian_prod_helper(H, S, P, Res), % Calc pairs with current element.
    cartesian_prod(L, S, Res, LRes), % Calc pairs with left tree.
    cartesian_prod(R, S, LRes, Prod). % Calc pairs with right tree.

% cartesian_prod_helper(+El, +Set, +TmpRet, -Ret)
% Creates cartesian product of Set with Element.
cartesian_prod_helper(_, nil, Prod, Prod).
cartesian_prod_helper(El, bst(kv(H,_), L, R), P, Prod) :- 
    ins(kv((El,H),nil), P, Nr),
    cartesian_prod_helper(El, L, Nr, Lp),
    cartesian_prod_helper(El, R, Lp, Prod).


% intersection(+Representation1, +Representation2, +IntersectionRepresentation)
intersection(odfa(Tf, _, Ss, As, _, A), odfa(Tf2, _, Ss2, As2, _, A), odfa(Tfc, Tfc, Ssc, Asc, Ls, A)) :-
    combine_tf(Tf, Tf2, Tfc), % Create transition fn operationg on state pairs.
    combine_ss(Ss, Ss2, Ssc), % Convert starting state to pair of states.
    combine_as(As, As2, Asc), % Convert accepting states to pairs of states.
    get_states(Tfc, Ls).

% get_not_in(+List, +BlockSet, -NotBlocked)
get_not_in(L, S, Res) :- get_not_in(L, S, nil, Res).

% get_not_in(+List, +BlockSet, +TmpRes, -NotBlocked)
get_not_in([], _, Res, Res).
get_not_in([H|T], S, R, Res) :- 
    ( has(H, S) ->
        get_not_in(T, S, R, Res)
    ;
    	ins(kv(H,nil), R, Nr),
        get_not_in(T, S, Nr, Res)
    ).

% complement(+ Representation, -ComlementRepresentation)
complement(odfa(Fp, Cfp, Ss, As, States, A), odfa(Fp, Cfp, Ss, Nas, States, A)) :- 
    get_not_in(States, As, Nas).
