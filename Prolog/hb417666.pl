% Hubert Badocha

% Opis reprezentacji:
% reprezentacja to cztery wartości:
% 1. Funckja przejścia w formie:
%       mapa ze stanów w mape z elementu alfabetu do stanu.
% 2. Stan początkowy
% 3. Zbiór stanów akceptujących
% 4. Alfabet

:- use_module(library(lists)).


%%%%%%%%%%%%%%%%% BST %%%%%%%%%%%%%%%%%

% ins(+KeyValuePair, +Tree, +Res)
ins(kv(El, V), nil, bst(kv(El, V), nil, nil)).
ins(kv(El, V), bst(kv(X, Vx), L, R), bst(kv(X2, Vx2), L2, R2)) :-
    ( El @< X -> % Check is el should be in left subtree.
        ins(kv(El, V), L, Nl), % Insert to left subtree.
        (X, Vx, Nl, R) = (X2, Vx2, L2, R2)
    ;
        ( El @> X -> % Check is el should be in right subtree.
            ins(kv(El, V), R, Nr), % Insert to right subtree.
            (X, Vx, L, Nr) = (X2, Vx2, L2, R2)
        ;
            (X, V, L, R) = (X2, Vx2, L2, R2) % El should replace root.
        )
    ).


lookup(El, bst(kv(El, Kv), _, _), Kv). % Check if element is in root.
lookup(El, bst(kv(K, _), L, _), V) :- 
    ( nonvar(El) -> % Depending if we are generating or not change order.
                    % Order change for complexity. 
    	El @< K, lookup(El, L, V) % Check left subtree is El should be there.
    ;   
    	lookup(El, L, V), El @< K
    ).
lookup(El, bst(kv(K, _), _, R), V) :- 
    ( nonvar(El) -> % Depending if we are generating or not change order.
                  % Order change for complexity. 
    	El @> K, lookup(El, R, V) % Check right subtree is El should be there.
    ;   
    	lookup(El, R, V), El @> K
    ).

% lookup_default(+El, +Tree, +Default, -Res)
lookup_default(El, T, Def, Res) :- 
    (lookup(El, T, Res) -> % Check if element is in the tree
    	Res = Res % If it is, do nothing.
    ;
    	Res = Def % If it is not, return default value.
    ).

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% correct(+Automat, -Reprezentacja)
% TODO zamienić AS na zbiór
correct(dfa(Tf, Ss, As), odfa(Tf_, Ss, As, Alpha)) :- 
    transform_tf(Tf, Tf_), % Transform transition function to map of maps
    validate_tf(Tf_, Alpha), % Validate that transition function is correct
    validate_states(Tf_, Tf, Ss, As). % Validate that provided states correct.


% transform_tf(+TransitionFunctionOrg, -TransitionFunctionMaps)
transform_tf(Tf, Res) :- transform_tf(Tf, nil, Res).
% transform_tf(+TransitionFunctionOrg, +Tmp, -TransitionFunctionMaps)
transform_tf([], Res, Res).
transform_tf([fp(S1, C, S2)|T], Cur, Res) :-
    lookup_default(S1, Cur, nil, V),    % Get transitions from current state.
    \+ has(C, V),               % Assert that transition wasn't added before.
   	ins(kv(C, S2), V, Nv),      % Insert new transtion from the state.     
    ins(kv(S1, Nv), Cur, P),    % Update transition for current state.
    transform_tf(T, P, Res).    % Convert rest of the transitions.

% validate_tf(+TransitionFunction, -Alphabet)
validate_tf(bst(kv(_, V), L, R), Alphabet) :- 
    keys(V, Alphabet),  % Get alphabet.
    Alphabet \= [],     % Assume alphabet is sane.
    validate_tf(Alphabet, L),   % Check that transitions in left subtree 
                                % have the same alphabet.
    validate_tf(Alphabet, R).   % Check that transitions in right subtree 
                                % have the same alphabet.

% validate_tf(+Alphabet, +TransitionFunction)
validate_tf(_, nil).
validate_tf(Alphabet, bst(kv(_, V), L, R)) :-
    keys(V, Alphabet),  % Check that alphabet is same as provided.
    validate_tf(Alphabet, L), % Check left subtree.
    validate_tf(Alphabet, R). % Check right subtree.

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
    accept_(Oa, W). 
% accept_(+ODFA, ?Work)
accept_(odfa(_, S, As, _), []) :- member(S, As). 
                                % If word is empty check if state accepts.
accept_(odfa(Tf, S, As, A), [H|T]) :-
    ( nonvar(H) ->                % Check if we are generating.
        % We are not generating.
        get_state_transformations(Tf, S, Trs), 
                                    % Get transitions from current state.
        apply_transformation(Trs, H, Ns), % Get transition for head.
    	accept_(odfa(Tf,Ns, As, A), T) % Check tail.
    ;   
        % We are generating
    	accept_(odfa(Tf,Ns, As, A), T), % Check if tail can accept.
        in_alphabet(H, Tf),          % Check if head is from alphabet.
        get_state_transformations(Tf, S, Trs),
                                     % Get transitions from current state.
        apply_transformation(Trs, H, Ns) 
                        % Check if there head transitioning to desired state.
    ).


% in_alphabet(+El, +TransitionFunction)
in_alphabet(X, bst(kv(_, V), _, _)) :- has(X, V).

% get_state_transformations(+TransitionFunction, +State, -Transitions).
get_state_transformations(Tf, S, Trs) :- lookup(S, Tf, Trs).

% apply_transformation(+Transitions, +Letter, -NewState).
apply_transformation(Trs, L, Ns) :- lookup(L, Trs, Ns).

% subset(+A, +B)
% Check is B is subset of A
subset(A, B) :-
    sort(A, Sa),
    sort(B, Sb),
    segment(Sa, Sb).

% empty(+DFA)
empty(A) :- 
    correct(A, E),
    empty_(E).

% empty(+ODFA)
empty_(odfa(Tf, Ss, As, _)) :-
    reachable_states(Tf, Ss, Rs), % get states reachable from staring state.
    \+ overlap(Rs, As). % check if any of reachable states is accepting.

% get_dest_states(+LetterToState, -States)
get_dest_states(Ts, States) :- values(Ts, States).

% get_states_reachable_from(+TransitionFunction, +StartingState, -States)
get_states_reachable_from(Tf,Ss, States) :-
    get_state_transformations(Tf, Ss, Ts),
    get_dest_states(Ts, States).

% reachable_state(+TransitionFunction, +StartingState, -ReachableStates)
reachable_states(Tf, Ss, Rs) :- reachable_states(Tf, Ss, [], Rs).
% reachable_state(+TransitionFunction, +StartingState, +Tmp, -ReachableStates)
reachable_states(Tf, Ss, Res, Rs) :-
    get_states_reachable_from(Tf, Ss, States),
    ( subset(Res, States) ->
        Rs = Res
    ;   
        add_states(Tf, States, Res, Rs)
        ).

% add_states(+TransitionFunction, +StatesToAdd, +Tmp, -ReachableStates)
add_states(_, [], Res, Res).
add_states(Tf, [H|T], Res, Rs) :-
    (member(H, Res) -> 
        add_states(Tf, T, Res, Rs)
    ;   
        reachable_states(Tf, H, [H|Res], R_),add_states(Tf, T, R_, Rs)
    ).

% overlap(+List1, +List2)
overlap(L1, [H|_]) :- member(H, L1).
overlap(L1, [_|T]) :- overlap(L1, T).

% equal(+DFA, +DFA)
equal(A1, A2) :- 
    subsetEq(A1, A2), 
    subsetEq(A2, A1).

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
combine_as(As, As2, Res) :- cartesian_prod(As, As2, [], Res).

% cartesian_prod(+L1, +L2, +Tmp, -Res)
% Calculate cartesian product of two lists.
cartesian_prod([], _, Prod, Prod).
cartesian_prod([H|T], X, P, Prod) :- 
    cartesian_prod_helper(H, X, P, Res), % Calc pairs with current element.
    cartesian_prod(T, X, Res, Prod). % Calc pairs with tail.

% cartesian_prod_helper(+El, +List, +TmpRet, -Ret)
% Creates cartesian product of List with Element.
cartesian_prod_helper(_, [], Prod, Prod).
cartesian_prod_helper(El, [H|T], P, Prod) :- 
    cartesian_prod_helper(El, T, [(El, H)|P], Prod).


% intersection(+Representation1, +Representation2, +IntersectionRepresentation)
intersection(odfa(Tf, Ss, As, A), odfa(Tf2, Ss2, As2, A), odfa(Tfc, Ssc, Asc, A)) :-
    combine_tf(Tf, Tf2, Tfc), % Create transition fn operationg on state pairs.
    combine_ss(Ss, Ss2, Ssc), % Convert starting state to pair of states.
    combine_as(As, As2, Asc). % Convert accepting states to pairs of states. 

% get_not_in(+TransitionFunction, -State)
get_states(Fp, States) :- keys(Fp, States).

% get_not_in(+List, +BlockList, -NotBlocked)
get_not_in(L1, L2, Res) :-
    sort(L1, SL1),
    sort(L2, SL2),
    get_not_in(SL1, SL2, [], Res).

% get_not_in(+List, +BlockList, +TmpRes, -NotBlocked)
% List and Blocklist are assumed to be sorted.
get_not_in([], _, Res, Res).
get_not_in([H|T], [], R, Res) :- append([H|T], R, Res).
get_not_in([H|T], [Hd|Td], R, Res) :-
    ( H = Hd ->
        get_not_in(T, Td, R, Res)
    ;
        get_not_in(T, [Hd|Td], [H|R], Res)
    ).

% complement(+ Representation, -ComlementRepresentation)
complement(odfa(Fp, Ss, As, A), odfa(Fp, Ss, Nas, A)) :- 
    get_states(Fp, States),
    get_not_in(States, As, Nas).