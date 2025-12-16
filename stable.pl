% stable.pl – Gale–Shapley stable matching for an event

:- module(stable, [
    stable_matching/3
]).

:- use_module(models).
:- use_module(preferences).
:- use_module(explain).
:- use_module(constraints).

stable_matching(EventId, Matches, Explanation) :-
    findall(P, models:participant_event(P, EventId), Ps),
    findall(S, (member(S, Ps), models:role(S, startup)), Startups),
    findall(H, (member(H, Ps), \+ models:role(H, startup)), Hosts),
    (   Startups == []
    ;   Hosts == []
    ),
    !,
    Matches = [],
    explain:explain_stable(EventId, Matches, Explanation).

stable_matching(EventId, Matches, Explanation) :-
    findall(P, models:participant_event(P, EventId), Ps),
    findall(S, (member(S, Ps), models:role(S, startup)), Startups),
    findall(H, (member(H, Ps), \+ models:role(H, startup)), Hosts),

    build_startup_prefs(Startups, Hosts, SPrefs),
    build_host_prefs(Hosts, Startups, HPrefs),

    init_states(SPrefs, States0),
    gs_loop(States0, [], HPrefs, Engaged),

    findall(startup_host(S,H),
            member(engaged(H,S), Engaged),
            Matches),

    explain:explain_stable(EventId, Matches, Explanation).

build_startup_prefs([], _, []).
build_startup_prefs([S|Ss], Hosts, [s_prefs(S, Prefs)|Rest]) :-
    preference_list(S, Hosts, Prefs),
    build_startup_prefs(Ss, Hosts, Rest).

build_host_prefs([], _, []).
build_host_prefs([H|Hs], Startups, [h_prefs(H, Prefs)|Rest]) :-
    preference_list(H, Startups, Prefs),
    build_host_prefs(Hs, Startups, Rest).

preference_list(P, Others, Prefs) :-
    findall(Score-Other,
            ( member(Other, Others),
              Other \= P,
              constraints:wants_to_meet(P, Other),
              preferences:preference_score(P, Other, Score),
              Score > 0
            ),
            Raw),
    (   Raw == []
    ->  Prefs = []
    ;   sort(Raw, Asc),
        reverse(Asc, Desc),
        pairs_values(Desc, Prefs)
    ).

pairs_values([], []).
pairs_values([_-V|Rest], [V|Vs]) :-
    pairs_values(Rest, Vs).

init_states([], []).
init_states([s_prefs(S, Prefs) | Rest], [s_state(S, Prefs) | States]) :-
    init_states(Rest, States).

gs_loop(States, Engaged, HPrefs, FinalEngaged) :-
    (   free_startup_with_option(States, Engaged, S, [H|RestPrefs], OtherStates)
    ->  (   select(engaged(H,CurrentS), Engaged, EngagedRest)
        ->  (   host_prefers(H, S, CurrentS, HPrefs)
            ->  NewEngaged = [engaged(H,S)|EngagedRest]
            ;   NewEngaged = [engaged(H,CurrentS)|EngagedRest]
            ),
            NewStates  = [s_state(S,RestPrefs)|OtherStates]
        ;   NewEngaged = [engaged(H,S)|Engaged],
            NewStates  = [s_state(S,RestPrefs)|OtherStates]
        ),
        gs_loop(NewStates, NewEngaged, HPrefs, FinalEngaged)
    ;   FinalEngaged = Engaged
    ).

free_startup_with_option(States, Engaged, S, Prefs, OtherStates) :-
    select(s_state(S, Prefs), States, OtherStates),
    Prefs = [_|_],
    \+ member(engaged(_, S), Engaged).

host_prefers(H, New, Current, HPrefs) :-
    member(h_prefs(H, List), HPrefs),
    index_of(New, List, NewI),
    index_of(Current, List, CurrI),
    NewI < CurrI.

index_of(X, [X|_], 1) :- !.
index_of(X, [_|T], I) :-
    index_of(X, T, I1),
    I is I1 + 1.
