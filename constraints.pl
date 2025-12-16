% constraints.pl – global + personal matchmaking constraints

:- module(constraints, [
    min_meetings_per_participant/1,
    max_meetings_per_participant/1,
    avoid_conflicting_orgs/1,
    blocked_pair/2,
    wants_to_meet/2
]).

:- use_module(models).

min_meetings_per_participant(0).
max_meetings_per_participant(3).

avoid_conflicting_orgs([]).

:- dynamic personal_block/2.

blocked_pair(P1, P2) :-
    ( personal_block(P1, P2)
    ; personal_block(P2, P1)
    ), !.
blocked_pair(P1, P2) :-
    models:participant(P1, _, _, Org1),
    models:participant(P2, _, _, Org2),
    avoid_conflicting_orgs(Blocked),
    ( member(Org1-Org2, Blocked)
    ; member(Org2-Org1, Blocked)
    ), !.

wants_to_meet(P1, P2) :-
    P1 \= P2,
    \+ blocked_pair(P1, P2).
