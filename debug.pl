% debug.pl – explain why pairs are (not) scheduled

:- module(debug, [
    why_not_scheduled/4    
]).

:- use_module(models).
:- use_module(match).
:- use_module(constraints).

why_not_scheduled(EventId, P1, P2, Explanation) :-
    (   already_scheduled(EventId, P1, P2, Explanation)
    ->  true
    ;   blocked_reason(P1, P2, Explanation)
    ->  true
    ;   bad_match_reason(P1, P2, Explanation)
    ->  true
    ;   no_free_slot_reason(EventId, P1, P2, Explanation)
    ->  true
    ;   fallback_reason(EventId, P1, P2, Explanation)
    ).

% --- case 1: they ARE scheduled ---

already_scheduled(EventId, P1, P2, Explanation) :-
    (   models:meeting(MId, EventId, P1, P2, SlotId, Status)
    ;   models:meeting(MId, EventId, P2, P1, SlotId, Status)
    ),
    Status \= cancelled,
    models:participant(P1, Name1, _, Org1),
    models:participant(P2, Name2, _, Org2),
    ( models:timeslot(EventId, SlotId, Label)
    -> SlotLabel = Label
    ;  SlotLabel = SlotId
    ),
    format(string(Explanation),
           "They are already scheduled in event ~w: meeting ~w at slot ~w (~w) between ~w (~w) and ~w (~w).",
           [EventId, MId, SlotId, SlotLabel, Name1, Org1, Name2, Org2]).

% --- case 2: hard blocks ---

blocked_reason(P1, P2, Explanation) :-
    constraints:blocked_pair(P1, P2),
    models:participant(P1, Name1, _, Org1),
    models:participant(P2, Name2, _, Org2),
    format(string(Explanation),
           "They are not scheduled because this pair is explicitly blocked by your constraints (~w / ~w vs ~w / ~w).",
           [Name1, Org1, Name2, Org2]).

% --- case 3: non-positive match score ---

bad_match_reason(P1, P2, Explanation) :-
    match:match_score(P1, P2, Score),
    Score =< 0,
    models:participant(P1, Name1, _, Org1),
    models:participant(P2, Name2, _, Org2),
    format(string(Explanation),
           "They are not scheduled because their structural match score is ~w (≤ 0), so they are not considered a good fit (~w / ~w vs ~w / ~w).",
           [Score, Name1, Org1, Name2, Org2]).

% --- case 4: no shared free slot ---

no_free_slot_reason(EventId, P1, P2, Explanation) :-
    findall(SlotId-Label,
            models:timeslot(EventId, SlotId, Label),
            Slots),
    Slots \= [],
    findall(SlotId-Label,
            ( member(SlotId-Label, Slots),
              \+ models:meeting_conflict(EventId, P1, SlotId, _),
              \+ models:meeting_conflict(EventId, P2, SlotId, _)
            ),
            FreeShared),
    FreeShared == [],
    conflicting_meetings(EventId, P1, P2, Conflicts),
    models:participant(P1, Name1, _, _Org1),
    models:participant(P2, Name2, _, _Org2),
    conflicts_text(Conflicts, ConfText),
    format(string(Explanation),
           "There is no slot where both are free in event ~w.\n\n~w and ~w have no overlapping free slot.\n\nConflicting meetings:\n~w",
           [EventId, Name1, Name2, ConfText]).

conflicting_meetings(EventId, P1, P2, Conflicts) :-
    findall(conflict(SlotId, SlotLabel, Who, OtherName, OtherOrg, MId),
            ( models:timeslot(EventId, SlotId, SlotLabel),
              ( models:meeting(MId, EventId, P1, OtherId, SlotId, Status),
                Status \= cancelled,
                Who = p1
              ; models:meeting(MId, EventId, OtherId, P1, SlotId, Status),
                Status \= cancelled,
                Who = p1
              ; models:meeting(MId, EventId, P2, OtherId, SlotId, Status),
                Status \= cancelled,
                Who = p2
              ; models:meeting(MId, EventId, OtherId, P2, SlotId, Status),
                Status \= cancelled,
                Who = p2
              ),
              models:participant(OtherId, OtherName, _, OtherOrg)
            ),
            Conflicts).

conflicts_text([], "  (no conflicting meetings recorded, but no free joint slot was found).").
conflicts_text(Conflicts, Text) :-
    findall(Line,
            ( member(conflict(SlotId, SlotLabel, Who, OtherName, OtherOrg, MId), Conflicts),
              ( Who == p1 -> WhoTxt = "P1" ; WhoTxt = "P2" ),
              format(string(Line),
                     "  - Slot ~w (~w): ~w is already in meeting ~w with ~w (~w)",
                     [SlotId, SlotLabel, WhoTxt, MId, OtherName, OtherOrg])
            ),
            Lines),
    atomic_list_concat(Lines, "\n", Text).

% --- case 5: default explanation ---

fallback_reason(EventId, P1, P2, Explanation) :-
    models:participant(P1, Name1, _, Org1),
    models:participant(P2, Name2, _, Org2),
    match:match_score(P1, P2, Score),
    format(string(Explanation),
           "They are currently not scheduled together in event ~w, but there is no hard block and their match score is ~w.\n\nIn other words: this pair is feasible – the scheduler simply did not pick them given the current objectives and constraints.",
           [EventId, Score, Name1, Org1, Name2, Org2]).
