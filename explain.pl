% explain.pl – human-readable explanations

:- module(explain, [
    explain_schedule/4,      
    explain_stable/3      
]).

:- use_module(models).
:- use_module(match).


explain_schedule(EventId, Schedule, TotalScore, Text) :-
    event(EventId, Name, Date, Loc),
    length(Schedule, NumMeetings),
    format(string(Header),
           "Optimized schedule for ~w (~w, ~w).",
           [Name, Date, Loc]),
    format(string(Summary),
           "Total match score: ~w. Meetings scheduled: ~w.",
           [TotalScore, NumMeetings]),
    schedule_lines(EventId, Schedule, Lines),
    atomic_list_concat(Lines, "\n", LinesText),
    atomic_list_concat([Header, Summary, LinesText], "\n\n", Text).

schedule_lines(_, [], []).
schedule_lines(EventId,
               [scheduled(P1,P2,SlotId,Score)|Rest],
               [Line|Ls]) :-
    participant(P1, Name1, _, Org1),
    participant(P2, Name2, _, Org2),
    ( timeslot(EventId, SlotId, Label) -> SlotLabel = Label ; SlotLabel = SlotId ),
    format(string(Line),
           "- ~w (~w) ↔ ~w (~w) at ~w (score ~w)",
           [Name1, Org1, Name2, Org2, SlotLabel, Score]),
    schedule_lines(EventId, Rest, Ls).


/* --------- Stable matching explanation ---------- */

explain_stable(EventId, Matches, Text) :-
    event(EventId, Name, Date, Loc),
    (   Matches == []
    ->  format(string(Text),
               "Stable matching for ~w (~w, ~w):\nNo stable pairs could be found.",
               [Name, Date, Loc])
    ;   length(Matches, N),
        format(string(Header),
               "Stable matching for ~w (~w, ~w).",
               [Name, Date, Loc]),
        format(string(Summary),
               "Number of stable pairs: ~w.",
               [N]),
        stable_lines(Matches, Lines),
        atomic_list_concat(Lines, "\n", LinesText),
        atomic_list_concat([Header, Summary, LinesText], "\n\n", Text)
    ).

stable_lines([], []).
stable_lines([startup_host(S,H)|Rest], [Line|Ls]) :-
    participant(S, SName, _, SOrg),
    participant(H, HName, _, HOrg),
    format(string(Line),
           "- Startup ~w (~w) matched with ~w (~w)",
           [SName, SOrg, HName, HOrg]),
    stable_lines(Rest, Ls).
