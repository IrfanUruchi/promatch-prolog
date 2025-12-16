% analytics.pl – event-level statistics and fairness metrics

:- module(analytics, [
    event_stats/2   
]).

:- use_module(models).
:- use_module(library(lists)).


event_stats(EventId, Stats) :-

    findall(P, participant_event(P, EventId), Ps0),
    sort(Ps0, Participants),
    length(Participants, NumParticipants),

    meetings_for_event(EventId, Meetings0),
    include(active_meeting, Meetings0, Meetings),
    length(Meetings, NumMeetings),

    maplist(participant_meeting_count(EventId, Meetings), Participants, Counts),

    participants_with_meetings(Counts, WithMeetings),
    coverage_value(NumParticipants, WithMeetings, Coverage),
    min_max_avg(Counts, MinM, MaxM, AvgM),

    Stats = stats{
        participants: NumParticipants,
        meetings: NumMeetings,
        participants_with_meetings: WithMeetings,
        coverage: Coverage,
        min_meetings: MinM,
        max_meetings: MaxM,
        avg_meetings: AvgM
    }.

% Internals

active_meeting(meeting(_Id, _EventId, _P1, _P2, _Slot, Status)) :-
    Status \= cancelled.

participant_meeting_count(EventId, Meetings, P, Count) :-
    include(meeting_involving(EventId, P), Meetings, Mine),
    length(Mine, Count).

meeting_involving(EventId, P,
                  meeting(_Id, EventId, P1, P2, _Slot, Status)) :-
    Status \= cancelled,
    (P = P1 ; P = P2).


participants_with_meetings(Counts, N) :-
    include(<(0), Counts, NonZero),   % 0 < Count
    length(NonZero, N).

coverage_value(0, _, 0.0) :- !.
coverage_value(Total, With, Coverage) :-
    Coverage is With / Total.

min_max_avg([], 0, 0, 0.0).
min_max_avg(Counts, MinM, MaxM, AvgM) :-
    min_list(Counts, MinM),
    max_list(Counts, MaxM),
    sum_list(Counts, Sum),
    length(Counts, N),
    ( N =:= 0 -> AvgM = 0.0
    ; AvgM is Sum / N
    ).
