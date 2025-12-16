% scheduler.pl – automatic meeting scheduler (greedy, DB-safe)

:- module(scheduler, [
    auto_schedule_event/2   
]).

:- use_module(library(lists)).
:- use_module(models).
:- use_module(match).

auto_schedule_event(EventId, Schedule) :-
    findall(SlotId, models:timeslot(EventId, SlotId, _), Slots0),
    sort(Slots0, Slots),
    Slots \= [],

    findall(P, models:participant_event(P, EventId), Ps0),
    sort(Ps0, Participants),
    Participants \= [],

    findall(Score-P1-P2,
        ( select(P1, Participants, Rest),
          member(P2, Rest),
          match:match_score(P1, P2, Score),
          Score > 0,
          \+ models:pair_has_active_meeting(EventId, P1, P2)
        ),
        RawPairs),
    RawPairs \= [],

    sort(RawPairs, Asc),
    reverse(Asc, SortedPairs),

    greedy_fill(EventId, SortedPairs, Slots, [], ScheduleUnordered),
    reverse(ScheduleUnordered, Schedule).

greedy_fill(_, [], _, Acc, Acc).
greedy_fill(EventId, [Score-P1-P2 | Rest], Slots, Acc, Final) :-
    (   choose_slot(EventId, P1, P2, Slots, SlotId)
    ->  Acc1 = [scheduled(P1,P2,SlotId,Score)|Acc]
    ;   Acc1 = Acc
    ),
    greedy_fill(EventId, Rest, Slots, Acc1, Final).

choose_slot(EventId, P1, P2, Slots, SlotId) :-
    member(SlotId, Slots),
    \+ models:unavailable(EventId, P1, SlotId),
    \+ models:unavailable(EventId, P2, SlotId),
    \+ models:meeting_conflict(EventId, P1, SlotId, _),
    \+ models:meeting_conflict(EventId, P2, SlotId, _).
