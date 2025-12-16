% optimizer.pl – global CLP(FD) scheduler with fairness objective (+ availability)

:- module(optimizer, [
    optimal_schedule/3
]).

:- use_module(library(clpfd)).
:- use_module(library(lists)).

:- use_module(models).
:- use_module(match).
:- use_module(constraints).
:- use_module(explain).

optimal_schedule(EventId, Schedule, Explanation) :-
    findall(P0, models:participant_event(P0, EventId), Ps0),
    sort(Ps0, Participants),
    Participants \= [],

    findall(S0, models:timeslot(EventId, S0, _), Ss0),
    sort(Ss0, Slots),
    Slots \= [],
    length(Slots, NumSlots),

    findall(pair(P1,P2,Score),
            ( select(P1, Participants, Rest),
              member(P2, Rest),
              match:match_score(P1, P2, Score),
              Score > 0,
              \+ forbidden_pair(P1, P2)
            ),
            Pairs),
    Pairs \= [],

    pairs_slots_variables(Pairs, NumSlots, Vars),

    enforce_constraints(EventId, Participants, Slots, NumSlots, Vars),

    extract_scores(Vars, ScoreVars),
    sum(ScoreVars, #=, TotalScore),

    participant_activity(Participants, Vars, PartBools, PartCount),

    findall(S, member(pair(_,_,S), Pairs), AllScores),
    sum_list(AllScores, MaxTotalScore),
    K is MaxTotalScore + 1,

    Objective #= PartCount * K + TotalScore,

    variables_only(Vars, PairVarInts),
    append(PartBools, [PartCount, TotalScore, Objective], ExtraVars),
    append(PairVarInts, ExtraVars, AllIntVars),

    labeling([max(Objective)], AllIntVars),

    build_schedule(Vars, Slots, Schedule),
    explain:explain_schedule(EventId, Schedule, TotalScore, Explanation).


% Vars


pairs_slots_variables([], _, []).
pairs_slots_variables([pair(P1,P2,Score)|Rest], NumSlots,
                      [var(P1,P2,Score,Var)|Vs]) :-
    Var in 0..NumSlots,
    pairs_slots_variables(Rest, NumSlots, Vs).

variables_only([], []).
variables_only([var(_,_,_,Var)|Rest], [Var|Vs]) :-
    variables_only(Rest, Vs).

% Constraints 

enforce_constraints(Participants, NumSlots, Vars) :-
    forall(member(P, Participants),
           enforce_no_double_booking(P, NumSlots, Vars)),
    forall(member(P, Participants),
           enforce_quota(P, Vars)).

enforce_constraints(EventId, Participants, Slots, NumSlots, Vars) :-
    enforce_constraints(Participants, NumSlots, Vars),
    forall(member(P, Participants),
           enforce_availability(EventId, P, Slots, Vars)).

enforce_no_double_booking(P, NumSlots, Vars) :-
    findall(Var,
            ( member(var(P,_,_,Var), Vars)
            ; member(var(_,P,_,Var), Vars)
            ),
            List),
    ( List = [] ->
        true
    ; numlist(1, NumSlots, SlotIds),
      forall(member(Slot, SlotIds),
             max_one_in_slot(List, Slot))
    ).

max_one_in_slot(List, Slot) :-
    findall(B,
            ( member(V, List),
              B in 0..1,
              B #<==> (V #= Slot)
            ),
            Bs),
    sum(Bs, #=<, 1).

enforce_quota(P, Vars) :-
    constraints:min_meetings_per_participant(Min),
    constraints:max_meetings_per_participant(Max),
    findall(Var,
            ( member(var(P,_,_,Var), Vars)
            ; member(var(_,P,_,Var), Vars)
            ),
            List),
    ( List = [] ->
        true
    ; count_meetings(List, Count),
      Count #>= Min,
      Count #=< Max
    ).

count_meetings(List, Count) :-
    findall(B,
            ( member(Var, List),
              B in 0..1,
              B #<==> (Var #> 0)
            ),
            Bs),
    sum(Bs, #=, Count).


enforce_availability(EventId, P, Slots, Vars) :-
    findall(Idx,
        ( nth1(Idx, Slots, SlotId),
          models:unavailable(EventId, P, SlotId)
        ),
        BadIdxs),
    ( BadIdxs == [] -> true
    ; findall(Var,
          ( member(var(P,_,_,Var), Vars)
          ; member(var(_,P,_,Var), Vars)
          ),
          PairVars),
      forall(member(I, BadIdxs),
             forall(member(V, PairVars),
                    V #\= I))
    ).


% Objective extraction

extract_scores([], []).
extract_scores([var(_,_,Score,Var)|Rest], [S|Ss]) :-
    B in 0..1,
    B #<==> (Var #> 0),
    S #= Score * B,
    extract_scores(Rest, Ss).

participant_activity(Participants, Vars, PartBools, PartCount) :-
    findall(BP,
            ( member(P, Participants),
              participant_meeting_indicator(P, Vars, BP)
            ),
            PartBools),
    sum(PartBools, #=, PartCount).

participant_meeting_indicator(P, Vars, Bp) :-
    findall(V,
            ( member(var(P,_,_,V), Vars)
            ; member(var(_,P,_,V), Vars)
            ),
            List),
    ( List == [] ->
        Bp #= 0
    ;   findall(Bi,
                ( member(V, List),
                  Bi in 0..1,
                  Bi #<==> (V #> 0)
                ),
                Bs),
        sum(Bs, #=, CountP),
        Bp in 0..1,
        Bp #<==> (CountP #> 0)
    ).

% Build schedule

build_schedule([], _, []).
build_schedule([var(P1,P2,Score,Var)|Rest], Slots, Out) :-
    ( Var =:= 0 ->
        Out = OutRest
    ; nth1(Var, Slots, SlotId),
      Out = [scheduled(P1,P2,SlotId,Score)|OutRest]
    ),
    build_schedule(Rest, Slots, OutRest).

forbidden_pair(P1, P2) :-
    constraints:blocked_pair(P1, P2).
