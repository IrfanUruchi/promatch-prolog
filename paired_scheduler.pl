% paired_scheduler.pl – combine stable matching + CLP scheduling

:- module(paired_scheduler, [
    stable_then_schedule/4   
]).

:- use_module(library(clpfd)).
:- use_module(models).
:- use_module(stable).
:- use_module(match).
:- use_module(optimizer).
:- use_module(explain).



stable_then_schedule(EventId, Pairs, Schedule, Explanation) :-
    stable:stable_matching(EventId, Pairs, StableText),
    (   Pairs == []
    ->  Schedule    = [],
        Explanation = StableText
    ;   constrained_optimal_schedule(EventId, Pairs, Schedule, TotalScore),
        explain_unified(EventId, Pairs, Schedule, TotalScore,
                        StableText, Explanation)
    ).


% Run CLP(FD) schedule optimization restricted to given stable pairs


constrained_optimal_schedule(EventId, Pairs, Schedule, TotalScore) :-
   
    findall(S0, models:timeslot(EventId, S0, _), Ss0),
    sort(Ss0, Slots),
    (   Slots == []
    ->  Schedule = [],
        TotalScore = 0
    ;   length(Slots, NumSlots),

       
        findall(P0, models:participant_event(P0, EventId), Ps0),
        sort(Ps0, Participants),

        
        build_pair_vars(Pairs, NumSlots, Vars),

       
        (   once((
                optimizer:enforce_constraints(EventId, Participants, Slots, NumSlots, Vars),
                optimizer:extract_scores(Vars, ScoreVars),
                sum(ScoreVars, #=, TotalScore),
                optimizer:variables_only(Vars, IntVars),
                labeling([max(TotalScore)], IntVars),
                optimizer:build_schedule(Vars, Slots, Schedule)
            ))
        ->  true
        ;   Schedule = [],
            TotalScore = 0
        )
    ).

build_pair_vars([], _, []).
build_pair_vars([startup_host(S,H)|Rest], NumSlots, [var(S,H,Score,Var)|Vs]) :-
    match:match_score(S, H, Score),
    Var in 0..NumSlots,  % 0 = not scheduled, 1..N = slot index
    build_pair_vars(Rest, NumSlots, Vs).


% Unified explanation text

explain_unified(EventId, Pairs, Schedule, TotalScore,
                StableText, FinalText) :-
    explain:explain_schedule(EventId, Schedule, TotalScore, SchedText),
    build_pair_list_text(Pairs, PairText),
    atomic_list_concat(
        [ "=== Stable Matching Phase ===",
          StableText,
          "=== Selected Pairs ===",
          PairText,
          "=== Schedule Optimization Phase ===",
          SchedText
        ],
        "\n\n",
        FinalText
    ).

build_pair_list_text([], "(none)").
build_pair_list_text(Pairs, Text) :-
    findall(Line,
            ( member(startup_host(S,H), Pairs),
              models:participant(S, SN, _, SO),
              models:participant(H, HN, _, HO),
              format(string(Line), "- ~w (~w) ↔ ~w (~w)", [SN,SO,HN,HO])
            ),
            Lines),
    atomic_list_concat(Lines, "\n", Text).
