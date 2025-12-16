% match.pl – smarter, explainable matchmaking logic (robust to missing profiles)

:- module(match, [
    match_score/3,
    match_explanations/3,
    best_matches_for/3
]).

:- use_module(models).
:- use_module(constraints).
:- use_module(library(lists), [sum_list/2]).

match_score(P1, P2, TotalScore) :-
    scored_reasons(P1, P2, Reasons),
    findall(S, member(score(_,S,_), Reasons), Scores),
    sum_list(Scores, TotalScore).

match_explanations(P1, P2, ReasonsText) :-
    scored_reasons(P1, P2, Reasons),
    findall(Line,
            ( member(score(Label,Score,Detail), Reasons),
              Score > 0,
              format(string(Line), "~w (+~d): ~w", [Label,Score,Detail])
            ),
            ReasonsText).



safe_profile(P, Int, Off, Need) :-
    ( models:profile(P, Int0, Off0, Need0)
    -> Int = Int0, Off = Off0, Need = Need0
    ;  Int = [],  Off = [],  Need = []
    ).

safe_role(P, Role) :-
    ( models:role(P, R) -> Role = R ; Role = participant ).

scored_reasons(P1, P2, Reasons) :-
    safe_profile(P1, Int1, Off1, Need1),
    safe_profile(P2, Int2, Off2, Need2),

    overlap(Int1, Int2, IntCommon),
    length(IntCommon, LInt),
    IntScore is LInt * 2,
    interest_detail(IntCommon, IntDetail),

    intersection(Off1, Need2, C12),
    intersection(Off2, Need1, C21),
    length(C12, L12),
    length(C21, L21),
    ONScore is (L12 + L21) * 3,
    offer_need_detail(C12, C21, ONDetail),

    org_score(P1, P2, OrgScore, OrgDetail),

    role_score(P1, P2, RoleScore, RoleDetail),

    Reasons = [
        score('Interest overlap', IntScore, IntDetail),
        score('Offers/needs fit', ONScore, ONDetail),
        score('Organization diversity', OrgScore, OrgDetail),
        score('Role complementarity', RoleScore, RoleDetail)
    ].

interest_detail([], "They do not share explicit interest tags.").
interest_detail(Common, Detail) :-
    Common \= [],
    atomic_list_concat(Common, ', ', Tags),
    format(string(Detail), "Both are interested in: ~w.", [Tags]).

offer_need_detail([], [], "No strong offer/need complementarity detected.").
offer_need_detail(C12, C21, Detail) :-
    ( C12 \= [] ->
        atomic_list_concat(C12, ', ', OneWay),
        format(string(P1Text), "P1 offers ~w that P2 needs. ", [OneWay])
    ;   P1Text = ""
    ),
    ( C21 \= [] ->
        atomic_list_concat(C21, ', ', OtherWay),
        format(string(P2Text), "P2 offers ~w that P1 needs. ", [OtherWay])
    ;   P2Text = ""
    ),
    string_concat(P1Text, P2Text, Detail0),
    ( Detail0 = "" -> Detail = "Minimal offer/need complementarity."
    ; Detail = Detail0
    ).

org_score(P1, P2, Score, Detail) :-
    models:participant(P1, _, _, Org1),
    models:participant(P2, _, _, Org2),
    ( Org1 == Org2 ->
        Score = 0,
        format(string(Detail),
               "Both are from ~w – intra-organization match (no diversity bonus).", [Org1])
    ; Score = 1,
      format(string(Detail),
             "They come from different organizations (~w vs ~w), adding diversity.",
             [Org1, Org2])
    ).

role_score(P1, P2, Score, Detail) :-
    safe_role(P1, Role1),
    safe_role(P2, Role2),
    role_pair_score(Role1, Role2, Score, Detail).

role_pair_score(R,R, 1, Detail) :-
    format(string(Detail),
           "Both are ~w – peer networking can still be useful.", [R]).
role_pair_score(startup, investor, 4,
                "Startup ↔ investor: strong funding / dealflow potential.").
role_pair_score(investor, startup, 4,
                "Investor ↔ startup: strong funding / dealflow potential.").
role_pair_score(startup, corporate, 3,
                "Startup ↔ corporate: partnership or pilot opportunities.").
role_pair_score(corporate, startup, 3,
                "Corporate ↔ startup: partnership or pilot opportunities.").
role_pair_score(research, startup, 3,
                "Research ↔ startup: applied research & commercialization potential.").
role_pair_score(startup, research, 3,
                "Startup ↔ research: applied research & commercialization potential.").
role_pair_score(_, _, 2,
                "Different roles: some complementarity expected.").

overlap(List1, List2, Common) :-
    intersection(List1, List2, Common).

intersection([], _, []).
intersection([X|Xs], Ys, [X|Zs]) :-
    member(X, Ys), !,
    intersection(Xs, Ys, Zs).
intersection([_|Xs], Ys, Zs) :-
    intersection(Xs, Ys, Zs).

best_matches_for(EventId, PId, MatchesSorted) :-
    findall(Score-OtherId,
            ( models:participant_in_event(OtherId, EventId),
              OtherId \= PId,
              constraints:wants_to_meet(PId, OtherId),
              constraints:wants_to_meet(OtherId, PId),
              match_score(PId, OtherId, Score),
              Score > 0
            ),
            Raw),
    sort(Raw, Asc),
    reverse(Asc, MatchesSorted).
