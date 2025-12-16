% preferences.pl – participant preference model

:- module(preferences, [
    preference_score/3       
]).

:- use_module(models).
:- use_module(match).
:- use_module(feedback).


preference_score(P1, P2, Score) :-
    match:match_score(P1, P2, Base),
    extra_bias(P1, P2, RoleBias),
    feedback:pair_feedback_bias(P1, P2, FbBias),
    Score is Base + RoleBias + FbBias.



extra_bias(P1, P2, Bias) :-
    role(P1, R1),
    role(P2, R2),
    role_bias(R1, R2, Bias), !.
extra_bias(_, _, 0).

role_bias(investor, startup, 3).
role_bias(startup, investor, 1).

role_bias(corporate, startup, 1).

role_bias(_, _, 0).
