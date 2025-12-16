% feedback.pl – store and use meeting feedback

:- module(feedback, [
    record_feedback/2,       
    pair_feedback_bias/3    
]).

:- use_module(models).
:- use_module(library(lists), [sum_list/2]).

:- dynamic feedback/3.


record_feedback(MeetingId, DirAtom) :-
    ( DirAtom == up   -> Delta = 1
    ; DirAtom == down -> Delta = -1
    ; Delta = 0
    ),
    models:meeting(MeetingId, _EventId, P1, P2, _Slot, Status),
    Status \= cancelled,
    ( Delta =\= 0 ->
        assertz(feedback(P1, P2, Delta)),
        assertz(feedback(P2, P1, Delta))
    ;   true
    ).

pair_feedback_bias(From, To, Bias) :-
    findall(Delta, feedback(From, To, Delta), Ds),
    ( Ds == [] -> Bias = 0
    ; sum_list(Ds, Bias)
    ).
