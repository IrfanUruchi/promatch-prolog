% models.pl – core data model for ProMatch (+ persistence)

:- module(models, [
    % persisted facts
    event/4,
    participant/4,
    participant_event/2,
    profile/4,
    timeslot/3,
    meeting/6,
    role/2,
    unavailable/3,            

    next_meeting_id/1,

    load_db/0,
    save_db/0,

    reset_demo_data/0,

    % helpers
    participant_in_event/2,
    participant_timeslots/3,
    meetings_for_event/2,
    participant_meetings/3,
    meeting_conflict/4,
    canonical_pair/4,
    meeting_between/6,        

    create_event/4,
    update_event/4,
    delete_event/1,

    add_participant/5,                
    attach_participant_to_event/2,     
    remove_participant_from_event/2,  
    delete_participant/1,             

    add_timeslot/3,                   
    update_timeslot_label/3,         
    assert_timeslot/3,            

    assert_meeting/6,                 
    update_meeting_status/2,
    delete_meeting/1,

    clear_auto_meetings/1,
    pair_has_active_meeting/3,

    % availability mutations
    set_unavailable/3,               
    clear_unavailable/3,              
    unavailable_slots/3               
]).

:- use_module(library(lists)).

:- dynamic event/4.
:- dynamic participant/4.
:- dynamic participant_event/2.
:- dynamic profile/4.
:- dynamic timeslot/3.
:- dynamic meeting/6.
:- dynamic next_meeting_id/1.
:- dynamic role/2.
:- dynamic unavailable/3.

% Persistence 


db_file('data/promatch_db.pl').

load_db :-
    db_file(File),
    (   exists_file(File)
    ->  retractall(event(_,_,_,_)),
        retractall(participant(_,_,_,_)),
        retractall(participant_event(_,_)),
        retractall(profile(_,_,_,_)),
        retractall(timeslot(_,_,_)),
        retractall(meeting(_,_,_,_,_,_)),
        retractall(role(_,_)),
        retractall(unavailable(_,_,_)),
        retractall(next_meeting_id(_)),
        catch(consult(File), _Err, true),
        ( next_meeting_id(_) -> true ; assertz(next_meeting_id(1)) )
    ;   ( next_meeting_id(_) -> true ; assertz(next_meeting_id(1)) )
    ).

save_db :-
    db_file(File),
    setup_call_cleanup(
        open(File, write, Out, [encoding(utf8)]),
        ( format(Out, '%% ProMatch persisted data (auto-generated).~n', []),
          format(Out, '%% Do not edit manually unless you know what you are doing.~n~n', []),

          ( next_meeting_id(N) -> portray_clause(Out, next_meeting_id(N)) ; true ),

          findall(event(A,B,C,D), event(A,B,C,D), Es0), sort(Es0, Es),
          forall(member(E, Es), portray_clause(Out, E)),

          findall(participant(A,B,C,D), participant(A,B,C,D), Ps0), sort(Ps0, Ps),
          forall(member(P, Ps), portray_clause(Out, P)),

          findall(role(A,B), role(A,B), Rs0), sort(Rs0, Rs),
          forall(member(R, Rs), portray_clause(Out, R)),

          findall(participant_event(A,B), participant_event(A,B), PEs0), sort(PEs0, PEs),
          forall(member(PE, PEs), portray_clause(Out, PE)),

          findall(profile(A,B,C,D), profile(A,B,C,D), Pr0), sort(Pr0, Pr),
          forall(member(PR, Pr), portray_clause(Out, PR)),

          findall(timeslot(A,B,C), timeslot(A,B,C), Ts0), sort(Ts0, Ts),
          forall(member(T, Ts), portray_clause(Out, T)),

          findall(unavailable(A,B,C), unavailable(A,B,C), U0), sort(U0, U),
          forall(member(Ux, U), portray_clause(Out, Ux)),

          findall(meeting(A,B,C,D,E,F), meeting(A,B,C,D,E,F), Ms0), sort(Ms0, Ms),
          forall(member(M, Ms), portray_clause(Out, M))
        ),
        close(Out)
    ).


% Demo data 

reset_demo_data :-
    retractall(event(_,_,_,_)),
    retractall(participant(_,_,_,_)),
    retractall(participant_event(_,_)),
    retractall(profile(_,_,_,_)),
    retractall(timeslot(_,_,_)),
    retractall(meeting(_,_,_,_,_,_)),
    retractall(next_meeting_id(_)),
    retractall(role(_,_)),
    retractall(unavailable(_,_,_)),
    assertz(next_meeting_id(1)),
    seed_events,
    seed_participants,
    seed_profiles,
    seed_timeslots,
    save_db.

seed_events :-
    assertz(event(e1, 'AI & Startups Matchmaking', '2025-12-20', 'Online')),
    assertz(event(e2, 'Fintech & Banking B2B', '2026-01-15', 'Skopje')).

seed_participants :-
    assertz(participant(p1, 'Alice', 'alice@startupa.com', 'Startup A')),                assertz(role(p1, startup)),
    assertz(participant(p2, 'Bob',   'bob@investorfund.com', 'Investor Fund')),         assertz(role(p2, investor)),
    assertz(participant(p3, 'Carol', 'carol@bigcorp.com', 'BigCorp')),                  assertz(role(p3, corporate)),
    assertz(participant(p4, 'Dave',  'dave@ai-lab.org', 'AI Lab')),                     assertz(role(p4, research)),
    assertz(participant(p5, 'Eli',   'eli@devhouse.io', 'DevHouse')),                   assertz(role(p5, engineer)),
    assertz(participant(p6, 'Fatima','fatima@city-hospital.org', 'City Hospital')),     assertz(role(p6, doctor)),
    assertz(participant(p10,'Judy',  'judy@accelerator.org', 'Startup Accelerator')),   assertz(role(p10, mentor)),

    assertz(participant_event(p1, e1)),
    assertz(participant_event(p2, e1)),
    assertz(participant_event(p3, e1)),
    assertz(participant_event(p4, e1)),
    assertz(participant_event(p5, e1)),
    assertz(participant_event(p6, e1)),
    assertz(participant_event(p10, e1)),

    assertz(participant_event(p3, e2)),
    assertz(participant_event(p4, e2)).

seed_profiles :-
    assertz(profile(p1, [ai, startups, saas], [product, early_stage], [investment, mentoring])),
    assertz(profile(p2, [ai, venture_capital], [investment, mentoring], [dealflow, innovative_startups])),
    assertz(profile(p3, [fintech, banking], [partnerships, corporate_resources], [innovation, pilots])),
    assertz(profile(p4, [ai, ml_research], [research, algorithms], [industry_partners, datasets])),
    assertz(profile(p5, [ai, devtools, platforms], [engineering_support, prototypes], [interesting_startups, b2b_products])),
    assertz(profile(p6, [healthtech, ai, clinical_workflows], [clinical_expertise], [better_tools, research_collaboration])),
    assertz(profile(p10,[startups, acceleration, ai], [mentoring, network], [strong_teams, high_potential_startups])).

seed_timeslots :-
    assertz(timeslot(e1, s1, '10:00 - 10:20')),
    assertz(timeslot(e1, s2, '10:25 - 10:45')),
    assertz(timeslot(e1, s3, '11:00 - 11:20')),
    assertz(timeslot(e2, s1, '14:00 - 14:20')),
    assertz(timeslot(e2, s2, '14:25 - 14:45')).

% Helpers


participant_in_event(PId, EventId) :-
    participant_event(PId, EventId).

participant_timeslots(EventId, _PId, Slots) :-
    findall(SlotId-Label, timeslot(EventId, SlotId, Label), Slots).

meetings_for_event(EventId, Meetings) :-
    findall(meeting(Id, EventId, P1, P2, SlotId, Status),
            meeting(Id, EventId, P1, P2, SlotId, Status),
            Meetings).

participant_meetings(EventId, PId, Meetings) :-
    findall(meeting(Id, EventId, P1, P2, SlotId, Status),
            ( meeting(Id, EventId, P1, P2, SlotId, Status),
              (P1 = PId ; P2 = PId)
            ),
            Meetings).

meeting_conflict(EventId, P, SlotId, Mid) :-
    meeting(Mid, EventId, P1, P2, SlotId, Status),
    Status \= cancelled,
    (P = P1 ; P = P2).

canonical_pair(P1, P2, A, B) :-
    ( P1 @=< P2 -> A = P1, B = P2 ; A = P2, B = P1 ).


meeting_between(MId, EventId, P1, P2, SlotId, Status) :-
    canonical_pair(P1, P2, A, B),
    meeting(MId, EventId, A, B, SlotId, Status).

pair_has_active_meeting(EventId, P1, P2) :-
    canonical_pair(P1, P2, A, B),
    meeting(_MId, EventId, A, B, _Slot, Status),
    Status \= cancelled.


% Meeting ID + insertion + updates 

get_next_meeting_id(Id) :-
    retract(next_meeting_id(Id)),
    Id2 is Id + 1,
    assertz(next_meeting_id(Id2)).

assert_meeting(EventId, P1, P2, SlotId, Status, MeetingId) :-
    canonical_pair(P1, P2, A, B),
    A \= B,
    \+ pair_has_active_meeting(EventId, A, B),
    \+ meeting_conflict(EventId, A, SlotId, _),
    \+ meeting_conflict(EventId, B, SlotId, _),
    \+ unavailable(EventId, A, SlotId),
    \+ unavailable(EventId, B, SlotId),
    get_next_meeting_id(MeetingId),
    assertz(meeting(MeetingId, EventId, A, B, SlotId, Status)),
    save_db.

update_meeting_status(MId, NewStatus) :-
    meeting(MId, EventId, P1, P2, SlotId, _OldStatus),
    retractall(meeting(MId, EventId, P1, P2, SlotId, _)),
    assertz(meeting(MId, EventId, P1, P2, SlotId, NewStatus)),
    save_db.

delete_meeting(MId) :-
    retractall(meeting(MId, _, _, _, _, _)),
    save_db.

clear_auto_meetings(EventId) :-
    retractall(meeting(_MId, EventId, _P1, _P2, _Slot, auto)),
    save_db.


% Availability 

set_unavailable(EventId, PId, SlotId) :-
    ( unavailable(EventId, PId, SlotId) -> true
    ; assertz(unavailable(EventId, PId, SlotId)),
      save_db
    ).

clear_unavailable(EventId, PId, SlotId) :-
    retractall(unavailable(EventId, PId, SlotId)),
    save_db.

unavailable_slots(EventId, PId, Slots) :-
    findall(S, unavailable(EventId, PId, S), Ss),
    sort(Ss, Slots).


% Events / participants / slots mutations (persisted)

create_event(Id, Name, Date, Location) :-
    ( event(Id, _, _, _) ->
        true
    ; assertz(event(Id, Name, Date, Location)),
      save_db
    ).

update_event(Id, Name, Date, Location) :-
    retractall(event(Id, _, _, _)),
    assertz(event(Id, Name, Date, Location)),
    save_db.

delete_event(EventId) :-
    retractall(meeting(_, EventId, _, _, _, _)),
    retractall(timeslot(EventId, _, _)),
    retractall(unavailable(EventId, _, _)),
    retractall(participant_event(_, EventId)),
    retractall(event(EventId, _, _, _)),
    cleanup_orphan_participants,
    save_db.

add_participant(PId, Name, Email, Org, RoleOrEmpty) :-
    ( participant(PId, _, _, _) ->
        true
    ; assertz(participant(PId, Name, Email, Org))
    ),
    ( nonvar(RoleOrEmpty),
      RoleOrEmpty \= ''
    -> retractall(role(PId, _)),
       assertz(role(PId, RoleOrEmpty))
    ; true
    ),
    save_db.

attach_participant_to_event(PId, EventId) :-
    ( participant_event(PId, EventId) ->
        true
    ; assertz(participant_event(PId, EventId)),
      save_db
    ).

remove_participant_from_event(PId, EventId) :-
    retractall(participant_event(PId, EventId)),
    retractall(unavailable(EventId, PId, _)),
    retractall(meeting(_, EventId, PId, _, _, _)),
    retractall(meeting(_, EventId, _, PId, _, _)),
    cleanup_orphan_participant(PId),
    save_db.

delete_participant(PId) :-
    retractall(participant_event(PId, _)),
    retractall(unavailable(_, PId, _)),
    retractall(meeting(_, _, PId, _, _, _)),
    retractall(meeting(_, _, _, PId, _, _)),
    retractall(profile(PId, _, _, _)),
    retractall(role(PId, _)),
    retractall(participant(PId, _, _, _)),
    save_db.

add_timeslot(EventId, SlotId, Label) :-
    ( timeslot(EventId, SlotId, _) ->
        true
    ; assertz(timeslot(EventId, SlotId, Label)),
      save_db
    ).

update_timeslot_label(EventId, SlotId, NewLabel) :-
    retractall(timeslot(EventId, SlotId, _)),
    assertz(timeslot(EventId, SlotId, NewLabel)),
    save_db.

assert_timeslot(EventId, Label, SlotId) :-
    findall(S, timeslot(EventId, S, _), Existing),
    length(Existing, N),
    N1 is N + 1,
    format(atom(SlotId), 's~d', [N1]),
    add_timeslot(EventId, SlotId, Label).

cleanup_orphan_participants :-
    findall(P, participant(P,_,_,_), Ps),
    forall(member(P, Ps), cleanup_orphan_participant(P)).

cleanup_orphan_participant(PId) :-
    ( participant_event(PId, _) ->
        true
    ; retractall(profile(PId, _, _, _)),
      retractall(role(PId, _)),
      retractall(participant(PId, _, _, _))
    ).
