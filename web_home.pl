% web_home.pl – home page, events, event detail, console, constraints, explain, feedback
% (+ delete/remove + availability + rerun modes)

:- module(web_home, [
    home_page/1,
    events_page/1,
    event_page/1,
    event_console_page/1,
    check_constraints_page/1,

    create_event_handler/1,
    add_participant_handler/1,
    remove_participant_handler/1,
    delete_event_handler/1,

    add_slot_handler/1,
    add_meeting_handler/1,

    update_meeting_handler/1,
    delete_meeting_handler/1,

    set_unavailable_handler/1,
    clear_unavailable_handler/1,

    export_schedule_handler/1,
    why_match_page/1,
    feedback_handler/1,

    % DCGs used by web_layout/app_shell
    home_content//2,
    events_content//1,
    event_detail_content//5,
    event_not_found_content//0,
    event_console_content//6,
    constraints_content//2,
    why_match_content//4
]).

:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_dispatch)).  
:- use_module(library(aggregate)).
:- use_module(library(lists)).

:- use_module(models).
:- use_module(match).
:- use_module(scheduler).
:- use_module(optimizer).
:- use_module(paired_scheduler).
:- use_module(constraints).
:- use_module(explain).
:- use_module(feedback).
:- use_module(web_layout).


% Helpers

safe_count(Goal, Count) :-
    (   predicate_property(Goal, defined)
    ->  aggregate_all(count, Goal, Count)
    ;   Count = 0
    ).

default_event_id([event(Id,_,_,_)|_], Id).
default_event_id([], '').

active_meeting(meeting(_Id, _EventId, _P1, _P2, _Slot, Status)) :-
    Status \= cancelled.


% HOME

home_page(_Request) :-
    safe_count(models:event(_,_,_,_),       EventsCount),
    safe_count(models:participant(_,_,_,_), ParticipantsCount),
    safe_count(models:meeting(_,_,_,_,_,_), MeetingsCount),
    findall(event(Id, Name, Date, Location),
            models:event(Id, Name, Date, Location),
            Events),
    Stats = stats{ events:EventsCount, participants:ParticipantsCount, meetings:MeetingsCount },
    reply_html_page(
        title('ProMatch – Smart B2B matchmaking'),
        \app_shell(home, web_home:home_content(Stats, Events))
    ).

home_content(Stats, Events) -->
    html([
        div(class(hero), [
            div([
                h2(class('hero-left-title'), 'Turn event data into real meetings'),
                p(class('hero-left-sub'),
                  'ProMatch turns your event participants, roles, availability and preferences into a conflict-free schedule you can export.'),
                div(class('hero-pills'), [
                    span(class('hero-pill'), 'Events · participants · slots'),
                    span(class('hero-pill'), 'Availability-aware scheduling'),
                    span(class('hero-pill'), 'Greedy + CLP(FD) + Stable')
                ])
            ]),
            div(class('hero-panel'), [
                div(class('hero-panel-col'), [
                    span(class('hero-panel-label'), 'Today''s snapshot'),
                    div(class('hero-panel-value'), [
                        span('Events: '),       b(Stats.events),       br([]),
                        span('Participants: '), b(Stats.participants), br([]),
                        span('Meetings: '),     b(Stats.meetings)
                    ])
                ]),
                div(class('hero-panel-col'), [
                    span(class('hero-panel-label'), 'Quick actions'),
                    a([href('/events'), class('btn-ghost')], 'Browse events'),
                    a([href('/event_console'), class('btn-ghost')], 'Generate schedule')
                ])
            ])
        ]),

        div(class('grid-2'), [
            div(class('section-card'), [
                div(class('section-header-row'), [
                    h2('Workspace overview'),
                    span(class('section-header-sub'),
                        'A quick feel for how full your current setup is.')
                ]),
                div(class('section-body'), [
                    div(class('stats-row'), [
                        \stat_card('Events', Stats.events, 'Defined in your knowledge base'),
                        \stat_card('Participants', Stats.participants, 'Across all events'),
                        \stat_card('Meetings', Stats.meetings, 'Currently scheduled')
                    ])
                ])
            ]),
            div(class('section-card'), [
                div(class('section-header-row'), [ h2('Under the hood') ]),
                div(class('section-body'), [
                    p('ProMatch is a small set of Prolog predicates with a clean UI shell:'),
                    ul([
                        li('Models: events, participants, roles, availability'),
                        li('Matching: pair scoring + explanations'),
                        li('Scheduling: Greedy + CLP(FD), stable pairing option'),
                        li('Constraints: sanity checks like double-booking')
                    ])
                ])
            ])
        ]),

        div(class('section-card'), [
            div(class('section-header-row'), [
                h2('Upcoming events'),
                span(class('section-header-sub'),
                     'What''s currently configured.')
            ]),
            div(class('section-body'), [ \events_table(Events) ])
        ])
    ]).

stat_card(Label, Value, Caption) -->
    html(div(class('stat-card'), [
        div(class('stat-label'), Label),
        div(class('stat-value'), Value),
        div(class('stat-caption'), Caption)
    ])).


% EVENTS LIST

events_page(_Request) :-
    findall(event(Id, Name, Date, Location),
            models:event(Id, Name, Date, Location),
            Events),
    reply_html_page(
        title('ProMatch – Events'),
        \app_shell(events, web_home:events_content(Events))
    ).

events_content(Events) -->
    html([
        div(class('section-card'), [
            div(class('section-header-row'), [
                h2('Events'),
                span(class('section-header-sub'),
                    'Browse events. Each event contains participants, slots, availability blocks and meetings.')
            ]),
            div(class('section-body'), [ \events_table(Events) ])
        ]),

        div(class('section-card'), [
            div(class('section-header-row'), [
                h2('Add a new event'),
                span(class('section-header-sub'),
                    'Create the basics. Add participants and slots afterwards.')
            ]),
            div(class('section-body'), [
                form([method(post), action('/create_event')],
                     [
                       div(style('display:flex; flex-wrap:wrap; gap:16px; max-width:720px;'),
                           [
                             div(style('flex:1 1 120px; min-width:140px;'), [
                               label([for(id), style('font-size:12px; color:#9ca3af;')],'Event ID'), br([]),
                               input([type(text),name(id),id(id),placeholder('e3 or ai_summit'),
                                      style('width:100%; padding:6px; border-radius:10px; border:1px solid #4b5563; background:#020617; color:#e5e7eb;')])
                             ]),
                             div(style('flex:3 1 260px; min-width:220px;'), [
                               label([for(name), style('font-size:12px; color:#9ca3af;')],'Name'), br([]),
                               input([type(text),name(name),id(name),placeholder('My Matchmaking Event'),
                                      style('width:100%; padding:6px; border-radius:10px; border:1px solid #4b5563; background:#020617; color:#e5e7eb;')])
                             ]),
                             div(style('flex:1 1 160px; min-width:160px;'), [
                               label([for(date), style('font-size:12px; color:#9ca3af;')],'Date'), br([]),
                               input([type(date),name(date),id(date),
                                      style('width:100%; padding:6px; border-radius:10px; border:1px solid #4b5563; background:#020617; color:#e5e7eb;')])
                             ]),
                             div(style('flex:1 1 160px; min-width:160px;'), [
                               label([for(location), style('font-size:12px; color:#9ca3af;')],'Location'), br([]),
                               input([type(text),name(location),id(location),placeholder('Online or city'),
                                      style('width:100%; padding:6px; border-radius:10px; border:1px solid #4b5563; background:#020617; color:#e5e7eb;')])
                             ]),
                             div(style('flex:0 0 120px; align-self:flex-end;'), [
                               button([type(submit), class('btn-primary')],'Create event')
                             ])
                           ])
                     ])
            ])
        ])
    ]).

events_table([]) -->
    html(p('No events configured yet. Use the form below to add one.')).
events_table(Events) -->
    html(table(class('data-table'), [
        thead(tr([
            th('ID'), th('Event'), th('Date'), th('Location'), th('Actions')
        ])),
        tbody(\event_rows(Events))
    ])).

event_rows([]) --> [].
event_rows([event(Id, Name, Date, Location)|Rest]) -->
    {
        format(atom(DetailHref),  '/event?id=~w', [Id]),
        format(atom(ConsoleHref), '/event_console?event_id=~w', [Id])
    },
    html(tr([
        td(Id),
        td([span(class('event-name'), Name)]),
        td(Date),
        td(Location),
        td([
            a([href(DetailHref),  class('btn-link')], 'Open'),
            ' ',
            a([href(ConsoleHref), class('btn-link')], 'Console'),
            ' ',
            form([method(post), action('/delete_event'), style('display:inline; margin-left:6px;')], [
                input([type(hidden), name(event_id), value(Id)]),
                button([type(submit), class('btn-ghost-small')], 'Delete')
            ])
        ])
    ])),
    event_rows(Rest).


% EVENT DETAIL

event_page(Request) :-
    http_parameters(Request,
        [ id(EventId, [atom]),
          manual_error(ManualError, [optional(true), default('')])
        ]),
    (   models:event(EventId, Name, Date, Location)
    ->  findall(p(PId, PName, Org, Role, Email),
                ( models:participant_event(PId, EventId),
                  models:participant(PId, PName, Email, Org),
                  ( models:role(PId, R0) -> Role = R0 ; Role = participant )
                ),
                Participants),
        findall(slot(SlotId, Label),
                models:timeslot(EventId, SlotId, Label),
                Slots),
        models:meetings_for_event(EventId, Meetings),
        Event = event(EventId, Name, Date, Location),
        reply_html_page(
            title('ProMatch – Event'),
            \app_shell(events,
                       web_home:event_detail_content(
                           Event, Participants, Slots, Meetings, ManualError)))
    ;   reply_html_page(
            title('ProMatch – Event not found'),
            \app_shell(events, web_home:event_not_found_content))
    ).

event_detail_content(event(EventId, Name, Date, Location),
                     Participants, Slots, Meetings, ManualError) -->
    {
        maplist(participant_row(EventId), Participants, ParticipantRows),
        maplist(meeting_row(EventId), Meetings, MeetingRows),
        maplist(slot_row, Slots, SlotRows),
        format(atom(ConsoleHref), '/event_console?event_id=~w', [EventId])
    },
    html([
        div(class('section-card'), [
            div(class('section-header-row'), [
                h2(Name),
                span(class('section-header-sub'),
                    ['Event ID ', code(EventId), ' · ', Date, ' · ', Location])
            ]),
            div(class('section-body'), [
                p(['Use the Console to generate a schedule for ', code(EventId), '.']),
                a([href(ConsoleHref), class('btn-ghost')], 'Open Console')
            ])
        ]),

        div(class('grid-2'), [
            div(class('section-card'), [
                div(class('section-header-row'), [
                    h2('Participants'),
                    span(class('section-header-sub'),
                        'Everyone assigned to this event.')
                ]),
                div(class('section-body'), [
                    table(class('data-table'), [
                        thead(tr([ th('Name'), th('Role'), th('Organisation'), th('Email'), th('') ])),
                        tbody(ParticipantRows)
                    ]),
                    \add_participant_form(EventId),
                    \availability_section(EventId, Participants, Slots)
                ])
            ]),
            div(class('section-card'), [
                div(class('section-header-row'), [
                    h2('Add a manual meeting'),
                    span(class('section-header-sub'),
                        'Pick who meets whom and in which slot.')
                ]),
                div(class('section-body'), [
                    \manual_error_banner(ManualError),
                    form([method(post), action('/add_meeting')],
                         [
                           input([type(hidden), name(event_id), value(EventId)]),
                           div(style('display:flex; flex-direction:column; gap:10px; max-width:420px;'), [
                               div([
                                   label([for(p1), style('font-size:12px; color:#9ca3af;')],'Participant A'),
                                   br([]),
                                   select([name(p1), id(p1),
                                           style('width:100%; padding:6px; border-radius:8px; border:1px solid #4b5563; background:#020617; color:#e5e7eb;')],
                                          \participant_options(Participants))
                               ]),
                               div([
                                   label([for(p2), style('font-size:12px; color:#9ca3af;')],'Participant B'),
                                   br([]),
                                   select([name(p2), id(p2),
                                           style('width:100%; padding:6px; border-radius:8px; border:1px solid #4b5563; background:#020617; color:#e5e7eb;')],
                                          \participant_options(Participants))
                               ]),
                               div([
                                   label([for(slot), style('font-size:12px; color:#9ca3af;')],'Timeslot'),
                                   br([]),
                                   select([name(slot), id(slot),
                                           style('width:100%; padding:6px; border-radius:8px; border:1px solid #4b5563; background:#020617; color:#e5e7eb;')],
                                          \slot_options(Slots))
                               ]),
                               div(style('padding-top:4px;'), [
                                   button([type(submit), class('btn-primary')],'Create meeting')
                               ])
                           ])
                         ])
                ])
            ])
        ]),

        div(class('grid-2'), [
            div(class('section-card'), [
                div(class('section-header-row'), [
                    h2('Timeslots'),
                    span(class('section-header-sub'),
                        'Slots define when meetings can happen.')
                ]),
                div(class('section-body'), [
                    \timeslots_section(EventId, SlotRows)
                ])
            ])
        ]),

        div(class('section-card'), [
            div(class('section-header-row'), [
                h2(['Meetings for ', code(EventId)]),
                span(class('section-header-sub'),
                    'Every meeting currently stored for this event.')
            ]),
            div(class('section-body'), [
                \event_meetings_section(MeetingRows)
            ])
        ])
    ]).

manual_error_banner('same_person') -->
    html(p(style('font-size:12px; color:#f97373; margin-bottom:6px;'),
           'Participant A and Participant B must be different people.')).
manual_error_banner('missing_slot') -->
    html(p(style('font-size:12px; color:#f97373; margin-bottom:6px;'),
           'Please select a timeslot (or add one first).')).
manual_error_banner('no_slots') -->
    html(p(style('font-size:12px; color:#f97373; margin-bottom:6px;'),
           'This event has no timeslots yet. Add a timeslot first.')).
manual_error_banner('duplicate_pair') -->
    html(p(style('font-size:12px; color:#f97373; margin-bottom:6px;'),
           'This pair already has a meeting scheduled in this event.')).
manual_error_banner('conflict') -->
    html(p(style('font-size:12px; color:#f97373; margin-bottom:6px;'),
           'Conflict: one participant is already booked or unavailable in that timeslot.')).
manual_error_banner(_) --> [].

event_meetings_section([]) -->
    html(p('No meetings stored yet for this event. Use the Console scheduler or the manual form above.')).
event_meetings_section(MeetingRows) -->
    html(table(class('data-table'), [
        thead(tr([ th('Slot'), th('Participant A'), th('Participant B'), th('Status') ])),
        tbody(MeetingRows)
    ])).

event_not_found_content -->
    html(div(class('section-card'), [
        div(class('section-header-row'), [ h2('Event not found') ]),
        div(class('section-body'), [
            p('We couldn''t find this event. Go back to Events and pick one from the list.')
        ])
    ])).

participant_row(EventId, p(PId, Name, Org, Role, Email),
                tr([
                    td(Name),
                    td(Role),
                    td(Org),
                    td(Email),
                    td(
                      form([method(post), action('/remove_participant'), style('display:inline;')], [
                          input([type(hidden), name(event_id), value(EventId)]),
                          input([type(hidden), name(pid), value(PId)]),
                          button([type(submit), class('btn-ghost-small')], 'Remove')
                      ])
                    )
                ])).

participant_options([]) --> [].
participant_options([p(PId, Name, Org, Role, _Email)|Ps]) -->
    { format(atom(Label), '~w – ~w (~w)', [Name, Org, Role]) },
    html(option([value(PId)], Label)),
    participant_options(Ps).

slot_options([]) -->
    html(option([value('')], '-- No slots --')).
slot_options([slot(SlotId, Label)|Rest]) -->
    html(option([value(SlotId)], Label)),
    slot_options(Rest).

slot_row(slot(SlotId, Label), tr([ td(SlotId), td(Label) ])).

timeslots_section(EventId, []) -->
    html([
        p('No timeslots configured yet for this event.'),
        \timeslot_add_form(EventId)
    ]).
timeslots_section(EventId, SlotRows) -->
    html([
        table(class('data-table'), [
            thead(tr([ th('Slot ID'), th('Label') ])),
            tbody(SlotRows)
        ]),
        \timeslot_add_form(EventId)
    ]).

timeslot_add_form(EventId) -->
    html([
        div(style('margin-top:12px;'), [
            form([method(post), action('/add_slot')],
                 [
                   input([type(hidden), name(event_id), value(EventId)]),
                   div(style('display:flex; flex-wrap:wrap; gap:10px; max-width:420px;'), [
                       div(style('flex:1 1 80px; min-width:100px;'), [
                           label([for(slot_id), style('font-size:12px; color:#9ca3af;')],'New slot ID'),
                           br([]),
                           input([type(text), name(slot_id), id(slot_id), placeholder('s4'),
                                  style('width:100%; padding:6px; border-radius:8px; border:1px solid #4b5563; background:#020617; color:#e5e7eb;')])
                       ]),
                       div(style('flex:3 1 200px; min-width:160px;'), [
                           label([for(label), style('font-size:12px; color:#9ca3af;')],'Label'),
                           br([]),
                           input([type(text), name(label), id(label), placeholder('11:30 – 11:50'),
                                  style('width:100%; padding:6px; border-radius:8px; border:1px solid #4b5563; background:#020617; color:#e5e7eb;')])
                       ]),
                       div(style('flex:0 0 120px; align-self:flex-end;'), [
                           button([type(submit), class('btn-ghost-small')],'Add slot')
                       ])
                   ])
                 ])
        ])
    ]).

add_participant_form(EventId) -->
    html([
        hr([style('border-color:#111827; margin:12px 0;')]),
        p(style('font-size:12px; color:#9ca3af; margin-bottom:6px;'),
          'Add a participant directly to this event:'),
        form([method(post), action('/add_participant')],
             [
               input([type(hidden), name(event_id), value(EventId)]),
               div(style('display:flex; flex-wrap:wrap; gap:10px; max-width:680px;'), [
                   div(style('flex:1 1 90px; min-width:100px;'), [
                       label([for(pid), style('font-size:12px; color:#9ca3af;')],'Participant ID'), br([]),
                       input([type(text), name(pid), id(pid), placeholder('p11 or alice_2'),
                              style('width:100%; padding:6px; border-radius:8px; border:1px solid #4b5563; background:#020617; color:#e5e7eb;')])
                   ]),
                   div(style('flex:2 1 160px; min-width:160px;'), [
                       label([for(name), style('font-size:12px; color:#9ca3af;')],'Name'), br([]),
                       input([type(text), name(name), id(name), placeholder('Alice'),
                              style('width:100%; padding:6px; border-radius:8px; border:1px solid #4b5563; background:#020617; color:#e5e7eb;')])
                   ]),
                   div(style('flex:2 1 160px; min-width:160px;'), [
                       label([for(email), style('font-size:12px; color:#9ca3af;')],'Email'), br([]),
                       input([type(email), name(email), id(email), placeholder('alice@company.com'),
                              style('width:100%; padding:6px; border-radius:8px; border:1px solid #4b5563; background:#020617; color:#e5e7eb;')])
                   ]),
                   div(style('flex:2 1 160px; min-width:160px;'), [
                       label([for(org), style('font-size:12px; color:#9ca3af;')],'Organisation'), br([]),
                       input([type(text), name(org), id(org), placeholder('Startup A'),
                              style('width:100%; padding:6px; border-radius:8px; border:1px solid #4b5563; background:#020617; color:#e5e7eb;')])
                   ]),
                   div(style('flex:1 1 160px; min-width:160px;'), [
                       label([for(role), style('font-size:12px; color:#9ca3af;')],'Role (optional)'), br([]),
                       select([name(role), id(role),
                               style('width:100%; padding:6px; border-radius:8px; border:1px solid #4b5563; background:#020617; color:#e5e7eb;')],
                              [
                                  option([value('')], 'No specific role'),
                                  option([value(startup)],    'Startup'),
                                  option([value(investor)],   'Investor'),
                                  option([value(corporate)],  'Corporate'),
                                  option([value(research)],   'Research'),
                                  option([value(engineer)],   'Engineer'),
                                  option([value(student)],    'Student'),
                                  option([value(doctor)],     'Doctor'),
                                  option([value(mentor)],     'Mentor'),
                                  option([value(product_manager)], 'Product manager')
                              ])
                   ]),
                   div(style('flex:0 0 120px; align-self:flex-end;'), [
                       button([type(submit), class('btn-ghost-small')],'Add participant')
                   ])
               ])
             ])
    ]).

availability_section(EventId, Participants, Slots) -->
    {
        findall(block(PId, SlotId),
                models:unavailable(EventId, PId, SlotId),
                Blocks)
    },
    html(div(style('margin-top:14px;'), [
        h3(style('margin:0 0 6px 0; font-size:14px;'), 'Availability'),
        p(class(muted),
          'Block people from specific slots. Schedulers and manual meetings will respect this.'),
        \availability_add_form(EventId, Participants, Slots),
        \availability_blocks_table(EventId, Blocks)
    ])).

availability_add_form(EventId, Participants, Slots) -->
    html(form([method(post), action('/set_unavailable')],
              [
                input([type(hidden), name(event_id), value(EventId)]),
                div(style('display:flex; flex-wrap:wrap; gap:10px; max-width:520px; align-items:flex-end;'), [
                    div(style('flex:1 1 220px; min-width:220px;'), [
                        label([for(block_pid), style('font-size:12px; color:#9ca3af;')],'Participant'),
                        br([]),
                        select([name(pid), id(block_pid),
                                style('width:100%; padding:6px; border-radius:8px; border:1px solid #4b5563; background:#020617; color:#e5e7eb;')],
                               \participant_options(Participants))
                    ]),
                    div(style('flex:1 1 200px; min-width:200px;'), [
                        label([for(block_slot), style('font-size:12px; color:#9ca3af;')],'Slot'),
                        br([]),
                        select([name(slot_id), id(block_slot),
                                style('width:100%; padding:6px; border-radius:8px; border:1px solid #4b5563; background:#020617; color:#e5e7eb;')],
                               \slot_options_simple(Slots))
                    ]),
                    div(style('flex:0 0 120px;'), [
                        button([type(submit), class('btn-ghost-small')], 'Block slot')
                    ])
                ])
              ])).

slot_options_simple([]) --> html(option([value('')], '-- No slots --')).
slot_options_simple([slot(SlotId, Label)|Rest]) -->
    html(option([value(SlotId)], Label)),
    slot_options_simple(Rest).

availability_blocks_table(_EventId, []) -->
    html(p(class(muted), 'No unavailable slots set.')).
availability_blocks_table(EventId, Blocks) -->
    html(table(class('data-table'), [
        thead(tr([th('Participant'), th('Slot'), th('') ])),
        tbody(\availability_blocks_rows(EventId, Blocks))
    ])).

availability_blocks_rows(_, []) --> [].
availability_blocks_rows(EventId, [block(PId, SlotId)|Rest]) -->
    {
        ( models:participant(PId, Name, _Email, Org) -> true ; Name = PId, Org = '' ),
        ( models:timeslot(EventId, SlotId, SlotLabel) -> true ; SlotLabel = SlotId ),
        format(atom(Label), '~w (~w)', [Name, Org])
    },
    html(tr([
        td(Label),
        td(SlotLabel),
        td(form([method(post), action('/clear_unavailable'), style('display:inline;')], [
            input([type(hidden), name(event_id), value(EventId)]),
            input([type(hidden), name(pid), value(PId)]),
            input([type(hidden), name(slot_id), value(SlotId)]),
            button([type(submit), class('btn-ghost-small')], 'Remove')
        ]))
    ])),
    availability_blocks_rows(EventId, Rest).

meeting_row(EventId,
            meeting(_Mid, EventId, P1, P2, SlotId, Status),
            tr([ td(SlotLabel), td(LeftLabel), td(RightLabel), td(StatusAtom) ])) :-
    ( models:timeslot(EventId, SlotId, SlotLabel) -> true ; SlotLabel = SlotId ),
    models:participant(P1, Name1, _E1, Org1),
    models:participant(P2, Name2, _E2, Org2),
    format(atom(LeftLabel),  '~w (~w)', [Name1, Org1]),
    format(atom(RightLabel), '~w (~w)', [Name2, Org2]),
    format(atom(StatusAtom), '~w', [Status]).


% Handlers (persisted)

create_event_handler(Request) :-
    http_parameters(Request, [
        id(Id, []),
        name(Name, []),
        date(Date, []),
        location(Location, [])
    ]),
    models:create_event(Id, Name, Date, Location),
    http_redirect(see_other, '/events', Request).

add_participant_handler(Request) :-
    http_parameters(Request, [
        event_id(EventId, []),
        pid(PId, []),
        name(Name, []),
        email(Email, []),
        org(Org, []),
        role(RoleAtom, [optional(true)])
    ]),
    models:add_participant(PId, Name, Email, Org, RoleAtom),
    models:attach_participant_to_event(PId, EventId),
    format(atom(Location), '/event?id=~w', [EventId]),
    http_redirect(see_other, Location, Request).

remove_participant_handler(Request) :-
    http_parameters(Request, [
        event_id(EventId, []),
        pid(PId, [])
    ]),
    models:remove_participant_from_event(PId, EventId),
    format(atom(Location), '/event?id=~w', [EventId]),
    http_redirect(see_other, Location, Request).

delete_event_handler(Request) :-
    http_parameters(Request, [
        event_id(EventId, [])
    ]),
    models:delete_event(EventId),
    http_redirect(see_other, '/events', Request).

add_slot_handler(Request) :-
    http_parameters(Request, [
        event_id(EventId, []),
        slot_id(SlotId, []),
        label(Label, [])
    ]),
    models:add_timeslot(EventId, SlotId, Label),
    format(atom(Location), '/event?id=~w', [EventId]),
    http_redirect(see_other, Location, Request).

set_unavailable_handler(Request) :-
    http_parameters(Request, [
        event_id(EventId, []),
        pid(PId, []),
        slot_id(SlotId, [])
    ]),
    models:set_unavailable(EventId, PId, SlotId),
    format(atom(Location), '/event?id=~w', [EventId]),
    http_redirect(see_other, Location, Request).

clear_unavailable_handler(Request) :-
    http_parameters(Request, [
        event_id(EventId, []),
        pid(PId, []),
        slot_id(SlotId, [])
    ]),
    models:clear_unavailable(EventId, PId, SlotId),
    format(atom(Location), '/event?id=~w', [EventId]),
    http_redirect(see_other, Location, Request).

add_meeting_handler(Request) :-
    http_parameters(Request,
        [ event_id(EventId, [atom]),
          p1(P1, [atom]),
          p2(P2, [atom]),
          slot(SlotId, [optional(true), default('')])
        ]),
    (   P1 == P2
    ->  format(atom(Target), '/event?id=~w&manual_error=same_person', [EventId]),
        http_redirect(see_other, Target, Request)
    ;   SlotId == ''
    ->  (   models:timeslot(EventId, _, _)
        ->  format(atom(Target), '/event?id=~w&manual_error=missing_slot', [EventId])
        ;   format(atom(Target), '/event?id=~w&manual_error=no_slots', [EventId])
        ),
        http_redirect(see_other, Target, Request)
    ;   models:pair_has_active_meeting(EventId, P1, P2)
    ->  format(atom(Target), '/event?id=~w&manual_error=duplicate_pair', [EventId]),
        http_redirect(see_other, Target, Request)
    ;   (   models:unavailable(EventId, P1, SlotId)
        ;   models:unavailable(EventId, P2, SlotId)
        ;   models:meeting_conflict(EventId, P1, SlotId, _)
        ;   models:meeting_conflict(EventId, P2, SlotId, _)
        )
    ->  format(atom(Target), '/event?id=~w&manual_error=conflict', [EventId]),
        http_redirect(see_other, Target, Request)
    ;   models:assert_meeting(EventId, P1, P2, SlotId, manual, _MeetingId)
    ->  format(atom(Target), '/event?id=~w', [EventId]),
        http_redirect(see_other, Target, Request)
    ;   format(atom(Target), '/event?id=~w&manual_error=conflict', [EventId]),
        http_redirect(see_other, Target, Request)
    ).

update_meeting_handler(Request) :-
    http_parameters(Request, [
        meeting_id(MIdAtom, []),
        new_status(StatusAtom, []),
        event_id(EventId, [optional(true), default('')])
    ]),
    atom_number(MIdAtom, MId),
    models:update_meeting_status(MId, StatusAtom),
    ( EventId \= '' ->
        format(atom(Location), '/event_console?event_id=~w', [EventId])
    ;   Location = '/event_console'
    ),
    http_redirect(see_other, Location, Request).

delete_meeting_handler(Request) :-
    http_parameters(Request, [
        meeting_id(MIdAtom, []),
        event_id(EventId, [optional(true), default('')])
    ]),
    atom_number(MIdAtom, MId),
    models:delete_meeting(MId),
    ( EventId \= '' ->
        format(atom(Location), '/event_console?event_id=~w', [EventId])
    ;   Location = '/event_console'
    ),
    http_redirect(see_other, Location, Request).


% EVENT CONSOLE (scheduler)

event_console_page(Request) :-
    findall(event(Id,Name,Date,Loc),
            models:event(Id,Name,Date,Loc),
            Events),
    ( Events = [event(FirstId,_,_,_)|_] -> DefaultEventId = FirstId ; DefaultEventId = '' ),

    (   member(method(post), Request)
    ->  http_parameters(Request, [
            event_id(EventId, []),
            algo(Algo0, []),
            mode(Mode0, [optional(true), default(replace_auto)])
        ]),
        normalize_algo(Algo0, Algo),
        normalize_mode(Mode0, Mode),
        run_scheduler(Algo, EventId, Mode, Explanation, Meetings)
    ;   http_parameters(Request, [
            event_id(EventParam, [optional(true), default('')]),
            algo(AlgoParam,      [optional(true), default('')]),
            mode(ModeParam,      [optional(true), default(replace_auto)])
        ]),
        ( EventParam \= '' -> EventId = EventParam ; EventId = DefaultEventId ),
        ( AlgoParam  \= '' -> normalize_algo(AlgoParam, Algo) ; Algo = greedy ),
        normalize_mode(ModeParam, Mode),
        Explanation =
          'Schedulers are product-safe: confirmed/manual meetings are respected; availability is enforced; reruns can replace auto meetings or only fill gaps.',
        ( EventId = '' -> Meetings = []
        ; models:meetings_for_event(EventId, Meetings0),
          include(active_meeting, Meetings0, Meetings)
        )
    ),
    reply_html_page(
        title('ProMatch – Event console'),
        \app_shell(event_console,
                   web_home:event_console_content(Events, EventId, Algo, Mode, Explanation, Meetings))
    ).

% Normalizers

normalize_algo(greedy,  greedy)  :- !.
normalize_algo('greedy',greedy)  :- !.
normalize_algo(optimal, optimal) :- !.
normalize_algo('optimal',optimal):- !.
normalize_algo(stable,  stable)  :- !.
normalize_algo('stable',stable)  :- !.
normalize_algo(X, X).

normalize_mode(replace_auto, replace_auto) :- !.
normalize_mode('replace_auto', replace_auto) :- !.
normalize_mode(fill_gaps, fill_gaps) :- !.
normalize_mode('fill_gaps', fill_gaps) :- !.
normalize_mode(X, X).

% Scheduler runner 

run_scheduler(greedy, EventId, Mode, Explanation, MeetingsOut) :-
    apply_mode_before_run(EventId, Mode),
    (   scheduler:auto_schedule_event(EventId, _Schedule)
    ->  models:meetings_for_event(EventId, Meetings0),
        include(active_meeting, Meetings0, MeetingsOut),
        ( Mode == fill_gaps
        -> Explanation = 'Greedy scheduler filled gaps (existing meetings preserved).'
        ;  Explanation = 'Greedy scheduler regenerated auto meetings (manual/confirmed preserved).'
        )
    ;   Explanation =
        'Greedy scheduler could not place any meetings. Ensure timeslots exist and there are positive-score pairs.',
        MeetingsOut = []
    ).

run_scheduler(optimal, EventId, Mode, Explanation, MeetingsOut) :-
    apply_mode_before_run(EventId, Mode),
    (   optimizer:optimal_schedule(EventId, Schedule, Explanation0)
    ->  apply_schedule_to_db(EventId, Mode, Schedule),
        models:meetings_for_event(EventId, Meetings0),
        include(active_meeting, Meetings0, MeetingsOut),
        format(atom(Explanation), '~w~n~nMode: ~w.', [Explanation0, Mode])
    ;   Explanation =
        'Optimal CLP(FD) scheduler could not find a feasible schedule (no slots, no pairs, or constraints too strict).',
        MeetingsOut = []
    ).

run_scheduler(stable, EventId, Mode, Explanation, MeetingsOut) :-
    apply_mode_before_run(EventId, Mode),
    (   paired_scheduler:stable_then_schedule(EventId, _Pairs, Schedule, Explanation0)
    ->  apply_schedule_to_db(EventId, Mode, Schedule),
        models:meetings_for_event(EventId, Meetings0),
        include(active_meeting, Meetings0, MeetingsOut),
        format(atom(Explanation), '~w~n~nMode: ~w.', [Explanation0, Mode])
    ;   Explanation =
        'Stable + CLP(FD) could not produce a schedule. Check roles, preference scores, availability, and timeslots.',
        MeetingsOut = []
    ).

run_scheduler(Algo, _EventId, _Mode, Explanation, []) :-
    format(atom(Explanation), 'Unknown algorithm: ~w', [Algo]).

apply_mode_before_run(_EventId, fill_gaps) :- !.
apply_mode_before_run(EventId, replace_auto) :-
    models:clear_auto_meetings(EventId).

apply_schedule_to_db(EventId, replace_auto, Schedule) :-
    models:clear_auto_meetings(EventId),
    forall(member(scheduled(P1,P2,SlotId,_Score), Schedule),
           maybe_assert_generated_meeting(EventId, P1, P2, SlotId)).

apply_schedule_to_db(EventId, fill_gaps, Schedule) :-
    forall(member(scheduled(P1,P2,SlotId,_Score), Schedule),
           maybe_assert_generated_meeting(EventId, P1, P2, SlotId)).

maybe_assert_generated_meeting(EventId, P1, P2, SlotId) :-
    models:canonical_pair(P1, P2, A, B),
    (   (   models:unavailable(EventId, A, SlotId)
        ;   models:unavailable(EventId, B, SlotId)
        ;   models:pair_has_active_meeting(EventId, A, B)
        ;   models:meeting_conflict(EventId, A, SlotId, _)
        ;   models:meeting_conflict(EventId, B, SlotId, _)
        )
    ->  true
    ;   models:assert_meeting(EventId, A, B, SlotId, auto, _)
    ).

% UI for console 

event_console_content(Events, SelectedEventId, SelectedAlgo, SelectedMode, Explanation, Meetings) -->
    html([
        div(class('section-card'), [
            div(class('section-header-row'), [
                h2('Run scheduler'),
                span(class('section-header-sub'),
                    'Choose an event, algorithm, and rerun mode.')
            ]),
            div(class('section-body'), [
                \event_console_run_section(Events, SelectedEventId, SelectedAlgo, SelectedMode)
            ])
        ]),
        div(class('section-card'), [
            div(class('section-header-row'), [
                h2('Scheduler explanation'),
                span(class('section-header-sub'),
                    'Plain-language summary produced by your Prolog logic.')
            ]),
            div(class('section-body'), [
                textarea([readonly(readonly),
                          style('width:100%; min-height:210px; border-radius:10px; padding:8px; font-size:12px; background:#020617; color:#e5e7eb; border:1px solid #4b5563;')],
                         Explanation)
            ])
        ]),
        div(class('section-card'), [
            div(class('section-header-row'), [
                h2('Generated meetings'),
                span(class('section-header-sub'),
                    'Auto meetings can be replaced or gaps-only filled. Manual/confirmed stay.'),
                \export_link(SelectedEventId)
            ]),
            div(class('section-body'), [
                \event_console_meetings_section(SelectedEventId, Meetings)
            ])
        ])
    ]).

event_console_run_section(Events, _SelectedEventId, _SelectedAlgo, _SelectedMode) -->
    { Events == [] }, !,
    html(p('No events configured yet. Add events first.')).

event_console_run_section(Events, SelectedEventId, SelectedAlgo, SelectedMode) -->
    {
        algo_attrs(SelectedAlgo, GreedyAttrs, OptimalAttrs, StableAttrs),
        mode_attrs(SelectedMode, ReplaceAttrs, GapsAttrs)
    },
    html(
      form([method(post), action('/event_console')],
        div(style('display:flex; flex-wrap:wrap; gap:16px; align-items:flex-end;'),
          [
            div(style('min-width:260px;'), [
              label([for(event_id), style('font-size:12px; color:#9ca3af;')],'Event'),
              br([]),
              select([name(event_id), id(event_id),
                      style('min-width:260px; padding:6px; border-radius:999px; border:1px solid #4b5563; background:#020617; color:#e5e7eb;')],
                     \event_select_options(SelectedEventId, Events))
            ]),

            div(style('display:flex; flex-direction:column; gap:4px;'), [
              span(style('font-size:12px; color:#9ca3af;'),'Algorithm'),
              div(class('radio-row'), [
                input([type(radio), name(algo), value(greedy), id(algo_greedy)|GreedyAttrs]),
                label([for(algo_greedy)], 'Greedy'),
                input([type(radio), name(algo), value(optimal), id(algo_optimal)|OptimalAttrs]),
                label([for(algo_optimal)], 'Optimal'),
                input([type(radio), name(algo), value(stable), id(algo_stable)|StableAttrs]),
                label([for(algo_stable)], 'Stable + CLP(FD)')
              ])
            ]),

            div(style('display:flex; flex-direction:column; gap:4px;'), [
              span(style('font-size:12px; color:#9ca3af;'),'Rerun mode'),
              div(class('radio-row'), [
                input([type(radio), name(mode), value(replace_auto), id(mode_replace)|ReplaceAttrs]),
                label([for(mode_replace)], 'Replace auto'),
                input([type(radio), name(mode), value(fill_gaps), id(mode_gaps)|GapsAttrs]),
                label([for(mode_gaps)], 'Fill gaps only')
              ])
            ]),

            div(style('padding-top:12px;'), [
              button([type(submit), class('btn-primary')], 'Run scheduler')
            ])
          ]
        )
      )
    ).

algo_attrs(Algo, GreedyAttrs, OptimalAttrs, StableAttrs) :-
    ( Algo == greedy  -> GreedyAttrs  = [checked(checked)] ; GreedyAttrs  = [] ),
    ( Algo == optimal -> OptimalAttrs = [checked(checked)] ; OptimalAttrs = [] ),
    ( Algo == stable  -> StableAttrs  = [checked(checked)] ; StableAttrs  = [] ).

mode_attrs(Mode, ReplaceAttrs, GapsAttrs) :-
    ( Mode == fill_gaps
    -> ReplaceAttrs = [],               GapsAttrs = [checked(checked)]
    ;  ReplaceAttrs = [checked(checked)], GapsAttrs = []
    ).

event_select_options(_, []) --> [].
event_select_options(SelectedId, [event(Id, Name, Date, _Loc)|Rest]) -->
    { ( Id == SelectedId -> Extra = [selected(selected)] ; Extra = [] ),
      format(atom(Label), '~w – ~w', [Name, Date]) },
    html(option([value(Id)|Extra], Label)),
    event_select_options(SelectedId, Rest).

event_console_meetings_section(_EventId, []) -->
    html(p('No meetings yet. Run a scheduler to see a plan here.')).
event_console_meetings_section(EventId, Meetings) -->
    html(table(class('data-table'), [
        thead(tr([ th('Slot'), th('Participant A'), th('Participant B'), th('Score'), th('Status / Actions') ])),
        tbody(\meeting_console_rows(EventId, Meetings))
    ])).

meeting_console_rows(_, []) --> [].
meeting_console_rows(EventId, [M|Ms]) -->
    { meeting_console_row(EventId, M, Row) },
    html(Row),
    meeting_console_rows(EventId, Ms).

meeting_console_row(EventId,
                    meeting(MId, EventId, P1, P2, SlotId, Status),
                    tr([ td(SlotLabel), td(Label1), td(Label2), td(ScoreNum), td(Actions) ])) :-
    ( models:timeslot(EventId, SlotId, SlotLabel) -> true ; SlotLabel = SlotId ),
    models:participant(P1, Name1, _E1, Org1),
    models:participant(P2, Name2, _E2, Org2),
    format(atom(Label1), '~w (~w)', [Name1, Org1]),
    format(atom(Label2), '~w (~w)', [Name2, Org2]),
    ( catch(match:match_score(P1, P2, ScoreNum), _, ScoreNum = 0) ),
    ( Status == cancelled -> StatusLabel = 'Cancelled'
    ; Status == confirmed -> StatusLabel = 'Confirmed'
    ; Status == manual    -> StatusLabel = 'Manual'
    ; Status == auto      -> StatusLabel = 'Auto'
    ; StatusLabel = Status
    ),
    format(atom(IntroHref), '/ai?event_id=~w&from=~w&to=~w', [EventId, P1, P2]),
    format(atom(ExplainHref), '/why_match?meeting_id=~w', [MId]),
    Actions = [
        span(class('status-label'), StatusLabel), ' ',
        form([method(post), action('/update_meeting'), style('display:inline; margin-left:6px;')], [
            input([type(hidden), name(meeting_id), value(MId)]),
            input([type(hidden), name(event_id),  value(EventId)]),
            input([type(hidden), name(new_status), value(confirmed)]),
            button([type(submit), class('btn-ghost-small')], 'Confirm')
        ]),
        ' ',
        form([method(post), action('/update_meeting'), style('display:inline; margin-left:4px;')], [
            input([type(hidden), name(meeting_id), value(MId)]),
            input([type(hidden), name(event_id),  value(EventId)]),
            input([type(hidden), name(new_status), value(cancelled)]),
            button([type(submit), class('btn-ghost-small')], 'Cancel')
        ]),
        ' ',
        form([method(post), action('/delete_meeting'), style('display:inline; margin-left:4px;')], [
            input([type(hidden), name(meeting_id), value(MId)]),
            input([type(hidden), name(event_id),  value(EventId)]),
            button([type(submit), class('btn-ghost-small')], 'Delete')
        ]),
        ' ',
        a([class('btn-ghost-small'), href(ExplainHref)], 'Explain'),
        ' ',
        a([class('btn-ghost-small'), href(IntroHref)], 'Intro'),
        ' ',
        form([method(post), action('/feedback'), style('display:inline; margin-left:4px;')], [
            input([type(hidden), name(meeting_id), value(MId)]),
            input([type(hidden), name(direction),  value(up)]),
            button([type(submit), class('btn-ghost-small')], '👍')
        ]),
        ' ',
        form([method(post), action('/feedback'), style('display:inline; margin-left:2px;')], [
            input([type(hidden), name(meeting_id), value(MId)]),
            input([type(hidden), name(direction),  value(down)]),
            button([type(submit), class('btn-ghost-small')], '👎')
        ])
    ].

export_link('') --> [].
export_link(EventId) -->
    { format(atom(Href), '/export_schedule?event_id=~w', [EventId]) },
    html(a([href(Href), class('btn-ghost-small')], 'Export CSV')).


% EXPORT SCHEDULE (CSV)

export_schedule_handler(Request) :-
    http_parameters(Request, [ event_id(EventId, []) ]),
    format('Content-type: text/csv; charset=utf-8~n'),
    format('Content-Disposition: attachment; filename="promatch_~w_schedule.csv"~n~n', [EventId]),
    format('meeting_id,event_id,slot,participant_a,org_a,participant_b,org_b,status,score~n', []),
    models:meetings_for_event(EventId, Meetings),
    forall(member(meeting(MId, EventId, P1, P2, SlotId, Status), Meetings),
           (
               ( models:timeslot(EventId, SlotId, SlotLabel) -> true ; SlotLabel = SlotId ),
               models:participant(P1, Name1, _E1, Org1),
               models:participant(P2, Name2, _E2, Org2),
               ( catch(match:match_score(P1, P2, ScoreNum), _, ScoreNum = 0) ),
               format('~w,~w,"~w","~w","~w","~w","~w",~w,~w~n',
                      [ MId, EventId, SlotLabel, Name1, Org1, Name2, Org2, Status, ScoreNum ])
           )).


% CONSTRAINTS PAGE

check_constraints_page(_Request) :-
    constraints:min_meetings_per_participant(Min),
    constraints:max_meetings_per_participant(Max),
    findall(V, quota_violation(V), VsQ),
    findall(V, double_booking_violation(V), VsD),
    append(VsQ, VsD, Violations),
    Global = global{min:Min, max:Max},
    reply_html_page(
        title('ProMatch – Constraints'),
        \app_shell(constraints, web_home:constraints_content(Global, Violations))
    ).

quota_violation(viol(quota, EventId, Message)) :-
    models:participant_event(P, EventId),
    models:participant(P, Name, _Email, Org),
    models:participant_meetings(EventId, P, Meetings),
    include(active_meeting, Meetings, Active),
    length(Active, Count),
    constraints:min_meetings_per_participant(Min),
    constraints:max_meetings_per_participant(Max),
    ( Count < Min ; Count > Max ),
    format(atom(Message),
           'Participant ~w (~w) has ~w meetings (outside the target range ~w–~w).',
           [Name, Org, Count, Min, Max]).

double_booking_violation(viol(double_booking, EventId, Message)) :-
    models:participant_event(P, EventId),
    models:participant(P, Name, _Email, Org),
    bad_slot(EventId, P, SlotId),
    findall(MId,
            ( models:meeting(MId, EventId, P1, P2, SlotId, Status),
              Status \= cancelled,
              (P1 = P ; P2 = P)
            ),
            MIds),
    length(MIds, Count),
    Count > 1,
    format(atom(Message),
           'In slot ~w, ~w (~w) is in ~w meetings at the same time.',
           [SlotId, Name, Org, Count]).

bad_slot(EventId, P, SlotId) :-
    models:meeting(_MId, EventId, P1, P2, SlotId, Status),
    Status \= cancelled,
    (P1 = P ; P2 = P),
    findall(M,
            ( models:meeting(M, EventId, A, B, SlotId, S),
              S \= cancelled,
              (A=P ; B=P)
            ),
            Ms),
    length(Ms, N),
    N > 1.

constraints_content(Global, Violations) -->
    { Min = Global.min, Max = Global.max },
    html([
        div(class('section-card'), [
            div(class('section-header-row'), [
                h2('Global constraints'),
                span(class('section-header-sub'),
                    'High-level rules every schedule should respect.')
            ]),
            div(class('section-body'), [
                p(['Target meetings per active participant: ', Min, ' to ', Max, '.']),
                p('Availability blocks are enforced by both manual and automatic scheduling.')
            ])
        ]),
        div(class('section-card'), [
            div(class('section-header-row'), [
                h2('Current violations'),
                span(class('section-header-sub'),
                    'Diagnostics from the meetings currently stored.')
            ]),
            div(class('section-body'), [
                \constraints_body(Violations)
            ])
        ])
    ]).

constraints_body([]) -->
    html(p('No violations detected for the current state.')).
constraints_body(Violations) -->
    html(table(class('data-table'), [
        thead(tr([ th('Type'), th('Event'), th('Details') ])),
        tbody(\violation_rows(Violations))
    ])).

violation_rows([]) --> [].
violation_rows([viol(Type, EventId, Msg)|Rest]) -->
    html(tr([ td(Type), td(EventId), td(Msg) ])),
    violation_rows(Rest).


% WHY THIS MATCH? + FEEDBACK

why_match_page(Request) :-
    http_parameters(Request, [ meeting_id(MIdAtom, []) ]),
    atom_number(MIdAtom, MId),
    (   models:meeting(MId, EventId, P1, P2, SlotId, Status),
        models:event(EventId, EventName, Date, Loc),
        ( models:timeslot(EventId, SlotId, SlotLabel) -> true ; SlotLabel = SlotId ),
        models:participant(P1, Name1, Email1, Org1),
        models:participant(P2, Name2, Email2, Org2),
        ( models:role(P1, R1) -> Role1 = R1 ; Role1 = participant ),
        ( models:role(P2, R2) -> Role2 = R2 ; Role2 = participant ),
        match:match_score(P1, P2, Score),
        match:match_explanations(P1, P2, ReasonLines)
    ->  Event = event(EventId, EventName, Date, Loc),
        Left  = p(P1, Name1, Org1, Role1, Email1),
        Right = p(P2, Name2, Org2, Role2, Email2),
        Ctx = ctx{ meeting_id:MId, slot:SlotLabel, status:Status, score:Score, lines:ReasonLines },
        reply_html_page(
            title('ProMatch – Why this match?'),
            \app_shell(event_console,
                       web_home:why_match_content(Event, Left, Right, Ctx))
        )
    ;   reply_html_page(
            title('ProMatch – Match not found'),
            \app_shell(event_console,
                html(div(class('section-card'), [
                    div(class('section-header-row'), [ h2('Match not found') ]),
                    div(class('section-body'), [
                        p('We couldn’t find this meeting.'),
                        a([href('/event_console'), class('btn-ghost')], 'Back to console')
                    ])
                ])))
        )
    ).

why_match_content(event(EventId, EventName, Date, Loc),
                  p(_P1, Name1, Org1, Role1, Email1),
                  p(_P2, Name2, Org2, Role2, Email2),
                  Ctx) -->
    { Score = Ctx.score, ReasonLines = Ctx.lines, SlotLabel = Ctx.slot, Status = Ctx.status, MId = Ctx.meeting_id,
      format(atom(BackHref), '/event_console?event_id=~w', [EventId]) },
    html([
        div(class('section-card'), [
            div(class('section-header-row'), [
                h2('Why this match?'),
                span(class('section-header-sub'),
                    ['Meeting ', MId, ' in ', EventName, ' (', Date, ', ', Loc, ')'])
            ]),
            div(class('section-body'), [
                div(class('grid-2'), [
                    div(class('mini-profile'), [
                        h3('Participant A'),
                        p([b(Name1), ' – ', Org1]),
                        p(style('font-size:12px; color:#9ca3af;'), ['Role: ', Role1]),
                        p(style('font-size:12px; color:#9ca3af;'), ['Email: ', Email1])
                    ]),
                    div(class('mini-profile'), [
                        h3('Participant B'),
                        p([b(Name2), ' – ', Org2]),
                        p(style('font-size:12px; color:#9ca3af;'), ['Role: ', Role2]),
                        p(style('font-size:12px; color:#9ca3af;'), ['Email: ', Email2])
                    ])
                ]),
                hr([style('border-color:#111827; margin:12px 0;')]),
                div(class('score-block'), [
                    span(style('font-size:12px; color:#9ca3af;'), 'Total match score'),
                    div(style('font-size:32px; font-weight:600; margin-top:4px;'), Score),
                    p(style('font-size:12px; color:#9ca3af; margin-top:4px;'),
                      ['Slot: ', SlotLabel, ' · Status: ', Status])
                ]),
                \reasons_list(ReasonLines),
                div(style('margin-top:16px;'), [ a([href(BackHref), class('btn-ghost')], 'Back to console') ])
            ])
        ])
    ]).

reasons_list([]) -->
    html(p(style('font-size:12px; color:#9ca3af; margin-top:10px;'),
           'No positive sub-scores found for this pair.')).
reasons_list(Lines) -->
    html([
        div(style('margin-top:16px; margin-bottom:6px; font-size:13px; color:#9ca3af;'),
            'Reasoning breakdown'),
        ul(class('reason-list'), \reason_items(Lines))
    ]).

reason_items([]) --> [].
reason_items([Line|Rest]) --> html(li(Line)), reason_items(Rest).

feedback_handler(Request) :-
    http_parameters(Request, [
        meeting_id(MIdAtom, []),
        direction(DirectionAtom, [])
    ]),
    atom_number(MIdAtom, MId),
    ( DirectionAtom = up ; DirectionAtom = down ),
    ( catch(feedback:record_feedback(MId, DirectionAtom), _, true) ),
    ( models:meeting(MId, EventId, _P1, _P2, _Slot, _Status)
    -> format(atom(Location), '/event_console?event_id=~w', [EventId])
    ;  Location = '/event_console'
    ),
    http_redirect(see_other, Location, Request).
