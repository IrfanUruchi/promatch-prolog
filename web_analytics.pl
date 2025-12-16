% web_analytics.pl – Analytics page UI

:- module(web_analytics, [
    analytics_page/1
]).

:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).

:- use_module(models).
:- use_module(analytics).
:- use_module(web_layout).



analytics_page(Request) :-

    findall(event(Id, Name, Date, Loc),
            models:event(Id, Name, Date, Loc),
            Events),

    ( Events = [event(FirstId,_,_,_)|_] -> DefaultEventId = FirstId
    ;                                     DefaultEventId = ''
    ),

    http_parameters(Request, [
        event_id(EventParam, [optional(true)])
    ]),

    ( nonvar(EventParam), EventParam \= ''
    -> EventId = EventParam
    ;  EventId = DefaultEventId
    ),

    build_state(EventId, Events, CardDCG),
    reply_html_page(
        title('ProMatch – Analytics'),
        \app_shell(analytics, CardDCG)
    ).

%  State

build_state('', Events, web_analytics:analytics_content(Events, none)) :- !.
build_state(EventId, Events, web_analytics:analytics_content(Events, some(EventId-Stats))) :-
    analytics:event_stats(EventId, Stats).

% Page content

analytics_content(Events, MaybeStats) -->
    {
        ( MaybeStats = some(EventId-Stats)
        -> SelectedId = EventId,
           StatsDict  = Stats
        ;  SelectedId = '',
           StatsDict  = stats{
                           participants:0, meetings:0,
                           participants_with_meetings:0,
                           coverage:0.0,
                           min_meetings:0, max_meetings:0, avg_meetings:0.0
                         }
        ),
        CovFloat = StatsDict.coverage,
        CovPct is round(CovFloat * 100)
    },
    html([
        % Top card: picker + button
        div(class('section-card'), [
            div(class('section-header-row'), [
                h2('Event analytics'),
                span(class('section-header-sub'),
                    'Pick an event to see participation and meeting stats.')
            ]),
            div(class('section-body'), [
                form([method(get), action('/analytics')],
                     div(style('display:flex; flex-wrap:wrap; gap:10px; align-items:flex-end;'),
                         [
                           div(style('min-width:260px;'), [
                               label([for(event_id), style('font-size:12px; color:#9ca3af;')],
                                     'Event'),
                               br([]),
                               select([name(event_id), id(event_id),
                                       style('min-width:260px; padding:5px; border-radius:999px; border:1px solid #4b5563; background:#020617; color:#e5e7eb;')],
                                      \event_options(SelectedId, Events))
                           ]),
                           div(style('padding-top:12px;'), [
                               button([type(submit), class('btn-primary')], 'View analytics')
                           ])
                         ]))
            ])
        ]),
        % Stats card
        \stats_section(SelectedId, StatsDict, CovPct)
    ]).

event_options(_, []) --> html(option([value('')], '-- No events --')).
event_options(Selected, [event(Id, Name, Date, _Loc)|Rest]) -->
    {
        ( Id == Selected -> Extra = [selected(selected)] ; Extra = [] ),
        format(atom(Label), '~w – ~w', [Name, Date])
    },
    html(option([value(Id)|Extra], Label)),
    event_options(Selected, Rest).

stats_section('', _Stats, _CovPct) -->
    html(div(class('section-card'), [
        div(class('section-header-row'), [
            h2('Analytics'),
            span(class('section-header-sub'),
                'Select an event above to see coverage and load.')
        ]),
        div(class('section-body'), [
            p('Once you pick an event, this panel will summarise participants, meetings and basic fairness metrics.')
        ])
    ])).
stats_section(EventId, Stats, CovPct) -->
    {
        Participants = Stats.participants,
        Meetings     = Stats.meetings,
        MinM         = Stats.min_meetings,
        MaxM         = Stats.max_meetings,
        AvgMFloat    = Stats.avg_meetings,
        AvgM         is round(AvgMFloat)
    },
    html(div(class('section-card'), [
        div(class('section-header-row'), [
            h2(['Analytics for ', code(EventId)]),
            span(class('section-header-sub'),
                'Numbers reflect the meetings currently stored in the system.')
        ]),
        div(class('section-body'), [
            div(class('stats-row'), [
                \small_stat('Participants', Participants, 'Assigned to this event'),
                \small_stat('Meetings', Meetings, 'Across all timeslots'),
                \small_stat('Coverage', CovPct, '% of participants have at least one meeting'),
                \small_stat('Min meetings', MinM, 'Least busy participant'),
                \small_stat('Max meetings', MaxM, 'Most booked participant'),
                \small_stat('Average', AvgM, 'Average meetings per participant')
            ])
        ])
    ])).

small_stat(Label, Value, Caption) -->
    html(div(class('stat-card'), [
        div(class('stat-label'), Label),
        div(class('stat-value'), Value),
        div(class('stat-caption'), Caption)
    ])).
