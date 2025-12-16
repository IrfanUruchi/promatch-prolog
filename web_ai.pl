% web_ai.pl – AI assistant web UI (hardened)

:- module(web_ai, [
    ai_page/1,
    ai_content//7
]).

:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(time)).  % call_with_time_limit/2

:- use_module(models).
:- use_module(match).
:- use_module(ai).
:- use_module(web_layout).

% Entry point 

ai_page(Request) :-
    catch(ai_page_impl(Request),
          E,
          ai_page_error(E)).

ai_page_impl(Request) :-
    % all events
    findall(event(Id, Name, Date, Loc),
            models:event(Id, Name, Date, Loc),
            Events),
    ( Events = [event(DefaultEventId,_,_,_)|_] -> true ; DefaultEventId = '' ),

    http_parameters(Request, [
        event_id(EventParam, [optional(true), default('')]),
        from(FromParam,      [optional(true), default('')]),
        to(ToParam,          [optional(true), default('')])
    ]),

    ( EventParam \= '' -> EventId0 = EventParam ; EventId0 = DefaultEventId ),
    ( models:event(EventId0,_,_,_) -> EventId = EventId0 ; EventId = DefaultEventId ),

    participants_for_event(EventId, Participants),

    
    default_from_to(Participants, FromDefault, ToDefault),

    ( FromParam \= '' -> FromId0 = FromParam ; FromId0 = FromDefault ),
    ( ToParam   \= '' -> ToId0   = ToParam   ; ToId0   = ToDefault ),

    % prevent same person unless nothing is selected
    (   FromId0 \= '', FromId0 == ToId0
    ->  Error = same_person,
        FromId = FromId0,
        ToId   = ToId0,
        IntroText   = '“From” and “To” must be different people.',
        ReasonsText = '',
        Matches     = []
    ;   Error = none,
        FromId = FromId0,
        ToId   = ToId0,
        build_ai_state_safe(EventId, FromId, ToId, IntroText, ReasonsText, Matches)
    ),

    safe_reply_html_page(
        title('ProMatch – AI assistant'),
        \app_shell(ai,
                   web_ai:ai_content(EventId, Events, FromId, ToId, Error,
                                     IntroText, ReasonsText-Matches))
    ).

ai_page_error(E) :-
    % If the client already disconnected, avoid spamming errors
    ( E = error(socket_error(_),_) -> true
    ; E = error(io_error(write,_),_) -> true
    ; print_message(error, E)
    ),
    safe_reply_html_page(
        title('ProMatch – AI assistant (error)'),
        \app_shell(ai,
            html(div(class('section-card'),[
                div(class('section-header-row'), [ h2('AI assistant failed') ]),
                div(class('section-body'), [
                    p('Something went wrong while generating the intro.'),
                    pre(\show_error(E))
                ])
            ])))
    ).

show_error(E) -->
    { term_string(E, S) },
    html(S).

safe_reply_html_page(Title, Body) :-
    catch(
        reply_html_page(Title, Body),
        E,
        (   E = error(socket_error(_),_) -> true
        ;   E = error(io_error(write,_),_) -> true
        ;   throw(E)
        )
    ).


% Data helpers

participants_for_event('', []) :- !.
participants_for_event(EventId, Participants) :-
    findall(P, models:participant_event(P, EventId), Ps0),
    sort(Ps0, Ps),
    Participants = Ps.

default_from_to([P1,P2|_], P1, P2) :- !.
default_from_to([P|_], P, '') :- !.
default_from_to([], '', '').

build_ai_state_safe('', _From, _To, 'Select an event to start.', '', []) :- !.
build_ai_state_safe(_EventId, '', _To, 'Select a sender participant.', '', []) :- !.
build_ai_state_safe(_EventId, _From, '', 'Select a recipient participant.', '', []) :- !.
build_ai_state_safe(EventId, From, To, IntroText, ReasonsText, Matches) :-
    % optional: prevent the /ai page hanging forever
    catch(call_with_time_limit(5,
          build_ai_state(EventId, From, To, IntroText, ReasonsText, Matches)),
          _,
          ( IntroText = 'Could not generate intro (timeout or internal error).',
            ReasonsText = '',
            Matches = []
          )).

build_ai_state(EventId, From, To, IntroText, ReasonsText, Matches) :-
    % reasons
    ( catch(match:match_explanations(From, To, ReasonsLines), _, ReasonsLines = [])
    -> true ; ReasonsLines = []
    ),
    ( ReasonsLines = [] ->
        ReasonsText = "No positive structural reasons found for this pair."
    ; atomic_list_concat(ReasonsLines, "\n", ReasonsText)
    ),

    % intro
    ( catch(ai:intro_suggestion(From, To, ReasonsLines, IntroText0), _, fail)
    -> IntroText = IntroText0
    ;  IntroText = 'No intro suggestion available for this pair.'
    ),

    % best matches sidebar
    ( catch(match:best_matches_for(EventId, From, RawMatches), _, RawMatches = [])
    -> take_first_n(6, RawMatches, Matches)
    ;  Matches = []
    ).

take_first_n(0, _, []) :- !.
take_first_n(_, [], []) :- !.
take_first_n(N, [X|Xs], [X|Ys]) :-
    N1 is N - 1,
    take_first_n(N1, Xs, Ys).


% Page content

ai_content(EventId, Events, FromId, ToId, Error, IntroText, ReasonsText-Matches) -->
    html([
        div(class('section-card'), [
            div(class('section-header-row'), [
                h2('Generate an intro message'),
                span(class('section-header-sub'),
                    'Select an event and two people – ProMatch will suggest how to break the ice.')
            ]),
            div(class('section-body'), [
                \ai_error_banner(Error),
                \ai_form(EventId, Events, FromId, ToId)
            ])
        ]),
        div(class('grid-2'), [
            div(class('section-card'), [
                div(class('section-header-row'), [
                    h2('Suggested message'),
                    span(class('section-header-sub'),
                        'Based on your profiles + matching logic (no external magic required).')
                ]),
                div(class('section-body'), [
                    textarea([readonly(readonly),
                              style('width:100%; min-height:210px; border-radius:10px; padding:8px; font-size:12px; background:#020617; color:#e5e7eb; border:1px solid #4b5563;')],
                             IntroText),
                    div(style('margin-top:10px;'), [
                        b('Why this meeting makes sense:'),
                        br([]),
                        textarea([readonly(readonly),
                                  style('width:100%; min-height:110px; border-radius:10px; padding:8px; font-size:12px; background:#020617; color:#e5e7eb; border:1px solid #4b5563;')],
                                 ReasonsText)
                    ])
                ])
            ]),
            div(class('section-card'), [
                div(class('section-header-row'), [
                    h2('Top matches'),
                    span(class('section-header-sub'),
                        'Best-scoring partners for the selected person.')
                ]),
                div(class('section-body'), [
                    \ai_matches_sidebar(EventId, Matches)
                ])
            ])
        ])
    ]).

ai_error_banner(same_person) -->
    html(p(style('font-size:12px; color:#f97373; margin-bottom:6px;'),
           '“From” and “To” must be different people.')).
ai_error_banner(_) --> [].

% Form + selects

ai_form(EventId, Events, FromId, ToId) -->
    { participants_for_event(EventId, Participants) },
    html(form([method(get), action('/ai')],
              [
                div(style('display:flex; flex-wrap:wrap; gap:10px; align-items:flex-end;'), [
                    div(style('min-width:220px;'), [
                        label([for(event_id_sel), style('font-size:12px; color:#9ca3af;')], 'Event'),
                        br([]),
                        select([name(event_id), id(event_id_sel),
                                style('min-width:220px; padding:5px; border-radius:999px; border:1px solid #4b5563; background:#020617; color:#e5e7eb;')],
                               \ai_event_options(EventId, Events))
                    ]),
                    div(style('min-width:160px;'), [
                        label([for(from_sel), style('font-size:12px; color:#9ca3af;')], 'From'),
                        br([]),
                        select([name(from), id(from_sel),
                                style('min-width:160px; padding:5px; border-radius:999px; border:1px solid #4b5563; background:#020617; color:#e5e7eb;')],
                               \ai_participant_options(Participants, FromId))
                    ]),
                    div(style('min-width:160px;'), [
                        label([for(to_sel), style('font-size:12px; color:#9ca3af;')], 'To'),
                        br([]),
                        select([name(to), id(to_sel),
                                style('min-width:160px; padding:5px; border-radius:999px; border:1px solid #4b5563; background:#020617; color:#e5e7eb;')],
                               \ai_participant_options(Participants, ToId))
                    ]),
                    div(style('padding-top:12px;'), [
                        button([type(submit), class('btn-primary')], 'Generate intro')
                    ])
                ])
              ])).

ai_event_options(_, []) --> [].
ai_event_options(CurrentId, [event(Id, Name, Date, _Loc)|Rest]) -->
    {
        ( Id == CurrentId -> Extra = [selected(selected)] ; Extra = [] ),
        format(atom(Label), '~w – ~w', [Name, Date])
    },
    html(option([value(Id)|Extra], Label)),
    ai_event_options(CurrentId, Rest).

ai_participant_options([], _Selected) -->
    html(option([value('')], '--')).
ai_participant_options([P|Ps], Selected) -->
    {
        ( models:participant(P, Name, _, Org) -> true ; Name = P, Org = '' ),
        format(atom(Label), '~w (~w)', [Name, Org]),
        ( P == Selected -> Extra = [selected(selected)] ; Extra = [] )
    },
    html(option([value(P)|Extra], Label)),
    ai_participant_options(Ps, Selected).


% Sidebar: top matches


ai_matches_sidebar(_, []) -->
    html(p('Choose “From” to see their best partners here.')).
ai_matches_sidebar(_EventId, Matches) -->
    html(table(class('data-table'), [
        thead(tr([ th('Participant'), th('Score') ])),
        tbody(\ai_match_rows(Matches))
    ])).

ai_match_rows([]) --> [].
ai_match_rows([Score-OtherId|Rest]) -->
    {
        ( models:participant(OtherId, Name, _, Org) -> true ; Name = OtherId, Org = '' ),
        format(atom(Label), '~w (~w)', [Name, Org])
    },
    html(tr([ td(Label), td(Score) ])),
    ai_match_rows(Rest).
