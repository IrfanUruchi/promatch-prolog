% web.pl – HTTP server + route wiring for ProMatch

:- module(web, [
    start_server/1
]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).

:- use_module(web_home).
:- use_module(web_ai).
:- use_module(web_analytics).

% Routes


:- http_handler(root(.),             web_home:home_page,              []).

% Core workspace
:- http_handler(root(events),        web_home:events_page,            []).
:- http_handler(root(event),         web_home:event_page,             []).
:- http_handler(root(event_console), web_home:event_console_page,     []).

% Constraints
:- http_handler(root(constraints),       web_home:check_constraints_page, []).
:- http_handler(root(check_constraints), web_home:check_constraints_page, []).

% Mutations (POST)
:- http_handler(root(create_event),       web_home:create_event_handler,       [method(post)]).
:- http_handler(root(update_event),       web_home:update_event_handler,       [method(post)]).

:- http_handler(root(add_participant),    web_home:add_participant_handler,    [method(post)]).
:- http_handler(root(remove_participant), web_home:remove_participant_handler, [method(post)]).

:- http_handler(root(import_participants), web_home:import_participants_handler,[method(post)]).

:- http_handler(root(set_unavailable),    web_home:set_unavailable_handler,    [method(post)]).
:- http_handler(root(clear_unavailable),  web_home:clear_unavailable_handler,  [method(post)]).

:- http_handler(root(add_slot),           web_home:add_slot_handler,           [method(post)]).
:- http_handler(root(update_slot),        web_home:update_slot_handler,        [method(post)]).

:- http_handler(root(add_meeting),        web_home:add_meeting_handler,        [method(post)]).
:- http_handler(root(delete_meeting),     web_home:delete_meeting_handler,     [method(post)]).
:- http_handler(root(update_meeting),     web_home:update_meeting_handler,     [method(post)]).

:- http_handler(root(delete_event),       web_home:delete_event_handler,       [method(post)]).

% Export + Explain
:- http_handler(root(export_schedule), web_home:export_schedule_handler,  []).
:- http_handler(root(why_match),       web_home:why_match_page,           []).

% Feedback (POST because your UI uses forms)
:- http_handler(root(feedback),        web_home:feedback_handler,         [method(post)]).

% AI + Analytics
:- http_handler(root(ai),        web_ai:ai_page,               []).
:- http_handler(root(analytics), web_analytics:analytics_page, []).

start_server(Port) :-
    http_server(http_dispatch, [port(Port), bind('0.0.0.0')]).

