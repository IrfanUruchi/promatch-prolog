% ai.pl – intro message generator (Cerebras + safe fallback)

:- module(ai, [
    intro_suggestion/4
]).

:- use_module(models).
:- use_module(library(readutil)).          % read_file_to_string/3
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).

% Public API

intro_suggestion(FromP, ToP, Reasons, Text) :-
    (   call_cerebras_intro(FromP, ToP, Reasons, Text)
    ->  true
    ;   template_intro(FromP, ToP, Reasons, Text)
    ).

% Fallback template (always available)

template_intro(FromP, ToP, Reasons, Text) :-
    models:participant(FromP, FromName, _FromEmail, FromOrg),
    models:participant(ToP,   ToName,   _ToEmail,   _ToOrg),
    ( Reasons = [] ->
        Context = "Our profiles look complementary based on interests and roles."
    ; take_first_n(3, Reasons, Top),
      atomic_list_concat(Top, "\n- ", Tail),
      atomic_list_concat(["- ", Tail], Context)
    ),
    format(string(Text),
"Hi ~w,

I'm ~w from ~w. I thought it could be valuable for us to meet because:

~w

Would you be open to a short meeting during the matchmaking sessions?

Best,
~w",
           [ToName, FromName, FromOrg, Context, FromName]).

take_first_n(0, _, []) :- !.
take_first_n(_, [], []) :- !.
take_first_n(N, [X|Xs], [X|Ys]) :-
    N1 is N - 1,
    take_first_n(N1, Xs, Ys).

% Cerebras integration

call_cerebras_intro(FromP, ToP, Reasons, Text) :-
    cerebras_api_key(ApiKey),
    models:participant(FromP, FromName, _FromEmail, FromOrg),
    models:participant(ToP,   ToName,   _ToEmail,   _ToOrg),
    build_intro_prompt(FromName, FromOrg, ToName, Reasons, Prompt),
    catch(
        cerebras_chat(Prompt, ApiKey, Text),
        _Error,
        fail
    ).


cerebras_api_key(ApiKey) :-
    (   getenv('CEREBRAS_API_KEY', K),
        nonvar(K),
        K \== ''
    ->  ApiKey = K
    ;   (   exists_file('/run/secrets/cerebras_api_key'),
            read_file_to_string('/run/secrets/cerebras_api_key', S0, []),
            trim_ws_string(S0, S1),
            S1 \== ""
        ->  ApiKey = S1
        ;   exists_file('data/cerebras.key'),
            read_file_to_string('data/cerebras.key', S2, []),
            trim_ws_string(S2, S3),
            S3 \== "",
            ApiKey = S3
        )
    ).

cerebras_model(Model) :-
    ( getenv('CEREBRAS_MODEL', M), nonvar(M), M \== '' )
    -> Model = M
    ;  Model = "llama-3.3-70b".

trim_ws_string(In, Out) :-
    string_codes(In, Cs0),
    trim_ws_codes(Cs0, Cs1),
    string_codes(Out, Cs1).

trim_ws_codes(Cs0, Cs) :-
    drop_leading_ws(Cs0, CsA),
    reverse(CsA, RevA),
    drop_leading_ws(RevA, RevB),
    reverse(RevB, Cs).

drop_leading_ws([C|Cs], Out) :-
    code_type(C, space),
    !,
    drop_leading_ws(Cs, Out).
drop_leading_ws(Cs, Cs).


% Prompt 

build_intro_prompt(FromName, FromOrg, ToName, Reasons, Prompt) :-
    reasons_text(Reasons, ReasonsText),
    format(string(Prompt),
"Write a short, friendly email from ~w at ~w to ~w about meeting at a B2B matchmaking event.

Constraints:
Keep it under 140 words.
Use clear, natural English.
Do not use Markdown.
Do not use bullet characters such as •, -, *, or numbered lists.
Mention two or three specific reasons why the meeting makes sense.

Reasons from the matching logic:
~w

Return only the email body (no subject line, no extra commentary).",
           [FromName, FromOrg, ToName, ReasonsText]).

reasons_text([], "Their profiles look complementary based on interests and roles.") :- !.
reasons_text(Reasons, Text) :-
    take_first_n(3, Reasons, Top),
    % join as plain sentences, no bullets, no numbering
    atomic_list_concat(Top, " ; ", Text).

sanitize_no_bullets(In, Out) :-
    string_codes(In, Cs),
    exclude(is_bullet_code, Cs, Cs2),
    string_codes(Out, Cs2).

is_bullet_code(0'•).
is_bullet_code(0'-).
is_bullet_code(0'*).


% HTTP call

cerebras_chat(Prompt, ApiKey, ReplyText) :-
    URL = 'https://api.cerebras.ai/v1/chat/completions',
    cerebras_model(Model),
    Body = _{
        model: Model,
        messages: [ _{role:"user", content:Prompt} ],
        max_tokens: 256
    },
    atomic_list_concat(['Bearer ', ApiKey], AuthHeader),
    http_post(
        URL,
        json(Body),
        Result,
        [ json_object(dict),
          request_header('Authorization'=AuthHeader),
          request_header('User-Agent'='ProMatch/1.0 (SWI-Prolog)'),
          timeout(10)
        ]
    ),
    extract_message_text(Result, ReplyText).

extract_message_text(Dict, Text) :-
    (   get_dict(choices, Dict, Choices),
        is_list(Choices),
        Choices = [First|_],
        get_dict(message, First, Msg),
        get_dict(content, Msg, Text0)
    ->  normalize_space(atom(A), Text0),
        atom_string(A, Text)
    ;   term_string(Dict, Text)
    ).
