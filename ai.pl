:- use_module(library(readutil)).   % read_file_to_string/3
:- use_module(library(strings)).    % string_trim/2 (SWI 9+)
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).

% Key and model loading

cerebras_api_key(ApiKey) :-
    
    (   getenv('CEREBRAS_API_KEY', K),
        nonvar(K),
        K \== ''
    ->  ApiKey = K
    ;   % 2) Docker secret file (recommended for “real” deployments)
        (   exists_file('/run/secrets/cerebras_api_key'),
            read_file_to_string('/run/secrets/cerebras_api_key', S0, []),
            string_trim(S0, S1),
            S1 \== ""
        ->  ApiKey = S1
        ;   % 3) Optional local file (dev fallback) e.g. data/cerebras.key
            exists_file('data/cerebras.key'),
            read_file_to_string('data/cerebras.key', S2, []),
            string_trim(S2, S3),
            S3 \== "",
            ApiKey = S3
        )
    ).

cerebras_model(Model) :-
    ( getenv('CEREBRAS_MODEL', M), nonvar(M), M \== '' )
    -> Model = M
    ;  Model = "llama-3.3-70b".

% Prompt

build_intro_prompt(FromName, FromOrg, ToName, Reasons, Prompt) :-
    reasons_text(Reasons, ReasonsText),
    format(string(Prompt),
"Write a short, friendly email from ~w at ~w to ~w about meeting at a B2B matchmaking event.

Requirements:
Keep it under 140 words.
Use clear, natural English.
Do NOT use Markdown.
Do NOT use bullet characters.
Mention 2–3 specific reasons why the meeting makes sense.

Reasons from the matching logic:
~w

Return ONLY the email body (no subject line, no extra commentary).",
           [FromName, FromOrg, ToName, ReasonsText]).

reasons_text([], "Their profiles look complementary based on interests and roles.") :- !.
reasons_text(Reasons, Text) :-
    % take first 3 reasons to keep prompt focused
    take_first_n(3, Reasons, Top),
    numbered_lines(Top, 1, Lines),
    atomic_list_concat(Lines, "\n", Text).

numbered_lines([], _, []).
numbered_lines([R|Rs], I, [Line|Ls]) :-
    format(string(Line), "~d) ~w", [I, R]),
    I1 is I + 1,
    numbered_lines(Rs, I1, Ls).

take_first_n(0, _, []) :- !.
take_first_n(_, [], []) :- !.
take_first_n(N, [X|Xs], [X|Ys]) :-
    N1 is N - 1,
    take_first_n(N1, Xs, Ys).

% Cerebras call

cerebras_chat(Prompt, ReplyText) :-
    cerebras_api_key(ApiKey),
    cerebras_model(Model),
    URL = 'https://api.cerebras.ai/v1/chat/completions',
    Body = _{
        model: Model,
        messages: [ _{role:"user", content:Prompt} ],
        max_tokens: 256
    },
    format(string(AuthHeader), "Bearer ~w", [ApiKey]),
    http_post(
        URL,
        json(Body),
        Result,
        [ json_object(dict),
          request_header('Authorization'=AuthHeader),
          request_header('User-Agent'='ProMatch/1.0 (SWI-Prolog)')
        ]
    ),
    extract_message_text(Result, ReplyText).
