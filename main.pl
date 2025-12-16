% main.pl – entry point for ProMatch (persistent + docker-friendly)

:- use_module(web).
:- use_module(models).

main(_Argv) :-
    models:load_db,

    (   \+ models:event(_,_,_,_)
    ->  models:reset_demo_data
    ;   true
    ),

    (   getenv('PORT', PortAtom),
        catch(atom_number(PortAtom, Port0), _, fail)
    ->  Port = Port0
    ;   Port = 3000
    ),

    format("Starting ProMatch on http://0.0.0.0:~w/ (open http://localhost:~w/ on your machine)~n",
           [Port, Port]),

    web:start_server(Port),

    format("Server running. Press Ctrl+C to stop.~n"),
    wait_forever.

wait_forever :-
    repeat,
    sleep(3600),
    fail.

:- initialization(main, main).
