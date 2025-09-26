:- use_module(library(prolog_codewalk)).

:- initialization(main).

main :-
    (   run_static_analysis
    ->  halt(0)
    ;   halt(1)
    ).

run_static_analysis :-
    prolog_load_context(directory, Dir),
    directory_file_path(Dir, '..', Root),
    directory_file_path(Root, 'diag.pl', Target),
    catch(load_files(Target, [silent(true)]), Error,
          (print_message(error, Error), fail)),
    (   run_analysis
    ->  format('Static analysis passed (no undefined predicates).~n', []), !
    ;   format(user_error, 'Static analysis detected undefined predicates.~n', []),
        fail
    ).

run_analysis :-
    catch(prolog_walk_code([undefined(error), source(false)]), Error,
          (print_message(error, Error), fail)).
