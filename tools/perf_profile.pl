:- initialization(main).

main :-
    prolog_load_context(directory, Dir),
    directory_file_path(Dir, '..', Root),
    directory_file_path(Root, 'diag.pl', File),
    consult(File),
    benchmark_cases(Cases),
    maplist(run_case, Cases),
    halt.

benchmark_cases([
    [(slow_performance, yes), (popups, yes), (disk_full, unsure)],
    [(overheating, yes), (loud_fan, yes), (visible_dust, yes)],
    [(wifi_disconnects, yes), (slow_internet, yes)],
    [(blue_screen, yes), (frequent_crashes, yes)]
]).

run_case(Answers) :-
    statistics(runtime, [Start|_]),
    find_causes(Answers, Causes),
    statistics(runtime, [Stop|_]),
    Duration is Stop - Start,
    format('Case ~w -> ~2f ms, top cause: ~w~n', [Answers, Duration, Causes]).
