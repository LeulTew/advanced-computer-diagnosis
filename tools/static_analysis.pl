:- use_module(library(prolog_xref)).

:- initialization(static_analysis_main).

static_analysis_main :-
    (   run_static_analysis
    ->  halt(0)
    ;   halt(1)
    ).

run_static_analysis :-
    absolute_file_name('diag.pl', Target,
                       [file_type(prolog), access(read), relative_to('.')]),
    catch(xref_source(Target, [silent(true)]), Error,
          (print_message(error, Error), fail)),
    catch(load_files(Target, [silent(true)]), Error,
          (print_message(error, Error), fail)),
    findall(Module-Name/Arity,
        ( xref_called(Target, Callable, _),
          \+ xref_defined(Target, Callable, _),
          callable_predicate_indicator(Callable, Module, Name/Arity),
          \+ is_builtin(Module, Name/Arity)
        ), RawUndefined),
    sort(RawUndefined, Undefined),
    (   Undefined == []
    ->  format('Static analysis passed (no undefined predicates).~n', [])
    ;   format(user_error, 'Static analysis detected undefined predicates:~n', []),
        forall(member(Module-Name/Arity, Undefined),
               format(user_error, '  ~w:~w/~w~n', [Module, Name, Arity])),
        fail
    ).

callable_predicate_indicator(Callable, Module, Name/Arity) :-
    strip_module(Callable, Module, Plain),
    functor(Plain, Name, Arity).

is_builtin(Module, Name/Arity) :-
    functor(Head, Name, Arity),
    ( predicate_property(Module:Head, built_in)
    ; predicate_property(system:Head, built_in)
    ), !.
