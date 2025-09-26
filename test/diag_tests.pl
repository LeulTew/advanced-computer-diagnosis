:- use_module(library(readutil)).

:- ( current_predicate(user:symptom/1)
    -> true
    ;  consult('../diag.pl')
    ).

:- begin_tests(diag_tests).

:- consult('test/integration_tests.pl').

% Ensure core facts are present
test(symptom_exists) :-
    user:symptom(slow_performance).

test(cause_exists) :-
    user:cause(virus).

% Test that finding causes returns at least one expected cause for a known symptom
test(find_causes_basic) :-
    user:find_causes([(slow_performance, yes)], Causes),
    once(( member((virus,_), Causes) ; member((hardware_failure,_), Causes) )).

% Test confidence adjustment with no answer
test(find_causes_with_no) :-
    user:find_causes([(slow_performance, no)], Causes),
    memberchk((virus, Conf), Causes),
    Conf < 0.9.  % Should be lower than base

test(refined_confidence_model) :-
    user:find_causes([(overheating, yes), (loud_fan, yes)], Causes),
    memberchk((fan_failure, FanConf), Causes),
    FanConf > 0.7,
    memberchk((dust, DustConf), Causes),
    DustConf > 0.6.

test(contradiction_resolution_non_interactive) :-
    user:diagnosis_from_answers(
        [(overheating, yes), (cool_temperatures, yes)],
        _Causes,
        FinalAnswers
    ),
    memberchk((overheating, unsure), FinalAnswers),
    memberchk((cool_temperatures, unsure), FinalAnswers).

test(expanded_knowledge_base) :-
    user:find_causes([(display_artifacts, yes)], Causes),
    memberchk((gpu_overheating, GpuConf), Causes),
    GpuConf > 0.6.

test(probabilistic_reasoning) :-
    user:find_causes([(disk_full, yes)], Causes),
    Causes = [(storage_full, TopConf)|_],
    TopConf > 0.8.

test(parse_menu_choice_plain) :-
    user:parse_menu_choice("1", Choice),
    Choice =:= 1.

test(parse_menu_choice_trailing_period) :-
    user:parse_menu_choice("12.", Choice),
    Choice =:= 12.

test(parse_menu_choice_spaces) :-
    user:parse_menu_choice(" 5 ", Choice),
    Choice =:= 5.

test(parse_menu_choice_invalid, [fail]) :-
    user:parse_menu_choice("abc", _).

test(session_summary_payload) :-
    Answers = [(slow_performance, yes), (overheating, no), (wifi_disconnects, unsure)],
    user:build_session_payload([(virus, 0.85)], Answers, Payload),
    Payload = _{positives: Positives, negatives: Negatives, unsures: Unsures, diagnoses: Diagnoses, timestamp: _},
    Positives == [slow_performance],
    Negatives == [overheating],
    Unsures == [wifi_disconnects],
    Diagnoses = [_].

test(analytics_logging, [setup(setup_analytics(File, PrevFile)), cleanup(teardown_analytics(File, PrevFile))]) :-
    Answers = [(slow_performance, yes), (overheating, no)],
    user:log_session([(virus, 0.92)], Answers),
    read_file_to_string(File, Content, []),
    once(sub_string(Content, _, _, _, "slow_performance")),
    once(sub_string(Content, _, _, _, '"virus"')).

test(persist_learned_knowledge, [setup(setup_autosave(File, PrevFile)), cleanup(teardown_autosave(File, PrevFile))]) :-
    TempSymptom = temp_persist_symptom,
    user:assertz(symptom(TempSymptom)),
    call_cleanup(
        (
            user:record_learnt_fact(symptom(TempSymptom)),
            tmp_autosave_file(File, Terms),
            !,
            memberchk(symptom(TempSymptom), Terms)
        ),
        user:retractall(symptom(TempSymptom))
    ).

setup_autosave(File, PrevFile) :-
    File = 'data/test_autosave.pl',
    make_directory_path('data'),
    user:kb_autosave_file(PrevFile),
    user:with_autosave_suspended((
        retractall(user:kb_autosave_file(_)),
        assertz(user:kb_autosave_file(File)),
        retractall(user:learned_fact(_))
    )),
    (exists_file(File) -> delete_file(File) ; true).

teardown_autosave(File, PrevFile) :-
    user:with_autosave_suspended((
        retractall(user:kb_autosave_file(_)),
        assertz(user:kb_autosave_file(PrevFile)),
        retractall(user:learned_fact(_))
    )),
    (exists_file(File) -> delete_file(File) ; true).

tmp_autosave_file(File, Terms) :-
    exists_file(File),
    open(File, read, Stream),
    call_cleanup(read_terms(Stream, Terms), close(Stream)).

read_terms(Stream, Terms) :-
    read_term(Stream, Term, []),
    (   Term == end_of_file
    ->  Terms = []
    ;   Terms = [Term|Rest],
        read_terms(Stream, Rest)
    ).

setup_analytics(File, PrevFile) :-
    File = 'data/test_analytics.jsonl',
    (exists_file(File) -> delete_file(File) ; true),
    (user:analytics_file(PrevFile) -> true ; PrevFile = 'data/analytics.jsonl'),
    retractall(user:analytics_file(_)),
    assertz(user:analytics_file(File)).

teardown_analytics(File, PrevFile) :-
    retractall(user:analytics_file(_)),
    assertz(user:analytics_file(PrevFile)),
    (exists_file(File) -> delete_file(File) ; true).

:- end_tests(diag_tests).
