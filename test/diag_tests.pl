:- begin_tests(diag_tests).

% Ensure core facts are present
test(symptom_exists) :-
    user:symptom(slow_performance).

test(cause_exists) :-
    user:cause(virus).

% Test that finding causes returns at least one expected cause for a known symptom
test(find_causes_basic) :-
    user:find_causes([slow_performance], Causes),
    ( member((virus,_), Causes) ; member((hardware_failure,_), Causes) ).

:- end_tests(diag_tests).
