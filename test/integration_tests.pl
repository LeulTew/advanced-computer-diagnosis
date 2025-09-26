:- begin_tests(integration).

% Simulate a diagnosis with clear malware indicators
% Expect malware to appear among top-ranked causes.
test(malware_session) :-
    user:diagnosis_from_answers(
        [(popups, yes), (slow_performance, yes), (disk_full, no)],
        Causes,
        _FinalAnswers
    ),
    memberchk((malware, Confidence), Causes),
    Confidence > 0.7.

% Ensure follow-up rules enrich the answer set (visible_dust is asked)
test(followup_rule_triggers) :-
    user:diagnosis_from_answers(
        [(overheating, yes)],
        _Causes,
        FinalAnswers
    ),
    memberchk((visible_dust, _), FinalAnswers).

test(followup_display_artifacts) :-
    user:diagnosis_from_answers(
        [(display_artifacts, yes)],
        _Causes,
        FinalAnswers
    ),
    memberchk((overheating, _), FinalAnswers).

:- end_tests(integration).
