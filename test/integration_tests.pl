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

test(natural_language_prefill_integration) :-
    Text = "My computer throws popups, feels sluggish, and the wifi disconnects constantly.",
    user:prefill_from_text(Text, Prefilled),
    sort(Prefilled, Sorted),
    memberchk((popups, yes), Sorted),
    memberchk((slow_performance, yes), Sorted),
    memberchk((wifi_disconnects, yes), Sorted).

test(session_summary_render_integration) :-
    Answers = [(slow_performance, yes), (overheating, no), (wifi_disconnects, unsure)],
    with_output_to(string(Output), user:print_session_summary(Answers)),
    once(sub_string(Output, _, _, _, "Session Summary")),
    once(sub_string(Output, _, _, _, "Confirmed symptoms")),
    once(sub_string(Output, _, _, _, "slow_performance")).

test(localized_menu_prompt_integration) :-
    user:current_locale(Prev),
    setup_call_cleanup(
        (user:retractall(current_locale(_)), user:assertz(current_locale(es))),
        (user:localized_text(menu_prompt, [], Text),
         once(sub_string(Text, _, _, _, "Seleccione una opción"))),
        (user:retractall(current_locale(_)), user:assertz(current_locale(Prev))))
    .

:- end_tests(integration).
