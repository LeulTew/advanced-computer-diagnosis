% Knowledge base
:- dynamic symptom/1, cause/1, solution/2, symptom_cause/2, confidence/2.

% Initial knowledge
symptom(slow_performance).
symptom(frequent_crashes).
symptom(blue_screen).
symptom(strange_noises).
symptom(overheating).
symptom(unresponsive_programs).
symptom(slow_internet).
symptom(popups).
symptom(boot_failures).
symptom(screen_flickering).
symptom(mouse_lag).
symptom(frequent_restarts).
symptom(application_freezing).
symptom(no_sound).
symptom(unexpected_shutdowns).

cause(virus).
cause(hardware_failure).
cause(software_corruption).
cause(malware).
cause(driver_issue).
cause(hard_drive_failure).
cause(ram_issue).
cause(overheating).
cause(power_supply_issue).
cause(network_problem).
cause(os_corruption).
cause(too_many_programs_running).
cause(outdated_software).
cause(incompatible_software).
cause(dust).
cause(fan_failure).

solution(virus, "Run a full system scan with updated antivirus software.").
solution(hardware_failure, "Check hardware connections and consider replacing faulty components.").
solution(software_corruption, "Reinstall the affected software or reset it to default settings.").
solution(malware, "Use anti-malware software to detect and remove malicious programs.").
solution(driver_issue, "Update or reinstall device drivers.").
solution(hard_drive_failure, "Back up data and replace the hard drive.").
solution(ram_issue, "Test RAM modules and replace if necessary.").
solution(overheating, "Ensure proper ventilation, clean fans, and reduce workload to prevent overheating.").
solution(power_supply_issue, "Check the power supply and replace it if faulty.").
solution(network_problem, "Check network connections and router settings.").
solution(os_corruption, "Repair or reinstall the operating system.").
solution(too_many_programs_running, "Close unnecessary programs and free up system resources.").
solution(outdated_software, "Update all software to the latest versions.").
solution(incompatible_software, "Remove or find alternatives for incompatible software.").
solution(dust, "Clean the computer internals, especially fans and heat sinks.").
solution(fan_failure, "Replace the malfunctioning fan and monitor temperatures.").

symptom_cause(slow_performance, virus).
symptom_cause(slow_performance, hardware_failure).
symptom_cause(slow_performance, too_many_programs_running).
symptom_cause(frequent_crashes, software_corruption).
symptom_cause(frequent_crashes, hardware_failure).
symptom_cause(blue_screen, driver_issue).
symptom_cause(blue_screen, hardware_failure).
symptom_cause(strange_noises, hard_drive_failure).
symptom_cause(strange_noises, fan_failure).
symptom_cause(overheating, dust).
symptom_cause(overheating, fan_failure).
symptom_cause(unresponsive_programs, virus).
symptom_cause(unresponsive_programs, ram_issue).
symptom_cause(slow_internet, network_problem).
symptom_cause(slow_internet, malware).
symptom_cause(popups, malware).
symptom_cause(boot_failures, os_corruption).
symptom_cause(boot_failures, hard_drive_failure).
symptom_cause(screen_flickering, driver_issue).
symptom_cause(screen_flickering, hardware_failure).
symptom_cause(mouse_lag, driver_issue).
symptom_cause(mouse_lag, hardware_failure).
symptom_cause(frequent_restarts, power_supply_issue).
symptom_cause(frequent_restarts, overheating).
symptom_cause(application_freezing, software_corruption).
symptom_cause(application_freezing, ram_issue).
symptom_cause(no_sound, driver_issue).
symptom_cause(unexpected_shutdowns, power_supply_issue).
symptom_cause(unexpected_shutdowns, overheating).

confidence(virus, 0.9).
confidence(hardware_failure, 0.7).
confidence(software_corruption, 0.5).
confidence(malware, 0.8).
confidence(driver_issue, 0.6).
confidence(hard_drive_failure, 0.7).
confidence(ram_issue, 0.6).
confidence(overheating, 0.8).
confidence(power_supply_issue, 0.8).
confidence(network_problem, 0.5).
confidence(os_corruption, 0.7).
confidence(too_many_programs_running, 0.6).
confidence(outdated_software, 0.5).
confidence(incompatible_software, 0.5).
confidence(dust, 0.6).
confidence(fan_failure, 0.7).

% Main program
main :-
    repeat,
    nl,
    write('Choose an option:'), nl,
    write('1. Start diagnosis'), nl,
    write('2. Learn new symptom'), nl,
    write('3. Learn new cause'), nl,
    write('4. Learn new solution'), nl,
    write('5. View knowledge base'), nl,
    write('6. Exit'), nl,
    read(Choice),
    (   handle_choice(Choice)
    ->  (Choice == 6 -> !; fail)
    ;   write('An error occurred. Please try again.'), nl, fail
    ).

% Handle user choices
handle_choice(1) :- start_diagnosis, !.
handle_choice(2) :- learn_symptom, !.
handle_choice(3) :- learn_cause, !.
handle_choice(4) :- learn_solution, !.
handle_choice(5) :- view_knowledge_base, !.
handle_choice(6) :- write('Goodbye!'), nl, !.
handle_choice(_) :- write('Invalid choice. Please try again.'), nl.

% Start the diagnosis process
start_diagnosis :-
    write('Welcome to the Advanced Computer Problem Diagnosis Expert System!'), nl,
    write('I will ask you about symptoms. Please answer with yes., no., or unsure.'), nl,
    findall(Symptom, (symptom(Symptom), Symptom \= unexpected_shutdowns), Symptoms),
    ask_symptoms(Symptoms, [], Observed),
    find_causes(Observed, Causes),
    present_diagnosis(Causes),
    !,  % Cut to prevent backtracking
    true.  % Explicitly succeed

% Ask about symptoms iteratively
ask_symptoms([], Observed, Observed).
ask_symptoms([Symptom|Rest], AccObserved, Observed) :-
    ask_symptom(Symptom, Answer),
    (Answer == yes ->
        ask_symptoms(Rest, [Symptom|AccObserved], Observed);
    ask_symptoms(Rest, AccObserved, Observed)).

% Ask about a symptom
ask_symptom(Symptom, Answer) :-
    atom_concat('Does the computer exhibit ', Symptom, Q1),
    atom_concat(Q1, ' (yes./no./unsure.) ', Question),
    repeat,
    write(Question),
    read(UserAnswer),
    (member(UserAnswer, [yes, no, unsure]) ->
        Answer = UserAnswer;
    write('Invalid input. Please enter yes., no., or unsure.'), nl, fail).

% Find potential causes based on observed symptoms
find_causes(Observed, Causes) :-
    findall(Cause, (
        member(Symptom, Observed),
        symptom_cause(Symptom, Cause)
    ), CausesWithDuplicates),
    sort(CausesWithDuplicates, UniqueCauses),
    (UniqueCauses == [] ->
        Causes = [(unknown, 1.0)];
        findall((Cause, AdjustedConf), (
            member(Cause, UniqueCauses),
            adjust_confidence(Cause, Observed, AdjustedConf)
        ), CausesWithConf),
        sort(2, @>=, CausesWithConf, Causes)
    ).

% Adjust confidence based on the number of matching symptoms
adjust_confidence(Cause, Observed, AdjustedConf) :-
    confidence(Cause, BaseConf),
    findall(1, (member(Symptom, Observed), symptom_cause(Symptom, Cause)), Matches),
    length(Matches, MatchCount),
    AdjustedConf is BaseConf * (1 + 0.1 * MatchCount).

% Present the diagnosis results
present_diagnosis(Causes) :-
    write('Based on the symptoms, the problem(s) might be:'), nl,
    present_causes(Causes),
    offer_explanation(Causes),
    !.  % Cut to prevent backtracking

% Present causes and their solutions
present_causes([]).
present_causes([(unknown, _)|_]) :-
    write('No known cause matches the observed symptoms.'), nl, !.
present_causes([(Cause, Conf)|Rest]) :-
    format('~w (Confidence: ~2f): ', [Cause, Conf]),
    (solution(Cause, Solution) ->
        format('Solution found: ~w~n', [Solution]),
        write(Solution)
    ;
        write('No specific solution available.')
    ),
    nl,
    present_causes(Rest).

% Offer an explanation of the diagnosis
offer_explanation(Causes) :-
    nl, write('Would you like an explanation of the diagnosis? (yes./no.) '),
    read(Answer),
    (Answer == yes ->
        explain_diagnosis(Causes);
    true),
    !.  % Cut to prevent backtracking

% Explain the diagnosis process
explain_diagnosis(Causes) :-
    nl, write('The system uses the following logic for diagnosis:'), nl,
    write('1. It asks about known symptoms, allowing for uncertainty.'), nl,
    write('2. It matches symptoms to possible causes based on confidence levels.'), nl,
    write('3. It adjusts confidence based on the number of matching symptoms.'), nl,
    write('4. If multiple causes are likely, it prioritizes them based on adjusted confidence.'), nl,
    nl, write('Here are the specific symptoms and their likely causes:'), nl,
    explain_causes(Causes),
    !.  % Cut to prevent backtracking

% Explain individual causes
explain_causes([]).
explain_causes([(unknown, _)|_]) :-
    write('No further explanation is available because no matching cause was found.'), nl, !.
explain_causes([(Cause, Conf)|Rest]) :-
    format('Cause: ~w, Adjusted Confidence: ~2f~n', [Cause, Conf]),
    (solution(Cause, Solution) ->
        format('Solution: ~w~n', [Solution]);
    write('No specific solution available.')), nl,
    explain_causes(Rest).

% Learn a new symptom
learn_symptom :-
    write('Enter a new symptom: '), nl,
    read(NewSymptom),
    (symptom(NewSymptom) ->
        write('This symptom already exists in the knowledge base.');
    asserta(symptom(NewSymptom)),
    write('New symptom added to the knowledge base.')),
    nl.

% Learn a new cause
learn_cause :-
    write('Enter a new cause: '), nl,
    read(NewCause),
    (cause(NewCause) ->
        write('This cause already exists. Do you want to add a new symptom-cause relationship? (yes./no.) '),
        read(Answer),
        (Answer == yes ->
            add_symptom_cause(NewCause);
        write('No changes made to the knowledge base.'));
    asserta(cause(NewCause)),
    write('New cause added. '),
    add_symptom_cause(NewCause)),
    nl.

% Add a symptom-cause relationship
add_symptom_cause(Cause) :-
    write('Enter the related symptom: '), nl,
    read(Symptom),
    (symptom(Symptom) ->
        (symptom_cause(Symptom, Cause) ->
            write('This symptom-cause relationship already exists.');
        asserta(symptom_cause(Symptom, Cause)),
        write('New cause-symptom relationship added to the knowledge base.'),
        add_confidence(Cause));
    write('This symptom does not exist in the knowledge base. Please add it first.')),
    nl.

% Add confidence for a cause
add_confidence(Cause) :-
    write('Enter confidence level for this cause (0.0 - 1.0): '),
    read(Conf),
    (number(Conf), Conf >= 0.0, Conf =< 1.0 ->
        asserta(confidence(Cause, Conf)),
        write('New confidence level added.');
    write('Invalid confidence level. Please enter a number between 0.0 and 1.0.'),
    add_confidence(Cause)).

% Learn a new solution
learn_solution :-
    write('Enter a cause: '), nl,
    read(Cause),
    atom_string(Cause, CauseString),
    write('Enter the solution: '), nl,
    capture_solution(Solution),
    validate_solution(Solution, CauseString).

% Capture solution input using read_line_to_string/2
capture_solution(Solution) :-
    flush_output,
    read(user_input, Solution),
    format('Debug: Solution entered: "~w"~n', [Solution]).

% Validate the solution
validate_solution(Solution, Cause) :-
    (Solution == '' ->
        write('Empty solution detected. Please try again.'), nl,
        learn_solution;
        format('You entered: "~w"~n', [Solution]),
        write('Is this correct? (yes./no.) '),
        flush_output,
        read(Confirm),
        (Confirm == yes ->
            confirm_solution(Solution, Cause);
            Confirm == no ->
            write('Let\'s try again.'), nl,
            learn_solution;
            write('Invalid input. Please enter "yes" or "no".'), nl,
            validate_solution(Solution, Cause))).

% Clean up and confirm the solution
confirm_solution(Solution, Cause) :-
    atom_string(CauseAtom, Cause),
    atom_string(SolutionAtom, Solution),
    normalize_space(atom(CauseNorm), CauseAtom),
    normalize_space(atom(SolutionNorm), SolutionAtom),
    (solution(CauseNorm, OldSolution) ->
        format('A solution for this cause already exists: ~w~n', [OldSolution]),
        write('Do you want to update it? (yes./no.) '),
        flush_output,
        read(Answer),
        (Answer == yes ->
            update_solution(CauseNorm, SolutionNorm);
        Answer == no ->
            write('No changes made to the knowledge base.'), nl;
        write('Invalid input. Please enter "yes" or "no".'), nl,
        confirm_solution(Solution, Cause));
    assertz(solution(CauseNorm, SolutionNorm)),
    write('New solution added to the knowledge base.'), nl).


% Update the solution
update_solution(Cause, Solution) :-
    retract(solution(Cause, _)),
    assertz(solution(Cause, Solution)),
    write('Solution updated in the knowledge base.'), nl.


% view_knowledge_base/0 with options to view specific parts or all sections.
view_knowledge_base :-
    nl,
    % Provide options for the user to view specific parts or the entire knowledge base.
    write('Choose what to view:'), nl,
    write('1. View all'), nl,
    write('2. View symptoms'), nl,
    write('3. View causes'), nl,
    write('4. View symptom-cause relationships'), nl,
    write('5. View confidences'), nl,
    write('6. View solutions'), nl,
    write('7. Exit'), nl,
    write('|: '),
    read(Choice),
    handle_view_choice(Choice).

% Handle user's choice based on their input
handle_view_choice(1) :-
    view_all, !.  % View all sections.
handle_view_choice(2) :-
    view_symptoms, !.  % View only symptoms.
handle_view_choice(3) :-
    view_causes, !.  % View only causes.
handle_view_choice(4) :-
    view_symptom_cause, !.  % View only symptom-cause relationships.
handle_view_choice(5) :-
    view_confidences, !.  % View only confidences.
handle_view_choice(6) :-
    view_solutions, !.  % View only solutions.
handle_view_choice(7) :-
    write('Exiting view knowledge base.'), !.  % Exit.
handle_view_choice(_) :-
    write('Invalid option. Please try again.'), nl,
    view_knowledge_base.

% View all sections of the knowledge base in a structured format
view_all :-
    view_symptoms,
    view_causes,
    view_symptom_cause,
    view_confidences,
    view_solutions.

% View only the symptoms, each on a new line
view_symptoms :-
    nl, write('Symptoms:'), nl,
    findall(S, symptom(S), Symptoms),
    print_list(Symptoms).

% View only the causes, each on a new line
view_causes :-
    nl, write('Causes:'), nl,
    findall(C, confidence(C, _), Causes),
    print_list(Causes).

% View symptom-cause relationships in a structured format
view_symptom_cause :-
    nl, write('Symptom-Cause Relationships:'), nl,
    findall((S, C), symptom_cause(S, C), SCPairs),
    print_pairs(SCPairs).

% View the confidence levels associated with each cause
view_confidences :-
    nl, write('Confidences:'), nl,
    findall((C, Conf), confidence(C, Conf), Confidences),
    print_pairs(Confidences).

% View the solutions associated with each cause in a structured format
view_solutions :-
    nl, write('Solutions:'), nl,
    findall((C, Sol), solution(C, Sol), Solutions),
    print_pairs(Solutions).

% Utility to print a list of items, each on a new line
print_list([]).
print_list([H|T]) :-
    write('- '), write(H), nl,
    print_list(T).

% Utility to print pairs of items in a structured format
print_pairs([]).
print_pairs([(A, B)|T]) :-
    write('- '), write(A), write(' -> '), write(B), nl,
    print_pairs(T).

    
% Debug: List all solutions
debug_list_solutions :-
    write('Listing all solutions in the knowledge base:'), nl,
    forall(solution(Cause, Solution),
           format('Cause: ~w, Solution: ~w~n', [Cause, Solution])).