:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_files)).

% Knowledge Base
:- dynamic symptom/1, cause/2, solution/2, cause_confidence/2.

% Sample knowledge base
symptom(slow_performance).
symptom(frequent_crashes).
symptom(blue_screen).
symptom(strange_noises).
symptom(overheating).
symptom(unresponsive_programs).
symptom(slow_internet).
symptom(popups).

cause(virus, slow_performance).
cause(virus, frequent_crashes).
cause(virus, popups).
cause(hardware_failure, blue_screen).
cause(hardware_failure, strange_noises).
cause(dust, overheating).
cause(dust, strange_noises).
cause(too_many_programs_running, unresponsive_programs).
cause(bad_internet_connection, slow_internet).
cause(malware, popups).
cause(software_corruption, unresponsive_programs).

solution(virus, 'Run a full system scan with updated antivirus software.').
solution(hardware_failure, 'Check hardware connections and consider replacing faulty components.').
solution(dust, 'Clean the computer internals, especially fans and heat sinks.').
solution(too_many_programs_running, 'Close unnecessary programs and free up system resources.').
solution(bad_internet_connection, 'Check your internet connection and router settings.').
solution(malware, 'Run anti-malware software to remove threats.').
solution(software_corruption, 'Reinstall the affected software or reset it to default settings.').

cause_confidence(virus, 0.9).
cause_confidence(hardware_failure, 0.7).
cause_confidence(dust, 0.6).
cause_confidence(too_many_programs_running, 0.6).
cause_confidence(bad_internet_connection, 0.6).
cause_confidence(malware, 0.8).
cause_confidence(software_corruption, 0.5).

% Inference Engine
diagnose_with_symptoms(SymptomList, Problem) :-
    find_causes_with_confidence(SymptomList, CausesWithConfidence),
    prioritize_multiple_causes(CausesWithConfidence, PrioritizedCauses),
    suggest_solutions(PrioritizedCauses, Problem).

find_causes_with_confidence(SymptomList, CausesWithConfidence) :-
    findall(Cause-Confidence,
        ( member(Symptom, SymptomList),
          cause(Cause, Symptom),
          cause_confidence(Cause, Confidence)
        ),
        CausesWithConfidence).

prioritize_multiple_causes(CausesWithConfidence, PrioritizedCauses) :-
    sort(2, @>=, CausesWithConfidence, PrioritizedCauses).

suggest_solutions([], '').
suggest_solutions([Cause-Confidence|Rest], Result) :-
    solution(Cause, Solution),
    format(atom(FormattedCause), '~w (Confidence: ~2f): ~w', [Cause, Confidence, Solution]),
    suggest_solutions(Rest, RestResult),
    (RestResult = '' -> Result = FormattedCause; format(atom(Result), '~w | ~w', [FormattedCause, RestResult])).

% HTTP Handlers
:- http_handler(root(.), http_reply_from_files('.', []), [prefix]).
:- http_handler('/api/diagnose', handle_diagnose, [method(post), cors]).
:- http_handler('/api/symptoms', handle_symptoms, [method(get), cors]).

% Start the server
start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% Handle diagnose request
handle_diagnose(Request) :-
    http_read_json_dict(Request, Symptoms),
    diagnose_from_dict(Symptoms, Problem),
    (   Problem = ''
    ->  reply_json_dict(_{error: 'No diagnosis could be made based on the symptoms provided.'})
    ;   reply_json_dict(_{problem: Problem})
    ).

% Convert dict to symptom list and diagnose
diagnose_from_dict(Symptoms, Problem) :-
    dict_pairs(Symptoms, _, Pairs),
    maplist(pair_to_symptom, Pairs, SymptomList),
    diagnose_with_symptoms(SymptomList, Problem).

pair_to_symptom(Symptom-Value, Symptom-Value).

% Handle symptoms request
handle_symptoms(_Request) :-
    findall(Symptom, symptom(Symptom), Symptoms),
    reply_json_dict(_{symptoms: Symptoms}).

% Main predicate to start the server
:- initialization(start_server(8000)).
