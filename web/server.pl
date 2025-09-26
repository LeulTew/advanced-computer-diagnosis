#!/usr/bin/env swipl

:- module(server, [start/0, start/1, stop/0]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_cors)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(pairs)).

:- dynamic web_server/1.
:- dynamic web_dir/1.

:- multifile user:file_search_path/2.
:- multifile user:http_safe_file/2.

:- initialization(init_server).

init_server :-
    setup_web_directory,
    ensure_diag_loaded.

setup_web_directory :-
    module_property(server, file(AbsFile)),
    file_directory_name(AbsFile, WebDir),
    retractall(web_dir(_)),
    assertz(web_dir(WebDir)).

user:http_safe_file(Path, _Options) :-
    web_dir(WebDir),
    atom_concat(WebDir, Rest, Path),
    \+ sub_atom(Rest, 0, _, _, '..').

ensure_diag_loaded :-
    project_root(Root),
    directory_file_path(Root, 'diag.pl', Diag),
    (   current_module(diag)
    ->  true
    ;   user:ensure_loaded(Diag)
    ).

ensure_diag_available :-
    (   current_predicate(user:symptom/1)
    ->  true
    ;   ensure_diag_loaded
    ).

project_root(Root) :-
    module_property(server, file(AbsFile)),
    file_directory_name(AbsFile, Dir),
    file_directory_name(Dir, Root).

http:location(static, root('assets/'), []).

:- http_handler(root(.), serve_index, []).
:- http_handler(static(.), serve_static, [prefix]).
:- http_handler(root(api/symptoms), api_symptoms, [method(get)]).
:- http_handler(root(api/diagnose), api_diagnose, [method(post)]).

start :-
    (   getenv('PORT', PortAtom)
    ->  atom_number(PortAtom, Port)
    ;   Port = 8080
    ),
    start(Port).

start(Port) :-
    (   web_server(Port)
    ->  format('Server already running on port ~w.~n', [Port])
    ;   http_server(http_dispatch, [port(Port), timeout(60), workers(16)]),
        assertz(web_server(Port)),
        format('📡  Web UI available at http://localhost:~w/~n', [Port])
    ).

stop :-
    forall(web_server(Port),
           ( http_stop_server(Port, []), retract(web_server(Port)) )),
    format('🛑  Web server stopped.~n', []).

serve_index(Request) :-
    web_dir(Dir),
    directory_file_path(Dir, 'index.html', File),
    determine_mime(File, Mime),
    http_reply_file(
        File,
        [unsafe(true), cache(false), headers([content_type(Mime)])],
        Request
    ).

serve_static(Request) :-
    memberchk(path_info(Relative0), Request),
    to_atom(Relative0, Relative),
    Relative \= '',
    \+ sub_atom(Relative, _, _, _, '..'),
    web_dir(Dir),
    directory_file_path(Dir, 'assets', AssetsDir),
    directory_file_path(AssetsDir, Relative, File),
    (   exists_file(File)
    ->  determine_mime(File, Mime),
        http_reply_file(
            File,
            [ unsafe(true),
              cache_extensions([css, js, mjs, cjs, png, jpg, jpeg, svg, ico, webp, json, map, txt, wasm]),
              headers([content_type(Mime)])
            ],
            Request
        )
    ;   throw(http_reply(not_found(Relative)))
    ).

serve_static(_Request) :-
    throw(http_reply(not_found('static asset'))).

determine_mime(File, Mime) :-
    file_name_extension(_, Ext0, File),
    downcase_atom(Ext0, Ext),
    (   mime_from_extension(Ext, Mime0)
    ->  Mime = Mime0
    ;   Mime = 'application/octet-stream'
    ).

mime_from_extension('html', 'text/html; charset=UTF-8').
mime_from_extension('htm', 'text/html; charset=UTF-8').
mime_from_extension('css', 'text/css; charset=UTF-8').
mime_from_extension('js', 'application/javascript; charset=UTF-8').
mime_from_extension('mjs', 'application/javascript; charset=UTF-8').
mime_from_extension('cjs', 'application/javascript; charset=UTF-8').
mime_from_extension('json', 'application/json; charset=UTF-8').
mime_from_extension('map', 'application/json; charset=UTF-8').
mime_from_extension('txt', 'text/plain; charset=UTF-8').
mime_from_extension('png', 'image/png').
mime_from_extension('jpg', 'image/jpeg').
mime_from_extension('jpeg', 'image/jpeg').
mime_from_extension('svg', 'image/svg+xml').
mime_from_extension('ico', 'image/x-icon').
mime_from_extension('webp', 'image/webp').
mime_from_extension('wasm', 'application/wasm').

api_symptoms(Request) :-
    ensure_diag_available,
    cors_enable(Request, [methods([get])]),
    findall(Symptom, user:symptom(Symptom), Symptoms),
    maplist(symptom_pair_dict, Symptoms, Pairs),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, SortedDicts),
    reply_json_dict(_{symptoms: SortedDicts}).

symptom_pair_dict(Symptom, Label-Dict) :-
    normalize_label(Symptom, Label),
    Dict = _{id: Symptom, label: Label}.

normalize_label(Atom, Label) :-
    atom_string(Atom, Source),
    split_string(Source, "_", "_", Parts),
    maplist(capitalize_string, Parts, Capitalized),
    maplist(string_to_atom, Capitalized, AtomParts),
    atomic_list_concat(AtomParts, ' ', Label).

string_to_atom(Str, Atom) :-
    atom_string(Atom, Str).

capitalize_string(Str, Capitalized) :-
    (   Str = ""
    ->  Capitalized = ""
    ;   sub_string(Str, 0, 1, _, First),
        sub_string(Str, 1, _, 0, Rest),
        string_upper(First, UpperFirst),
        string_concat(UpperFirst, Rest, Capitalized)
    ).

api_diagnose(Request) :-
    ensure_diag_available,
    cors_enable(Request, [methods([post])]),
    http_read_json_dict(Request, Payload),
    (   _{symptoms: RawSymptoms} :< Payload
    ->  true
    ;   throw(http_reply(bad_request('JSON body must include a "symptoms" array.')))
    ),
    validate_answers(RawSymptoms, Answers),
    (   Answers == []
    ->  throw(http_reply(bad_request('Select at least one symptom before running a diagnosis.')))
    ;   true
    ),
    debug(api, 'api_diagnose incoming answers: ~q', [Answers]),
    (   catch(user:diagnosis_from_answers(Answers, Causes, FinalAnswers), Error,
              (   print_message(error, Error), fail
              ))
    ->  true
    ;   debug(api, 'diagnosis_from_answers failed for answers: ~q', [Answers]),
        throw(http_reply(server_error('Diagnosis failed unexpectedly.')))
    ),
    prepare_response(Causes, FinalAnswers, Response),
    reply_json_dict(Response).

validate_answers(RawList, Answers) :-
    maplist(validate_answer, RawList, Validated),
    exclude(=(skip), Validated, Filtered),
    maplist(answer_tuple, Filtered, Answers).

validate_answer(Dict, Result) :-
    (   get_dict(id, Dict, IdValue),
        get_dict(answer, Dict, AnswerValue)
    ->  true
    ;   throw(http_reply(bad_request('Each symptom entry must include "id" and "answer".')))
    ),
    to_atom(IdValue, Symptom),
    (   user:symptom(Symptom)
    ->  true
    ;   throw(http_reply(bad_request('Unknown symptom: ~w'-[IdValue])))
    ),
    to_atom(AnswerValue, AnswerRaw),
    downcase_atom(AnswerRaw, AnswerLower),
    (   memberchk(AnswerLower, [skip, none, ''])
    ->  Result = skip
    ;   memberchk(AnswerLower, [yes, no, unsure])
    ->  Result = answer{symptom:Symptom, answer:AnswerLower}
    ;   throw(http_reply(bad_request('Answer must be yes, no, unsure, or skip.')))
    ).

answer_tuple(answer{symptom:Symptom, answer:Answer}, (Symptom, Answer)).

prepare_response(Causes, FinalAnswers, Response) :-
    maplist(cause_result_dict, Causes, CauseDicts),
    categorize_answers(FinalAnswers, Positives, Negatives, Unsures),
    maplist(summary_label, Positives, PosLabels),
    maplist(summary_label, Negatives, NegLabels),
    maplist(summary_label, Unsures, UnsureLabels),
    Response = _{
        diagnoses: CauseDicts,
        summary: _{
            positives: PosLabels,
            negatives: NegLabels,
            unsures: UnsureLabels
        }
    }.

cause_result_dict((Cause, Confidence), Dict) :-
    ConfidenceRounded is round(Confidence * 1000) / 1000,
    (   user:solution(Cause, Solution)
    ->  true
    ;   Solution = 'No specific solution recorded.'
    ),
    normalize_label(Cause, Label),
    Dict = _{cause: Cause, label: Label, confidence: ConfidenceRounded, solution: Solution}.

categorize_answers(Answers, Positives, Negatives, Unsures) :-
    include(answer(yes), Answers, Pos),
    include(answer(no), Answers, Neg),
    include(answer(unsure), Answers, Uns),
    maplist(symptom_only, Pos, Positives),
    maplist(symptom_only, Neg, Negatives),
    maplist(symptom_only, Uns, Unsures).

symptom_only((Symptom, _), Symptom).

answer(Value, (_, Value)).

summary_label(Symptom, Dict) :-
    normalize_label(Symptom, Label),
    Dict = _{id: Symptom, label: Label}.

to_atom(Value, Atom) :-
    (   atom(Value)
    ->  Atom = Value
    ;   string(Value)
    ->  atom_string(Atom, Value)
    ;   throw(http_reply(bad_request('Expected atom or string value, got: ~w'-[Value])))
    ).
