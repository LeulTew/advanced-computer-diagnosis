:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(statistics)).
:- use_module(library(apply)).

:- initialization(perf_profile_main).

perf_profile_main :-
    current_prolog_flag(argv, Argv),
    parse_options(Argv, Options),
    load_engine,
    benchmark_scenarios(AllScenarios),
    select_scenarios(Options.scenario, AllScenarios, Selected),
    maplist(run_benchmark(Options), Selected, Summaries),
    print_text_summary(Summaries),
    maybe_write_output(Options, Summaries),
    halt.

load_engine :-
    absolute_file_name('diag.pl', Target,
        [file_type(prolog), access(read), relative_to('.')]),
    load_files(Target, [silent(true)]).

parse_options(Argv, Options) :-
    Default = options{
        iterations: 30,
        format: json,
        output: 'data/perf_profile.json',
        scenario: all
    },
    parse_args(Argv, Default, Options).

parse_args([], Options, Options).
parse_args([Arg|Rest], Acc, Options) :-
    ( Arg = '--help'
    -> print_usage,
       halt(0)
    ; atom_concat('--iterations=', ValueAtom, Arg)
    -> atom_number(ValueAtom, Iter),
       (   Iter > 0
       ->  Acc1 = Acc.put(iterations, Iter)
       ;   print_message(error, main(invalid_iterations(Iter))),
           halt(1)
       )
    ; atom_concat('--format=', FormatAtom, Arg)
    -> atom_string(FormatAtom, FormatStr),
       downcase_atom(FormatAtom, FormatLower),
       (   member(FormatLower, [json, text])
       ->  Acc1 = Acc.put(format, FormatLower)
       ;   print_message(error, main(unsupported_format(FormatStr))),
           halt(1)
       )
    ; atom_concat('--output=', PathAtom, Arg)
    -> atom_string(PathAtom, PathStr),
       (   PathStr = ''
       ->  Acc1 = Acc.put(output, none)
       ;   Acc1 = Acc.put(output, PathStr)
       )
    ; Arg = '--no-output'
    -> Acc1 = Acc.put(output, none)
    ; atom_concat('--scenario=', ScenarioAtom, Arg)
    -> Acc1 = Acc.put(scenario, ScenarioAtom)
    ;   print_message(error, main(unrecognized_argument(Arg))),
        halt(1)
    ),
    parse_args(Rest, Acc1, Options).

print_usage :-
    format('Usage: swipl -q -s tools/perf_profile.pl [options]~n'),
    format('Options:~n'),
    format('  --iterations=N       Number of iterations per scenario (default 30)~n'),
    format('  --format=(json|text) Output format when writing to file (default json)~n'),
    format('  --output=PATH        Write report to PATH (default data/perf_profile.json)~n'),
    format('  --no-output          Skip writing report to disk~n'),
    format('  --scenario=ID        Run only the scenario with the given ID~n'),
    format('  --help               Show this message~n').

benchmark_scenarios([
    scenario{
        id: slow_malware,
        description: 'Popups and sluggish apps (malware-heavy path)',
        goal: diagnosis,
        answers: [(popups, yes), (slow_performance, yes), (disk_full, unsure)]
    },
    scenario{
        id: overheating_chain,
        description: 'Overheating triggering follow-up questions',
        goal: diagnosis,
        answers: [(overheating, yes)]
    },
    scenario{
        id: network_latency,
        description: 'Wi-Fi instability and slow internet',
        goal: diagnosis,
        answers: [(wifi_disconnects, yes), (slow_internet, yes), (network_latency_spikes, yes)]
    },
    scenario{
        id: healthy_baseline,
        description: 'Healthy system signals for baseline measurement',
        goal: diagnosis,
        answers: [(normal_boot, yes), (sound_works, yes), (high_performance, yes), (clean_desktop, yes)]
    },
    scenario{
        id: pure_find_causes,
        description: 'Direct inference engine call without follow-ups',
        goal: find_causes,
        answers: [(display_artifacts, yes), (overheating, yes), (loud_fan, unsure)]
    }
]).

select_scenarios(all, Scenarios, Scenarios).
select_scenarios(ID, Scenarios, [Scenario]) :-
    member(Scenario, Scenarios),
    Scenario.id == ID,
    !.
select_scenarios(ID, _Scenarios, _) :-
    print_message(error, main(unknown_scenario(ID))),
    halt(1).

run_benchmark(Options, Scenario0, Summary) :-
    Iterations = Options.iterations,
    scenario_goal(Scenario0.goal, Scenario0.answers, Goal),
    run_iterations(Iterations, Goal, StatsList),
    stats_summary(StatsList, Metrics),
    maplist(answer_signature, Scenario0.answers, AnswerStrings),
    Summary = scenario{
        id: Scenario0.id,
        description: Scenario0.description,
        goal: Scenario0.goal,
        iterations: Iterations,
        answers: AnswerStrings,
        metrics: Metrics
    }.

scenario_goal(diagnosis, Answers, diagnosis_goal(Answers)).
scenario_goal(find_causes, Answers, find_causes_goal(Answers)).

diagnosis_goal(Answers) :-
    user:diagnosis_from_answers(Answers, _Causes, _FinalAnswers).

find_causes_goal(Answers) :-
    user:find_causes(Answers, _Causes).

run_iterations(0, _Goal, []).
run_iterations(N, Goal, [Stats|Rest]) :-
    N > 0,
    measure_goal(Goal, Stats),
    N1 is N - 1,
    run_iterations(N1, Goal, Rest).

measure_goal(Goal, stats{cpu_ms:CpuMs, wall_ms:WallMs, inferences:Inferences, memory_bytes:MemBytes}) :-
    garbage_collect,
    trim_stacks,
    statistics(globalused, G0),
    statistics(localused, L0),
    statistics(trailused, T0),
    statistics(heapused, H0),
    call_time(silent_goal(Goal), time{cpu:Cpu, wall:Wall, inferences:Inferences}),
    statistics(globalused, G1),
    statistics(localused, L1),
    statistics(trailused, T1),
    statistics(heapused, H1),
    CpuMs is Cpu * 1000,
    WallMs is Wall * 1000,
    MemBytes0 is (G1-G0) + (L1-L0) + (T1-T0) + (H1-H0),
    MemBytes is max(0, MemBytes0).

stats_summary(StatsList, Summary) :-
    extract_values(StatsList, wall_ms, WallValues),
    extract_values(StatsList, cpu_ms, CpuValues),
    extract_values(StatsList, memory_bytes, MemBytesValues),
    extract_values(StatsList, inferences, InfValues),
    convert_bytes_to_kb(MemBytesValues, MemKBValues),
    metric_summary(WallValues, WallSummary),
    metric_summary(CpuValues, CpuSummary),
    metric_summary(MemKBValues, MemSummary),
    metric_summary(InfValues, InfSummary),
    Summary = metrics{
        wall_ms: WallSummary,
        cpu_ms: CpuSummary,
        memory_kb: MemSummary,
        inferences: InfSummary
    }.

extract_values(List, Key, Values) :-
    maplist(get_dict(Key), List, Values).

convert_bytes_to_kb(Bytes, KB) :-
    maplist(bytes_to_kb, Bytes, KB).

bytes_to_kb(Bytes, KB) :-
    KB is Bytes / 1024.0.

silent_goal(Goal) :-
    setup_call_cleanup(
        open_null_stream(Stream),
        with_output_to(Stream, Goal),
        close(Stream)
    ).

metric_summary([], metrics{min:0, max:0, avg:0, median:0, p95:0}).
metric_summary(Values, metrics{min:Min, max:Max, avg:Avg, median:Median, p95:P95}) :-
    min_list(Values, Min),
    max_list(Values, Max),
    sum_list(Values, Sum),
    length(Values, Len),
    Avg is Sum / Len,
    msort(Values, Sorted),
    median_sorted(Sorted, Median),
    percentile_sorted(Sorted, 95, P95).

median_sorted(List, Median) :-
    length(List, Len),
    (   Len =:= 0
    ->  Median = 0
    ;   Mid is Len // 2,
        (   Len mod 2 =:= 1
        ->  Index is Mid + 1,
            nth1(Index, List, Median)
        ;   nth1(Mid, List, A),
            Index2 is Mid + 1,
            nth1(Index2, List, B),
            Median is (A + B) / 2
        )
    ).

percentile_sorted(List, Percent, Value) :-
    length(List, Len),
    (   Len =:= 0
    ->  Value = 0
    ;   PosFloat is (Percent/100.0) * Len,
        ceiling(PosFloat, Index0),
        Index is max(1, min(Len, Index0)),
        nth1(Index, List, Value)
    ).

answer_signature((Symptom, Response), Signature) :-
    format(string(Signature), '~w=~w', [Symptom, Response]).

print_text_summary(Summaries) :-
    get_time(Stamp),
    format_time(string(Timestamp), '%FT%T%:z', Stamp),
    format('~n=== Diagnosis Performance Benchmarks (~w) ===~n', [Timestamp]),
    forall(member(Summary, Summaries), print_scenario_summary(Summary)),
    nl.

print_scenario_summary(Summary) :-
    format('Scenario ~w - ~w~n', [Summary.id, Summary.description]),
    format('  Goal: ~w | Iterations: ~d~n', [Summary.goal, Summary.iterations]),
    format('  Answers: ~w~n', [Summary.answers]),
    print_metric('Wall time (ms)', Summary.metrics.wall_ms, 4),
    print_metric('CPU time (ms)', Summary.metrics.cpu_ms, 4),
    print_metric('Memory (KB)', Summary.metrics.memory_kb, 4),
    print_metric('Inferences', Summary.metrics.inferences, 0),
    nl.

print_metric(Label, Metric, Decimals) :-
    format_number(Metric.avg, Decimals, AvgF),
    format_number(Metric.median, Decimals, MedianF),
    format_number(Metric.min, Decimals, MinF),
    format_number(Metric.max, Decimals, MaxF),
    format_number(Metric.p95, Decimals, P95F),
    format('    ~w: avg=~w (median=~w, min=~w, p95=~w, max=~w)~n',
           [Label, AvgF, MedianF, MinF, P95F, MaxF]).

format_number(Value, 0, Formatted) :-
    !,
    Formatted is round(Value).
format_number(Value, Decimals, Formatted) :-
    Factor is 10^Decimals,
    Formatted is round(Value * Factor) / Factor.

maybe_write_output(Options, Summaries) :-
    (   Options.output == none
    ->  true
    ;   write_report(Options, Summaries)
    ).

write_report(Options, Summaries) :-
    ensure_directory(Options.output),
    get_time(Stamp),
    format_time(string(Timestamp), '%FT%T%:z', Stamp),
    Report = _{
        generated_at: Timestamp,
        iterations: Options.iterations,
        format: Options.format,
        scenarios: Summaries
    },
    (   Options.format == json
    ->  open(Options.output, write, Stream),
        call_cleanup(
            json_write_dict(Stream, Report, [width(0)]),
            close(Stream)
        )
    ;   open(Options.output, write, Stream),
        call_cleanup(
            ( format(Stream, 'Benchmark report generated at ~w~n~n', [Timestamp]),
              forall(member(Summary, Summaries),
                     print_scenario_summary_stream(Stream, Summary))
            ),
            close(Stream)
        )
    ),
    format('Saved benchmark report to ~w (~w format).~n', [Options.output, Options.format]).

print_scenario_summary_stream(Stream, Summary) :-
    format(Stream, 'Scenario ~w - ~w~n', [Summary.id, Summary.description]),
    format(Stream, '  Goal: ~w | Iterations: ~d~n', [Summary.goal, Summary.iterations]),
    format(Stream, '  Answers: ~w~n', [Summary.answers]),
    print_metric_stream(Stream, 'Wall time (ms)', Summary.metrics.wall_ms, 4),
    print_metric_stream(Stream, 'CPU time (ms)', Summary.metrics.cpu_ms, 4),
    print_metric_stream(Stream, 'Memory (KB)', Summary.metrics.memory_kb, 4),
    print_metric_stream(Stream, 'Inferences', Summary.metrics.inferences, 0),
    nl(Stream).

print_metric_stream(Stream, Label, Metric, Decimals) :-
    format_number(Metric.avg, Decimals, AvgF),
    format_number(Metric.median, Decimals, MedianF),
    format_number(Metric.min, Decimals, MinF),
    format_number(Metric.max, Decimals, MaxF),
    format_number(Metric.p95, Decimals, P95F),
    format(Stream, '    ~w: avg=~w (median=~w, min=~w, p95=~w, max=~w)~n',
           [Label, AvgF, MedianF, MinF, P95F, MaxF]).

ensure_directory(none).
ensure_directory(Path) :-
    file_directory_name(Path, Dir),
    ( Dir == '' ; Dir == '.' -> true ; make_directory_path(Dir) ).
