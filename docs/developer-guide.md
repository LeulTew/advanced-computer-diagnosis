# Developer Guide

This document captures the internal architecture, conventions, and extension points for the Advanced Computer Problem Diagnosis Expert System.

## 1. Runtime Architecture

```mermaid
flowchart LR
    UI[Conversational CLI] -->|gathers| SYM[Symptom Answers]
    SYM --> RULE[Rule-Based Chaining]
    RULE --> CONTRA[Contradiction Resolver]
    CONTRA --> PROB[Scoring Engine]
    PROB --> RANK[Ranked Causes]
    RANK --> UI
    KB[(Knowledge Base)] -->{dynamic predicates} PROB
    KB --> UI
    KB <--> PERSIST[Autosave Layer]
    RANK --> ANALYTICS[Analytics Logger]
```

### Key Modules

| Area | Predicates | Notes |
| --- | --- | --- |
| CLI | `main/0`, `display_main_menu/0`, `handle_choice/1` | Colorized ANSI output, multi-language prompts, and menu handlers. |
| Diagnosis Flow | `start_diagnosis/1`, `ask_symptoms/3`, `apply_followup_rules/2`, `resolve_contradictions/2` | Guided questioning with rule chaining and conflict resolution. |
| Scoring | `adjust_confidence/3`, `bayesian_adjustment/4`, `combine_confidence/3` | Hybrid weighted + naive Bayesian confidence computation. |
| Persistence | `record_learnt_fact/1`, `autosave_kb/0`, `load_learned_facts/1` | Captures learned facts and writes snapshots to `data/kb_dynamic.pl`. |
| Analytics | `log_session/2`, `append_session_payload/1`, `show_analytics_summary/0` | JSONL session logging plus summary statistics. |
| Localization | `localized_text/3`, `set_diagnosis_mode/1`, translation facts | English/Spanish prompts, with dynamic mode toggling for automated tests. |
| Knowledge | Dynamic predicates (`symptom/1`, `cause/1`, `symptom_probability/3`, etc.) | Extensible data model. |

## 2. Knowledge Model

Dynamic predicates allow live updates:

- `symptom/1`, `cause/1`, `solution/2`, `symptom_cause/2`
- `confidence/2` (prior belief)
- `confidence_weight/2` (UI-tunable scoring weights)
- `symptom_probability/3` (P(symptom|cause) for Bayesian reasoning)
- `followup_rule/3` (forward chaining rules)
- `contradiction/2` (mutually exclusive symptoms)

### Adding New Knowledge

Use the CLI learning options or assert facts in Prolog. Autosave logic records only *new* items and writes them to `data/kb_dynamic.pl`.

For code changes, update the base knowledge section in `diag.pl` and add aliases, probabilities, and follow-up rules to keep the expert system coherent.

## 3. Extending the Engine

### a. New Follow-Up Rules

Add `followup_rule(TriggerSymptom, FollowupSymptom, LocaleKey).`

1. Ensure `FollowupSymptom` exists as `symptom/1`.
2. Add translations (`locale_text/3`) for the follow-up key.
3. Tests can use `diagnosis_from_answers/3` to validate rule behavior.

### b. New Probabilities

Add `symptom_probability/3` entries to enrich Bayesian scoring. Values should be floats `0.0-1.0`. Use multiple entries per cause if needed.

### c. New Locales

Add `supported_locale/2` entry and replicate the `locale_text/3` block. Most prompts reuse format strings; keep placeholders consistent.

### d. Non-interactive Integrations

Use `diagnosis_from_answers/3` for embedding in services or tests. It reuses the full pipeline while disabling interactive prompts.

## 4. Tooling & Automation

| Tool | Command | Purpose |
| --- | --- | --- |
| Automated Install | `bash scripts/install.sh` | Installs SWI-Prolog, bootstraps optional voice dependencies, and runs health checks. |
| Full Check Suite | `./scripts/run_checks.sh` | Runs static analysis plus all unit/integration tests with one command. |
| Unit + Integration Tests | `swipl -s diag.pl -s test/diag_tests.pl -g run_tests -g halt` | Validate inference, scoring, and integration flows individually. |
| Static Analysis | `swipl -q -s tools/static_analysis.pl` | Reports undefined predicates/drift via `prolog_xref`. |
| Performance Profiling (CLI) | `./scripts/run_profile.sh` | Benchmarks representative diagnosis scenarios and writes `data/perf_profile.json`. |
| Performance Profiling (raw) | `swipl -q -s tools/perf_profile.pl` | Run the benchmarking harness directly with custom CLI flags. |
| GitHub Actions | `.github/workflows/ci.yml` | Installs SWI-Prolog, optional Python deps, and runs tests on pushes/PRs. |

## 5. File Layout

```
.
├── diag.pl                 # Main expert system
├── test/
│   ├── diag_tests.pl       # Unit tests
│   └── integration_tests.pl# End-to-end tests
├── tools/
│   ├── static_analysis.pl  # Undefined predicate checker
│   └── perf_profile.pl     # Benchmark harness
├── scripts/
│   ├── run_checks.sh       # Aggregated static analysis + tests
│   ├── run_profile.sh      # Convenience wrapper for perf profiling
│   └── voice_capture.py    # Speech-to-text bridge (optional)
├── data/
│   ├── .gitignore          # Ignore generated snapshots
│   ├── kb_dynamic.pl       # Autosaved knowledge (generated)
│   └── perf_profile.json   # Latest performance benchmark report (generated)
├── docs/
│   ├── user-guide.md
│   └── developer-guide.md
└── README.md
```

## 6. Coding Guidelines

- Prefer pure predicates; rely on dynamic predicates for evolving knowledge.
- Keep `read/1` interactions limited to CLI-specific predicates; expose wrapper predicates for programmatic use.
- When adding user-facing prompts, provide translation entries and reuse UI helpers (`ui_notice/2`, `ui_warning/2`, etc.).
- Preserve testability: guard IO-heavy code with `diagnosis_mode/1` checks so automated tests remain non-interactive.

## 7. Future Hooks

- **HTTP API**: Wrap `diagnosis_from_answers/3` in `library(http/http_dispatch)` handlers.
- **Web UI Revival**: Combine the API with the legacy HTML templates in `Old Versions/` or modern frameworks.
- **Machine Learning**: Capture analytics exports and train external models, feeding predictions back via a foreign interface.

Happy hacking! Contributions welcome via pull requests. Refer to the Changelog for recent updates and the issue tracker for open roadmap items.
