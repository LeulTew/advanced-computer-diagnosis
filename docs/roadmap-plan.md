# Roadmap Execution Strategy

> Generated to accompany `tasks.md`. Each item below outlines the intended implementation path, prerequisites, and any immediate blockers.

## Core Expert System

| Task | Implementation Strategy | Dependencies | Current Notes |
| --- | --- | --- | --- |
| Persist learned knowledge | Introduce automatic persistence hooks after `learn_*` predicates that append to a dedicated knowledge snapshot (`data/kb_dynamic.pl`). Provide manual flush command to regenerate clean snapshot via `listing/0`. | File system write access | Ready to implement. |
| Refine confidence model | Replace flat weighting with tunable parameters (positive, negative, unsure) and normalization by evidence strength; expose configuration via dynamic predicate `confidence_weight/2`. | Existing scoring predicates | Requires regression tests for scoring. |
| Add contradictory symptom handling | Define `contradiction/2` facts and perform pre-diagnosis validation that warns/asks user to resolve conflicts. | Expanded knowledge base | Needs curated contradiction pairs. |
| Expand knowledge base | Source additional symptoms/causes/solutions from reputable troubleshooting guides; encode in `diag.pl`. | Research time | Pending data collection. |
| Implement rule-based chaining | Introduce intermediate rules (e.g., `rule(if Symptom then ask AnotherSymptom)`) enabling forward- and backward-chaining via explicit predicates. | Enhanced inference engine | Design in progress. |
| Add probabilistic reasoning | Prototype lightweight Bayesian network using `library(pairs)` and custom probability propagation; evaluate `plbayes` as optional dependency. | External research | Blocked pending library evaluation. |

## User Experience

| Task | Implementation Strategy | Dependencies | Current Notes |
| --- | --- | --- | --- |
| Improve CLI messaging | Add ANSI color helpers, better prompts, and validation hints. Wrap all writes via `ui_` module. | Terminal supporting ANSI | Implementation straightforward. |
| Provide session summary | Accumulate asked symptoms and final diagnoses; print summary block + recommended actions and persistence reminder. | Completed diagnosis data structure | Ready to implement. |
| Add analytics | Introduce `analytics_log/1` predicate writing to JSON/CSV with counters; optionally integrate with `library(http/json)`. | Persistence format decision | Need storage location. |
| Natural language input | Embed simple phrase matching via `library(dcg/basics)` and synonyms list before mapping to canonical symptoms. | Additional NLP data | Requires dataset of paraphrases. |
| Multi-language support | Externalize strings into `locale/3` facts and allow locale selection at startup. | Translation files | Pending translations. |
| Voice interface | Explore shell integration with `arecord`/speech-to-text via Vosk or external API; orchestrate via Prolog external process. | External binaries | Blocked pending library selection. |

## Tooling & Testing

| Task | Implementation Strategy | Dependencies | Current Notes |
| --- | --- | --- | --- |
| CI integration | Configure GitHub Actions workflow running `swipl -q -s test/diag_tests.pl -g run_tests,halt`. | GitHub Actions | Workflow template ready. |
| Static analysis | Add script using `list_undefined` and `list_undefined` check; integrate into CI. | SWI-Prolog tooling | Requires baseline cleanup. |
| Performance profiling | Use `statistics/2` hooks to measure diagnosis time; optional benchmarking predicates. | Sample scenarios | Pending test harness. |
| Integration tests | Script representative diagnosis sessions using `with_output_to` and pre-seeded answers; assert outputs. | Deterministic IO handling | Needs CLI refactor for testability. |

## Documentation

| Task | Implementation Strategy | Dependencies | Current Notes |
| --- | --- | --- | --- |
| User guide | Create `docs/user-guide.md` with end-to-end walkthroughs and troubleshooting tips. | Up-to-date features | Ready once new UX lands. |
| Developer guide | Document architecture internals, dynamic predicates, extension guidelines in `docs/developer-guide.md`. | Stable code structure | To follow feature refactors. |
| Changelog & versioning | Establish `CHANGELOG.md` with Keep a Changelog format and semantic version tags. | Release discipline | Kick off at next release. |
| API documentation | Document any future HTTP endpoints; blocked until APIs exist. | API implementation | Blocked. |
| Video tutorials | Outline script, capture demo via OBS; host on repo wiki or YouTube. | Recording setup | Requires time investment. |

## Distribution & Outreach

| Task | Implementation Strategy | Dependencies | Current Notes |
| --- | --- | --- | --- |
| Package for easy install | Author `install.sh` (Fish-compatible) and `Makefile` to install dependencies and launch app. | OS packaging tools | Pending after persistence updates. |
| Web interface revival | Modernize legacy HTML using SWI-Prolog HTTP server or React front-end fetching via JSON API. | Backend endpoints | Requires API groundwork. |
| Simple frontend | Develop lightweight HTML/CSS form served via Prolog HTTP server; reuse inference predicates. | HTTP server integration | Tied to web revival task. |
| Collect feedback | Provide `feedback.md` template and CLI option linking to GitHub issues or mailto. | Repo configuration | Straightforward. |
| Docker containerization | Create `Dockerfile` bundling SWI-Prolog and project with entrypoint to `main/0`. | Docker tooling | Ready to implement. |
| Mobile app | Prototype Flutter or React Native client hitting upcoming API. | API availability | Blocked pending API. |
| API endpoints | Expose REST endpoints using `library(http/http_dispatch)` bridging to diagnosis predicates. | HTTP server | Dependent on service layer design. |

## Advanced Features

| Task | Implementation Strategy | Dependencies | Current Notes |
| --- | --- | --- | --- |
| Machine learning integration | Collect labeled sessions, train simple classifier (e.g., Python scikit-learn), integrate via foreign function interface. | Dataset, FFI bridge | Blocked pending data. |
| Real-time monitoring | Interface with OS metrics (e.g., `wmic`, `lsof`) via Prolog shell commands; map anomalies to symptoms. | Platform-specific scripts | Needs security review. |
| Collaborative knowledge | Build web submission form storing proposed rules for moderator review. | Web interface | Dependent on API + authentication. |
| Plugin system | Define plugin manifest and dynamic module loader that registers new predicates. | Modular architecture refactor | Requires design spike. |
| Offline mode | Bundle knowledge snapshots and avoid external dependencies; ensure file writes degrade gracefully. | Persistence improvements | Planned alongside package task. |
| Security hardening | Sanitize inputs, rate-limit HTTP endpoints, audit file operations. | Web/API implementation | Blocked until interfaces exist. |

---

This strategy document will evolve as items transition from planning to implementation. Updates should mirror progress recorded in `tasks.md`.
