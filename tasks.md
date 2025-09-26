# Project Improvement Roadmap

This roadmap highlights actionable steps to elevate the Advanced Computer Problem Diagnosis Expert System. Items are grouped by theme and ordered roughly from highest to lowest impact. Completed items are marked with ✅.

## Core Expert System
- [x] **Persist learned knowledge** – Allow saving new symptoms/causes/solutions to disk (e.g., using `tell/1` and `listing/0`). 
- [x] **Refine confidence model** – Experiment with Bayesian or weighted scoring that accounts for negative/unsure answers. 
- [x] **Add contradictory symptom handling** – Detect conflicting inputs (e.g., mutually exclusive symptoms) and guide the user.
- [x] **Expand knowledge base** – Incorporate additional hardware, software, and network issues sourced from current support FAQs.
- [x] **Implement rule-based chaining** – Add forward/backward chaining for multi-step diagnoses (e.g., if cause A, check symptom B).
- [x] **Add probabilistic reasoning** – Integrate full Bayesian networks for more accurate uncertainty handling.

## User Experience
- [x] **Improve CLI messaging** – Add color, clearer prompts, and validation hints to minimize user confusion.
- [x] **Provide session summary** – After diagnosis, summarize symptoms, matched causes, and recommended next steps.
- [x] **Add analytics** – Track number of diagnoses, most frequent causes, and export basic reports.
- [x] **Natural language input** – Integrate NLP to allow users to describe issues in plain English.
- [x] **Multi-language support** – Localize prompts and outputs for non-English users.
- [x] **Voice interface** – Add speech-to-text for hands-free troubleshooting.

## Tooling & Testing
- [x] **Automate knowledge base linting** – Write Prolog unit tests (using `plunit`) to verify facts are consistent and non-duplicated. 
- [x] **CI integration** – Configure GitHub Actions to run tests and style checks on every push/pr.
- [ ] **Static analysis** – Evaluate using `swipl -q -g "list_undefined."` or similar to detect undefined predicates.
- [ ] **Performance profiling** – Add benchmarks for diagnosis speed and memory usage.
- [ ] **Integration tests** – Test full user workflows end-to-end.

## Documentation
- [ ] **User guide** – Create a walkthrough for typical troubleshooting scenarios. 
- [ ] **Developer guide** – Document architecture, data flow, and how to extend the knowledge base safely. 
- [ ] **Changelog & versioning** – Establish semantic versioning with release notes.
- [ ] **API documentation** – If APIs are added, document endpoints and usage.
- [ ] **Video tutorials** – Create screencasts for setup and usage.

## Distribution & Outreach
- [ ] **Package for easy install** – Provide a simple shell script or package to install dependencies and launch the system.
- [ ] **Web interface revival** – Modernize the HTML interface found in `Old Versions/` or rebuild with a lightweight frontend (e.g., using Prolog's HTTP server or Node.js/React).
- [ ] **Simple frontend** – Build a basic web UI for diagnosis without CLI (e.g., form-based input, results display).
- [ ] **Collect feedback** – Integrate a simple feedback mechanism (email or issue template) for reported gaps.
- [ ] **Docker containerization** – Package the app in a Docker image for easy deployment.
- [ ] **Mobile app** – Develop a companion app for Android/iOS using frameworks like Flutter or React Native.
- [ ] **API endpoints** – Expose RESTful APIs for third-party integrations (e.g., diagnose via HTTP POST).

## Advanced Features
- [ ] **Machine learning integration** – Use ML to learn from user feedback and improve diagnoses over time.
- [ ] **Real-time monitoring** – Integrate with system tools to pull live hardware/software metrics.
- [ ] **Collaborative knowledge** – Allow users to submit new rules via a web form, moderated approval.
- [ ] **Plugin system** – Enable third-party plugins for specialized diagnostics (e.g., GPU issues).
- [ ] **Offline mode** – Cache knowledge base for offline use.
- [ ] **Security hardening** – Add input sanitization and rate limiting for web interfaces.
