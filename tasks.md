# Project Improvement Roadmap

This roadmap highlights actionable steps to elevate the Advanced Computer Problem Diagnosis Expert System. Items are grouped by theme and ordered roughly from highest to lowest impact.

## Core Expert System
- [ ] **Persist learned knowledge** – Allow saving new symptoms/causes/solutions to disk (e.g., using `tell/1` and `listing/0`).
- [ ] **Refine confidence model** – Experiment with Bayesian or weighted scoring that accounts for negative/unsure answers.
- [ ] **Add contradictory symptom handling** – Detect conflicting inputs (e.g., mutually exclusive symptoms) and guide the user.
- [ ] **Expand knowledge base** – Incorporate additional hardware, software, and network issues sourced from current support FAQs.

## User Experience
- [ ] **Improve CLI messaging** – Add color, clearer prompts, and validation hints to minimize user confusion.
- [ ] **Provide session summary** – After diagnosis, summarize symptoms, matched causes, and recommended next steps.
- [ ] **Add analytics** – Track number of diagnoses, most frequent causes, and export basic reports.

## Tooling & Testing
- [ ] **Automate knowledge base linting** – Write Prolog unit tests (using `plunit`) to verify facts are consistent and non-duplicated.
- [ ] **CI integration** – Configure GitHub Actions to run tests and style checks on every push/pr.
- [ ] **Static analysis** – Evaluate using `swipl -q -g "list_undefined."` or similar to detect undefined predicates.

## Documentation
- [ ] **User guide** – Create a walkthrough for typical troubleshooting scenarios.
- [ ] **Developer guide** – Document architecture, data flow, and how to extend the knowledge base safely.
- [ ] **Changelog & versioning** – Establish semantic versioning with release notes.

## Distribution & Outreach
- [ ] **Package for easy install** – Provide a simple shell script or package to install dependencies and launch the system.
- [ ] **Web interface revival** – Modernize the HTML interface found in `Old Versions/` or rebuild with a lightweight frontend.
- [ ] **Collect feedback** – Integrate a simple feedback mechanism (email or issue template) for reported gaps.
