# Advanced Computer Problem Diagnosis Expert System

[![SWI-Prolog](https://img.shields.io/badge/Prolog-SWI--Prolog-orange)](https://www.swi-prolog.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Build Status](https://github.com/LeulTew/advanced-computer-diagnosis/actions/workflows/test.yml/badge.svg)](https://github.com/LeulTew/advanced-computer-diagnosis/actions)

An intelligent, interactive expert system built in Prolog for diagnosing common computer hardware and software issues. Leverages a dynamic knowledge base with confidence-weighted reasoning to provide accurate diagnoses and actionable solutions.

## Table of Contents

- [Features](#features)
- [Architecture](#architecture)
- [Installation](#installation)
- [Usage](#usage)
- [Examples](#examples)
- [Testing](#testing)
- [Contributing](#contributing)
- [License](#license)

## Features

- **Comprehensive Knowledge Base**: Pre-loaded with symptoms, causes, and solutions for common PC issues (e.g., slow performance, crashes, overheating).
- **Interactive Diagnosis**: Asks users about symptoms with yes/no/unsure options for nuanced input.
- **Confidence Scoring**: Bayesian-inspired confidence adjustment accounts for positive, negative, and uncertain evidence.
- **Learning Capability**: Dynamically add new symptoms, causes, and solutions at runtime.
- **Persistence**: Save and load knowledge bases to/from files for long-term evolution.
- **Knowledge Inspection**: View and manage the internal knowledge base sections.
- **Extensible Design**: Easy to expand with new rules and facts.

## Architecture

The system is implemented in SWI-Prolog, utilizing a rule-based expert system architecture:

### Core Components

1. **Knowledge Base**:
   - **Dynamic Predicates**: `symptom/1`, `cause/1`, `solution/2`, `symptom_cause/2`, `confidence/2` store facts that can be modified at runtime.
   - **Initial Facts**: Pre-defined symptoms (e.g., `slow_performance`), causes (e.g., `virus`), and mappings with base confidence levels.

2. **Inference Engine**:
   - **Diagnosis Logic**: `find_causes/2` matches observed symptoms to potential causes using weighted scoring.
   - **Confidence Adjustment**: `adjust_confidence/2` refines base confidence based on symptom answers (yes: +1, no: -0.5, unsure: 0), normalized by symptom count per cause.
   - **Presentation Layer**: Formats and ranks diagnoses with solutions.

3. **User Interface**:
   - **Main Loop**: `main/0` provides a menu-driven interface for diagnosis, learning, and knowledge management.
   - **Interactive Prompts**: Handles user input with validation and error handling.

4. **Persistence Layer**:
   - **Save/Load**: `save_kb/1` and `consult/1` enable exporting/importing the knowledge base to Prolog files.

### Data Flow

```
User Input (Symptoms) → Inference Engine → Confidence Scoring → Diagnosis Output
                      ↓
              Knowledge Base (Dynamic Facts)
                      ↓
              Persistence (File I/O)
```

### Key Algorithms

- **Symptom Collection**: Iterative questioning with uncertainty handling.
- **Cause Matching**: Set-based intersection of symptom-cause relations.
- **Scoring**: Weighted sum of evidence per cause, scaled by base confidence.

## Installation

### Prerequisites

- [SWI-Prolog](https://www.swi-prolog.org/) 9.0+ (recommended for full compatibility).

### Setup

1. **Install SWI-Prolog**:
   ```fish
   sudo apt update
   sudo apt install swi-prolog
   ```

2. **Clone the Repository**:
   ```fish
   git clone https://github.com/LeulTew/advanced-computer-diagnosis.git
   cd advanced-computer-diagnosis
   ```

3. **Verify Installation**:
   ```fish
   swipl -s diag.pl -g halt
   ```

## Usage

Launch the expert system:

```fish
swipl -s diag.pl -g main
```

### Menu Options

1. **Start Diagnosis**: Answer symptom questions to receive diagnoses.
2. **Learn New Symptom**: Add symptoms to the knowledge base.
3. **Learn New Cause**: Add causes and link to symptoms.
4. **Learn New Solution**: Provide solutions for causes.
5. **View Knowledge Base**: Inspect stored facts.
6. **Save Knowledge**: Export KB to a file.
7. **Load Knowledge**: Import KB from a file.
8. **Exit**: Quit the system.

### Persistence

- Save: Choose option 6, enter filename (e.g., `my_kb.pl`).
- Load: Choose option 7, enter filename.

## Examples

### Sample Diagnosis Session

```
Choose an option:
1. Start diagnosis
> 1
Welcome to the Advanced Computer Problem Diagnosis Expert System!
Does the computer exhibit slow_performance (yes./no./unsure.) yes
Does the computer exhibit frequent_crashes (yes./no./unsure.) no
...
Based on the symptoms, the problem(s) might be:
virus (Confidence: 1.35): Solution: Run a full system scan with updated antivirus software.
hardware_failure (Confidence: 0.84): Solution: Check hardware connections and consider replacing faulty components.
```

### Adding Knowledge

```
Choose an option:
2. Learn new symptom
> 2
Enter a new symptom: blue_screen_of_death
New symptom added.
```

## Testing

Run the unit tests:

```fish
swipl -s diag.pl -s test/diag_tests.pl -g run_tests -g halt
```

Tests cover core predicates like `find_causes/2` and confidence adjustments.

## Contributing

Contributions welcome! Please:

1. Fork the repository.
2. Create a feature branch (`git checkout -b feature/amazing-feature`).
3. Commit changes (`git commit -m 'Add amazing feature'`).
4. Push to the branch (`git push origin feature/amazing-feature`).
5. Open a Pull Request.

### Development Setup

- Ensure SWI-Prolog is installed.
- Run tests before submitting PRs.
- Follow Prolog best practices for dynamic predicates and backtracking.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

Built with ❤️ using SWI-Prolog. Empowering users to troubleshoot computers intelligently.
