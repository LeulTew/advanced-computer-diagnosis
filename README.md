# Advanced Computer Problem Diagnosis Expert System

This project is an interactive expert system built in Prolog to help diagnose common computer issues. The knowledge base maps user-provided symptoms to likely causes and suggested solutions, and it allows incremental learning so the system can evolve over time.

## Features

- Rich library of computer symptoms, causes, and solutions.
- Interactive diagnosis dialogue with confidence-weighted results.
- Ability to learn new symptoms, causes, and solutions at runtime.
- Knowledge base inspection tools to review stored facts.

## Requirements

- [SWI-Prolog](https://www.swi-prolog.org/) 8.2 or newer is recommended.

## Getting Started

1. **Install Prolog**
   - On Debian/Ubuntu (including WSL):
     ```fish
     sudo apt update
     sudo apt install swi-prolog
     ```
2. **Launch the Expert System**
   ```fish
   cd "/home/leul/Documents/Github/Assign 3"
   swipl -s diag.pl -g main.
   ```
3. Follow the prompts to answer questions about the computer's symptoms.

### Optional: Persisting Learned Knowledge

The current implementation stores new facts only for the active session. To make learned knowledge persistent, consider exporting the knowledge base to a file using `tell/1` or `save_program/2`.

## Project Structure

- `diag.pl` – main Prolog expert system.
- `Old Versions/` – legacy HTML and Prolog assets kept for reference.

## Development Workflow

1. Make changes to the project files.
2. Run linting or tests as applicable (see the roadmap in `tasks.md`).
3. Commit your changes:
   ```fish
   git add .
   git commit -m "Describe your change"
   ```
4. Push to your GitHub repository (see publishing notes below).

## Publishing to GitHub

Authenticate with Git using your own credentials (for example, by running `gh auth login` or configuring an SSH key), then create a remote repository and push:

```fish
git remote add origin git@github.com:LeulTew/advanced-computer-diagnosis.git
git branch -M main
git push -u origin main
```

> **Security note:** Never share personal access tokens or passwords in plain text. Use environment variables or a credentials manager when needed.

## License

Add your preferred license here (e.g., MIT, GPL). If you are unsure, start with [MIT License](https://choosealicense.com/licenses/mit/).
