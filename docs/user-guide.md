# User Guide

Welcome to the **Advanced Computer Problem Diagnosis Expert System**! This guide walks you through typical troubleshooting sessions and explains how to make the most of the CLI experience.

## 1. Launching the System

```fish
swipl -s diag.pl -g main
```

You will see a colorized menu with options for running diagnoses, teaching the system, viewing analytics, switching languages, and more. Navigate using plain numeric options (e.g., `1`).

## 2. Running a Diagnosis

Choose **Start diagnosis (guided)** to enter the traditional yes/no workflow.

1. Optionally describe the issue in free text. The system will pre-fill symptoms that match your description.
2. Answer each symptom prompt with `yes.`, `no.`, or `unsure.` (these atoms still require the trailing period).
3. If the system detects contradictory answers, it will offer to resolve them for you.
4. Follow-up questions may appear through rule-based chaining (e.g., confirming dust accumulation after overheating is reported).
5. Results show:
   - Ranked causes with final confidence scores blending weighted and Bayesian reasoning.
   - Recommended solutions for each cause.
   - A session summary listing confirmed, rejected, and uncertain symptoms.
   - An entry in the analytics log for trend tracking.

### Natural Language Mode

Choose **Start diagnosis (natural language)** to begin with an open-ended description. The system parses key phrases ("blue screen", "no sound", etc.) and seeds the symptom list before the guided prompts.

### Voice Mode

Select **Start diagnosis (voice input)** for a hands-free session.

- Install dependencies listed in `requirements.txt` (`vosk`, `sounddevice`) and download a Vosk acoustic model (`VOSK_MODEL_PATH`).
- Speak after the tone and press Enter when finished.
- The recognized text seeds the symptom list; the guided flow follows as usual.
- If voice prerequisites are missing, the CLI falls back to the guided mode automatically.

## 3. Managing Knowledge

- **Learn new symptom/cause/solution**: Provide new facts during runtime. They are persisted automatically to `data/kb_dynamic.pl` and immediately included in future diagnoses.
- **View knowledge base**: Inspect symptoms, causes, relationships, confidences, and solutions.
- **Save/Load knowledge**: Export/import knowledge snapshots manually if desired.

## 4. Analytics and Reporting

- Each diagnosis appends a JSON line to `data/analytics.jsonl` capturing timestamp, confirmed/rejected symptoms, and ranked causes.
- Use **Show analytics summary** (option 10) to view localized quick stats such as total sessions and top causes. The command reads directly from the analytics log.
- All generated analytics are ignored by Git and safe for local experimentation.
- Want deeper insights? Pipe `data/analytics.jsonl` through `jq`, load it into a notebook, or feed it into your favorite dashboard tool.

## 5. Multi-language Support

Switch languages via **Change language** (currently English `en` and Spanish `es`). Prompts, summaries, and menu labels update instantly. Diagnoses still expect the Prolog atoms `yes.`, `no.`, and `unsure.` for user responses.

## 6. Helpful Scripts

| Script | Purpose |
| --- | --- |
| `scripts/install.sh` | Automated dependency installer and health-check runner (`bash scripts/install.sh`). |
| `tools/static_analysis.pl` | Detect undefined predicates and knowledge base drift (`swipl -q -s tools/static_analysis.pl`). |
| `tools/perf_profile.pl` | Run repeatable diagnosis scenarios and capture latency, CPU, memory, and inference metrics (`swipl -q -s tools/perf_profile.pl`). |
| `scripts/run_checks.sh` | Shell wrapper that runs static analysis plus all unit/integration tests in one go. |
| `scripts/run_profile.sh` | Convenience launcher for the performance benchmarks (results land in `data/perf_profile.json`). |

## 7. Tips & Troubleshooting

- **Unrecognized Symptoms**: Add them via the learning options. They are persisted automatically.
- **Conflicting Inputs**: When the system flags contradictions, choose which symptom should remain positive or mark both as `unsure.`
- **Analytics Reset**: Remove `data/analytics.jsonl` if you want a fresh log. A new file is created on the next run.
- **Voice Mode Issues**: Ensure your microphone is accessible, `sounddevice` can detect it, and `VOSK_MODEL_PATH` points to an unpacked Vosk model directory.

Enjoy fast, explainable computer troubleshooting! If you get stuck, check the Developer Guide for deeper technical insights or file an issue on the repository.
