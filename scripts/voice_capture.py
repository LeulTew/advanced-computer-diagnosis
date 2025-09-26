#!/usr/bin/env python3
"""Capture a short voice description and emit recognized text for Prolog integration.

Dependencies:
  - vosk
  - sounddevice
Provide an offline VOSK model path via the VOSK_MODEL_PATH environment variable
(or place the English small model under models/vosk-model-small-en-us-0.15).
"""
from __future__ import annotations

import json
import os
import queue
import sys
import time

try:
    import sounddevice as sd  # type: ignore
    from vosk import KaldiRecognizer, Model  # type: ignore
except Exception:  # pragma: no cover - optional dependency
    print("", end="")
    sys.exit(0)

SAMPLE_RATE = 16_000
BLOCK_SIZE = 8_000
CAPTURE_SECONDS = 8

MODEL_PATH = os.environ.get(
    "VOSK_MODEL_PATH",
    os.path.join("models", "vosk-model-small-en-us-0.15"),
)

if not os.path.isdir(MODEL_PATH):  # pragma: no cover - runtime guard
    print("", end="")
    sys.exit(0)


def main() -> int:
    model = Model(MODEL_PATH)
    recognizer = KaldiRecognizer(model, SAMPLE_RATE)
    audio_queue: "queue.Queue[bytes]" = queue.Queue()

    def callback(indata, frames, _time, status) -> None:  # pragma: no cover - I/O hook
        if status:
            # Stream warnings/errors are ignored for simplicity.
            pass
        audio_queue.put(bytes(indata))

    with sd.RawInputStream(
        samplerate=SAMPLE_RATE,
        blocksize=BLOCK_SIZE,
        dtype="int16",
        channels=1,
        callback=callback,
    ):
        start = time.time()
        transcript: list[str] = []
        while time.time() - start < CAPTURE_SECONDS:
            try:
                data = audio_queue.get(timeout=CAPTURE_SECONDS)
            except queue.Empty:
                break
            if recognizer.AcceptWaveform(data):
                result = json.loads(recognizer.Result())
                segment = result.get("text", "").strip()
                if segment:
                    transcript.append(segment)
        final_result = json.loads(recognizer.FinalResult())
        segment = final_result.get("text", "").strip()
        if segment:
            transcript.append(segment)

    output = " ".join(transcript).strip()
    print(output, end="")
    return 0


if __name__ == "__main__":
    sys.exit(main())
