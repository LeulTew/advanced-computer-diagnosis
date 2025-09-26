const symptomListEl = document.getElementById("symptom-list");
const searchInput = document.getElementById("symptom-search");
const clearButton = document.getElementById("clear-selection");
const runButton = document.getElementById("run-diagnosis");
const resultsEl = document.getElementById("results");
const statusEl = document.getElementById("status");

let allSymptoms = [];
let filteredSymptoms = [];

function showStatus(message, type = "info") {
  statusEl.textContent = message;
  statusEl.dataset.type = type;
}

function clearStatus() {
  statusEl.textContent = "";
  delete statusEl.dataset.type;
}

function createSymptomRow(symptom) {
  const row = document.createElement("div");
  row.className = "symptom-row";
  row.dataset.symptomId = symptom.id;

  const label = document.createElement("label");
  label.className = "symptom-label";
  label.textContent = symptom.label;
  label.setAttribute("for", `symptom-${symptom.id}`);

  const select = document.createElement("select");
  select.id = `symptom-${symptom.id}`;
  select.className = "symptom-select";
  select.innerHTML = `
    <option value="skip" selected>Skip</option>
    <option value="yes">Yes</option>
    <option value="no">No</option>
    <option value="unsure">Unsure</option>
  `;

  row.append(label, select);
  return row;
}

function renderSymptoms(symptoms) {
  symptomListEl.innerHTML = "";
  if (!symptoms.length) {
    symptomListEl.innerHTML = `<p class="muted">No symptoms match your search.</p>`;
    return;
  }

  const fragment = document.createDocumentFragment();
  symptoms.forEach((symptom) => {
    fragment.appendChild(createSymptomRow(symptom));
  });
  symptomListEl.appendChild(fragment);
}

function filterSymptoms(query) {
  const normalized = query.trim().toLowerCase();
  if (!normalized) {
    filteredSymptoms = [...allSymptoms];
  } else {
    filteredSymptoms = allSymptoms.filter((symptom) =>
      symptom.label.toLowerCase().includes(normalized) || symptom.id.includes(normalized)
    );
  }
  renderSymptoms(filteredSymptoms);
}

function gatherSelections() {
  const selections = [];
  const rows = symptomListEl.querySelectorAll(".symptom-row");

  rows.forEach((row) => {
    const select = row.querySelector("select");
    if (!select) return;

    const value = select.value;
    if (value && value !== "skip") {
      selections.push({ id: row.dataset.symptomId, answer: value });
    }
  });

  return selections;
}

async function loadSymptoms() {
  showStatus("Loading symptoms…");
  try {
    const response = await fetch("/api/symptoms");
    if (!response.ok) {
      throw new Error(`Failed to load symptoms (HTTP ${response.status})`);
    }
    const data = await response.json();
    allSymptoms = data.symptoms || [];
    filteredSymptoms = [...allSymptoms];
    renderSymptoms(filteredSymptoms);
    clearStatus();
  } catch (error) {
    showStatus(error.message || "Unable to fetch symptoms.", "error");
  }
}

function renderDiagnoses(diagnoses) {
  if (!diagnoses.length) {
    return `<p class="muted">No known causes match the supplied symptoms.</p>`;
  }

  const items = diagnoses.map((entry) => {
    const confidencePercent = (entry.confidence * 100).toFixed(1);
    return `
      <div class="diagnosis-card">
        <h3>${entry.label}</h3>
        <p class="confidence">Confidence: ${confidencePercent}%</p>
        <p class="solution">${entry.solution}</p>
      </div>
    `;
  });

  return `<div class="diagnosis-grid">${items.join("")}</div>`;
}

function renderSummary(summary) {
  const toList = (items) =>
    items.length
      ? `<ul>${items.map((item) => `<li>${item.label}</li>`).join("")}</ul>`
      : `<p class="muted">Nothing recorded</p>`;

  return `
    <div class="summary-grid">
      <section>
        <h3>Confirmed symptoms</h3>
        ${toList(summary.positives || [])}
      </section>
      <section>
        <h3>Ruled out</h3>
        ${toList(summary.negatives || [])}
      </section>
      <section>
        <h3>Uncertain</h3>
        ${toList(summary.unsures || [])}
      </section>
    </div>
  `;
}

function renderResults(payload) {
  const { diagnoses = [], summary = {} } = payload;
  const html = `
    <h3>Ranked diagnoses</h3>
    ${renderDiagnoses(diagnoses)}
    <h3>Session summary</h3>
    ${renderSummary(summary)}
  `;
  resultsEl.innerHTML = html;
}

async function runDiagnosis() {
  clearStatus();
  const selections = gatherSelections();
  if (!selections.length) {
    showStatus("Select at least one symptom before running a diagnosis.", "error");
    return;
  }

  showStatus("Analyzing…");
  runButton.disabled = true;
  try {
    const response = await fetch("/api/diagnose", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ symptoms: selections })
    });

    if (!response.ok) {
      const errorText = await response.text();
      let message = `Diagnosis failed (HTTP ${response.status}).`;
      try {
        const asJson = JSON.parse(errorText);
        if (asJson.message) message = asJson.message;
      } catch (_) {
        if (errorText) message = errorText;
      }
      throw new Error(message);
    }

    const payload = await response.json();
    renderResults(payload);
    showStatus("Diagnosis completed.", "success");
  } catch (error) {
    showStatus(error.message || "Unexpected error while diagnosing.", "error");
  } finally {
    runButton.disabled = false;
  }
}

function clearSelections() {
  const selects = symptomListEl.querySelectorAll("select");
  selects.forEach((select) => {
    select.value = "skip";
  });
  resultsEl.innerHTML = `<p class="muted">Selections cleared. Pick new symptoms and run again.</p>`;
  clearStatus();
}

// Event bindings
document.addEventListener("DOMContentLoaded", loadSymptoms);
searchInput.addEventListener("input", (event) => filterSymptoms(event.target.value));
clearButton.addEventListener("click", clearSelections);
runButton.addEventListener("click", runDiagnosis);

// Accessibility helpers
searchInput.addEventListener("keydown", (event) => {
  if (event.key === "Escape") {
    searchInput.value = "";
    filterSymptoms("");
  }
});
