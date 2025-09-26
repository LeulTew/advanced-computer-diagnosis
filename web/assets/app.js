const symptomListEl = document.getElementById("symptom-list");
const searchInput = document.getElementById("symptom-search");
const clearButton = document.getElementById("clear-selection");
const runButton = document.getElementById("run-diagnosis");
const resultsEl = document.getElementById("results");
const statusEl = document.getElementById("status");
const rootEl = document.documentElement;
const themeToggle = document.getElementById("theme-toggle");
const themeToggleIcon = themeToggle?.querySelector(".theme-toggle__icon");
const themeToggleLabel = themeToggle?.querySelector(".theme-toggle__label");
const statusIcons = { info: "ℹ️", success: "✅", error: "⚠️", loading: "⏳" };
const runButtonDefaultLabel = runButton?.textContent?.trim() || "Run diagnosis";
const DEFAULT_SKELETON_CARD_COUNT = 3;

let statusClearTimer = null;

const THEME_STORAGE_KEY = "acpd-theme";
let userSetTheme = false;
let colorSchemeMediaQuery;

function getStoredTheme() {
  try {
    return localStorage.getItem(THEME_STORAGE_KEY);
  } catch (_) {
    return null;
  }
}

function persistTheme(theme) {
  userSetTheme = true;
  try {
    localStorage.setItem(THEME_STORAGE_KEY, theme);
  } catch (_) {
    // Ignore storage failures (e.g., private mode)
  }
}

function resolvePreferredTheme(storedTheme) {
  if (storedTheme === "dark" || storedTheme === "light") {
    return storedTheme;
  }

  if (typeof window !== "undefined" && window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").matches) {
    return "dark";
  }

  return "light";
}

function applyTheme(theme) {
  const resolvedTheme = theme === "dark" ? "dark" : "light";
  const targetTheme = resolvedTheme === "dark" ? "light" : "dark";

  rootEl.setAttribute("data-theme", resolvedTheme);

  if (themeToggle) {
    themeToggle.dataset.theme = resolvedTheme;
    themeToggle.setAttribute("aria-pressed", resolvedTheme === "dark" ? "true" : "false");
    themeToggle.setAttribute("aria-label", `Switch to ${targetTheme} mode`);

    if (themeToggleIcon) {
      themeToggleIcon.textContent = targetTheme === "dark" ? "🌙" : "☀️";
    }

    if (themeToggleLabel) {
      themeToggleLabel.textContent = `${targetTheme.charAt(0).toUpperCase()}${targetTheme.slice(1)} mode`;
    }
  }
}

function initTheme() {
  const storedTheme = getStoredTheme();
  userSetTheme = storedTheme === "dark" || storedTheme === "light";
  const startingTheme = resolvePreferredTheme(storedTheme);

  applyTheme(startingTheme);

  if (window.matchMedia) {
    colorSchemeMediaQuery = window.matchMedia("(prefers-color-scheme: dark)");
    const handlePreferenceChange = (event) => {
      if (userSetTheme) return;
      applyTheme(event.matches ? "dark" : "light");
    };

    if (colorSchemeMediaQuery.addEventListener) {
      colorSchemeMediaQuery.addEventListener("change", handlePreferenceChange);
    } else if (colorSchemeMediaQuery.addListener) {
      colorSchemeMediaQuery.addListener(handlePreferenceChange);
    }
  }

  if (themeToggle) {
    themeToggle.addEventListener("click", () => {
      const currentTheme = rootEl.getAttribute("data-theme") === "dark" ? "dark" : "light";
      const nextTheme = currentTheme === "dark" ? "light" : "dark";
      applyTheme(nextTheme);
      persistTheme(nextTheme);
    });
  }
}

initTheme();

let allSymptoms = [];
let filteredSymptoms = [];

function sanitizeText(value) {
  if (value === null || value === undefined) return "";
  const text = String(value);
  const map = {
    "&": "&amp;",
    "<": "&lt;",
    ">": "&gt;",
    '"': "&quot;",
    "'": "&#39;"
  };
  return text.replace(/[&<>"']/g, (char) => map[char]);
}

function showStatus(message, type = "info", options = {}) {
  if (!statusEl) return;

  const icon = statusIcons[type] || statusIcons.info;
  const { autoClear = type !== "error" && type !== "loading", delay = 4200 } = options;

  statusEl.dataset.type = type;
  statusEl.innerHTML = `
    <span class="status-icon" aria-hidden="true">${icon}</span>
    <span class="status-message">${sanitizeText(message)}</span>
  `;

  if (statusClearTimer) {
    window.clearTimeout(statusClearTimer);
    statusClearTimer = null;
  }

  if (autoClear) {
    statusClearTimer = window.setTimeout(() => {
      clearStatus();
    }, delay);
  }
}

function clearStatus() {
  if (!statusEl) return;
  if (statusClearTimer) {
    window.clearTimeout(statusClearTimer);
    statusClearTimer = null;
  }
  statusEl.innerHTML = "";
  statusEl.removeAttribute("data-type");
}

function setRunButtonBusy(isBusy) {
  if (!runButton) return;
  if (isBusy) {
    runButton.dataset.busy = "true";
  } else {
    delete runButton.dataset.busy;
  }
  runButton.disabled = isBusy;
  runButton.textContent = isBusy ? "Analyzing…" : runButtonDefaultLabel;
}

function renderDiagnosisSkeleton(cardCount = DEFAULT_SKELETON_CARD_COUNT) {
  if (!resultsEl) return;
  const cards = Array.from({ length: cardCount })
    .map(
      () => `
      <div class="skeleton-card">
        <span class="skeleton skeleton--line"></span>
        <span class="skeleton skeleton--line short"></span>
        <span class="skeleton skeleton--block"></span>
      </div>
    `
    )
    .join("");

  resultsEl.innerHTML = `
    <div class="diagnosis-loading" aria-hidden="true">
      <span class="skeleton skeleton--heading"></span>
      <div class="skeleton-grid">${cards}</div>
    </div>
  `;
}

function renderErrorState(message) {
  if (!resultsEl) return;
  resultsEl.innerHTML = `
    <div class="diagnosis-grid">
      <div class="diagnosis-card">
        <h3>We hit a snag</h3>
        <p class="solution">${sanitizeText(message)}</p>
      </div>
    </div>
  `;
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
  showStatus("Loading symptoms…", "loading", { autoClear: false });
  try {
    const response = await fetch("/api/symptoms");
    if (!response.ok) {
      throw new Error(`Failed to load symptoms (HTTP ${response.status})`);
    }
    const data = await response.json();
    allSymptoms = data.symptoms || [];
    filteredSymptoms = [...allSymptoms];
    renderSymptoms(filteredSymptoms);
    showStatus("Symptoms loaded. Start selecting what you notice.", "success");
  } catch (error) {
    showStatus(error.message || "Unable to fetch symptoms.", "error", { autoClear: false });
  }
}

function renderDiagnoses(diagnoses) {
  if (!diagnoses.length) {
    return `<p class="muted">No known causes match the supplied symptoms.</p>`;
  }

  const items = diagnoses.map((entry) => {
    const confidencePercent = (entry.confidence * 100).toFixed(1);
    const label = sanitizeText(entry.label ?? entry.cause ?? "Unknown cause");
    const solution = sanitizeText(entry.solution ?? "No specific solution recorded.");
    return `
      <div class="diagnosis-card">
        <h3>${label}</h3>
        <p class="confidence">Confidence: ${confidencePercent}%</p>
        <p class="solution">${solution}</p>
      </div>
    `;
  });

  return `<div class="diagnosis-grid">${items.join("")}</div>`;
}

function renderSummary(summary) {
  const resolveLabel = (item) => {
    if (!item) return "";
    if (typeof item === "string") return item;
    if (typeof item === "object") {
      if (item.label) return item.label;
      if (item.id) return item.id;
    }
    return String(item);
  };

  const toList = (items) =>
    items.length
      ? `<ul>${items.map((item) => `<li>${sanitizeText(resolveLabel(item))}</li>`).join("")}</ul>`
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
    showStatus("Select at least one symptom before running a diagnosis.", "error", { autoClear: false });
    if (resultsEl) {
      resultsEl.innerHTML = `<p class="muted">Pick one or more symptoms, then run the diagnosis.</p>`;
    }
    return;
  }

  showStatus("Analyzing your selections…", "loading", { autoClear: false });
  setRunButtonBusy(true);
  renderDiagnosisSkeleton();
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
    window.requestAnimationFrame(() => {
      resultsEl.scrollIntoView({ behavior: "smooth", block: "start" });
    });
  } catch (error) {
    const message = error.message || "Unexpected error while diagnosing.";
    showStatus(message, "error", { autoClear: false });
    renderErrorState(message);
  } finally {
    setRunButtonBusy(false);
  }
}

function clearSelections() {
  const selects = symptomListEl.querySelectorAll("select");
  selects.forEach((select) => {
    select.value = "skip";
  });
  if (resultsEl) {
    resultsEl.innerHTML = `<p class="muted">Selections cleared. Pick new symptoms and run again.</p>`;
  }
  showStatus("Selections cleared. Ready when you are.", "info");
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
