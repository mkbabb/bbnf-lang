"use strict";
let diagnosticsEnabled = false;
function enableDiagnostics() {
  diagnosticsEnabled = true;
}
function disableDiagnostics() {
  diagnosticsEnabled = false;
}
function isDiagnosticsEnabled() {
  return diagnosticsEnabled;
}
function mergeErrorState(state, label) {
  if (state.offset > state.furthest) {
    state.furthest = state.offset;
    state.expected = diagnosticsEnabled && label ? [label] : void 0;
    state.suggestions = [];
    state.secondarySpans = [];
  } else if (state.offset === state.furthest) {
    if (diagnosticsEnabled && label) {
      if (state.expected) {
        if (!state.expected.includes(label)) {
          state.expected.push(label);
        }
      } else {
        state.expected = [label];
      }
    }
  }
  return state;
}
function addSuggestion(state, suggestion) {
  if (diagnosticsEnabled) {
    state.suggestions.push(suggestion);
  }
}
function addSecondarySpan(state, offset, label) {
  if (diagnosticsEnabled) {
    state.secondarySpans.push({ offset, label });
  }
}
function reportUnclosedDelimiter(state, openText, openOffset) {
  if (!diagnosticsEnabled) return;
  const closeText = openText === "{" ? "}" : openText === "[" ? "]" : openText === "(" ? ")" : openText;
  addSuggestion(state, {
    kind: "unclosed-delimiter",
    message: `close the delimiter with \`${closeText}\``,
    openOffset
  });
  addSecondarySpan(state, openOffset, `unclosed \`${openText}\` opened here`);
}
let collectedDiagnostics = [];
function collectDiagnostic(state, errorOffset) {
  const src = state.src;
  const furthest = state.furthest >= 0 ? state.furthest : errorOffset;
  const before = src.slice(0, furthest);
  const lastNl = before.lastIndexOf("\n");
  const line = lastNl === -1 ? 1 : before.slice(0, lastNl + 1).split("\n").length;
  const column = lastNl === -1 ? furthest : furthest - lastNl - 1;
  const found = src.slice(furthest, furthest + 20).replace(/\n/g, "\\n");
  collectedDiagnostics.push({
    offset: errorOffset,
    furthestOffset: furthest,
    line,
    column,
    expected: state.expected ? [...state.expected] : [],
    suggestions: [...state.suggestions],
    secondarySpans: [...state.secondarySpans],
    found
  });
  resetErrorState(state);
}
function resetErrorState(state) {
  state.furthest = -1;
  state.expected = void 0;
  state.suggestions = [];
  state.secondarySpans = [];
}
function getCollectedDiagnostics() {
  return collectedDiagnostics;
}
function clearCollectedDiagnostics() {
  collectedDiagnostics = [];
}
function popLastDiagnostic() {
  return collectedDiagnostics.pop();
}
function skipWhitespace(state) {
  const src = state.src;
  let i = state.offset;
  while (i < src.length && src.charCodeAt(i) <= 32) i++;
  state.offset = i;
}
function skipBlockComments(state) {
  const src = state.src;
  let i = state.offset;
  while (i < src.length) {
    const ch = src.charCodeAt(i);
    if (ch <= 32) {
      i++;
      continue;
    }
    if (ch === 47 && src.charCodeAt(i + 1) === 42) {
      const end = src.indexOf("*/", i + 2);
      if (end === -1) break;
      i = end + 2;
      continue;
    }
    break;
  }
  state.offset = i;
}
exports.addSuggestion = addSuggestion;
exports.clearCollectedDiagnostics = clearCollectedDiagnostics;
exports.collectDiagnostic = collectDiagnostic;
exports.disableDiagnostics = disableDiagnostics;
exports.enableDiagnostics = enableDiagnostics;
exports.getCollectedDiagnostics = getCollectedDiagnostics;
exports.isDiagnosticsEnabled = isDiagnosticsEnabled;
exports.mergeErrorState = mergeErrorState;
exports.popLastDiagnostic = popLastDiagnostic;
exports.reportUnclosedDelimiter = reportUnclosedDelimiter;
exports.skipBlockComments = skipBlockComments;
exports.skipWhitespace = skipWhitespace;
//# sourceMappingURL=diagnostics-DpUY87_d.cjs.map
