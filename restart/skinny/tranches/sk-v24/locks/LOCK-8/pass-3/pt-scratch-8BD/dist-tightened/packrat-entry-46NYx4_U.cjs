"use strict";
const diagnostics = require("./diagnostics-DpUY87_d.cjs");
const LAZY_PARSER_CACHE = /* @__PURE__ */ new WeakMap();
function getLazyParser(fn) {
  const cached = LAZY_PARSER_CACHE.get(fn);
  if (cached !== void 0) {
    return cached;
  }
  const parser = fn();
  LAZY_PARSER_CACHE.set(fn, parser);
  return parser;
}
function createLazyCached(fn) {
  let cached;
  return (state) => {
    if (!cached) cached = fn();
    return cached.parser(state);
  };
}
function lazy(target, _propertyName, descriptor) {
  const method = descriptor.value.bind(target);
  descriptor.value = function() {
    return new Parser(
      createLazyCached(method),
      createParserContext("lazy", void 0, method)
    );
  };
}
const enabled = typeof process !== "undefined" && process.stderr?.isTTY === true && !process.env.NO_COLOR;
const bold = (s) => enabled ? `\x1B[1m${s}\x1B[22m` : s;
const italic = (s) => enabled ? `\x1B[3m${s}\x1B[23m` : s;
const red = (s) => enabled ? `\x1B[31m${s}\x1B[39m` : s;
const green = (s) => enabled ? `\x1B[32m${s}\x1B[39m` : s;
const yellow = (s) => enabled ? `\x1B[33m${s}\x1B[39m` : s;
const cyan = (s) => enabled ? `\x1B[36m${s}\x1B[39m` : s;
const gray = (s) => enabled ? `\x1B[90m${s}\x1B[39m` : s;
const bgRed = (s) => enabled ? `\x1B[41m${s}\x1B[49m` : s;
const bgGreen = (s) => enabled ? `\x1B[42m${s}\x1B[49m` : s;
const MAX_LINES = 4;
const MAX_LINE_WIDTH = 74;
let debugDepth = 0;
function summarizeLine(line, columnNum = 0) {
  const trimmed = line.trimEnd();
  const len = trimmed.length;
  const half = Math.floor(MAX_LINE_WIDTH / 2);
  if (len <= MAX_LINE_WIDTH) return trimmed;
  const mid = Math.min(columnNum, len);
  let start = Math.min(Math.max(mid - half, 0), len);
  let end = Math.min(mid + half, len);
  if (start === 0) {
    return trimmed.slice(0, end) + "...";
  } else if (end >= len) {
    return "..." + trimmed.slice(start);
  }
  return "..." + trimmed.slice(start, end) + "...";
}
function formatExpected(expected) {
  switch (expected.length) {
    case 0:
      return "";
    case 1:
      return `expected ${expected[0]}`;
    case 2:
      return `expected ${expected[0]} or ${expected[1]}`;
    default: {
      const last = expected[expected.length - 1];
      const rest = expected.slice(0, -1).join(", ");
      return `expected ${rest}, or ${last}`;
    }
  }
}
function lineNumberWidth(maxLine) {
  return String(maxLine).length;
}
function addCursor(state, cursor = "^", error = false) {
  const lines = state.src.split("\n");
  const { line: lineNum, column: columnNum } = state.getLineAndColumn();
  const lineIdx = lineNum - 1;
  const startIdx = Math.max(lineIdx - MAX_LINES, 0);
  const endIdx = Math.min(lineIdx + MAX_LINES + 1, lines.length);
  const lnWidth = lineNumberWidth(endIdx);
  const result = [];
  for (let i = startIdx; i < endIdx; i++) {
    const ln = i + 1;
    const isActive = i === lineIdx;
    const lineContent = summarizeLine(lines[i], isActive ? columnNum : 0);
    const pipe = gray("|");
    if (isActive) {
      const lnStr = bold(String(ln).padStart(lnWidth));
      const lineDisplay = error ? bold(red(lineContent)) : bold(green(lineContent));
      result.push(` ${lnStr} ${pipe} ${lineDisplay}`);
      if (cursor) {
        const pad = " ".repeat(lnWidth + 4 + columnNum);
        const cursorStr = error ? red(cursor) : green(cursor);
        result.push(`${pad}${cursorStr}`);
      }
    } else {
      const lnStr = gray(String(ln).padStart(lnWidth));
      result.push(` ${lnStr} ${pipe} ${lineContent}`);
    }
  }
  return result.join("\n");
}
function formatSecondarySpans(src, spans) {
  const lines = src.split("\n");
  const result = [];
  for (const span of spans) {
    let offsetAcc = 0;
    for (let i = 0; i < lines.length; i++) {
      const lineEnd = offsetAcc + lines[i].length + 1;
      if (span.offset < lineEnd) {
        const col = span.offset - offsetAcc;
        const lnWidth = Math.max(String(i + 1).length, 3);
        const pipe = gray("|");
        result.push(` ${" ".repeat(lnWidth)} ${pipe}`);
        result.push(
          ` ${gray(String(i + 1).padStart(lnWidth))} ${pipe} ${lines[i]}`
        );
        const markerPad = " ".repeat(lnWidth + 4 + col);
        result.push(`${markerPad}${cyan("-")} ${cyan(span.label)}`);
        break;
      }
      offsetAcc = lineEnd;
    }
  }
  return result.join("\n");
}
function formatSuggestions(suggestions) {
  const result = [];
  for (const s of suggestions) {
    const prefix = s.kind === "unclosed-delimiter" ? bold(yellow("help")) : bold(cyan("note"));
    result.push(`   = ${prefix}: ${s.message}`);
  }
  return result.join("\n");
}
function statePrint(state, name = "", parserString = "") {
  const finished = state.offset >= state.src.length;
  const isError = state.isError;
  let badge;
  if (isError) {
    badge = bgRed(bold(" Err x "));
  } else if (finished) {
    badge = bgGreen(bold(" Done √ "));
  } else {
    badge = bgGreen(bold(" Ok √ "));
  }
  const namePart = name ? `    ${yellow(italic(name))}` : "";
  const offsetPart = `    ${green(String(state.offset))}`;
  const parserPart = parserString ? `    ${green(parserString)}` : "";
  const header = `${badge}${namePart}${offsetPart}${parserPart}`;
  const cursor = isError ? "^^^" : finished ? "" : "^";
  const body = addCursor(state, cursor, isError);
  let output = `${header}
${body}`;
  if (isError && diagnostics.isDiagnosticsEnabled()) {
    const expected = state.expected ?? [];
    if (expected.length > 0) {
      output += `
   ${cyan(formatExpected(expected))}`;
    }
    const secondarySpans = state.secondarySpans;
    if (secondarySpans.length > 0) {
      output += `
${formatSecondarySpans(state.src, secondarySpans)}`;
    }
    const suggestions = state.suggestions;
    if (suggestions.length > 0) {
      output += `
${formatSuggestions(suggestions)}`;
    }
  }
  return output;
}
const PARSER_STRINGS = /* @__PURE__ */ new Map();
function parserPrint(parser) {
  if (PARSER_STRINGS.has(parser.id)) {
    return PARSER_STRINGS.get(parser.id);
  }
  const print = (innerParser, id) => {
    if (PARSER_STRINGS.has(innerParser.id)) {
      return PARSER_STRINGS.get(innerParser.id);
    }
    const { name, args, parser: innerInnerParser } = innerParser.context;
    const parserString = innerInnerParser != null ? print(innerInnerParser, id) : "unknown";
    const s2 = (() => {
      switch (name) {
        case "string":
          return `"${args[0]}"`;
        case "regex":
          return `${args[0]}`;
        case "wrap":
        case "trim": {
          const [left, right] = args;
          return `${print(left, id)} ${parserString} ${print(right, id)}`;
        }
        case "trimWhitespace":
          return `${parserString}?w`;
        case "not":
          return `!${parserString}`;
        case "opt":
          return `${parserString}?`;
        case "next": {
          const [next] = args;
          return `${parserString} >> ${print(next, id)}`;
        }
        case "skip": {
          const [skip] = args;
          return `${parserString} << ${print(skip, id)}`;
        }
        case "map":
          return parserString;
        case "all":
        case "then": {
          const items = args.map(
            (x) => print(x, id)
          );
          return `[${items.join(", ")}]`;
        }
        case "any":
        case "or": {
          const items = args.map(
            (x) => print(x, id)
          );
          return items.join(" | ");
        }
        case "many": {
          const [min, max] = args;
          const bounds = max === Infinity ? `${min},` : `${min},${max}`;
          return `${parserString} {${bounds}}`;
        }
        case "sepBy":
          return `${parserString} sepBy ${print(args[0], id)}`;
        case "lazy": {
          const [lazy2] = args;
          const p = getLazyParser(lazy2);
          if (!id) {
            const s3 = print(p, p.id);
            PARSER_STRINGS.set(p.id, s3);
            return s3;
          } else {
            return name;
          }
        }
        case "debug":
          return parserString;
        default:
          return void 0;
      }
    })();
    const result = s2 ?? name;
    if (!result) {
      throw new Error("parserPrint: missing parser context name");
    }
    if (id) {
      PARSER_STRINGS.set(innerParser.id, result);
    }
    return result;
  };
  const s = print(parser);
  PARSER_STRINGS.set(parser.id, s);
  return s;
}
function parserDebug(parser, name = "", recursivePrint = false, logger = console.error) {
  const debug = (state) => {
    debugDepth++;
    const indentStr = "  ".repeat(debugDepth - 1);
    const newState = parser.parser(state);
    const parserString = recursivePrint ? parserPrint(parser) : parser.context.name ?? "";
    const s = statePrint(
      newState,
      name,
      parserString
    );
    const indented = s.split("\n").map((line) => indentStr + line).join("\n");
    logger(indented);
    debugDepth--;
    return newState;
  };
  return new Parser(debug, createParserContext("debug", parser, logger));
}
function spanToString(span, src) {
  return src.substring(span.start, span.end);
}
function mergeSpans(a, b) {
  return { start: a.start, end: b.end };
}
class ParserState {
  constructor(src, value = void 0, offset = 0, isError = false, furthest = -1) {
    this.src = src;
    this.value = value;
    this.offset = offset;
    this.isError = isError;
    this.furthest = furthest;
  }
  /**
   * Furthest-offset error tracking, threaded per-parse (the Rust port's
   * `state.furthest_offset` model). `expected` is the accumulated label set
   * at `furthest`; `suggestions`/`secondarySpans` are the diagnostic extras
   * collected at `furthest` when diagnostics are enabled.
   */
  expected;
  suggestions = [];
  secondarySpans = [];
  ok(value, offset = 0) {
    this.offset += offset;
    this.unsafeSetValue(value);
    this.isError = false;
    return this;
  }
  err(value, offset = 0) {
    this.offset += offset;
    this.unsafeSetValue(value);
    this.isError = true;
    return this;
  }
  from(value, offset = 0) {
    this.offset += offset;
    this.unsafeSetValue(value);
    return this;
  }
  save() {
    return { offset: this.offset, value: this.value };
  }
  restore(saved) {
    this.offset = saved.offset;
    this.value = saved.value;
    this.isError = false;
    return this;
  }
  /** Type-erased value setter — single choke point for the mutable-state cast pattern. */
  unsafeSetValue(value) {
    this.value = value;
  }
  /** Type-erased parser invocation via .call() — single choke point for combinator type casts. */
  unsafeCall(parser) {
    parser.call(this);
  }
  /** Type-erased raw parser invocation via .parser() — for internal combinator plumbing. */
  unsafeCallRaw(parser) {
    parser.parser(this);
  }
  clone() {
    return new ParserState(
      this.src,
      this.value,
      this.offset,
      this.isError,
      this.furthest
    );
  }
  getColumnNumber() {
    const offset = this.offset;
    const lastNewline = this.src.lastIndexOf("\n", offset);
    const columnNumber = lastNewline === -1 ? offset : offset - (lastNewline + 1);
    return Math.max(0, columnNumber);
  }
  getLineNumber() {
    const newlineIndex = this.src.lastIndexOf("\n", this.offset);
    return newlineIndex >= 0 ? this.src.slice(0, newlineIndex).split("\n").length : 0;
  }
  /** Returns 1-based line and 0-based column for any offset. */
  getLineAndColumn(offset = this.offset) {
    const lastNewline = this.src.lastIndexOf("\n", offset - 1);
    const line = lastNewline === -1 ? 1 : this.src.slice(0, lastNewline + 1).split("\n").length;
    const column = lastNewline === -1 ? offset : offset - lastNewline - 1;
    return { line, column };
  }
  toString() {
    return statePrint(this);
  }
}
function createParserContext(name, parser, ...args) {
  return {
    name,
    parser,
    args
  };
}
function makeParser(parser, context) {
  return new Parser(parser, context);
}
function eof() {
  const eof2 = (state) => {
    if (state.offset >= state.src.length) {
      return state.ok(void 0);
    } else {
      diagnostics.mergeErrorState(state, "<end of input>");
      state.isError = true;
      return state;
    }
  };
  return makeParser(
    eof2,
    createParserContext("eof", void 0)
  );
}
function any(...parsers) {
  const n = parsers.length;
  let anyParser;
  if (n === 2) {
    const p0 = parsers[0];
    const p1 = parsers[1];
    anyParser = ((state) => {
      const savedOffset = state.offset;
      p0.parser(state);
      if (!state.isError) return state;
      state.offset = savedOffset;
      state.isError = false;
      p1.parser(state);
      if (!state.isError) return state;
      state.offset = savedOffset;
      state.isError = false;
      diagnostics.mergeErrorState(state);
      state.isError = true;
      return state;
    });
  } else {
    anyParser = ((state) => {
      const savedOffset = state.offset;
      for (let i = 0; i < n; i++) {
        parsers[i].parser(state);
        if (!state.isError) {
          return state;
        }
        state.offset = savedOffset;
        state.isError = false;
      }
      diagnostics.mergeErrorState(state);
      state.isError = true;
      return state;
    });
  }
  return makeParser(
    n === 1 ? parsers[0].parser : anyParser,
    createParserContext("any", void 0, ...parsers)
  );
}
function dispatch(table) {
  const tbl = new Int8Array(128).fill(-1);
  const parsers = [];
  const internParser = (parser) => {
    let idx = parsers.indexOf(parser);
    if (idx === -1) {
      idx = parsers.length;
      parsers.push(parser);
    }
    return idx;
  };
  for (const [chars, parser] of Object.entries(table)) {
    const idx = internParser(parser);
    if (chars.length === 3 && chars[1] === "-") {
      const lo = chars.charCodeAt(0);
      const hi = chars.charCodeAt(2);
      for (let c = lo; c <= hi; c++) tbl[c] = idx;
    } else {
      for (let i = 0; i < chars.length; i++) {
        tbl[chars.charCodeAt(i)] = idx;
      }
    }
  }
  const labelChars = Object.keys(table).map((k) => {
    if (k.length === 3 && k[1] === "-") return `'${k[0]}'-'${k[2]}'`;
    return [...k].map((c) => `'${c}'`).join(", ");
  }).join(", ");
  const label = `one of [${labelChars}]`;
  const dispatchParser = (state) => {
    const off = state.offset;
    const ch = state.src.charCodeAt(off);
    const idx = ch < 128 ? tbl[ch] : -1;
    if (idx >= 0) {
      return parsers[idx].parser(state);
    }
    diagnostics.mergeErrorState(state, label);
    state.isError = true;
    return state;
  };
  return makeParser(
    dispatchParser,
    createParserContext("dispatch", void 0, ...parsers)
  );
}
function all(...parsers) {
  return makeParser(
    parsers.length === 1 ? parsers[0].parser : fuseAll(parsers),
    createParserContext("all", void 0, ...parsers)
  );
}
function fuseAll(parsers) {
  const n = parsers.length;
  if (n === 2) {
    const p0 = parsers[0];
    const p1 = parsers[1];
    return ((state) => {
      const savedOffset = state.offset;
      let w = 0;
      const out = [void 0, void 0];
      p0.parser(state);
      if (state.isError) {
        state.offset = savedOffset;
        state.isError = true;
        return state;
      }
      if (state.value !== void 0) out[w++] = state.value;
      p1.parser(state);
      if (state.isError) {
        state.offset = savedOffset;
        state.isError = true;
        return state;
      }
      if (state.value !== void 0) out[w++] = state.value;
      if (w !== 2) out.length = w;
      return state.ok(out);
    });
  }
  if (n === 3) {
    const p0 = parsers[0];
    const p1 = parsers[1];
    const p2 = parsers[2];
    return ((state) => {
      const savedOffset = state.offset;
      let w = 0;
      const out = [void 0, void 0, void 0];
      p0.parser(state);
      if (state.isError) {
        state.offset = savedOffset;
        state.isError = true;
        return state;
      }
      if (state.value !== void 0) out[w++] = state.value;
      p1.parser(state);
      if (state.isError) {
        state.offset = savedOffset;
        state.isError = true;
        return state;
      }
      if (state.value !== void 0) out[w++] = state.value;
      p2.parser(state);
      if (state.isError) {
        state.offset = savedOffset;
        state.isError = true;
        return state;
      }
      if (state.value !== void 0) out[w++] = state.value;
      if (w !== 3) out.length = w;
      return state.ok(out);
    });
  }
  return ((state) => {
    const savedOffset = state.offset;
    const out = new Array(n);
    let w = 0;
    for (let i = 0; i < n; i++) {
      parsers[i].parser(state);
      if (state.isError) {
        state.offset = savedOffset;
        state.isError = true;
        return state;
      }
      if (state.value !== void 0) out[w++] = state.value;
    }
    if (w !== n) out.length = w;
    return state.ok(out);
  });
}
function string(str) {
  const len = str.length;
  const label = `"${str}"`;
  let stringParser;
  if (len === 1) {
    const code = str.charCodeAt(0);
    stringParser = ((state) => {
      if (state.src.charCodeAt(state.offset) === code) {
        state.offset += 1;
        state.unsafeSetValue(str);
        state.isError = false;
        return state;
      }
      diagnostics.mergeErrorState(state, label);
      state.isError = true;
      return state;
    });
  } else {
    stringParser = ((state) => {
      if (state.src.startsWith(str, state.offset)) {
        state.offset += len;
        state.unsafeSetValue(str);
        state.isError = false;
        return state;
      }
      diagnostics.mergeErrorState(state, label);
      state.isError = true;
      return state;
    });
  }
  return makeParser(
    stringParser,
    createParserContext("string", void 0, str)
  );
}
function regex(r, matchFunction) {
  const flags = r.flags.replace(/y/g, "");
  const sticky = new RegExp(r, flags + "y");
  const hasCustomMatch = matchFunction != null;
  const label = `/${r.source}/${r.flags}`;
  const regexParser = (state) => {
    if (state.offset >= state.src.length) {
      state.isError = true;
      return state;
    }
    const savedOffset = state.offset;
    sticky.lastIndex = savedOffset;
    if (hasCustomMatch) {
      const execResult = sticky.exec(state.src);
      const match = matchFunction(execResult);
      if (match) {
        return state.ok(match, sticky.lastIndex - savedOffset);
      } else if (match === "") {
        return state.ok(void 0);
      }
    } else if (sticky.test(state.src)) {
      const end = sticky.lastIndex;
      if (end > savedOffset) {
        state.offset = end;
        state.unsafeSetValue(state.src.substring(savedOffset, end));
        state.isError = false;
        return state;
      }
      state.unsafeSetValue(void 0);
      state.isError = false;
      return state;
    }
    diagnostics.mergeErrorState(state, label);
    state.isError = true;
    return state;
  };
  return makeParser(
    regexParser,
    createParserContext("regex", void 0, r)
  );
}
const trimStateWhitespace = (state) => {
  const src = state.src;
  const len = src.length;
  let offset = state.offset;
  if (offset >= len || src.charCodeAt(offset) > 32) return state;
  while (offset < len) {
    const c = src.charCodeAt(offset);
    if (c === 32 || c >= 9 && c <= 13) {
      offset++;
    } else {
      break;
    }
  }
  state.offset = offset;
  return state;
};
exports.whitespace = void 0;
function _initWhitespace() {
  exports.whitespace = regex(/\s*/);
  exports.whitespace.context.name = "whitespace";
}
const MEMO_OFFSET_BITS = 32;
const MEMO_OFFSET_SPAN = 2 ** MEMO_OFFSET_BITS;
const MEMO_MAX_ID = Math.floor(Number.MAX_SAFE_INTEGER / MEMO_OFFSET_SPAN);
function getCijKey(parser, offset) {
  if (parser.id > MEMO_MAX_ID || offset >= MEMO_OFFSET_SPAN) {
    throw new RangeError(
      `packrat memo key out of float64-safe budget: parser.id=${parser.id} (max ${MEMO_MAX_ID}), offset=${offset} (max ${MEMO_OFFSET_SPAN - 1})`
    );
  }
  return parser.id * MEMO_OFFSET_SPAN + offset;
}
function isLR(x) {
  return x.parser !== void 0;
}
let MEMO = /* @__PURE__ */ new Map();
let HEADS = /* @__PURE__ */ new Map();
let LR_STACK;
let PACKRAT_ARMED = false;
let CURRENT_SRC;
let GROWING = /* @__PURE__ */ new Map();
function packratEnter() {
  if (!PACKRAT_ARMED) return null;
  const saved = {
    memo: MEMO,
    heads: HEADS,
    growing: GROWING,
    lrStack: LR_STACK,
    currentSrc: CURRENT_SRC
  };
  MEMO = /* @__PURE__ */ new Map();
  HEADS = /* @__PURE__ */ new Map();
  GROWING = /* @__PURE__ */ new Map();
  LR_STACK = void 0;
  CURRENT_SRC = void 0;
  return saved;
}
function packratExit(saved) {
  if (saved === null) return;
  MEMO = saved.memo;
  HEADS = saved.heads;
  GROWING = saved.growing;
  LR_STACK = saved.lrStack;
  CURRENT_SRC = saved.currentSrc;
}
function snapshot(state) {
  return { offset: state.offset, value: state.value, isError: state.isError };
}
function applyAnswer(state, ans) {
  state.offset = ans.offset;
  state.value = ans.value;
  state.isError = ans.isError;
}
function resetPackrat() {
  if (!PACKRAT_ARMED) return;
  MEMO.clear();
  HEADS.clear();
  GROWING.clear();
  LR_STACK = void 0;
  CURRENT_SRC = void 0;
}
function makeMemoized(parser, name) {
  PACKRAT_ARMED = true;
  const p = parser;
  const evalParser = (state, pos) => {
    state.offset = pos;
    state.isError = false;
    state.value = void 0;
    parser.parser(state);
  };
  const recall = (pos, live) => {
    const key = getCijKey(p, pos);
    const cell = MEMO.get(key);
    const head = HEADS.get(pos);
    if (cell === void 0) {
      const growKey = GROWING.get(p.id);
      if (growKey !== void 0) {
        const growSeed = MEMO.get(growKey)?.ans;
        if (growSeed !== void 0 && !isLR(growSeed) && pos > growKey % MEMO_OFFSET_SPAN && pos <= growSeed.offset) {
          return { ans: { offset: pos, value: void 0, isError: false } };
        }
      }
    }
    if (head === void 0) return cell;
    if (cell === void 0 && head.parser.id !== p.id && !head.involvedSet.has(p.id)) {
      return { ans: { offset: pos, value: void 0, isError: true } };
    }
    if (head.evalSet.has(p.id)) {
      head.evalSet.delete(p.id);
      const scratch = live.clone();
      scratch.offset = pos;
      scratch.isError = false;
      scratch.value = void 0;
      parser.parser(scratch);
      const ans = snapshot(scratch);
      if (cell !== void 0) cell.ans = ans;
      else MEMO.set(key, { ans });
      return MEMO.get(key);
    }
    return cell;
  };
  const setupLR = (lr) => {
    if (lr.head === void 0) {
      lr.head = { parser: p, involvedSet: /* @__PURE__ */ new Set(), evalSet: /* @__PURE__ */ new Set() };
    }
    const head = lr.head;
    let s = LR_STACK;
    while (s !== void 0 && s.head !== head) {
      s.head = head;
      head.involvedSet.add(s.parser.id);
      s = s.next;
    }
  };
  const growLR = (state, pos, key, head) => {
    HEADS.set(pos, head);
    const prevGrowing = GROWING.get(p.id);
    GROWING.set(p.id, key);
    try {
      while (true) {
        head.evalSet = new Set(head.involvedSet);
        evalParser(state, pos);
        const ans = snapshot(state);
        const seed = MEMO.get(key).ans;
        if (ans.isError || ans.offset <= seed.offset) break;
        MEMO.set(key, { ans });
      }
      applyAnswer(state, MEMO.get(key).ans);
    } finally {
      if (prevGrowing !== void 0) GROWING.set(p.id, prevGrowing);
      else GROWING.delete(p.id);
      HEADS.delete(pos);
    }
  };
  const lrAnswer = (state, pos, key, lr) => {
    const head = lr.head;
    if (head.parser.id !== p.id) {
      applyAnswer(state, lr.seed);
      return;
    }
    MEMO.set(key, { ans: lr.seed });
    if (lr.seed.isError) {
      applyAnswer(state, lr.seed);
      return;
    }
    growLR(state, pos, key, head);
  };
  const memoizeFn = (state) => {
    if (CURRENT_SRC === void 0) {
      CURRENT_SRC = state.src;
    }
    const pos = state.offset;
    const key = getCijKey(p, pos);
    const m = recall(pos, state);
    if (m === void 0) {
      const lr = {
        seed: { offset: pos, value: void 0, isError: true },
        parser: p,
        head: void 0,
        next: LR_STACK
      };
      LR_STACK = lr;
      MEMO.set(key, { ans: lr });
      evalParser(state, pos);
      LR_STACK = lr.next;
      const cell = MEMO.get(key);
      if (lr.head !== void 0) {
        lr.seed = snapshot(state);
        lrAnswer(state, pos, key, lr);
      } else {
        cell.ans = snapshot(state);
        applyAnswer(state, cell.ans);
      }
      return state;
    }
    if (isLR(m.ans)) {
      setupLR(m.ans);
      applyAnswer(state, m.ans.seed);
      return state;
    }
    applyAnswer(state, m.ans);
    return state;
  };
  return new Parser(memoizeFn, createParserContext(name, p));
}
function memoize(parser) {
  return makeMemoized(parser, "memoize");
}
function mergeMemos(parser) {
  return makeMemoized(parser, "mergeMemo");
}
let PARSER_ID = 0;
const FLAG_NONE = 0;
const FLAG_TRIM_WS = 1;
const FLAG_EOF = 2;
class Parser {
  constructor(parser, context = {}) {
    this.parser = parser;
    this.context = context;
  }
  id = PARSER_ID++;
  state;
  flags = FLAG_NONE;
  parseState(val) {
    const epoch = packratEnter();
    try {
      return this.parseStateInner(val);
    } finally {
      packratExit(epoch);
    }
  }
  parseStateInner(val) {
    const state = new ParserState(val);
    this.parser(state);
    if (state.isError) {
      const furthest = state.furthest >= 0 ? state.furthest : state.offset;
      const errorState = new ParserState(val, void 0, furthest, true);
      errorState.expected = state.expected;
      errorState.suggestions = state.suggestions;
      errorState.secondarySpans = state.secondarySpans;
      errorState.furthest = furthest;
      this.state = errorState;
      if (diagnostics.isDiagnosticsEnabled()) {
        console.error(this.state.toString());
      }
    } else {
      this.state = state;
    }
    return state;
  }
  parse(val) {
    return this.parseState(val).value;
  }
  then(next) {
    const then = (state) => {
      const savedOffset = state.offset;
      this.parser(state);
      if (!state.isError) {
        const value1 = state.value;
        next.parser(state);
        if (!state.isError) {
          return state.ok([value1, state.value]);
        }
      }
      diagnostics.mergeErrorState(state);
      state.offset = savedOffset;
      state.isError = true;
      return state;
    };
    return new Parser(
      then,
      createParserContext("then", this, this, next)
    );
  }
  or(other) {
    const or = (state) => {
      const savedOffset = state.offset;
      this.parser(state);
      if (!state.isError) {
        return state;
      }
      state.offset = savedOffset;
      state.isError = false;
      return other.parser(state);
    };
    return new Parser(
      or,
      createParserContext("or", this, this, other)
    );
  }
  chain(fn) {
    const chain = (state) => {
      this.parser(state);
      if (state.isError) {
        return state;
      }
      return fn(state.value).parser(state);
    };
    return new Parser(
      chain,
      createParserContext("chain", this, fn)
    );
  }
  map(fn, mapError = false) {
    const map = (state) => {
      this.parser(state);
      if (!state.isError || mapError) {
        return state.ok(fn(state.value));
      }
      return state;
    };
    return new Parser(
      map,
      createParserContext("map", this)
    );
  }
  mapState(fn) {
    const mapState = (state) => {
      const oldOffset = state.offset;
      const oldValue = state.value;
      this.parser(state);
      if (state.isError) {
        return state;
      }
      const oldView = Object.create(state);
      oldView.offset = oldOffset;
      oldView.value = oldValue;
      return fn(state, oldView);
    };
    return new Parser(
      mapState,
      createParserContext("mapState", this)
    );
  }
  skip(parser) {
    const skip = (state) => {
      const savedOffset = state.offset;
      this.parser(state);
      if (!state.isError) {
        const value1 = state.value;
        parser.parser(state);
        if (!state.isError) {
          return state.ok(value1);
        }
      }
      diagnostics.mergeErrorState(state);
      state.offset = savedOffset;
      state.isError = true;
      return state;
    };
    return new Parser(
      skip,
      createParserContext("skip", this, parser)
    );
  }
  next(parser) {
    const next = (state) => {
      const savedOffset = state.offset;
      this.parser(state);
      if (!state.isError) {
        state.unsafeCallRaw(parser);
        if (!state.isError) {
          return state;
        }
      }
      diagnostics.mergeErrorState(state);
      state.offset = savedOffset;
      state.isError = true;
      return state;
    };
    return new Parser(
      next,
      createParserContext("next", this, parser)
    );
  }
  opt() {
    const opt = (state) => {
      const savedOffset = state.offset;
      this.parser(state);
      if (state.isError) {
        diagnostics.mergeErrorState(state);
        state.offset = savedOffset;
        return state.ok(void 0);
      }
      return state;
    };
    return new Parser(
      opt,
      createParserContext("opt", this)
    );
  }
  not(parser) {
    const negate = (state) => {
      const savedOffset = state.offset;
      const savedValue = state.value;
      this.parser(state);
      if (state.isError) {
        diagnostics.mergeErrorState(state);
        state.offset = savedOffset;
        return state.ok(savedValue);
      } else {
        state.offset = savedOffset;
        state.isError = true;
        return state;
      }
    };
    const not = (state) => {
      const savedOffset = state.offset;
      this.parser(state);
      if (state.isError) {
        diagnostics.mergeErrorState(state);
        state.offset = savedOffset;
        state.isError = true;
        return state;
      } else {
        const value1 = state.value;
        const offset1 = state.offset;
        parser.parser(state);
        if (state.isError) {
          state.offset = offset1;
          state.unsafeSetValue(value1);
          state.isError = false;
          return state;
        } else {
          diagnostics.mergeErrorState(state);
          state.offset = savedOffset;
          state.isError = true;
          return state;
        }
      }
    };
    return new Parser(
      parser ? not : negate,
      createParserContext("not", this, parser)
    );
  }
  /**
   * Set difference: match `this` only if `excluded` would NOT match at the
   * same starting position. Used for EBNF/BNF exception (`-`) semantics.
   */
  minus(excluded) {
    const inner = this;
    const minus = (state) => {
      const savedOffset = state.offset;
      state.unsafeCallRaw(excluded);
      if (!state.isError) {
        state.offset = savedOffset;
        state.isError = true;
        return state;
      }
      state.offset = savedOffset;
      state.isError = false;
      inner.parser(state);
      return state;
    };
    return new Parser(
      minus,
      createParserContext("minus", this, excluded)
    );
  }
  /**
   * Zero-width positive assertion: succeeds with `this`'s value when
   * `this` matches, but does NOT consume any input. The dual of
   * `not()` (no argument): where `not()` is zero-width negative
   * assertion, `peek()` is zero-width positive assertion.
   */
  peek() {
    const inner = this;
    const peek = (state) => {
      const savedOffset = state.offset;
      inner.parser(state);
      if (state.isError) {
        state.offset = savedOffset;
        return state;
      }
      const value = state.value;
      state.offset = savedOffset;
      state.value = value;
      return state;
    };
    return new Parser(
      peek,
      createParserContext("peek", this)
    );
  }
  /**
   * Consuming positive lookahead: parse `this`, then check that
   * `lookahead` matches at the resulting position without consuming it.
   * Returns `this`'s value; the lookahead is zero-width.
   * Mirrors Rust's `look_ahead()`.
   */
  lookAhead(lookahead) {
    const inner = this;
    const la = (state) => {
      const savedOffset = state.offset;
      inner.parser(state);
      if (state.isError) {
        state.offset = savedOffset;
        return state;
      }
      const value = state.value;
      const offsetAfterSelf = state.offset;
      state.unsafeCallRaw(lookahead);
      if (state.isError) {
        state.offset = savedOffset;
        state.isError = true;
        return state;
      }
      state.offset = offsetAfterSelf;
      state.unsafeSetValue(value);
      return state;
    };
    return new Parser(
      la,
      createParserContext("lookAhead", this, lookahead)
    );
  }
  wrap(start, end, discard = true) {
    if (!discard) {
      return all(start, this, end);
    }
    const inner = this;
    const wrapParser = (state) => {
      const savedOffset = state.offset;
      state.unsafeCallRaw(start);
      if (state.isError) {
        state.offset = savedOffset;
        return state;
      }
      const openEnd = state.offset;
      inner.parser(state);
      if (state.isError) {
        diagnostics.mergeErrorState(state);
        state.offset = savedOffset;
        state.isError = true;
        return state;
      }
      const value = state.value;
      state.unsafeCallRaw(end);
      if (state.isError) {
        diagnostics.mergeErrorState(state);
        diagnostics.reportUnclosedDelimiter(state, state.src.slice(savedOffset, openEnd), savedOffset);
        state.offset = savedOffset;
        state.isError = true;
        return state;
      }
      state.unsafeSetValue(value);
      return state;
    };
    return new Parser(
      wrapParser,
      createParserContext("wrap", this, start, end)
    );
  }
  /**
   * Call the parser with flag-based pre/post processing.
   * Fast path: flags === 0 just calls parser directly.
   */
  call(state) {
    if (this.flags === 0) {
      return this.parser(state);
    }
    if (this.flags === FLAG_TRIM_WS) {
      trimStateWhitespace(state);
      const savedOffset2 = state.offset;
      this.parser(state);
      if (state.isError) {
        diagnostics.mergeErrorState(state);
        state.offset = savedOffset2;
        state.isError = true;
        return state;
      }
      trimStateWhitespace(state);
      return state;
    }
    if (this.flags & FLAG_TRIM_WS) trimStateWhitespace(state);
    const savedOffset = state.offset;
    this.parser(state);
    if (state.isError) {
      diagnostics.mergeErrorState(state);
      state.offset = savedOffset;
      state.isError = true;
      return state;
    }
    if (this.flags & FLAG_TRIM_WS) trimStateWhitespace(state);
    if (this.flags & FLAG_EOF) {
      if (state.offset < state.src.length) {
        diagnostics.mergeErrorState(state, "<end of input>");
        diagnostics.addSuggestion(state, {
          kind: "trailing-content",
          message: "unexpected trailing content after parsed value"
        });
        state.offset = savedOffset;
        state.isError = true;
      }
    }
    return state;
  }
  trim(parser = exports.whitespace, discard = true) {
    if (!discard) {
      return all(parser, this, parser);
    }
    if (parser.context?.name === "whitespace") {
      const inner = this;
      const flaggedParser = new Parser(
        ((state) => inner.call(state)),
        createParserContext("trimWhitespace", this)
      );
      flaggedParser.flags = this.flags | FLAG_TRIM_WS;
      const whitespaceTrim = (state) => {
        trimStateWhitespace(state);
        const savedOffset = state.offset;
        inner.parser(state);
        if (state.isError) {
          diagnostics.mergeErrorState(state);
          state.offset = savedOffset;
          state.isError = true;
          return state;
        } else {
          trimStateWhitespace(state);
          return state;
        }
      };
      return new Parser(
        whitespaceTrim,
        createParserContext("trimWhitespace", this)
      );
    }
    return this.wrap(parser, parser);
  }
  many(min = 0, max = Infinity) {
    const many = (state) => {
      const est = min > 0 ? min : 0;
      const matches = est > 0 ? new Array(est) : [];
      let len = 0;
      for (let i = 0; i < max; i += 1) {
        const savedOffset = state.offset;
        this.parser(state);
        if (state.isError) {
          state.offset = savedOffset;
          state.isError = false;
          break;
        }
        if (state.offset === savedOffset) break;
        if (len < est) {
          matches[len] = state.value;
        } else {
          matches.push(state.value);
        }
        len++;
      }
      if (len < est) matches.length = len;
      if (len >= min) {
        return state.ok(matches);
      }
      diagnostics.mergeErrorState(state);
      state.isError = true;
      state.unsafeSetValue([]);
      return state;
    };
    return new Parser(
      many,
      createParserContext("many", this, min, max)
    );
  }
  /**
   * Strictly interleaving: `elem (sep elem)*`. Never accepts a trailing
   * separator — trailing sep acceptance is a grammar concern.
   */
  sepBy(sep, min = 0, max = Infinity) {
    const sepBy = (state) => {
      const est = min > 0 ? min : 0;
      const matches = est > 0 ? new Array(est) : [];
      let len = 0;
      {
        const savedOffset = state.offset;
        this.parser(state);
        if (state.isError) {
          state.offset = savedOffset;
          state.isError = false;
        } else if (state.offset !== savedOffset) {
          if (len < est) {
            matches[len] = state.value;
          } else {
            matches.push(state.value);
          }
          len++;
        }
      }
      while (len > 0 && len < max) {
        const cpBeforeSep = state.offset;
        sep.parser(state);
        if (state.isError) {
          state.offset = cpBeforeSep;
          state.isError = false;
          break;
        }
        const savedOffset = state.offset;
        this.parser(state);
        if (state.isError || state.offset === savedOffset) {
          state.offset = cpBeforeSep;
          state.isError = false;
          break;
        }
        if (len < est) {
          matches[len] = state.value;
        } else {
          matches.push(state.value);
        }
        len++;
      }
      if (len < est) matches.length = len;
      if (len >= min) {
        return state.ok(matches);
      }
      diagnostics.mergeErrorState(state);
      state.isError = true;
      state.unsafeSetValue([]);
      return state;
    };
    return new Parser(
      sepBy,
      createParserContext("sepBy", this, sep)
    );
  }
  eof() {
    const p = this.skip(eof());
    p.context = createParserContext("eof", this);
    return p;
  }
  /**
   * Error recovery combinator. On success, returns the result normally.
   * On failure, snapshots the current diagnostic into the collected
   * diagnostics list, then runs `sync` to skip past the bad content
   * and returns `sentinel`.
   *
   * This enables `many()` / `sepBy()` loops to keep going — each failed
   * element produces a diagnostic but doesn't halt the overall parse.
   */
  recover(sync, sentinel) {
    const inner = this;
    const recover = (state) => {
      const checkpoint = state.offset;
      inner.parser(state);
      if (!state.isError) {
        return state;
      }
      diagnostics.collectDiagnostic(state, checkpoint);
      state.isError = false;
      state.offset = checkpoint;
      sync.parser(state);
      if (state.isError) {
        diagnostics.popLastDiagnostic();
        state.offset = checkpoint;
        state.isError = true;
        return state;
      }
      return state.ok(sentinel);
    };
    return new Parser(
      recover,
      createParserContext("recover", this, sync, sentinel)
    );
  }
  debug(name = "", recursivePrint = false, logger = console.log) {
    return parserDebug(this, name, recursivePrint, logger);
  }
  toString() {
    return parserPrint(this);
  }
  static lazy(fn) {
    return new Parser(
      createLazyCached(fn),
      createParserContext("lazy", void 0, fn)
    );
  }
}
_initWhitespace();
exports.Parser = Parser;
exports.ParserState = ParserState;
exports.all = all;
exports.any = any;
exports.createLazyCached = createLazyCached;
exports.createParserContext = createParserContext;
exports.dispatch = dispatch;
exports.eof = eof;
exports.getLazyParser = getLazyParser;
exports.lazy = lazy;
exports.memoize = memoize;
exports.mergeMemos = mergeMemos;
exports.mergeSpans = mergeSpans;
exports.regex = regex;
exports.resetPackrat = resetPackrat;
exports.spanToString = spanToString;
exports.string = string;
exports.trimStateWhitespace = trimStateWhitespace;
//# sourceMappingURL=packrat-entry-46NYx4_U.cjs.map
