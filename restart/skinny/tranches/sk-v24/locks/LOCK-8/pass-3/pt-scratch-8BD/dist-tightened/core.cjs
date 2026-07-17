"use strict";
Object.defineProperty(exports, Symbol.toStringTag, { value: "Module" });
const packrat = require("./packrat-entry-46NYx4_U.cjs");
function containsDelimiter(text, delim) {
  return text.indexOf(delim) !== -1;
}
function splitBalanced(text, delim) {
  if (!containsDelimiter(text, delim)) return [text];
  const result = [];
  let depth = 0;
  let inString = null;
  let start = 0;
  for (let i = 0; i < text.length; i++) {
    const ch = text[i];
    if (ch === '"' || ch === "'") {
      if (inString === ch) {
        inString = null;
      } else if (inString === null) {
        inString = ch;
      }
      continue;
    }
    if (inString !== null) continue;
    if (ch === "(" || ch === "[") {
      depth++;
      continue;
    }
    if (ch === ")" || ch === "]") {
      depth = Math.max(0, depth - 1);
      continue;
    }
    if (ch === delim && depth === 0) {
      result.push(text.slice(start, i));
      start = i + 1;
    }
  }
  result.push(text.slice(start));
  return result;
}
exports.Parser = packrat.Parser;
exports.ParserState = packrat.ParserState;
exports.all = packrat.all;
exports.any = packrat.any;
exports.createLazyCached = packrat.createLazyCached;
exports.createParserContext = packrat.createParserContext;
exports.dispatch = packrat.dispatch;
exports.eof = packrat.eof;
exports.getLazyParser = packrat.getLazyParser;
exports.lazy = packrat.lazy;
exports.mergeSpans = packrat.mergeSpans;
exports.regex = packrat.regex;
exports.spanToString = packrat.spanToString;
exports.string = packrat.string;
exports.trimStateWhitespace = packrat.trimStateWhitespace;
Object.defineProperty(exports, "whitespace", {
  enumerable: true,
  get: () => packrat.whitespace
});
exports.containsDelimiter = containsDelimiter;
exports.splitBalanced = splitBalanced;
//# sourceMappingURL=core.cjs.map
