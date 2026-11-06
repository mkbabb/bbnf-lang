import { P, a, b, c, d, e, f, g, h, l, j, r, s, n, t, w } from "./packrat-entry-CS1td-8B.js";
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
export {
  P as Parser,
  a as ParserState,
  b as all,
  c as any,
  containsDelimiter,
  d as createLazyCached,
  e as createParserContext,
  f as dispatch,
  g as eof,
  h as getLazyParser,
  l as lazy,
  j as mergeSpans,
  r as regex,
  s as spanToString,
  splitBalanced,
  n as string,
  t as trimStateWhitespace,
  w as whitespace
};
//# sourceMappingURL=core.js.map
