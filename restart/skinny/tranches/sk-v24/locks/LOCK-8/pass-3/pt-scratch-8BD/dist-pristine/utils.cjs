"use strict";
Object.defineProperty(exports, Symbol.toStringTag, { value: "Module" });
const diagnostics = require("./diagnostics-DpUY87_d.cjs");
const packrat = require("./packrat-entry-46NYx4_U.cjs");
const comma = packrat.string(",").trim();
const colon = packrat.string(":").trim();
const jsonNull = packrat.string("null").map(() => null);
const jsonBool = packrat.string("true").or(packrat.string("false")).map((v) => v === "true");
const jsonNumber = packrat.regex(/-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?/).map(Number);
const jsonString = packrat.regex(/"(?:[^"\\]|\\(?:["\\/bfnrt]|u[0-9a-fA-F]{4}))*"/).map(
  (s) => s.indexOf("\\") === -1 ? s.slice(1, -1) : JSON.parse(s)
);
const jsonArray = packrat.Parser.lazy(
  () => jsonValue.sepBy(comma).trim().wrap(packrat.string("["), packrat.string("]"))
);
const jsonObject = packrat.Parser.lazy(
  () => jsonString.skip(colon).then(jsonValue.trim()).sepBy(comma).trim().wrap(packrat.string("{"), packrat.string("}")).map((pairs) => Object.fromEntries(pairs))
);
const jsonValue = packrat.dispatch({
  "{": jsonObject,
  "[": jsonArray,
  '"': jsonString,
  "-": jsonNumber,
  "0-9": jsonNumber,
  "t": jsonBool,
  "f": jsonBool,
  "n": jsonNull
});
const jsonParser = jsonValue.trim();
const delim = packrat.string(",").trim();
const doubleQuotes = packrat.string('"');
const singleQuotes = packrat.string("'");
const token = packrat.any(
  packrat.regex(/[^"]+/).wrap(doubleQuotes, doubleQuotes),
  packrat.regex(/[^']+/).wrap(singleQuotes, singleQuotes),
  packrat.regex(/[^,]+/)
);
const line = token.sepBy(delim).trim();
const csvParser = line.many();
function escapedString() {
  return packrat.string("\\").then(
    packrat.regex(/[bfnrt"'\\/]/).or(packrat.string("u").skip(packrat.regex(/[0-9a-fA-F]{4}/)))
  ).map(([, esc]) => esc);
}
function quotedString(quote = '"') {
  const inner = packrat.regex(new RegExp(`[^${quote}\\\\]+`)).or(escapedString());
  return inner.many().wrap(packrat.string(quote), packrat.string(quote)).map((parts) => parts.join(""));
}
function numberParser() {
  return packrat.regex(/-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?/).map(Number);
}
exports.skipBlockComments = diagnostics.skipBlockComments;
exports.skipWhitespace = diagnostics.skipWhitespace;
exports.csvParser = csvParser;
exports.escapedString = escapedString;
exports.jsonParser = jsonParser;
exports.numberParser = numberParser;
exports.quotedString = quotedString;
//# sourceMappingURL=utils.cjs.map
