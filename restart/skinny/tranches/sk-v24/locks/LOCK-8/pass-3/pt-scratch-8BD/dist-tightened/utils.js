import { s, b } from "./diagnostics-DDazRHgl.js";
import { f as dispatch, n as string, r as regex, P as Parser, c as any } from "./packrat-entry-CS1td-8B.js";
const comma = string(",").trim();
const colon = string(":").trim();
const jsonNull = string("null").map(() => null);
const jsonBool = string("true").or(string("false")).map((v) => v === "true");
const jsonNumber = regex(/-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?/).map(Number);
const jsonString = regex(/"(?:[^"\\]|\\(?:["\\/bfnrt]|u[0-9a-fA-F]{4}))*"/).map(
  (s2) => s2.indexOf("\\") === -1 ? s2.slice(1, -1) : JSON.parse(s2)
);
const jsonArray = Parser.lazy(
  () => jsonValue.sepBy(comma).trim().wrap(string("["), string("]"))
);
const jsonObject = Parser.lazy(
  () => jsonString.skip(colon).then(jsonValue.trim()).sepBy(comma).trim().wrap(string("{"), string("}")).map((pairs) => Object.fromEntries(pairs))
);
const jsonValue = dispatch({
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
const delim = string(",").trim();
const doubleQuotes = string('"');
const singleQuotes = string("'");
const token = any(
  regex(/[^"]+/).wrap(doubleQuotes, doubleQuotes),
  regex(/[^']+/).wrap(singleQuotes, singleQuotes),
  regex(/[^,]+/)
);
const line = token.sepBy(delim).trim();
const csvParser = line.many();
function escapedString() {
  return string("\\").then(
    regex(/[bfnrt"'\\/]/).or(string("u").skip(regex(/[0-9a-fA-F]{4}/)))
  ).map(([, esc]) => esc);
}
function quotedString(quote = '"') {
  const inner = regex(new RegExp(`[^${quote}\\\\]+`)).or(escapedString());
  return inner.many().wrap(string(quote), string(quote)).map((parts) => parts.join(""));
}
function numberParser() {
  return regex(/-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?/).map(Number);
}
export {
  csvParser,
  escapedString,
  jsonParser,
  numberParser,
  quotedString,
  s as skipBlockComments,
  b as skipWhitespace
};
//# sourceMappingURL=utils.js.map
