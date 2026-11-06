export const schema = "bbnf.architecture.frontend-css-helper.v3.1";

export function cssUnknowns(path, text) {
  if (!/@import|url\(/i.test(text)) return [];
  const offset = Math.max(0, text.search(/@import|url\(/i));
  return [{
    code: "CSS_IMPORT_CLASSIFICATION_UNSOUND",
    path,
    line: text.slice(0, offset).split("\n").length,
    column: 1,
    reason: "parsed CSS import url and layer semantics remain Unknown",
  }];
}
