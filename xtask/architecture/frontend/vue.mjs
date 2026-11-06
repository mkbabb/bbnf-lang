export const schema = "bbnf.architecture.frontend-vue-helper.v3.1";

export function vueUnknowns(path, text) {
  const line = (offset) => text.slice(0, offset).split("\n").length;
  const source = text.search(/<script\b[^>]*\bsrc\s*=/i);
  const script = text.search(/<script\b/i);
  const css = text.search(/<style[\s\S]*?(?:@import|url\()/i);
  const out = [];
  if (source >= 0) out.push({ code: "VUE_SCRIPT_SOURCE_OMITTED", path, line: line(source), column: 1, reason: "Vue script src resolution remains Unknown" });
  if (script >= 0) out.push({ code: "VUE_BLOCK_SPAN_MISATTRIBUTED", path, line: line(script), column: 1, reason: "Vue block offsets require file-correct span promotion" });
  if (css >= 0) out.push({ code: "CSS_IMPORT_CLASSIFICATION_UNSOUND", path, line: line(css), column: 1, reason: "Vue style import url and layer semantics remain Unknown" });
  return out;
}
