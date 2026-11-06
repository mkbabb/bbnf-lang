import fs from "node:fs";
import { cssUnknowns } from "./css.mjs";
import { vueUnknowns } from "./vue.mjs";

const input = JSON.parse(fs.readFileSync(0, "utf8"));
if (input.schema !== "bbnf.architecture.frontend-boundary-input.v3.1") {
  throw new Error("FRONTEND_HELPER_SCHEMA_MISMATCH: input schema differs");
}
if (!Array.isArray(input.files) || typeof input.face !== "string") {
  throw new Error("FRONTEND_HELPER_SCHEMA_MISMATCH: typed files are absent");
}
const files = input.files.slice().sort((left, right) =>
  left.path < right.path ? -1 : left.path > right.path ? 1 : 0
);
const unknowns = [];
const anchor = files.find((file) => file.path === "package.json")?.path
  ?? files[0]?.path;
if (!anchor) throw new Error("FRONTEND_HELPER_ANCHOR_MISMATCH: face has no captured input");

add("FRONTEND_APPLICATION_SCOPE_OMITTED", anchor, "configured TypeScript program facts are not yet promoted to Exact");
add("FRONTEND_TEST_OVERLAY_INCOMPLETE", anchor, "Playwright E2E and frontend test reachability remain Unknown");
add("VITE_ALIAS_RESOLUTION_UNSOUND", anchor, "Vite aliases plugins and export conditions are quarantined from exact edges");
add("GLOB_SEMANTICS_UNSOUND", anchor, "standards-correct configured glob expansion remains Unknown");
add("WORKER_QUERY_SEMANTICS_UNSOUND", anchor, "worker url raw and asset query meanings remain Unknown");
add("TYPE_ONLY_REEXPORT_MISCLASSIFIED", anchor, "type-only reexports remain Unknown rather than runtime exact");
add("DYNAMIC_IMPORT_BOUNDARY_SILENT", anchor, "computed dynamic imports emit required Unknown");
add("REQUIRE_BOUNDARY_SILENT", anchor, "CommonJS require resolution emits required Unknown");
add("ROUTE_CONTEXT_UNPROVEN", anchor, "route-shaped objects require a proven router context");

for (const file of files) {
  if (file.path.endsWith(".vue")) unknowns.push(...vueUnknowns(file.path, file.text));
  if (/\.(css|scss|sass|less)$/.test(file.path)) {
    unknowns.push(...cssUnknowns(file.path, file.text));
  }
}

unknowns.sort((left, right) => {
  const a = JSON.stringify(left);
  const b = JSON.stringify(right);
  return a < b ? -1 : a > b ? 1 : 0;
});
process.stdout.write(JSON.stringify({
  schema: "bbnf.architecture.frontend-boundary.v3.1",
  edges: [],
  unknowns,
}) + "\n");

function add(code, path, reason) {
  unknowns.push({ code, path, line: 1, column: 1, reason });
}
