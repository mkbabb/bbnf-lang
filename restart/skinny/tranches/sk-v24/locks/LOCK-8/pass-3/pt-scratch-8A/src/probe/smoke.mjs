import { all, string, regex } from "./_core.bundle.mjs";
const p = all(string('('), regex(/\d+/), string(')'));
const st = p.parseState("(42)");
console.log("value=", JSON.stringify(st.value), "isError=", st.isError, "offset=", st.offset);
