import hljs from "highlight.js/lib/core";
import bash from "highlight.js/lib/languages/bash";
import cpp from "highlight.js/lib/languages/cpp";
import javascript from "highlight.js/lib/languages/javascript";
import json from "highlight.js/lib/languages/json";
import plaintext from "highlight.js/lib/languages/plaintext";
import python from "highlight.js/lib/languages/python";
import rust from "highlight.js/lib/languages/rust";
import typescript from "highlight.js/lib/languages/typescript";

let initialized = false;

function ensureLanguages() {
    if (initialized) return;
    initialized = true;
    hljs.registerLanguage("bash", bash);
    hljs.registerLanguage("cpp", cpp);
    hljs.registerLanguage("javascript", javascript);
    hljs.registerLanguage("js", javascript);
    hljs.registerLanguage("json", json);
    hljs.registerLanguage("plaintext", plaintext);
    hljs.registerLanguage("python", python);
    hljs.registerLanguage("py", python);
    hljs.registerLanguage("rust", rust);
    hljs.registerLanguage("rs", rust);
    hljs.registerLanguage("typescript", typescript);
    hljs.registerLanguage("ts", typescript);
}

function escapeHtml(value: string): string {
    return value
        .replace(/&/g, "&amp;")
        .replace(/</g, "&lt;")
        .replace(/>/g, "&gt;");
}

export function useCodeHighlight() {
    ensureLanguages();

    function highlight(code: string, language?: string) {
        try {
            if (language && hljs.getLanguage(language)) {
                return {
                    html: hljs.highlight(code, { language }).value,
                    language,
                };
            }
            const auto = hljs.highlightAuto(code);
            return {
                html: auto.value,
                language: auto.language,
            };
        } catch {
            return {
                html: escapeHtml(code),
                language,
            };
        }
    }

    return { highlight };
}
