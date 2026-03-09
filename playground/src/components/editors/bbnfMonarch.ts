import * as monaco from "monaco-editor";

export function registerBBNFLanguage() {
    monaco.languages.register({ id: "bbnf" });

    monaco.languages.setMonarchTokensProvider("bbnf", {
        keywords: ["from", "epsilon"],

        directives: [
            "@import",
            "@recover",
            "@pretty",
            "@no_collapse",
        ],

        operators: ["|", ",", "<<", ">>", "*", "+", "?", "?w", "-", "=", ";", "."],

        tokenizer: {
            root: [
                // Line comments
                [/\/\/.*$/, "comment"],
                // Block comments
                [/\/\*/, "comment", "@comment"],
                // Directives
                [/@(import|recover|pretty|no_collapse)\b/, "keyword"],
                // Strings
                [/"([^"\\]|\\.)*"/, "string"],
                [/'([^'\\]|\\.)*'/, "string"],
                [/`([^`\\]|\\.)*`/, "string"],
                // Regex
                [/\/(?:[^/\\]|\\.)+\/[gimsuy]*/, "regexp"],
                // Compound operators
                [/<<|>>/, "operator"],
                [/\?w/, "operator"],
                [/\.\./, "operator"],
                // Single-char operators
                [/[|,;.*+?\-]/, "operator"],
                [/=/, "delimiter"],
                // Brackets
                [/[()[\]{}]/, "@brackets"],
                // Special tokens
                [/\bepsilon\b/, "constant"],
                [/ε/, "constant"],
                // Identifiers
                [/[_a-zA-Z][_a-zA-Z0-9-]*/, {
                    cases: {
                        "@keywords": "keyword",
                        "@default": "identifier",
                    },
                }],
                // Whitespace
                [/\s+/, "white"],
            ],
            comment: [
                [/[^/*]+/, "comment"],
                [/\*\//, "comment", "@pop"],
                [/[/*]/, "comment"],
            ],
        },
    });
}
