import type { Demo } from "./types";

export const errorRecoveryDemo: Demo = {
    id: "error-recovery",
    title: "Add error recovery",
    description: "Use @recover to parse past errors and collect diagnostics.",
    steps: [
        {
            grammar: `// A simple statement grammar
statement = assignment | print ;
assignment = /[a-z]+/ , "=" , /\\d+/ , ";" ;
print = "print" , /[a-z]+/ , ";" ;
program = statement* ;`,
            input: `x = 42;
print hello;`,
            entryRule: "program",
            annotation: "This grammar parses simple assignments and print statements. Try it with valid input first — both statements parse correctly.",
            highlightRegion: "grammar",
        },
        {
            input: `x = 42;
y = ;
print hello;`,
            annotation: "Now introduce an error: `y = ;` is missing a value. Without recovery, the parser stops at the first error and you lose the rest of the input.",
            highlightRegion: "input",
        },
        {
            grammar: `// A simple statement grammar — with recovery
statement = assignment | print ;
assignment = /[a-z]+/ , "=" , /\\d+/ , ";" ;
print = "print" , /[a-z]+/ , ";" ;
program = statement* ;

@recover statement /[^;]*;/ ;`,
            annotation: "Add `@recover statement /[^;]*;/` — when a statement fails, the parser skips to the next `;` and resumes. Now all three statements are processed, with the error collected as a diagnostic.",
            highlightRegion: "grammar",
        },
        {
            input: `x = 42;
y = ;
z = abc;
print hello;`,
            annotation: "Multiple errors are now collected in a single pass. Check the error count in the controls bar — each failed statement is reported with its position. The successfully parsed statements still appear in the AST.",
            highlightRegion: "controls",
        },
    ],
};
