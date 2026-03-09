import type { Example } from "../useExamples";

export const helloExample: Example = {
    name: "Hello",
    grammar: `// Simple Greeting Grammar
greeting = salutation , name , punctuation?;
salutation = "Hello" | "Hi" | "Hey";
name = /[A-Z][a-z]+/;
punctuation = "!" | "." | "?";`,
    input: `Hello World!`,
};
