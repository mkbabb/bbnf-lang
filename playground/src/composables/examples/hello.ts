import type { Example } from "../useExamples";

export const helloExample: Example = {
    name: "Hello",
    grammar: `greeting = salutation ?w , name , punctuation ? ;
salutation = "Hello" | "Hi" | "Hey" ;
name = /[A-Z][a-z]+/ ;
punctuation = "!" | "." | "?" ;`,
    input: `Hello World!`,
};
