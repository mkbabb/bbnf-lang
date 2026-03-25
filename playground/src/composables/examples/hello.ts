import type { Example } from "../useExamples";

export const helloExample: Example = {
    name: "Hello",
    grammar: `salutation = "Hello" | "Hi" | "Hey" ;
name = /[A-Z][a-z]+/ ;
punctuation = "!" | "." | "?" ;
greeting = salutation ?w , name , punctuation ? ;`,
    input: `Hello World!`,
};
