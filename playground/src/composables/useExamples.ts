import { ref } from "vue";
import { jsonExample, cssExample, mathExample, helloExample, bbnfExample } from "./examples";

export interface Example {
    name: string;
    grammar: string;
    input: string;
    /** Entry rule name. If omitted, uses the first rule in the grammar. */
    entryRule?: string;
    /** Short description shown in dropdown hover card. */
    description: string;
    /** Feature tags shown as pills. */
    tags?: string[];
}

const examples: Example[] = [
    { ...jsonExample, description: "RFC 8259 JSON — object, array, string, number, boolean, null", tags: ["@pretty", "recursive"] },
    { ...cssExample, description: "CSS L1.75 — selectors, declarations, @media, @supports, error recovery", tags: ["error recovery", "@pretty", "nesting"] },
    { ...bbnfExample, description: "BBNF parsing itself — self-hosting grammar with @pretty directives", tags: ["self-hosting", "@pretty", "meta"] },
    { ...mathExample, description: "Operator-precedence math — left-recursive via memoization", tags: ["precedence", "recursive"] },
    { ...helloExample, description: "Minimal grammar — single string literal", tags: ["beginner"] },
];

export function useExamples() {
    const currentExample = ref<Example>(examples[0]!);

    function selectExample(name: string) {
        const ex = examples.find((e) => e.name === name);
        if (ex) currentExample.value = ex;
    }

    return { examples, currentExample, selectExample };
}
