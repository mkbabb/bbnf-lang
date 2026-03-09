import { ref } from "vue";
import { jsonExample } from "./examples";
import { cssExample } from "./examples";
import { mathExample } from "./examples";
import { helloExample } from "./examples";

export interface Example {
    name: string;
    grammar: string;
    input: string;
    /** Entry rule name. If omitted, uses the first rule in the grammar. */
    entryRule?: string;
}

const examples: Example[] = [
    jsonExample,
    cssExample,
    mathExample,
    helloExample,
];

export function useExamples() {
    const currentExample = ref<Example>(examples[0]);

    function selectExample(name: string) {
        const ex = examples.find((e) => e.name === name);
        if (ex) currentExample.value = ex;
    }

    return { examples, currentExample, selectExample };
}
