<script setup lang="ts">
import { ref, computed, watch, onMounted, onBeforeUnmount } from "vue";
import { useRouter } from "vue-router";

const router = useRouter();

const presets = [
    {
        name: "JSON",
        grammar: `value = object | array | string | number | "true" | "false" | "null" ;
object = "{" , members? , "}" ;
members = member , ("," , member)* ;
member = string , ":" , value ;
array = "[" , elements? , "]" ;
elements = value , ("," , value)* ;
string = /"[^"]*"/ ;
number = /-?\\d+(\\.\\d+)?/ ;`,
        input: `{"name": "BBNF", "version": 1, "items": [1, 2, 3]}`,
    },
    {
        name: "CSS",
        grammar: `selectorSpan = /[^{};]+[^\\s{};]/ ;
propertyName = /[a-zA-Z_][\\w-]*/ ;
valueSpan = /[^;{}!,]+/ ;
declaration = propertyName , ":" , valueSpan , ";" ;
ruleBlock = "{" , declaration* , "}" ;
qualifiedRule = selectorSpan , ruleBlock ;
stylesheet = qualifiedRule* ;`,
        input: `.card { background: #fff; border-radius: 8px; }`,
    },
    {
        name: "Math",
        grammar: `expr = term , (("+" | "-") , term)* ;
term = factor , (("*" | "/") , factor)* ;
factor = number | "(" , expr , ")" ;
number = /\\d+(\\.\\d+)?/ ;`,
        input: `2 + 3 * (4 - 1)`,
    },
];

const activeIndex = ref(0);
const grammarText = ref(presets[0]!.grammar);
const inputText = ref(presets[0]!.input);
const astOutput = ref("");
const parseError = ref("");

// Once the user edits content, stop auto-cycling permanently
const userEngaged = ref(false);
let timer: ReturnType<typeof setInterval> | undefined;

function startTimer() {
    clearInterval(timer);
    if (userEngaged.value) return;
    timer = setInterval(() => {
        const next = (activeIndex.value + 1) % presets.length;
        selectPreset(next);
    }, 8000);
}

onMounted(() => {
    runMiniParse();
    startTimer();
});
onBeforeUnmount(() => { clearInterval(timer); });

function selectPreset(i: number) {
    activeIndex.value = i;
    grammarText.value = presets[i]!.grammar;
    inputText.value = presets[i]!.input;
    startTimer();
}

function onUserEdit() {
    if (!userEngaged.value) {
        userEngaged.value = true;
        clearInterval(timer);
    }
}

async function runMiniParse() {
    try {
        const { BBNFToASTWithImports, ASTToParser, analyzeGrammar, computeFirstSets, dedupGroups } =
            await import("@mkbabb/bbnf-lang");

        const result = BBNFToASTWithImports(grammarText.value);
        if (!result[1]) { parseError.value = "Grammar error"; astOutput.value = ""; return; }

        const ast = result[1].rules;
        dedupGroups(ast);
        const analysis = analyzeGrammar(ast);
        const firstNullable = computeFirstSets(ast, analysis);
        const nonterminals = ASTToParser(ast, analysis, firstNullable, [], false);

        for (const key of Object.keys(nonterminals)) {
            nonterminals[key] = nonterminals[key].trim();
        }

        const firstKey = ast.keys().next().value;
        if (!firstKey || !nonterminals[firstKey]) {
            parseError.value = "No entry rule";
            astOutput.value = "";
            return;
        }

        const parsed = nonterminals[firstKey].parse(inputText.value);
        if (parsed == null) {
            parseError.value = "Parse failed";
            astOutput.value = "";
        } else {
            parseError.value = "";
            astOutput.value = JSON.stringify(parsed, null, 2);
        }
    } catch (e: any) {
        parseError.value = e.message?.slice(0, 60) ?? "Error";
        astOutput.value = "";
    }
}

watch([grammarText, inputText], () => { runMiniParse(); });

function openInPlayground() {
    router.push({
        path: "/playground",
        query: {
            grammar: grammarText.value,
            input: inputText.value,
        },
    });
}

/** Simple BBNF grammar tokenizer for syntax highlighting. */
function highlightGrammar(text: string): string {
    return text.replace(
        /(@\w+)|(\/[^/]+\/)|("(?:[^"\\]|\\.)*")|([=;,|*+?(){}[\]])|([a-zA-Z_][\w-]*)/g,
        (match, directive, regex, str, op, ident) => {
            if (directive) return `<span class="hl-decorator">${esc(directive)}</span>`;
            if (regex) return `<span class="hl-regex">${esc(regex)}</span>`;
            if (str) return `<span class="hl-string">${esc(str)}</span>`;
            if (op) return `<span class="hl-operator">${esc(op)}</span>`;
            if (ident) return `<span class="hl-type">${esc(ident)}</span>`;
            return esc(match);
        },
    );
}

/** Simple JSON/CSS input highlighter. */
function highlightInput(text: string): string {
    return text.replace(
        /("(?:[^"\\]|\\.)*")|(-?\d+(?:\.\d+)?)|([{}[\]:,;])|([.#]?[a-zA-Z_][\w-]*)/g,
        (match, str, num, punct, ident) => {
            if (str) return `<span class="hl-string">${esc(str)}</span>`;
            if (num) return `<span class="hl-number">${esc(num)}</span>`;
            if (punct) return `<span class="hl-operator">${esc(punct)}</span>`;
            if (ident) return `<span class="hl-type">${esc(ident)}</span>`;
            return esc(match);
        },
    );
}

/** Highlight JSON AST output. */
function highlightJson(text: string): string {
    return text.replace(
        /("(?:[^"\\]|\\.)*")(:?)|(true|false|null)|(-?\d+(?:\.\d+)?)|([{}[\],])/g,
        (match, str, colon, keyword, num, punct) => {
            if (str) {
                const cls = colon ? "hl-string" : "hl-string";
                return `<span class="${cls}">${esc(str)}</span>${colon ? esc(colon) : ""}`;
            }
            if (keyword) return `<span class="hl-keyword">${esc(keyword)}</span>`;
            if (num) return `<span class="hl-number">${esc(num)}</span>`;
            if (punct) return `<span class="hl-operator">${esc(punct)}</span>`;
            return esc(match);
        },
    );
}

function esc(s: string): string {
    return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
}

const grammarHighlighted = computed(() => highlightGrammar(grammarText.value));
const inputHighlighted = computed(() => highlightInput(inputText.value));
const astHighlighted = computed(() => astOutput.value ? highlightJson(astOutput.value) : "");
</script>

<template>
    <section class="max-w-5xl mx-auto px-4 sm:px-8 py-10 sm:py-16">
        <h2 class="instrument-serif text-3xl text-center mb-2">See it in action</h2>
        <p class="text-sm text-muted-foreground text-center mb-8">Edit the grammar or input below — parsing runs live.</p>

        <div class="rounded-xl border border-border/40 bg-card/80 backdrop-blur-sm overflow-hidden
                    shadow-[3px_3px_0px_0px_rgba(0,0,0,0.08)] dark:shadow-[3px_3px_0px_0px_rgba(200,200,255,0.06)]">
            <div class="grid grid-cols-1 md:grid-cols-3 divide-y md:divide-y-0 md:divide-x divide-border/30 md:h-[12rem] md:h-[18rem]">
                <!-- Grammar -->
                <div class="p-4 flex flex-col h-[12rem] md:h-[18rem]">
                    <div class="flex items-center gap-2 mb-2 shrink-0">
                        <div class="h-2 w-2 rounded-full bg-pastel-green" />
                        <span class="instrument-serif text-sm text-pastel-green">Grammar</span>
                    </div>
                    <div class="relative flex-1 overflow-y-auto overflow-x-hidden">
                        <pre
                            class="absolute inset-0 overflow-y-auto text-xs leading-relaxed font-mono whitespace-pre-wrap break-words pointer-events-none scrollbar-hidden p-0 m-0"
                            aria-hidden="true"
                            v-html="grammarHighlighted + '\n'"
                        />
                        <textarea
                            v-model="grammarText"
                            spellcheck="false"
                            class="absolute inset-0 w-full h-full bg-transparent text-xs leading-relaxed font-mono resize-none outline-none scrollbar-hidden text-transparent caret-foreground p-0 m-0"
                            @input="onUserEdit"
                        />
                    </div>
                </div>

                <!-- Input -->
                <div class="p-4 flex flex-col h-[12rem] md:h-[18rem]">
                    <div class="flex items-center gap-2 mb-2 shrink-0">
                        <div class="h-2 w-2 rounded-full bg-pastel-blue" />
                        <span class="instrument-serif text-sm text-pastel-blue">Input</span>
                    </div>
                    <div class="relative flex-1 overflow-y-auto overflow-x-hidden">
                        <pre
                            class="absolute inset-0 overflow-y-auto text-xs leading-relaxed font-mono whitespace-pre-wrap break-words pointer-events-none scrollbar-hidden p-0 m-0"
                            aria-hidden="true"
                            v-html="inputHighlighted + '\n'"
                        />
                        <textarea
                            v-model="inputText"
                            spellcheck="false"
                            class="absolute inset-0 w-full h-full bg-transparent text-xs leading-relaxed font-mono resize-none outline-none scrollbar-hidden text-transparent caret-foreground p-0 m-0"
                            @input="onUserEdit"
                        />
                    </div>
                </div>

                <!-- AST Output -->
                <div class="p-4 flex flex-col h-[12rem] md:h-[18rem]">
                    <div class="flex items-center gap-2 mb-2 shrink-0">
                        <div class="h-2 w-2 rounded-full bg-pastel-purple" />
                        <span class="instrument-serif text-sm text-pastel-purple">Parsed AST</span>
                    </div>
                    <div class="flex-1 overflow-y-auto scrollbar-hidden">
                        <pre
                            v-if="astOutput"
                            class="text-xs leading-relaxed font-mono whitespace-pre-wrap break-words"
                            v-html="astHighlighted"
                        />
                        <p v-else-if="parseError" class="text-xs text-destructive mt-1">{{ parseError }}</p>
                        <p v-else class="text-xs text-muted-foreground/50 mt-1 italic">Waiting...</p>
                    </div>
                </div>
            </div>

            <!-- Bottom bar: preset dots + open in playground -->
            <div class="flex items-center justify-between px-4 py-3 border-t border-border/30">
                <div class="flex items-center gap-2">
                    <button
                        v-for="(preset, i) in presets"
                        :key="i"
                        class="px-2.5 py-0.5 rounded-full text-[11px] font-mono transition-all duration-300"
                        :class="i === activeIndex
                            ? 'bg-pastel-green/20 text-pastel-green'
                            : 'text-muted-foreground/50 hover:text-muted-foreground'"
                        @click="selectPreset(i)"
                    >
                        {{ preset.name }}
                    </button>
                </div>
                <button
                    class="text-xs text-muted-foreground hover:text-foreground transition-colors instrument-serif"
                    @click="openInPlayground"
                >
                    Open in Playground →
                </button>
            </div>
        </div>
    </section>
</template>
