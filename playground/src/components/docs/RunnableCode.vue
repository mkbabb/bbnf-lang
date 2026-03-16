<script setup lang="ts">
import { ref } from "vue";
import { ensureWasmLoaded } from "@/composables/wasm";

interface RunnableCodeData {
    grammar: string;
    input: string;
    language?: string;
    highlighted?: string;
}

const props = defineProps<{
    data: RunnableCodeData;
}>();

const output = ref("");
const running = ref(false);
const hasRun = ref(false);
const error = ref("");

async function run() {
    running.value = true;
    error.value = "";
    output.value = "";
    hasRun.value = true;

    try {
        await ensureWasmLoaded();
        const { getWasmModule } = await import("@/composables/wasm/loader");
        const mod = getWasmModule();

        // Compile the grammar
        const handle = mod.compile_grammar(props.data.grammar, undefined);
        if (handle === 0 || handle === undefined) {
            error.value = "Failed to compile grammar";
            return;
        }

        try {
            // Parse with the compiled grammar
            const result = mod.parse_with_grammar(handle, props.data.input);
            if (result?.success) {
                output.value = JSON.stringify(result.value, null, 2);
            } else {
                const diags = result?.diagnostics ?? [];
                error.value = diags.length
                    ? diags.map((d: any) => d.expected).join("\n")
                    : "Parse failed";
            }
        } finally {
            mod.free_grammar(handle);
        }
    } catch (e: any) {
        error.value = e.message ?? String(e);
    } finally {
        running.value = false;
    }
}
</script>

<template>
    <div class="rounded-lg border border-border/30 bg-muted/5 overflow-hidden">
        <!-- Grammar -->
        <div class="border-b border-border/20">
            <div class="flex items-center justify-between px-3 py-1.5 bg-muted/10">
                <span class="text-xs font-mono font-semibold text-muted-foreground uppercase tracking-wider">Grammar</span>
            </div>
            <pre class="px-4 py-3 text-sm font-mono overflow-x-auto !mt-0"><code v-if="data.highlighted" v-html="data.highlighted" /><code v-else>{{ data.grammar }}</code></pre>
        </div>

        <!-- Input -->
        <div class="border-b border-border/20">
            <div class="flex items-center justify-between px-3 py-1.5 bg-muted/10">
                <span class="text-xs font-mono font-semibold text-muted-foreground uppercase tracking-wider">Input</span>
            </div>
            <pre class="px-4 py-3 text-sm font-mono overflow-x-auto !mt-0"><code>{{ data.input }}</code></pre>
        </div>

        <!-- Run button + Output -->
        <div class="px-3 py-2 flex flex-col gap-2">
            <button
                @click="run"
                :disabled="running"
                class="self-start flex items-center gap-1.5 px-3 py-1.5 rounded-md text-sm font-mono font-medium transition-colors"
                :class="running
                    ? 'bg-muted/20 text-muted-foreground cursor-wait'
                    : 'bg-foreground/10 text-foreground hover:bg-foreground/20 cursor-pointer'"
            >
                <svg v-if="!running" width="12" height="12" viewBox="0 0 12 12" fill="currentColor">
                    <polygon points="2,1 10,6 2,11" />
                </svg>
                <svg v-else class="animate-spin" width="12" height="12" viewBox="0 0 12 12" fill="none" stroke="currentColor" stroke-width="1.5">
                    <circle cx="6" cy="6" r="4" stroke-dasharray="16" stroke-dashoffset="4" />
                </svg>
                {{ running ? "Running…" : "Run" }}
            </button>

            <div v-if="hasRun && !running" class="rounded-md border border-border/20 bg-muted/10 px-3 py-2">
                <div v-if="error" class="text-xs font-mono text-red-400 whitespace-pre-wrap">{{ error }}</div>
                <pre v-else class="text-xs font-mono text-foreground/80 whitespace-pre-wrap overflow-x-auto !mt-0"><code>{{ output }}</code></pre>
            </div>
        </div>
    </div>
</template>
