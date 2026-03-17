<script setup lang="ts">
import { ref, onUnmounted } from "vue";
import DocCard from "./DocCard.vue";

interface BenchConfig {
    id: string;
    label: string;
    wasmFn: string;
    input: string;
    iterations: number;
}

const props = defineProps<{
    config: BenchConfig;
}>();

const status = ref<"idle" | "running" | "done">("idle");
const opsPerSec = ref(0);
const medianMs = ref(0);
const error = ref("");

let cancelled = false;
onUnmounted(() => { cancelled = true; });

async function runBenchmark() {
    status.value = "running";
    error.value = "";

    try {
        const { ensureWasmLoaded, getWasmModule } = await import("@/composables/wasm/loader");
        await ensureWasmLoaded();
        const mod = getWasmModule();
        const fn = (mod as any)[props.config.wasmFn];
        if (typeof fn !== "function") {
            error.value = `WASM function "${props.config.wasmFn}" not found`;
            status.value = "idle";
            return;
        }

        const input = props.config.input;
        const iters = props.config.iterations;

        // Warmup: 10% of iterations
        const warmup = Math.max(10, Math.floor(iters * 0.1));
        for (let i = 0; i < warmup && !cancelled; i++) {
            fn(input, 80, 2, false);
        }

        // Timed runs
        const times: number[] = [];
        for (let i = 0; i < iters && !cancelled; i++) {
            const t0 = performance.now();
            fn(input, 80, 2, false);
            times.push(performance.now() - t0);
        }

        if (cancelled) return;

        times.sort((a, b) => a - b);
        const median = times[Math.floor(times.length / 2)]!;
        medianMs.value = Math.round(median * 1000) / 1000;
        opsPerSec.value = Math.round(1000 / median);
        status.value = "done";
    } catch (e) {
        error.value = String(e);
        status.value = "idle";
    }
}
</script>

<template>
    <DocCard :title="config.label">
        <template #header>
            <button
                class="px-3 py-1 text-xs font-mono rounded-md border border-border/40 hover:bg-muted/30 active:scale-95 transition-all disabled:opacity-40 disabled:cursor-not-allowed"
                :disabled="status === 'running'"
                @click="runBenchmark"
            >
                {{ status === "running" ? "Running..." : status === "done" ? "Re-run" : "Run Benchmark" }}
            </button>
        </template>

        <p v-if="error" class="text-xs text-red-400 mt-1">{{ error }}</p>

        <div v-if="status === 'done'" class="flex gap-6 mt-3">
            <div>
                <div class="text-2xl font-mono font-semibold" style="color: var(--color-pastel-cyan)">
                    {{ opsPerSec.toLocaleString() }}
                </div>
                <div class="text-[0.625rem] text-muted-foreground/60 uppercase tracking-wider">ops/sec</div>
            </div>
            <div>
                <div class="text-2xl font-mono font-semibold text-foreground/80">
                    {{ medianMs }}
                </div>
                <div class="text-[0.625rem] text-muted-foreground/60 uppercase tracking-wider">median ms</div>
            </div>
        </div>

        <div v-if="status === 'running'" class="flex items-center gap-2 mt-3">
            <div class="h-1 flex-1 bg-muted/20 rounded overflow-hidden">
                <div class="h-full bg-pastel-cyan/50 rounded animate-pulse" style="width: 60%" />
            </div>
            <span class="text-[0.625rem] text-muted-foreground/50">Benchmarking...</span>
        </div>
    </DocCard>
</template>
