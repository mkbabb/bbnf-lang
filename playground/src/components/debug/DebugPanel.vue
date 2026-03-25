<script setup lang="ts">
import { computed } from "vue";
import { ArrowRight, ChevronRight, CircleDot, Bug } from "lucide-vue-next";
import type { UseDebugSessionReturn } from "@/components/debug/useDebugSession";
import DebugToolbar from "./DebugToolbar.vue";

const props = defineProps<{
    session: UseDebugSessionReturn;
    inputText: string;
}>();

const emit = defineEmits<{
    jumpToRule: [ruleName: string];
}>();

const snap = computed(() => props.session.snapshot.value);

const inputPreview = computed(() => {
    if (!snap.value) return "";
    const off = snap.value.offset;
    const s = Math.max(0, off - 20);
    const e = Math.min(props.inputText.length, off + 40);
    return props.inputText.slice(s, e);
});

const cursorIdx = computed(() => {
    if (!snap.value) return 0;
    return snap.value.offset - Math.max(0, snap.value.offset - 20);
});

const statusText = computed(() => {
    if (!props.session.active.value) return "Ready";
    if (!snap.value) return "Starting\u2026";
    if (snap.value.completed) return snap.value.isError ? "Parse failed" : "Parse succeeded";
    return snap.value.isEntry ? `Entering ${snap.value.ruleName}` : `Exiting ${snap.value.ruleName}`;
});

const statusColor = computed(() => {
    if (!snap.value || !props.session.active.value) return "text-muted-foreground";
    if (snap.value.completed) return snap.value.isError ? "text-destructive" : "text-pastel-green";
    return "text-pastel-amber";
});
</script>

<template>
    <div class="flex h-full flex-col overflow-hidden">
        <!-- Toolbar strip -->
        <div class="flex items-center gap-2 px-3 py-1.5 border-b border-border/30 backdrop-blur-sm" style="background: var(--glass-bg);">
            <DebugToolbar :session="session" />
            <span class="dock-badge text-[0.625rem] font-mono" :class="statusColor">
                {{ statusText }}
            </span>
        </div>

        <!-- Scrollable content -->
        <div class="flex-1 overflow-y-auto scrollbar-hidden">
            <!-- Empty state -->
            <div
                v-if="!session.active.value && !snap"
                class="flex flex-col items-center justify-center gap-3 py-12 text-center"
            >
                <Bug class="h-10 w-10 text-muted-foreground/20" />
                <p class="text-sm text-muted-foreground/60">
                    Set breakpoints in the grammar gutter, then press
                    <kbd class="mx-0.5 rounded border border-border/40 bg-muted/30 px-1 py-0.5 text-[0.625rem] font-mono">&#9654;</kbd>
                    to start
                </p>
            </div>

            <template v-if="session.active.value || snap">
                <!-- Call Stack -->
                <section v-if="snap && !snap.completed" class="px-3 pt-2 pb-1">
                    <h4 class="mb-1.5 instrument-serif text-xs tracking-wide text-muted-foreground/60">
                        Call Stack
                    </h4>
                    <div class="space-y-px">
                        <button
                            class="flex w-full items-center gap-1.5 rounded-md px-2 py-1 text-left font-mono text-xs transition-colors hover:bg-accent/40"
                            :class="snap.isEntry
                                ? 'bg-pastel-amber/10 text-pastel-amber'
                                : 'bg-pastel-green/10 text-pastel-green'"
                            @click="emit('jumpToRule', snap.ruleName)"
                        >
                            <ArrowRight class="h-3 w-3 shrink-0" />
                            <span class="truncate">{{ snap.ruleName }}</span>
                            <span class="ml-auto text-[0.5rem] opacity-60">
                                {{ snap.isEntry ? "enter" : "exit" }}
                            </span>
                        </button>
                        <button
                            v-for="(frame, i) in snap.ruleStack"
                            :key="i"
                            class="flex w-full items-center gap-1.5 rounded-md px-2 py-1 text-left font-mono text-xs text-muted-foreground transition-colors hover:bg-accent/40"
                            @click="emit('jumpToRule', frame)"
                        >
                            <ChevronRight class="h-3 w-3 shrink-0 opacity-30" />
                            <span class="truncate">{{ frame }}</span>
                        </button>
                    </div>
                </section>

                <!-- Parse State -->
                <section v-if="snap" class="px-3 py-2 border-t border-border/15">
                    <h4 class="mb-1.5 instrument-serif text-xs tracking-wide text-muted-foreground/60">
                        Parse State
                    </h4>
                    <div class="grid grid-cols-[auto_1fr] gap-x-4 gap-y-1 font-mono text-xs">
                        <span class="text-muted-foreground/60">offset</span>
                        <span class="tabular-nums">{{ snap.offset }}</span>
                        <span class="text-muted-foreground/60">rule</span>
                        <span>{{ snap.ruleName || "\u2014" }}</span>
                        <span class="text-muted-foreground/60">phase</span>
                        <span :class="snap.isEntry ? 'text-pastel-amber' : 'text-pastel-green'">
                            {{ snap.isEntry ? "entering" : "exiting" }}
                        </span>
                    </div>
                </section>

                <!-- Input Context -->
                <section v-if="snap && !snap.completed && inputPreview" class="px-3 py-2 border-t border-border/15">
                    <h4 class="mb-1.5 instrument-serif text-xs tracking-wide text-muted-foreground/60">
                        Input
                    </h4>
                    <pre class="rounded-lg border border-border/20 p-2 font-mono text-[0.6875rem] leading-relaxed whitespace-pre-wrap break-all" style="background: var(--glass-bg);"
                    ><span class="text-pastel-green">{{ inputPreview.slice(0, cursorIdx) }}</span><span class="border-l-2 border-pastel-amber" /><span class="text-muted-foreground/50">{{ inputPreview.slice(cursorIdx) }}</span></pre>
                </section>
            </template>

            <!-- Breakpoints -->
            <section class="px-3 py-2 border-t border-border/15">
                <h4 class="mb-1.5 instrument-serif text-xs tracking-wide text-muted-foreground/60">
                    Breakpoints
                </h4>
                <p v-if="session.breakpoints.value.size === 0" class="text-xs italic text-muted-foreground/40">
                    Click the editor gutter to add
                </p>
                <div v-else class="space-y-px">
                    <button
                        v-for="bp in [...session.breakpoints.value]"
                        :key="bp"
                        class="flex w-full items-center gap-1.5 rounded-md px-2 py-1 text-left font-mono text-xs text-foreground/70 transition-colors hover:bg-destructive/8"
                        @click="session.toggleBreakpoint(bp)"
                    >
                        <CircleDot class="h-3 w-3 shrink-0 text-red-500" />
                        <span class="truncate">{{ bp }}</span>
                    </button>
                </div>
            </section>
        </div>
    </div>
</template>
