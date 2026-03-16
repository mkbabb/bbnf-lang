import { type Ref, watch, nextTick, createApp, h, defineAsyncComponent, onUnmounted } from "vue";
import CodeTabs from "@/components/docs/CodeTabs.vue";
const BenchChart = defineAsyncComponent(() => import("@/components/docs/BenchChart.vue"));
const LiveBench = defineAsyncComponent(() => import("@/components/docs/LiveBench.vue"));
const FlowChart = defineAsyncComponent(() => import("@/components/docs/FlowChart.vue"));
const RunnableCode = defineAsyncComponent(() => import("@/components/docs/RunnableCode.vue"));

/**
 * Hydrates interactive markdown components (code-tabs, bench-chart, live-bench)
 * into placeholder divs rendered by useMarkdown's custom fence handlers.
 */
export function useMarkdownComponents(containerRef: Ref<HTMLElement | null>, rendered: Ref<string>) {
    const apps: ReturnType<typeof createApp>[] = [];

    function cleanup() {
        for (const app of apps) {
            try { app.unmount(); } catch { /* already unmounted */ }
        }
        apps.length = 0;
    }

    function hydrate() {
        cleanup();
        const el = containerRef.value;
        if (!el) return;

        // Code tabs
        for (const node of el.querySelectorAll<HTMLElement>(".code-tabs-block")) {
            try {
                const tabs = JSON.parse(node.dataset.tabs ?? "[]");
                const app = createApp({ render: () => h(CodeTabs, { tabs }) });
                app.mount(node);
                apps.push(app);
            } catch { /* skip malformed */ }
        }

        // Bench charts
        for (const node of el.querySelectorAll<HTMLElement>(".bench-chart-block")) {
            try {
                const data = JSON.parse(node.dataset.chart ?? "{}");
                const app = createApp({ render: () => h(BenchChart, { data }) });
                app.mount(node);
                apps.push(app);
            } catch { /* skip malformed */ }
        }

        // Live benchmarks
        for (const node of el.querySelectorAll<HTMLElement>(".live-bench-block")) {
            try {
                const config = JSON.parse(node.dataset.bench ?? "{}");
                const app = createApp({ render: () => h(LiveBench, { config }) });
                app.mount(node);
                apps.push(app);
            } catch { /* skip malformed */ }
        }

        // Flow charts
        for (const node of el.querySelectorAll<HTMLElement>(".flow-chart-block")) {
            try {
                const data = JSON.parse(node.dataset.flow ?? "{}");
                const app = createApp({ render: () => h(FlowChart, { data }) });
                app.mount(node);
                apps.push(app);
            } catch { /* skip malformed */ }
        }

        // Runnable code examples
        for (const node of el.querySelectorAll<HTMLElement>(".runnable-code-block")) {
            try {
                const data = JSON.parse(node.dataset.runnable ?? "{}");
                const app = createApp({ render: () => h(RunnableCode, { data }) });
                app.mount(node);
                apps.push(app);
            } catch { /* skip malformed */ }
        }
    }

    watch(rendered, () => {
        nextTick(hydrate);
    }, { immediate: true });

    onUnmounted(cleanup);

    return { hydrate };
}
