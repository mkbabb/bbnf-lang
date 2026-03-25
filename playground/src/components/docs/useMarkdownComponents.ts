import { type Ref, watch, nextTick, createApp, h, defineAsyncComponent, onUnmounted } from "vue";
import CodeTabs from "@/components/docs/CodeTabs.vue";
import { lookupTerm, CATEGORY_COLORS } from "@/lib/termRegistry";
const BenchChart = defineAsyncComponent(() => import("@/components/docs/BenchChart.vue"));
const LiveBench = defineAsyncComponent(() => import("@/components/docs/LiveBench.vue"));
const FlowChart = defineAsyncComponent(() => import("@/components/docs/FlowChart.vue"));
const RunnableCode = defineAsyncComponent(() => import("@/components/docs/RunnableCode.vue"));

// ── Shared tooltip element ──────────────────────────────────────

let tooltipEl: HTMLDivElement | null = null;
let tooltipShowTimer: ReturnType<typeof setTimeout> | null = null;
let tooltipHideTimer: ReturnType<typeof setTimeout> | null = null;

function getTooltipEl(): HTMLDivElement {
    if (tooltipEl) return tooltipEl;
    tooltipEl = document.createElement("div");
    tooltipEl.className = "term-tooltip";
    tooltipEl.addEventListener("mouseenter", () => {
        if (tooltipHideTimer) { clearTimeout(tooltipHideTimer); tooltipHideTimer = null; }
    });
    tooltipEl.addEventListener("mouseleave", () => {
        hideTooltip();
    });
    document.body.appendChild(tooltipEl);
    return tooltipEl;
}

function showTooltip(target: HTMLElement, key: string) {
    const term = lookupTerm(key);
    if (!term) return;

    if (tooltipHideTimer) { clearTimeout(tooltipHideTimer); tooltipHideTimer = null; }
    if (tooltipShowTimer) { clearTimeout(tooltipShowTimer); tooltipShowTimer = null; }

    tooltipShowTimer = setTimeout(() => {
        const el = getTooltipEl();
        const color = CATEGORY_COLORS[term.category];

        let linkHtml = "";
        if (term.docSlug) {
            const href = `/docs/${term.docSlug}${term.docAnchor ? `#${term.docAnchor}` : ""}`;
            linkHtml = `<a class="term-tooltip-link" href="${href}">Learn more &rarr;</a>`;
        }

        el.innerHTML = `
            <span class="term-tooltip-badge" style="color: var(--color-${color}); background: color-mix(in srgb, var(--color-${color}) 12%, transparent); border: 1px solid color-mix(in srgb, var(--color-${color}) 25%, transparent);">${term.category}</span>
            <div class="term-tooltip-desc">${term.description}</div>
            ${linkHtml}
        `;

        // Position above target, centered
        const rect = target.getBoundingClientRect();
        el.style.visibility = "hidden";
        el.classList.add("visible");

        // Measure tooltip size
        requestAnimationFrame(() => {
            const tipRect = el.getBoundingClientRect();
            let left = rect.left + rect.width / 2 - tipRect.width / 2;
            let top = rect.top - tipRect.height - 8;

            // Viewport clamping
            const vw = window.innerWidth;
            const vh = window.innerHeight;
            left = Math.max(8, Math.min(left, vw - tipRect.width - 8));
            if (top < 8) {
                top = rect.bottom + 8; // flip below
            }
            top = Math.max(8, Math.min(top, vh - tipRect.height - 8));

            el.style.left = `${left}px`;
            el.style.top = `${top}px`;
            el.style.visibility = "";
        });
    }, 150);
}

function hideTooltip() {
    if (tooltipShowTimer) { clearTimeout(tooltipShowTimer); tooltipShowTimer = null; }
    if (tooltipHideTimer) { clearTimeout(tooltipHideTimer); tooltipHideTimer = null; }
    tooltipHideTimer = setTimeout(() => {
        tooltipEl?.classList.remove("visible");
    }, 100);
}

// ── Tooltip hydration ───────────────────────────────────────────

type Cleanup = () => void;

function hydrateTermTooltips(container: HTMLElement): Cleanup {
    const cleanups: (() => void)[] = [];

    for (const node of container.querySelectorAll<HTMLElement>("code.has-tooltip")) {
        const key = node.dataset.termKey;
        if (!key) continue;

        const onEnter = () => showTooltip(node, key);
        const onLeave = () => hideTooltip();
        const onClick = (e: Event) => {
            // Touch fallback: toggle tooltip on tap
            if ("ontouchstart" in window) {
                e.preventDefault();
                const el = getTooltipEl();
                if (el.classList.contains("visible")) {
                    hideTooltip();
                } else {
                    showTooltip(node, key);
                }
            }
        };

        node.addEventListener("mouseenter", onEnter);
        node.addEventListener("mouseleave", onLeave);
        node.addEventListener("click", onClick);

        cleanups.push(() => {
            node.removeEventListener("mouseenter", onEnter);
            node.removeEventListener("mouseleave", onLeave);
            node.removeEventListener("click", onClick);
        });
    }

    // Click-outside handler for touch devices
    const onClickOutside = (e: MouseEvent) => {
        if (tooltipEl && !tooltipEl.contains(e.target as Node)) {
            const target = e.target as HTMLElement;
            if (!target.classList?.contains("has-tooltip")) {
                hideTooltip();
            }
        }
    };
    document.addEventListener("click", onClickOutside, true);
    cleanups.push(() => document.removeEventListener("click", onClickOutside, true));

    return () => {
        for (const fn of cleanups) fn();
        cleanups.length = 0;
    };
}

// ── Main composable ─────────────────────────────────────────────

/**
 * Hydrates interactive markdown components (code-tabs, bench-chart, live-bench)
 * into placeholder divs rendered by useMarkdown's custom fence handlers.
 */
export function useMarkdownComponents(containerRef: Ref<HTMLElement | null>, rendered: Ref<string>) {
    const apps: ReturnType<typeof createApp>[] = [];
    let tooltipCleanup: Cleanup | null = null;

    function cleanup() {
        tooltipCleanup?.();
        tooltipCleanup = null;
        for (const app of apps) {
            try { app.unmount(); } catch { /* already unmounted */ }
        }
        apps.length = 0;
        // Hide any open tooltip
        tooltipEl?.classList.remove("visible");
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

        // Term tooltips
        tooltipCleanup = hydrateTermTooltips(el);
    }

    watch(rendered, () => {
        nextTick(hydrate);
    }, { immediate: true });

    onUnmounted(cleanup);

    return { hydrate };
}
