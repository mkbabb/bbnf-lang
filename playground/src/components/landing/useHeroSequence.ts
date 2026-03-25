import { ref, onMounted, onBeforeUnmount, type Ref } from "vue";
import { ScrollTimeline, easeOutCubic } from "@mkbabb/keyframes.js";
import type { TypewriterControl, TypewriterWord } from "./useTypewriter";
import { useHeroState } from "@/composables/useHeroState";

const snap = (v: number) => Math.round(v * 2) / 2;

/** Phase boundaries within the 0→1 scroll progress. */
const PHASE_A_END = 0.15; // force-delete current word
const PHASE_B_END = 0.40; // force-type "BBNF"
// Phase C: 0.40 → 1.0 — morph to navbar

/** Fraction of Phase C devoted to the typewriter↔logo crossfade. */
const CROSSFADE_FRAC = 0.08;

export type HeroPhase = "idle" | "deleting" | "typing" | "morphing";

/**
 * Orchestrates the hero scroll sequence: typewriter force-delete → force-type "BBNF" → morph to navbar.
 * Every frame is a pure function of scroll progress — fully reversible.
 */
export function useHeroSequence(
    typewriter: TypewriterControl,
    words: TypewriterWord[],
    morphMarkerRef: Ref<HTMLElement | null>,
    morphElementRef: Ref<HTMLElement | null>,
    navbarTargetSelector: string,
    options: {
        scrollThreshold?: number;
        damping?: number;
        /** Guard: morph needs no ancestor transforms. Set to a ref that's true once entrance anim is done. */
        entranceDone?: Ref<boolean>;
    } = {},
) {
    const bbnfIdx = words.findIndex((w) => w.text === "BBNF");
    const bbnfWord = words[bbnfIdx]!;

    const scrollProgress = ref(0);
    const phase = ref<HeroPhase>("idle");
    const morphElementOpacity = ref(0);
    const typewriterOpacity = ref(1);
    const cursorOpacity = ref(1);

    // Shared with NavBar
    const { morphProgress: sharedMorphProgress } = useHeroState();

    const timeline = new ScrollTimeline({
        threshold: options.scrollThreshold ?? 0.35,
        easing: easeOutCubic,
        smoothing: {
            damping: options.damping ?? 0.2,
            snapThreshold: 0.008,
        },
    });

    let rafId = 0;
    let lastPhase: HeroPhase = "idle";
    let frozenCharIndex = 0;
    let parentLocked = false;
    let lastMorphP = -1;

    // --- Morph transform helpers (extracted from useScrollMorph) ---

    function lockParent(el: HTMLElement) {
        const parent = el.parentElement;
        if (!parent || parentLocked) return;
        const rect = parent.getBoundingClientRect();
        parent.style.minWidth = `${rect.width}px`;
        parent.style.minHeight = `${rect.height}px`;
        parentLocked = true;
    }

    function unlockParent(el: HTMLElement) {
        const parent = el.parentElement;
        if (!parent || !parentLocked) return;
        parent.style.minWidth = "";
        parent.style.minHeight = "";
        parentLocked = false;
    }

    function clearMorphStyles(el: HTMLElement) {
        el.style.position = "";
        el.style.left = "";
        el.style.top = "";
        el.style.width = "";
        el.style.height = "";
        el.style.transform = "";
        el.style.transformOrigin = "";
        el.style.willChange = "";
        el.style.pointerEvents = "";
        el.style.zIndex = "";
    }

    function applyMorphTransform(p: number) {
        const marker = morphMarkerRef.value;
        const element = morphElementRef.value;
        const target = document.querySelector(navbarTargetSelector) as HTMLElement | null;
        if (!marker || !element || !target) return;

        if (p <= 0) {
            if (lastMorphP !== 0) {
                clearMorphStyles(element);
                unlockParent(element);
                lastMorphP = 0;
            }
            return;
        }

        if (p >= 1 && lastMorphP >= 1) return; // locked

        if (!parentLocked) lockParent(element);

        const m = marker.getBoundingClientRect();
        const t = target.getBoundingClientRect();

        const x = snap(m.x) * (1 - p) + snap(t.x) * p;
        const y = snap(m.y) * (1 - p) + snap(t.y) * p;
        const sx = m.width > 0 ? 1 + (t.width / m.width - 1) * p : 1;
        const sy = m.height > 0 ? 1 + (t.height / m.height - 1) * p : 1;

        element.style.position = "fixed";
        element.style.left = "0px";
        element.style.top = "0px";
        element.style.width = `${m.width}px`;
        element.style.height = `${m.height}px`;
        element.style.zIndex = "60";
        element.style.transform = `translate(${x}px,${y}px) scale(${sx},${sy})`;
        element.style.transformOrigin = "top left";
        element.style.willChange = p >= 1 ? "auto" : "transform";
        element.style.pointerEvents = p > 0.5 ? "none" : "";
        lastMorphP = p;
    }

    // --- rAF loop ---

    function update() {
        const p = timeline.tick();
        scrollProgress.value = p;

        if (p <= 0) {
            // Idle — resume autonomous typewriter
            phase.value = "idle";
            if (typewriter.isPaused.value) {
                typewriter.resume();
            }
            morphElementOpacity.value = 0;
            typewriterOpacity.value = 1;
            cursorOpacity.value = 1;
            sharedMorphProgress.value = 0;
            applyMorphTransform(0);
        } else if (p <= PHASE_A_END) {
            // Phase A: force-delete current word
            phase.value = "deleting";

            if (lastPhase === "idle") {
                // Snapshot char position at transition
                frozenCharIndex = typewriter.charIndex.value;
                typewriter.pause();
            }

            const sub = p / PHASE_A_END; // 0→1
            const charPos = Math.round(frozenCharIndex * (1 - sub));
            typewriter.setCharPosition(charPos);

            morphElementOpacity.value = 0;
            typewriterOpacity.value = 1;
            cursorOpacity.value = 1;
            sharedMorphProgress.value = 0;
            applyMorphTransform(0);
        } else if (p <= PHASE_B_END) {
            // Phase B: force-type "BBNF"
            phase.value = "typing";

            if (!typewriter.isPaused.value) {
                typewriter.pause();
            }

            const sub = (p - PHASE_A_END) / (PHASE_B_END - PHASE_A_END); // 0→1
            const charPos = Math.round(bbnfWord.text.length * sub);
            typewriter.forceWord(bbnfIdx, charPos);

            morphElementOpacity.value = 0;
            typewriterOpacity.value = 1;
            cursorOpacity.value = 1;
            sharedMorphProgress.value = 0;
            applyMorphTransform(0);
        } else {
            // Phase C: crossfade + morph to navbar
            phase.value = "morphing";

            if (!typewriter.isPaused.value) {
                typewriter.pause();
            }

            // Ensure BBNF is fully typed
            typewriter.forceWord(bbnfIdx, bbnfWord.text.length);

            const sub = (p - PHASE_B_END) / (1 - PHASE_B_END); // 0→1

            // Crossfade: typewriter text fades out, BbnfLogo overlay fades in
            const crossfade = Math.min(sub / CROSSFADE_FRAC, 1);
            typewriterOpacity.value = 1 - crossfade;
            cursorOpacity.value = 1 - crossfade;
            morphElementOpacity.value = crossfade;

            // Share morph progress with navbar (0→1 within Phase C)
            sharedMorphProgress.value = sub;

            // Guard: don't apply position:fixed if entrance animation isn't done
            if (!options.entranceDone || options.entranceDone.value) {
                applyMorphTransform(sub);
            }
        }

        lastPhase = phase.value;
        rafId = requestAnimationFrame(update);
    }

    onMounted(() => {
        sharedMorphProgress.value = 0;
        rafId = requestAnimationFrame(update);
    });

    onBeforeUnmount(() => {
        cancelAnimationFrame(rafId);
        const el = morphElementRef.value;
        if (el) {
            clearMorphStyles(el);
            unlockParent(el);
        }
        sharedMorphProgress.value = 1; // navbar shows logo on other pages
    });

    return {
        scrollProgress,
        phase,
        morphElementOpacity,
        typewriterOpacity,
        cursorOpacity,
    };
}
