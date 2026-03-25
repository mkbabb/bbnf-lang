import { ref, onBeforeUnmount, type Ref } from "vue";

export interface TypewriterWord {
    text: string;
    className: string;
    isCode?: boolean;
}

export interface TypewriterControl {
    displayText: Ref<string>;
    currentWord: Ref<TypewriterWord>;
    currentWordIndex: Ref<number>;
    isDeleting: Ref<boolean>;
    charIndex: Ref<number>;
    isPaused: Ref<boolean>;
    pause(): void;
    resume(): void;
    setCharPosition(n: number): void;
    forceWord(idx: number, charPos: number): void;
}

export function useTypewriter(
    words: TypewriterWord[],
    options?: {
        typingSpeed?: number;
        deletingSpeed?: number;
        pauseAfterType?: number;
        pauseAfterDelete?: number;
    },
): TypewriterControl {
    const typingSpeed = options?.typingSpeed ?? 120;
    const deletingSpeed = options?.deletingSpeed ?? 70;
    const pauseAfterType = options?.pauseAfterType ?? 3000;
    const pauseAfterDelete = options?.pauseAfterDelete ?? 800;

    const displayText = ref("");
    const currentWordIndex = ref(0);
    const currentWord = ref<TypewriterWord>(words[0]!);
    const isDeleting = ref(false);
    const charIndex = ref(0);
    const isPaused = ref(false);

    let timeoutId: ReturnType<typeof setTimeout> | undefined;
    let wordIdx = 0;

    function clearPending() {
        if (timeoutId !== undefined) {
            clearTimeout(timeoutId);
            timeoutId = undefined;
        }
    }

    function tick() {
        if (isPaused.value) return;

        const word = words[wordIdx]!;
        currentWord.value = word;

        if (!isDeleting.value) {
            charIndex.value++;
            displayText.value = word.text.slice(0, charIndex.value);

            if (charIndex.value >= word.text.length) {
                timeoutId = setTimeout(() => {
                    isDeleting.value = true;
                    tick();
                }, pauseAfterType);
                return;
            }
            const jitter = Math.random() * 40 - 10;
            timeoutId = setTimeout(tick, typingSpeed + jitter);
        } else {
            charIndex.value--;
            displayText.value = word.text.slice(0, charIndex.value);

            if (charIndex.value <= 0) {
                isDeleting.value = false;
                wordIdx = (wordIdx + 1) % words.length;
                currentWordIndex.value = wordIdx;
                timeoutId = setTimeout(tick, pauseAfterDelete);
                return;
            }
            const jitter = Math.random() * 20;
            timeoutId = setTimeout(tick, deletingSpeed + jitter);
        }
    }

    function pause() {
        clearPending();
        isPaused.value = true;
    }

    function resume() {
        if (!isPaused.value) return;
        isPaused.value = false;
        tick();
    }

    function setCharPosition(n: number) {
        const word = words[wordIdx]!;
        charIndex.value = Math.max(0, Math.min(n, word.text.length));
        displayText.value = word.text.slice(0, charIndex.value);
    }

    function forceWord(idx: number, charPos: number) {
        wordIdx = idx;
        currentWordIndex.value = idx;
        currentWord.value = words[idx]!;
        charIndex.value = Math.max(0, Math.min(charPos, words[idx]!.text.length));
        displayText.value = words[idx]!.text.slice(0, charIndex.value);
    }

    // Start the autonomous loop
    timeoutId = setTimeout(tick, 600);

    onBeforeUnmount(() => {
        clearPending();
    });

    return {
        displayText,
        currentWord,
        currentWordIndex,
        isDeleting,
        charIndex,
        isPaused,
        pause,
        resume,
        setCharPosition,
        forceWord,
    };
}
