import { ref, onBeforeUnmount } from "vue";

export interface TypewriterWord {
    text: string;
    className: string;
    isCode?: boolean;
}

export function useTypewriter(
    words: TypewriterWord[],
    options?: {
        typingSpeed?: number;
        deletingSpeed?: number;
        pauseAfterType?: number;
        pauseAfterDelete?: number;
    },
) {
    const typingSpeed = options?.typingSpeed ?? 120;
    const deletingSpeed = options?.deletingSpeed ?? 70;
    const pauseAfterType = options?.pauseAfterType ?? 3000;
    const pauseAfterDelete = options?.pauseAfterDelete ?? 800;

    const displayText = ref("");
    const currentWordIndex = ref(0);
    const currentWord = ref<TypewriterWord>(words[0]!);
    const isDeleting = ref(false);

    let timeoutId: ReturnType<typeof setTimeout> | undefined;
    let charIndex = 0;
    let wordIdx = 0;

    function tick() {
        const word = words[wordIdx]!;
        currentWord.value = word;

        if (!isDeleting.value) {
            // Typing forward
            charIndex++;
            displayText.value = word.text.slice(0, charIndex);

            if (charIndex >= word.text.length) {
                // Finished typing — pause then start deleting
                timeoutId = setTimeout(() => {
                    isDeleting.value = true;
                    tick();
                }, pauseAfterType);
                return;
            }
            // Random variation for realism
            const jitter = Math.random() * 40 - 10;
            timeoutId = setTimeout(tick, typingSpeed + jitter);
        } else {
            // Deleting backward
            charIndex--;
            displayText.value = word.text.slice(0, charIndex);

            if (charIndex <= 0) {
                // Finished deleting — move to next word
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

    // Start the loop
    timeoutId = setTimeout(tick, 600); // initial delay before first word starts typing

    onBeforeUnmount(() => {
        clearTimeout(timeoutId);
    });

    return { displayText, currentWord, currentWordIndex, isDeleting };
}
