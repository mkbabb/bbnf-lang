<template>
    <!-- Credit to Kevin Powell at https://codepen.io/kevinpowell/pen/PomqjxO -->
    <button class="dark-mode-toggle-button" v-bind="$attrs" @click="toggleDark()">
        <svg
            xmlns="http://www.w3.org/2000/svg"
            width="472.39"
            height="472.39"
            viewBox="0 0 472.39 472.39"
        >
            <g class="toggle-sun">
                <path
                    d="M403.21,167V69.18H305.38L236.2,0,167,69.18H69.18V167L0,236.2l69.18,69.18v97.83H167l69.18,69.18,69.18-69.18h97.83V305.38l69.18-69.18Zm-167,198.17a129,129,0,1,1,129-129A129,129,0,0,1,236.2,365.19Z"
                />
            </g>
            <g class="toggle-circle">
                <circle cx="236.2" cy="236.2" r="90" />
            </g>
        </svg>
    </button>
</template>

<script setup lang="ts">
import { useGlobalDark } from ".";

const { toggleDark } = useGlobalDark();
</script>

<style scoped>
.dark-mode-toggle-button {
    cursor: pointer;
    border: 0;
    opacity: 0.8;
    padding: 0;
    border-radius: 50%;
    position: relative;
    isolation: isolate;
    background: 0;

    transition: opacity 0.2s ease, background 0.2s ease;

    z-index: 999;

    svg {
        fill: hsl(var(--foreground));
        width: 100%;
        height: 100%;
    }

    &:hover,
    &:focus {
        outline: none;
        opacity: 1;
        background: hsl(0 0% 50% / 0.15);
    }
}

.dark-mode-toggle-button::before {
    content: "";
    display: block;
    position: absolute;
    inset: 0;
    border-radius: 50%;
    pointer-events: none;
    animation: pulseToDark 650ms ease-out;
}

.toggle-sun {
    transform-origin: center center;
    transition: transform 750ms cubic-bezier(0.11, 0.14, 0.29, 1.5);
}

.toggle-circle {
    transform: translateX(0%);
    transition: transform 500ms ease-out;
}

:global(.dark) {
    .dark-mode-toggle-button::before {
        animation: pulseToLight 650ms ease-out;
    }

    .toggle-sun {
        transform: rotate(0.5turn);
    }

    .toggle-circle {
        transform: translateX(-15%);
    }
}

@keyframes pulseToLight {
    0% {
        transform: scale(100);
        opacity: 0.5;
    }
    10% {
        transform: scale(1);
    }
    75% {
        opacity: 1;
    }
    100% {
        opacity: 0;
        transform: scale(1);
    }
}

@keyframes pulseToDark {
    0% {
        transform: scale(0);
        opacity: 0.5;
    }
    10% {
        transform: scale(1);
    }
    75% {
        opacity: 1;
    }
    100% {
        opacity: 0;
        transform: scale(1);
    }
}
</style>
