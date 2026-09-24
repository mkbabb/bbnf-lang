/**
 * 128-bit ASCII character set (bitset over code points 0..127).
 */
export class CharSet {
    private bits: Uint32Array;

    constructor() {
        this.bits = new Uint32Array(4); // 128 bits
    }

    add(code: number): void {
        if (code >= 0 && code < 128) {
            this.bits[code >> 5] |= 1 << (code & 31);
        }
    }

    has(code: number): boolean {
        if (code < 0 || code >= 128) return false;
        return (this.bits[code >> 5] & (1 << (code & 31))) !== 0;
    }

    addRange(from: number, to: number): void {
        for (let i = from; i <= to && i < 128; i++) {
            this.add(i);
        }
    }

    union(other: CharSet): void {
        this.bits[0] |= other.bits[0];
        this.bits[1] |= other.bits[1];
        this.bits[2] |= other.bits[2];
        this.bits[3] |= other.bits[3];
    }

    isDisjoint(other: CharSet): boolean {
        return (
            (this.bits[0] & other.bits[0]) === 0 &&
            (this.bits[1] & other.bits[1]) === 0 &&
            (this.bits[2] & other.bits[2]) === 0 &&
            (this.bits[3] & other.bits[3]) === 0
        );
    }

    isEmpty(): boolean {
        return (
            this.bits[0] === 0 &&
            this.bits[1] === 0 &&
            this.bits[2] === 0 &&
            this.bits[3] === 0
        );
    }

    /**
     * Returns a new CharSet containing only characters present in both sets.
     */
    intersection(other: CharSet): CharSet {
        const result = new CharSet();
        result.bits[0] = this.bits[0] & other.bits[0];
        result.bits[1] = this.bits[1] & other.bits[1];
        result.bits[2] = this.bits[2] & other.bits[2];
        result.bits[3] = this.bits[3] & other.bits[3];
        return result;
    }

    /**
     * Number of characters in the set (popcount).
     */
    len(): number {
        let count = 0;
        for (let i = 0; i < 4; i++) {
            let w = this.bits[i];
            // Brian Kernighan's popcount
            while (w) {
                w &= w - 1;
                count++;
            }
        }
        return count;
    }

    /**
     * Iterate over all character codes present in the set.
     */
    *[Symbol.iterator](): Generator<number> {
        for (let code = 0; code < 128; code++) {
            if (this.has(code)) yield code;
        }
    }

    clone(): CharSet {
        const c = new CharSet();
        c.bits.set(this.bits);
        return c;
    }
}
