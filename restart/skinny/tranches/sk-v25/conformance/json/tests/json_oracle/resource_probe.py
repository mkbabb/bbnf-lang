"""Fresh-process resource probe for the exact Gestalt-4 counterexample."""

import hashlib
import json
import os
from pathlib import Path
import subprocess
import tempfile
import time

from json_oracle.governance import child_environment, python_module_argv


ROOT = Path(__file__).resolve().parents[2]


def unique_lone_surrogate_keys() -> bytes:
    entry_count = 29_999
    extra_units = 131_072 - entry_count * 2

    def key(index: int) -> bytes:
        first = 0xD800 + index // 2048
        second = 0xD800 + index % 2048
        return f"\\u{first:04X}\\u{second:04X}".encode("ascii")

    members = [b'"' + key(0) + b"\\uD800" * extra_units + b'":0']
    members.extend(b'"' + key(index) + b'":0' for index in range(1, entry_count))
    source = b"{" + b",".join(members) + b"}"
    return source + b" " * (1_048_576 - len(source))


def measure(samples: int = 3) -> list[dict[str, object]]:
    source = unique_lone_surrogate_keys()
    if len(source) != 1_048_576:
        raise AssertionError("counterexample input size drift")
    results = []
    with tempfile.TemporaryDirectory(prefix="json-w0-g4-rss-") as directory:
        temporary = Path(directory)
        path = temporary / "unique-lone-surrogate-keys.json"
        path.write_bytes(source)
        for sample in range(1, samples + 1):
            stdout = temporary / f"sample-{sample}.stdout"
            stderr = temporary / f"sample-{sample}.stderr"
            started = time.monotonic()
            with stdout.open("wb") as output, stderr.open("wb") as errors:
                process = subprocess.Popen(
                    python_module_argv(ROOT, "json_oracle.cli", str(path)),
                    stdout=output,
                    stderr=errors,
                    env=child_environment(),
                )
                _, status, usage = os.wait4(process.pid, 0)
                process.returncode = os.waitstatus_to_exitcode(status)
            output = stdout.read_bytes()
            results.append(
                {
                    "counters": json.loads(output)["counters"],
                    "exit": process.returncode,
                    "rss_bytes": usage.ru_maxrss,
                    "sample": sample,
                    "stderr": stderr.read_text(),
                    "stdout_bytes": len(output),
                    "stdout_sha256": hashlib.sha256(output).hexdigest(),
                    "wall_seconds": round(time.monotonic() - started, 6),
                }
            )
    return results


if __name__ == "__main__":
    print(json.dumps(measure(), sort_keys=True, separators=(",", ":")))
