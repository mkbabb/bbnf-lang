import json
from pathlib import Path
import subprocess
import unittest

from json_oracle.governance import (
    BASE_ENVIRONMENT,
    NODE,
    PYTHON,
    PYTHON_FLAGS,
    child_environment,
    node_argv,
    python_code_argv,
    python_module_argv,
)


ROOT = Path(__file__).resolve().parents[2]


class GovernanceTests(unittest.TestCase):
    def test_outer_launcher_rejects_every_unbound_startup_control(self):
        controls = (
            "PYTHONOPTIMIZE",
            "NODE_OPTIONS",
            "NODE_PATH",
            "PYTHONHOME",
            "PYTHONPATH",
            "PYTHONSTARTUP",
            "PYTHONINSPECT",
            "PYTHONWARNINGS",
            "PYTHONPYCACHEPREFIX",
            "PYTHONUTF8",
            "PYTHONSAFEPATH",
            "PYTHONNOUSERSITE",
            "PYTHONDONTWRITEBYTECODE",
            "NODE_EXTRA_CA_CERTS",
            "NODE_ICU_DATA",
            "OPENSSL_CONF",
            "SSL_CERT_FILE",
            "SSL_CERT_DIR",
            "LD_PRELOAD",
        )
        for control in controls:
            environment = child_environment()
            environment[control] = "hostile"
            result = subprocess.run(
                [str(ROOT / "verify"), "--describe-runtime"],
                capture_output=True,
                text=True,
                env=environment,
            )
            with self.subTest(control=control):
                self.assertEqual(result.returncode, 78)
                self.assertEqual(result.stdout, "")
                self.assertEqual(result.stderr, "governed startup environment rejected\n")

    def test_platform_stripped_loader_controls_cannot_reach_python(self):
        environment = child_environment()
        environment.update(
            DYLD_INSERT_LIBRARIES="hostile",
            DYLD_LIBRARY_PATH="hostile",
            DYLD_FRAMEWORK_PATH="hostile",
        )
        result = subprocess.run(
            [str(ROOT / "verify"), "--describe-runtime"],
            check=True,
            capture_output=True,
            text=True,
            env=environment,
        )
        runtime = json.loads(result.stdout)
        self.assertEqual(runtime["python"]["startup"]["environment"], BASE_ENVIRONMENT)

    def test_direct_python_startup_flag_mutations_die_at_entry(self):
        mutations = {
            "optimize": ["-O", *PYTHON_FLAGS],
            "not_isolated": [flag for flag in PYTHON_FLAGS if flag != "-I"],
            "site_enabled": [flag for flag in PYTHON_FLAGS if flag != "-S"],
            "bytecode_enabled": [flag for flag in PYTHON_FLAGS if flag != "-B"],
            "utf8_unbound": list(PYTHON_FLAGS[:-2]),
        }
        for name, flags in mutations.items():
            result = subprocess.run(
                [PYTHON, *flags, str(ROOT / "verify.py"), "--describe-runtime"],
                capture_output=True,
                text=True,
                env=child_environment(),
            )
            with self.subTest(name=name):
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("governed Python startup mismatch", result.stderr)

    def test_one_minimal_environment_and_exact_child_argv(self):
        self.assertEqual(child_environment(), BASE_ENVIRONMENT)
        with self.assertRaises(ValueError):
            child_environment({"NODE_OPTIONS": "--import=hostile"})
        self.assertEqual(node_argv("--version"), [NODE, "--version"])
        self.assertEqual(
            python_module_argv(ROOT, "json_oracle.cli", "input.json")[:6],
            [PYTHON, *PYTHON_FLAGS],
        )

        code = "import json,os;print(json.dumps(dict(os.environ),sort_keys=True))"
        python = subprocess.run(
            python_code_argv(ROOT, code),
            check=True,
            capture_output=True,
            text=True,
            env=child_environment(),
        )
        node = subprocess.run(
            node_argv("-p", "JSON.stringify(process.env)"),
            check=True,
            capture_output=True,
            text=True,
            env=child_environment(),
        )
        self.assertEqual(json.loads(python.stdout), BASE_ENVIRONMENT)
        self.assertEqual(json.loads(node.stdout), BASE_ENVIRONMENT)


if __name__ == "__main__":
    unittest.main()
