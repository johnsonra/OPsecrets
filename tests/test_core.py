import subprocess
import unittest
from unittest import mock

from opsecrets import get_secret, op


class GetSecretTests(unittest.TestCase):
    def test_get_secret_prefers_environment(self) -> None:
        with mock.patch.dict("os.environ", {"SOME_SECRET": "abc123"}, clear=True):
            with mock.patch("opsecrets.core.op") as op_mock:
                self.assertEqual(get_secret("SOME_SECRET", "Private", "Item", "field"), "abc123")
                op_mock.assert_not_called()

    def test_get_secret_requires_path_parts_when_environment_missing(self) -> None:
        with mock.patch.dict("os.environ", {}, clear=True):
            with self.assertRaisesRegex(
                ValueError,
                "Secret not found and no additional arguments were provided for `op`",
            ):
                get_secret("MISSING_SECRET")

    def test_get_secret_falls_back_to_op(self) -> None:
        with mock.patch.dict("os.environ", {}, clear=True):
            with mock.patch("opsecrets.core.op", return_value="abc123") as op_mock:
                self.assertEqual(
                    get_secret("SOME_SECRET", "Private", "OPsecrets", "test_secret"),
                    "abc123",
                )
                op_mock.assert_called_once_with("Private", "OPsecrets", "test_secret")


class OpTests(unittest.TestCase):
    def test_op_builds_expected_command(self) -> None:
        completed_process = subprocess.CompletedProcess(
            args=["op", "read", "op://Private/OPsecrets/test_secret"],
            returncode=0,
            stdout="abc123\n",
            stderr="",
        )

        with mock.patch.dict("os.environ", {}, clear=True):
            with mock.patch("opsecrets.core.subprocess.run", return_value=completed_process) as run_mock:
                self.assertEqual(op("Private", "OPsecrets", "test_secret"), "abc123")
                run_mock.assert_called_once_with(
                    ["op", "read", "op://Private/OPsecrets/test_secret"],
                    capture_output=True,
                    check=True,
                    text=True,
                )

    def test_op_uses_windows_override_directory(self) -> None:
        completed_process = subprocess.CompletedProcess(
            args=[],
            returncode=0,
            stdout="abc123",
            stderr="",
        )

        with mock.patch.dict("os.environ", {"OP": "C:\\Program Files\\1Password\\"}, clear=True):
            with mock.patch("opsecrets.core.os.name", "nt"):
                with mock.patch("opsecrets.core.subprocess.run", return_value=completed_process) as run_mock:
                    op("Private", "OPsecrets", "test_secret")
                    run_mock.assert_called_once_with(
                        [r"C:\Program Files\1Password\op.exe", "read", "op://Private/OPsecrets/test_secret"],
                        capture_output=True,
                        check=True,
                        text=True,
                    )

    def test_op_raises_runtime_error_when_cli_call_fails(self) -> None:
        with mock.patch.dict("os.environ", {}, clear=True):
            with mock.patch(
                "opsecrets.core.subprocess.run",
                side_effect=subprocess.CalledProcessError(
                    returncode=1,
                    cmd=["op", "read", "op://Private/OPsecrets/test_secret"],
                ),
            ):
                with self.assertRaisesRegex(RuntimeError, "Error retrieving item from 1Password"):
                    op("Private", "OPsecrets", "test_secret")


if __name__ == "__main__":
    unittest.main()
