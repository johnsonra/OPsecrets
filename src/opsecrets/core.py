"""Helpers for retrieving secrets from the environment or 1Password."""

from __future__ import annotations

import os
import subprocess
from pathlib import PurePosixPath, PureWindowsPath

_OP_ERROR = (
    "Error retrieving item from 1Password. Please verify that `op` is installed "
    "and the correct item path is provided. See package documentation for more "
    "troubleshooting options."
)


def get_secret(secret_name: str, *path_parts: str) -> str:
    """Return an environment variable or fall back to 1Password."""
    secret_value = os.getenv(secret_name)
    if secret_value:
        return secret_value

    if not path_parts:
        raise ValueError(
            "Secret not found and no additional arguments were provided for `op`"
        )

    return op(*path_parts)


def op(*path_parts: str) -> str:
    """Read a secret directly from 1Password using the `op` CLI."""
    if not path_parts:
        raise ValueError("At least one 1Password path component is required.")

    command = [_op_executable(), "read", f"op://{'/'.join(path_parts)}"]

    try:
        result = subprocess.run(
            command,
            capture_output=True,
            check=True,
            text=True,
        )
    except (FileNotFoundError, subprocess.CalledProcessError) as exc:
        raise RuntimeError(_OP_ERROR) from exc

    return result.stdout.rstrip("\r\n")


def _op_executable() -> str:
    override = os.getenv("OP", "")
    if not override:
        return "op"

    trimmed_override = override.rstrip("\\/")
    lowered_override = trimmed_override.lower()
    if lowered_override.endswith("op") or lowered_override.endswith("op.exe"):
        return trimmed_override

    if os.name == "nt":
        return str(PureWindowsPath(trimmed_override) / "op.exe")

    return str(PurePosixPath(trimmed_override) / "op")
