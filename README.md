# OPsecrets for Python

This repository contains a small Python package for retrieving secrets from environment variables or 1Password. It keeps the same small surface area:

- `get_secret(secret_name, *path_parts)` returns an environment variable when it is defined and otherwise falls back to 1Password.
- `op(*path_parts)` reads a secret directly from 1Password through the `op` CLI.

## Installation

From the repository root:

```bash
python -m pip install -e .
```

You will also need the 1Password `op` command-line tool installed and configured:

https://developer.1password.com/docs/cli/get-started/

## Usage

Store secrets in the environment when they are already available on the host system:

```python
import os

from opsecrets import get_secret

os.environ["SOME_SECRET"] = "abc123"

api_key = get_secret("SOME_SECRET")
print(api_key)
```

Or fall back to 1Password when the environment variable is not set:

```python
from opsecrets import get_secret

api_key = get_secret("SOME_SECRET", "Private", "OPsecrets", "test_secret")
print(api_key)
```

To read directly from 1Password without the environment-variable check:

```python
from opsecrets import op

api_key = op("Private", "OPsecrets", "test_secret")
print(api_key)
```

## Windows configuration

If `op` is not available on the default `PATH`, set the `OP` environment variable to the directory that contains the `op` executable.

1. Verify that `op` works in PowerShell with `op --version`.
2. If needed, run `(Get-Command op).Source` in PowerShell to find the full path.
3. Set `OP` to the directory containing `op.exe`, excluding the executable name itself.

Example:

```powershell
$env:OP = 'C:\Program Files\1Password\'
```

Or persist it for Python sessions in your environment configuration:

```text
OP=C:\Program Files\1Password\
```
