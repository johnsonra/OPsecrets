# OPsecrets


The `OPsecrets` R package was developed because I found myself needing
to retrieve API keys and other sensitive information from 1Password for
various projects when I was developing Shiny dashboards on my laptop.
Then when the code was deployed to a Posit server, I had to make slight
changes to the code to retrieve these secrets from the local
environment.

This package is designed to provide a single tool to retrieve secrets
from either the environment or from 1Password using the `op` command
line tool. A single call to `get_secret` can be used to retrieve secrets
from an environmental variable, or if the environmental variable is
undefined, from 1Password using the `op` command line tool.

This package is designed to work on Unix-like systems (e.g. Linux,
macOS) and Windows, but it may require additional configuration on
Windows to work with 1Password.

## Installation

To install the package, you can use the `install_github` command from
the `remotes` package in R as follows (first install the `remotes`
package if you haven’t already):

    remotes::install_github("johsonra/OPsecrets")

You will also want to install the `op` command line tool from 1Password.
You can find instructions for installing the `op` command line tool
[here](https://support.1password.com/command-line-getting-started/).

### Windows

If you find that `op` isn’t in the default PATH on Windows, `get_secret`
will look for the `OP` environment variable in R for the full path to
the `op` executable.

1.  First, verify that `op` works in a Windows terminal running
    `powershell` with `op --version` as described on the [1Password
    getting-started
    page](https://support.1password.com/command-line-getting-started/).
    If this doesn’t work, review the instructions on the 1Password site
    to ensure that `op` is installed correctly.

2.  If `op` is installed correctly but `get_secret` is still having
    trouble finding `op`, you can set the `OP` environment variable in R
    to the path for the `op` executable. To find the full path on
    Windows, run `(Get-Command op).Source` in the `powershell` and copy
    the path returned (ending with “\\” and excluding “op.exe” - for
    example “C:\path\to\op\\”).

3.  To permanently add `OP` to the environment variables in R, open
    `.Renviron` with `usethis::edit_r_environ()` and add a new line with
    something like the following (making sure you put “`\\`” for each
    backslash): “`OP=C:\\path\\to\\op\\`”.

4.  Restart R and run `Sys.getenv("OP")` to verify that the `OP`
    environment variable is set correctly.

## Usage

Once you have the package installed, you will want to ensure the secrets
you want to access are stored correctly for the system you are running.
For example, for developing and testing code on my laptop, prefer to
store the secrets in 1Password. For deploying code to a Posit server, I
prefer to securely store the secrets in the environment.

### 1Password

To store a secret in 1Password, create a new item in 1Password
(typically a password, but this could also be included as part of a
login item). For example, I created a new item with the title
“OPsecrets” and a password called “test_secret” in my Private vault.

<img src="img/1password_item.png" width="400" />

### Environment

To store a secret in the environment, you can set the secret as an
environmental variable. For example, in my testing scripts I refer to
the environment variable, `SOME_SECRET`. To set this on temporarily, you
can use

    Sys.setenv(SOME_SECRET='abc123')

or to set an environmental variable permanently on a Posit shiny server,
you can do this securely using the Vars module. Open your dashboard as
an administrator in Posit, click on the settings icon at the top right
of the page and then the “Vars” module. Enter the name and value for
your environmental variable and click “Add Variable”.

<img src="img/posit_setenv.png" width="300" />

You can also set the environmental variable in the `.Renviron` file
(e.g. using `usethis::edit_r_environ()`), but this is stored in plain
text and is not secure.

### Retrieving Secrets

Once we have set up our various systems properly we can retrieve the
secret with the `get_secret` function. Following our example above, lets
say that we want to retrieve an API key that will be stored either in an
environmental variable (when run on our Posit server) or in 1Password
(when run locally).

We can do this with a single call to `get_secret`. This will first look
for the the environment variable `SOME_SECRET`. On our Posit server this
return that value. `SOME_SECRET` will not be defined when running
locally, so `get_secret` will look in the “Private” vault of 1Password
for an item called “OPsecrets” and return the value for “test_secret”.

``` r
api_key <- get_secret('SOME_SECRET', 'Private', 'OPsecrets', 'test_secret')

print(api_key)
```

    [1] "abc123"

### Acessing `op` directly

If you want to access the `op` command line tool directly, you can use
the `op` function. Again, assuming we have our API key called
“test_secret” in an item called “OPsecrets” in the “Private” vault of
1Password, we can retrieve this secret directly from 1Password with the
following command in R:

``` r
api_key <- op('Private', 'OPsecrets', 'test_secret')

print(api_key)
```

    [1] "abc123"
