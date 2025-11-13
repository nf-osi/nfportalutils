# Logs into Synapse.

Wrapper around
https://python-docs.synapse.org/build/html/index.html#synapseclient.Synapse.login
Username and password authentication is not supported. Recommendation is
to store `SYNAPSE_AUTH_TOKEN` in environment, so login can be used
simply as `syn_login()`.

## Usage

``` r
syn_login(authtoken = Sys.getenv("SYNAPSE_AUTH_TOKEN"))
```

## Arguments

- authtoken:

  Uses `SYNAPSE_AUTH_TOKEN` environmental variable, or a personal access
  token (PAT) can be provided.

## Examples

``` r
if (FALSE) { # \dontrun{
library(nfportalutils)
syn_login()
} # }
```
