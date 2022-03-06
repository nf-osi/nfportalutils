## Testing Framework

This uses `testthat` for testing; maintainers and contributors should read these notes carefully.
This package is largely an API package, and testing a real API is tricky.
The implementation here takes some ideas from framework for APIs/secrets described 
[here](https://gargle.r-lib.org/articles/managing-tokens-securely.html) and 
[here](https://books.ropensci.org/http-testing/security-chapter.html).   
Basically, the above suggests: 

1. We encrypt the appropriate `SYNAPSE_AUTH_TOKEN` secret with `PKG_PWD` as key.
Commit the encrypted secret as e.g. `ins/secret.rds`.
See https://github.com/r-lib/gargle/tree/main/inst/secret.  

2. Then depending on the test environment:  
a) For testing locally, store in .Renviron the variable `PKG_PWD` used to decrypt `SYNAPSE_AUTH_TOKEN`.  
b) For testing with Github CI, store `PKG_PWD` as secret in the test environment.  

3. Only if `PKG_PWD` is accessible to get decrypted `SYNAPSE_AUTH_TOKEN` are tests run.

However, there are some limitations to this approach; see especially the first reference.
To make this easier, the main adaptations are: 

- We DO NOT use a committed encrypted `SYNAPSE_AUTH_TOKEN`.
It is expected that contributors are members of the NF-OSI team and create their
own test `SYNAPSE_AUTH_TOKEN` and set that as `TEST_SYNAPSE_AUTH_TOKEN` in their .Renviron file;
this token will automatically have access to the test repo.
(Tests that write data run on assets in the test repo only.)
If you are a potential contributor who is not an NF-OSI member, 
request to be added to the test repo for writing/running tests.
**If `TEST_SYNAPSE_AUTH_TOKEN` is not available, dependent tests are simply skipped.**

- During tests, the `TEST_SYNAPSE_AUTH_TOKEN` is temporarily set as `SYNAPSE_AUTH_TOKEN` to create the synapseclient object.

- Given that most tests depend on a successful login, and that tests are run
according to the alphabetical naming of test*.R files, 
`test_auth_login.R` is named as such to ensure that it runs first
(the order configuration can be encoded formally in DESCRIPTION later if needed).
Because the testthat environment inherits from the global environment, 
the synapseclient will be available after `test_auth_login.R`.

Known issues:

- Make sure the global environment is truly cleared before running `devtools::test()`. 
If `.syn` pointer is still around from a previous call, this causes some issues. 
