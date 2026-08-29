# JHEEM2 downstream integration

This directory protects the real package boundary between `locations` and
JHEEM2 without adding JHEEM2 to `locations` package dependencies or its normal
`testthat` suite.

`REVISION` pins the exact public
[`CIPHER-Epi/jheem2`](https://github.com/CIPHER-Epi/jheem2) commit installed by
CI. `run.R` then exercises representative JHEEM2 workflows for:

- data-manager location validation, including an accepted alias and an invalid
  code;
- JHEEM entity sanitization;
- outcome-location metadata classification; and
- nested-likelihood containment, overlap, and name-to-code discovery.

The test-only R6 probes override only setup-heavy initializers or supply an
abstract method. The downstream methods under test are inherited unchanged
from the installed JHEEM2 package.

## Running locally

Use clean temporary libraries so an already-installed copy of either package
cannot hide an installation problem:

```sh
integration_library="$(mktemp -d)"
export R_LIBS_USER="$integration_library"
R CMD INSTALL --library="$integration_library" .
R CMD INSTALL --library="$integration_library" ../jheem2
Rscript integration/jheem2/run.R
```

The local JHEEM2 checkout must be at the commit recorded in `REVISION`.

## Updating the pin

Update `REVISION` in a dedicated pull request after reviewing the downstream
changes. Run this contract locally, then allow both `R-CMD-check` and the
`JHEEM2-integration` workflow to pass. A pin update is not bundled with an
unrelated `locations` behavior change, so upstream breakage remains easy to
distinguish from a candidate-package regression.
