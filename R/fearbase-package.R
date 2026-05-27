#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end

# Import the core plotting and data-manipulation packages at package scope.
# The project intentionally exposes several historical functions that use
# unqualified tidyverse calls, so these package-level imports keep the
# namespace stable while individual functions continue to use explicit `pkg::`
# calls where they already do so.
#' @import dplyr
#' @import ggplot2
#' @import patchwork
NULL
