# Copyright (c) Facebook, Inc. and its affiliates.

# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

.onLoad <- function(libname, pkgname) {
  if (reticulate::py_module_available("nevergrad")) {
    # Delay load module will only be loaded when accessed via $
    ng <<- reticulate::import("nevergrad", delay_load = TRUE)
  } else {
    packageStartupMessage(paste(
      "Please, install nevergrad Python package and restart your R session to run Robyn:\n",
      'reticulate::use_condaenv("r-reticulate") # Run this once\n',
      'reticulate::conda_install("r-reticulate", "nevergrad", pip = TRUE)'))
  }
}
