# Copyright (c) Facebook, Inc. and its affiliates.

# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)
  if (reticulate::py_module_available("nevergrad")) {
    ng <<- reticulate::import("nevergrad", delay_load = TRUE)
  }
}
