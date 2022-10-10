# global reference to scipy (will be initialized in .onLoad)
scipy <- NULL

.onLoad <- function(libname, pkgname) {
  # reticulate::configure_environment(pkgname)
  #
  # # use superassignment to update global reference to scipy
  # scipy <<- reticulate::import("scipy", delay_load = TRUE)
  #
  # tf <<- reticulate::import("tensorflow", delay_load = TRUE)
  # layers <<- reticulate::import("keras", delay_load = TRUE)$layers
  # activations <<- reticulate::import("keras", delay_load = TRUE)$activations
}



# tf <- import("tensorflow")
# layers <- import("keras")$layers
# activations <- import("keras")$activations
