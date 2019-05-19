.onAttach <- function(libname, pkgname) {
  ## This function should not be called by the user.
  ## It displays a message when the package is being loaded.
  packageStartupMessage(## display message
    "\n Welcome to adRes,",
    "\n ",
    "\n This package has been created to document the analysis of the paper:",
    "\n Adaptive responses of animals to climate change are most likely insufficient",
    "\n by Radchuk et al. Nature Communications (2019).",
    "\n ",
    "\n Type ?adRes for information about how to use this package!",
    "\n ",
    "\n WARNING: this package has not been conceived for general use.",
    "\n"
  )
}

#' Provide some basic information about the code of the package
#'
#' This is a function for development purposes only and is thus not exported.
#' This function provides the number of functions and lines of code of the
#' package.
#'
info_package <- function() {
  print(paste("number of functions =", length(ls("package:adRes"))))
  if (requireNamespace("R.utils", quietly = TRUE)) {
    files <- dir(paste0(system.file(package = "adRes"), "/R/"))
    filenames_R <- paste0(system.file(package = "adRes"), "/R/", files)
    lines_code <- sum(sapply(filenames_R, function(file) R.utils::countLines(file)))
    print(paste("number of lines of code =", lines_code))
  } else {message("Install the package R.utils for more info.")}
  return(invisible(NULL))
}
