.onAttach <- function(libname, pkgname)
{
  packageStartupMessage(paste("Have fun diffusing with Rcpp and RcppEigen!",
                              "Consider forking 'diffusr' on GitHub."))
}
