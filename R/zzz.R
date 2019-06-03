.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to EtaModelCC R package\n
Climate Change Data of South America generated from Eta Model of CPTEC/INPE - Brazil")
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "Carlos Amaral Holbig",
    devtools.desc.author = '"Carlos Holbig <carlos.holbig@gmail.com> [aut, cre]"',
    devtools.desc.license = "BSD 3-Clause License",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])

  invisible()
}
