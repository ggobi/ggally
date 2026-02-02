vdiffr__str_standardise <- function(s) {
  sep <- "-"
  s <- gsub("[^a-z0-9]", sep, tolower(s))
  s <- gsub(paste0(sep, sep, "+"), sep, s)
  s <- gsub(paste0("^", sep, "|", sep, "$"), "", s)
  s
}

ggally_expect_doppelganger <- function(name, plot) {
  name <- vdiffr__str_standardise(name)

  # When testing locally, make sure the name is standardised with what vdiffr expects
  if (interactive()) {
    internal_str_standardise <- getFromNamespace("str_standardise", "vdiffr")
    if (!identical(internal_str_standardise(name), name)) {
      stop("Snapshot name '", name, "' is not standardised")
    }
  }

  if (
    identical(
      tolower(Sys.getenv("_R_CHECK_DEPENDS_ONLY_", "false")),
      "true"
    )
  ) {
    file <- paste0(name, ".svg")
    testthat::announce_snapshot_file(name = file)

    message("Skipping vdiffr tests on depends-only check")
    expect_true(TRUE) # Avoid empty test
    return()
  }

  if (packageVersion("ggplot2") < "3.5.2.9001") {
    # Keep snapshot around, but skip the test
    file <- paste0(name, ".svg")
    testthat::announce_snapshot_file(name = file)
    # Go through the whole process of writing the SVG
    # to ensure that the file can be created, using all the gtable code
    vdiffr::write_svg(plot, tempfile(file, fileext = ".svg"))
    expect_true(TRUE) # Avoid empty test
    return()
  }

  vdiffr::expect_doppelganger(name, plot)
}
