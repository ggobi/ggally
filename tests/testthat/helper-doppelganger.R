ggally_expect_doppelganger <- function(name, plot) {
  if (packageVersion("ggplot2") < "3.5.2.9001") {
    # Keep snapshot around, but skip the test
    vdiffr__str_standardise <- getFromNamespace("str_standardise", "vdiffr")
    file <- paste0(
      vdiffr__str_standardise(name),
      ".svg"
    )
    testthat::announce_snapshot_file(name = file)
    # Go through the whole process of writing the SVG
    # to ensure that the file can be created, using all the gtable code
    vdiffr::write_svg(plot, tempfile(file, fileext = ".svg"))
    expect_true(TRUE) # Avoid empty test
  } else {
    vdiffr::expect_doppelganger(name, plot)
  }
}
