.onLoad <- function(...) {
  registerS3method("print", "glyphplot", `_print_glyphplot`)
  registerS3method("print", "legend_guide_box", `_print_legend_guide_box`)

  S7::methods_register()
}
