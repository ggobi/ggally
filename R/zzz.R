.onLoad <- function(...) {
  registerS3method("print", "glyphplot", `_print_glyphplot`)
  registerS3method("print", "legend_guide_box", `_print_legend_guide_box`)

  if ("S7_on_load" %in% getNamespaceExports("S7")) {
    S7::S7_on_load()
  } else {
    S7::methods_register()
  }
}

.onUnload <- function(...) {
  if ("S7_on_unload" %in% getNamespaceExports("S7")) {
    S7::S7_on_unload()
  }
}

if ("S7_on_build" %in% getNamespaceExports("S7")) {
  S7::S7_on_build()
}
