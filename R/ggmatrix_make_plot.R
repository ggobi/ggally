ggmatrix_na_plot <- function(plotType, types, sectionAes) {
  subType <- types$na
  subTypeName <- get_subtype_name(subType)
  p <- make_ggmatrix_plot_obj(
    wrapp(subType, funcArgName = subTypeName),
    mapping = sectionAes
  )
  return(p)
}


ggmatrix_continuous_plot <- function(plotType, types, sectionAes) {
  subType <- types$continuous
  subTypeName <- get_subtype_name(subType)

  if (identical(subTypeName, "ggally_density")) {
    sectionAes <- add_and_overwrite_aes(sectionAes, aes_string(group = sectionAes$colour))
  }

  p <- make_ggmatrix_plot_obj(
    wrapp(subType, funcArgName = subTypeName),
    mapping = sectionAes
  )
  return(p)

}


ggmatrix_combo_plot <- function(plotType, types, sectionAes) {
  subType <- types$combo
  subTypeName <- get_subtype_name(subType)

  # isCombo
  if ( ! (
    identical(subTypeName, "ggally_dot") ||
    identical(subTypeName, "ggally_facetdensity")
  ) ) {
    sectionAes <- mapping_color_to_fill(sectionAes)
  }

  p <- make_ggmatrix_plot_obj(
    wrapp(subType, funcArgName = subTypeName),
    mapping = sectionAes
  )
  return(p)
}


ggmatrix_discrete_plot <- function(plotType, types, sectionAes) {
  subType <- types$discrete
  subTypeName <- get_subtype_name(subType)

  if (identical(subTypeName, "ggally_facetbar")) {
    if (!is.null(sectionAes$colour)) {
      sectionAes <- add_and_overwrite_aes(sectionAes, aes_string(fill = sectionAes$colour))
    }
  }
  p <- make_ggmatrix_plot_obj(
    wrapp(subType, funcArgName = subTypeName),
    mapping = sectionAes
  )
  return(p)
}


ggmatrix_continuous_diag_plot <- function(plotType, types, sectionAes) {
  sectionAes$y <- NULL

  subType <- types$continuous
  subTypeName <- get_subtype_name(subType)

  if (! identical(subTypeName, "ggally_density")) {
    sectionAes <- mapping_color_to_fill(sectionAes)
  }

  p <- make_ggmatrix_plot_obj(
    wrapp(subType, funcArgName = subTypeName),
    mapping = sectionAes
  )
  return(p)
}


ggmatrix_discrete_diag_plot <- function(plotType, types, sectionAes) {
  sectionAes$y <- NULL

  subType <- types$discrete
  subTypeName <- get_subtype_name(subType)

  sectionAes <- mapping_color_to_fill(sectionAes)

  p <- make_ggmatrix_plot_obj(
    wrapp(subType, funcArgName = subTypeName),
    mapping = sectionAes
  )
  return(p)
}


ggmatrix_label_plot <- function(plotType, types, sectionAes, label) {
  sectionAes$y <- NULL

  p <- make_ggmatrix_plot_obj(
    wrapp(
      "diagAxis",
      params = c("label" = label),
      funcArgName = "ggally_diagAxis"
    ),
    mapping = sectionAes
  )
  return(p)
}


ggmatrix_plot_list <- function(type) {
  switch(type,
    "na" = ggmatrix_na_plot,
    "na-diag" = ggmatrix_na_plot,
    "continuous" = ggmatrix_continuous_plot,
    "combo" = ggmatrix_combo_plot,
    "discrete" = ggmatrix_discrete_plot,
    "continuous-diag" = ggmatrix_continuous_diag_plot,
    "discrete-diag" = ggmatrix_discrete_diag_plot,
    "label" = ggmatrix_label_plot
  )
}
