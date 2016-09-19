

make_label_plot <- function(types, sectionAes, label) {
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



ggmatrix_plot_list <- (function(){
  make_diag_plot_wrapper <- function(sub_type_val) {
    plot_fn <- make_plot_wrapper(sub_type_val)

    function(types, sectionAes) {
      sectionAes$y <- NULL
      plot_fn(types, sectionAes)
    }
  }

  make_plot_wrapper <- function(sub_type_val) {
    function(types, sectionAes) {
      sub_type <- types[[sub_type_val]]
      sub_type_name <- get_subtype_name(sub_type)

      p <- make_ggmatrix_plot_obj(
        wrapp(sub_type, funcArgName = sub_type_name),
        mapping = sectionAes
      )
      return(p)
    }
  }

  na_fn <- make_plot_wrapper("na")
  na_diag_fn <- make_plot_wrapper("na")
  continuous_fn <- make_plot_wrapper("continuous")
  combo_fn <- make_plot_wrapper("combo")
  discrete_fn <- make_plot_wrapper("discrete")
  continuous_diag_fn <- make_diag_plot_wrapper("continuous")
  discrete_diag_fn <- make_diag_plot_wrapper("discrete")

  function(type) {
    switch(type,
      "na" = na_fn,
      "na-diag" = na_diag_fn,
      "continuous" = continuous_fn,
      "combo" = combo_fn,
      "discrete" = discrete_fn,
      "continuous-diag" = continuous_diag_fn,
      "discrete-diag" = discrete_diag_fn,
      "label" = make_label_plot
    )
  }
})()
