# v1_ggmatrix_theme() is deprecated

    Code
      v1_ggmatrix_theme()
    Condition
      Warning:
      `v1_ggmatrix_theme()` was deprecated in GGally 2.2.2.
      i This function will be removed in future releases.
    Output
      List of 2
       $ strip.background:List of 5
        ..$ fill         : chr "white"
        ..$ colour       : NULL
        ..$ linewidth    : NULL
        ..$ linetype     : NULL
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ strip.placement : chr "outside"
       - attr(*, "class")= chr [1:2] "theme" "gg"
       - attr(*, "complete")= logi FALSE
       - attr(*, "validate")= logi TRUE

# ggally_cor_v1_5() is deprecated

    Code
      p <- ggally_cor_v1_5(tips, ggplot2::aes(!!as.name("total_bill"), !!as.name(
        "tip")))
    Condition
      Warning:
      `ggally_cor_v1_5()` was deprecated in GGally 2.2.2.
      i Please use `ggally_cor()` instead.

