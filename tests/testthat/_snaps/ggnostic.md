# fn_switch

    Code
      fn(dummy_dt, ggplot2::aes(value = !!as.name("B")))
    Condition
      Error in `fn()`:
      ! function could not be found for `value` or `default`. Please include one of these two keys as a function.

# error checking

    Code
      get_cols(c("not_there", ".fitted", ".se.fit", ".resid", ".std.resid", ".sigma",
        ".hat", ".cooksd"))
    Condition
      Error in `match_nostic_columns()`:
      ! Could not match `columnsY`: "not_there" to choices: "mpg", ".fitted", ".se.fit", ".resid", ".hat", ".sigma", ".cooksd", and ".std.resid"

