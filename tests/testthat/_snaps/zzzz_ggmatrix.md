# bad types

    Code
      ggpairs(tips, 1:2, lower = "blank", diag = "blank", upper = list(continuous = "BAD_TYPE"))
    Condition
      Error in `value[[3L]]()`:
      ! Error retrieving `GGally` function.
      Please provide a string such as "points" for `ggally_points()`
      For a list of all predefined functions, check out `vig_ggally("ggally_plots")`
      A custom function may be supplied directly: `wrap(my_fn, param = val)`
      Function provided: `BAD_TYPE()`

