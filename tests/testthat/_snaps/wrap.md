# errors

    Code
      wrap(fn, NA)
    Condition
      Error in `wrap()`:
      ! all parameters must be named arguments

---

    Code
      wrap(fn, y = TRUE, 5)
    Condition
      Error in `wrap()`:
      ! all parameters must be named arguments

---

    Code
      wrapp(fn, list(5))
    Condition
      Error in `wrapp()`:
      ! `params` must be a named list, named vector, or "NULL".

---

    Code
      wrapp(fn, table(1:10, 1:10))
    Condition
      Error in `wrapp()`:
      ! `params` must be a named list, named vector, or "NULL".

---

    Code
      wrapp(fn, list(A = 4, 5))
    Condition
      Error in `wrapp()`:
      ! `params` must be a named list, named vector, or "NULL".

---

    Code
      wrap("does not exist", A = 5)
    Condition
      Error in `value[[3L]]()`:
      ! Error retrieving `GGally` function.
      Please provide a string such as "points" for `ggally_points()`
      For a list of all predefined functions, check out `vig_ggally("ggally_plots")`
      A custom function may be supplied directly: `wrap(my_fn, param = val)`
      Function provided: `does not exist()`

---

    Code
      wrapp("does not exist", list(A = 5))
    Condition
      Error in `value[[3L]]()`:
      ! Error retrieving `GGally` function.
      Please provide a string such as "points" for `ggally_points()`
      For a list of all predefined functions, check out `vig_ggally("ggally_plots")`
      A custom function may be supplied directly: `wrap(my_fn, param = val)`
      Function provided: `does not exist()`

