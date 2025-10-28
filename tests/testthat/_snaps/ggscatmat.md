# stops

    Code
      ggscatmat(flea, columns = c(1, 2))
    Condition
      Error in `ggscatmat()`:
      ! Not enough numeric variables to make a scatter plot matrix

---

    Code
      ggscatmat(flea, columns = c(1, 1, 1))
    Condition
      Error in `ggscatmat()`:
      ! All of your variables are factors. Need numeric variables to make scatter plot matrix.

---

    Code
      scatmat(flea, columns = c(1, 1, 1))
    Condition
      Error in `scatmat()`:
      ! All of your variables are factors. Need numeric variables to make scatterplot matrix.

