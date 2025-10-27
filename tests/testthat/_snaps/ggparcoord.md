# stops

    Code
      ggparcoord(data = diamonds.samp, columns = c(1, 5:10), groupColumn = NULL,
      order = "anyClass")
    Condition
      Error in `ggparcoord()`:
      ! can't use the `order` methods "anyClass" or "allClass" without specifying groupColumn

---

    Code
      ggparcoord(data = diamonds.samp, columns = c(1, 5:10), groupColumn = NULL,
      order = "allClass")
    Condition
      Error in `ggparcoord()`:
      ! can't use the `order` methods "anyClass" or "allClass" without specifying groupColumn

---

    Code
      ggparcoord(data = diamonds.samp, columns = c(1, 5:10), groupColumn = c(1, 2))
    Condition
      Error in `ggparcoord()`:
      ! invalid value for `groupColumn`; must be a single numeric or character index

---

    Code
      ggparcoord(data = diamonds.samp, columns = c(1, 5:10), groupColumn = 0+1i)
    Condition
      Error in `ggparcoord()`:
      ! invalid value for `groupColumn`; must be a single numeric or character index

---

    Code
      ggparcoord(data = diamonds.samp, columns = c(1, 5:10), groupColumn = 2, scale = "notValid")
    Condition
      Error in `ggparcoord()`:
      ! invalid value for `scale`; must be one of "std", "robust", "uniminmax", "globalminmax", "center", or "centerObs".

---

    Code
      ggparcoord(data = diamonds.samp, columns = c(1, 5:10), groupColumn = 2,
      centerObsID = nrow(diamonds.samp) + 10)
    Condition
      Error in `ggparcoord()`:
      ! invalid value for `centerObsID`; must be a single numeric row index

---

    Code
      ggparcoord(data = diamonds.samp, columns = c(1, 5:10), groupColumn = 2,
      missing = "notValid")
    Condition
      Error in `ggparcoord()`:
      ! invalid value for `missing`; must be one of "exclude", "mean", "median", "min10", "random".

---

    Code
      ggparcoord(data = diamonds.samp, columns = c(1, 5:10), groupColumn = 2, order = "notValid")
    Condition
      Error in `ggparcoord()`:
      ! invalid value for `order`
      i must either be a vector of column indices or one of `skewness`, `allClass`, `anyClass`, `Outlying`, `Skewed`, `Clumpy`, `Sparse`, `Striated`, `Convex`, `Skinny`, `Stringy`, or `Monotonic`

---

    Code
      ggparcoord(data = diamonds.samp, columns = c(1, 5:10), groupColumn = 2, order = 0+1i)
    Condition
      Error in `ggparcoord()`:
      ! invalid value for `order`
      i must either be a vector of column indices or one of `skewness`, `allClass`, `anyClass`, `Outlying`, `Skewed`, `Clumpy`, `Sparse`, `Striated`, `Convex`, `Skinny`, `Stringy`, or `Monotonic`

---

    Code
      ggparcoord(data = diamonds.samp, columns = c(1, 5:10), groupColumn = 2,
      showPoints = 1)
    Condition
      Error in `ggparcoord()`:
      ! invalid value for `showPoints`; must be a logical operator

---

    Code
      ggparcoord(data = diamonds.samp, columns = c(1, 5:10), groupColumn = 2,
      alphaLines = "notAColumn")
    Condition
      Error in `ggparcoord()`:
      ! `alphaLines` column is missing in data

---

    Code
      ggparcoord(data = tmpDt, columns = c(1, 5:10), groupColumn = 2, alphaLines = "price")
    Condition
      Error in `ggparcoord()`:
      ! missing data in `alphaLines` column

---

    Code
      ggparcoord(data = diamonds.samp, columns = c(1, 5:10), groupColumn = 2,
      alphaLines = "price")
    Condition
      Error in `ggparcoord()`:
      ! invalid value for `alphaLines` column; max range must be from 0 to 1

---

    Code
      ggparcoord(data = diamonds.samp, columns = c(1, 5:10), groupColumn = 2,
      alphaLines = -0.1)
    Condition
      Error in `ggparcoord()`:
      ! invalid value for `alphaLines`; must be a scalar value between 0 and 1

---

    Code
      ggparcoord(data = diamonds.samp, columns = c(1, 5:10), groupColumn = 2,
      alphaLines = 1.1)
    Condition
      Error in `ggparcoord()`:
      ! invalid value for `alphaLines`; must be a scalar value between 0 and 1

---

    Code
      ggparcoord(data = diamonds.samp, columns = c(1, 5:10), groupColumn = 2,
      boxplot = 1)
    Condition
      Error in `ggparcoord()`:
      ! invalid value for `boxplot`; must be a logical operator

---

    Code
      ggparcoord(data = diamonds.samp, columns = c(1, 5:10), groupColumn = 2,
      shadeBox = c(1, 2))
    Condition
      Error in `ggparcoord()`:
      ! invalid value for `shadeBox`; must be a single color

---

    Code
      ggparcoord(data = diamonds.samp, columns = c(1, 5:10), groupColumn = 2,
      shadeBox = "notacolor")
    Condition
      Error in `ggparcoord()`:
      ! invalid value for `shadeBox`; must be a valid R color

---

    Code
      ggparcoord(diamonds.samp, columns = c(1, 5:10), groupColumn = 2, splineFactor = NULL)
    Condition
      Error in `ggparcoord()`:
      ! invalid value for `splineFactor`; must be a logical or numeric value

