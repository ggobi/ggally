# stops

    Code
      pm["total_bill", 1]
    Condition
      Error in `check_i_j()`:
      ! `i` may only be a single numeric value

---

    Code
      pm[1, "total_bill"]
    Condition
      Error in `check_i_j()`:
      ! `j` may only be a single numeric value

---

    Code
      pm["total_bill", 1] <- p
    Condition
      Error in `check_i_j()`:
      ! `i` may only be a single numeric value

---

    Code
      pm[1, "total_bill"] <- p
    Condition
      Error in `check_i_j()`:
      ! `j` may only be a single numeric value

---

    Code
      pm[0, 1]
    Condition
      Error in `check_i_j()`:
      ! `i` may only be in the range from 1:4

---

    Code
      pm[1, 0]
    Condition
      Error in `check_i_j()`:
      ! `j` may only be in the range from 1:3

---

    Code
      pm[5, 1]
    Condition
      Error in `check_i_j()`:
      ! `i` may only be in the range from 1:4

---

    Code
      pm[1, 4]
    Condition
      Error in `check_i_j()`:
      ! `j` may only be in the range from 1:3

# get

    Code
      a[2, 1]
    Condition
      Error in `getPlot()`:
      ! unknown plot object type.
      i Position: i = 2, j = 1 str(plotObj): int [1:4] 1 2 3 4

