# crosstalk works with ggduo and ggpairs

    Code
      ggpairs(sd, 3:5)
    Condition
      Error in `fix_column_values()`:
      ! Make sure your numeric "columns" values are less than or equal to 4.
      * columns = c(3, 4, 5)

---

    Code
      ggpairs(sd, c("Petal.Length", "Petal.Width", crosstalk_key()))
    Condition
      Error in `fix_column_values()`:
      ! Columns in `columns` not found in data: `'.crossTalkKey'`.
      i Choices: `Sepal.Length`, `Sepal.Width`, `Petal.Length`, and `Petal.Width`

---

    Code
      ggduo(sd, c(1:2, 5), 3:5)
    Condition
      Error in `fix_column_values()`:
      ! Make sure your numeric "columnsX" values are less than or equal to 4.
      * columnsX = c(1, 2, 5)

---

    Code
      ggduo(sd, c("Sepal.Length", "Sepal.Width", crosstalk_key()), c("Petal.Length",
        "Petal.Width"))
    Condition
      Error in `fix_column_values()`:
      ! Columns in `columnsX` not found in data: `'.crossTalkKey'`.
      i Choices: `Sepal.Length`, `Sepal.Width`, `Petal.Length`, and `Petal.Width`

---

    Code
      ggduo(sd, c("Sepal.Length", "Sepal.Width"), c("Petal.Length", "Petal.Width",
        crosstalk_key()))
    Condition
      Error in `fix_column_values()`:
      ! Columns in `columnsY` not found in data: `'.crossTalkKey'`.
      i Choices: `Sepal.Length`, `Sepal.Width`, `Petal.Length`, and `Petal.Width`

