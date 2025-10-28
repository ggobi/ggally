# stops

    Code
      ggpairs(tips, columns = c("tip", "day", "not in tips"))
    Condition
      Error in `fix_column_values()`:
      ! Columns in `columns` not found in data: `'not in tips'`.
      i Choices: `total_bill`, `tip`, `sex`, `smoker`, `day`, `time`, and `size`

---

    Code
      ggduo(tips, columnsX = c("tip", "day", "not in tips"), columnsY = "smoker")
    Condition
      Error in `fix_column_values()`:
      ! Columns in `columnsX` not found in data: `'not in tips'`.
      i Choices: `total_bill`, `tip`, `sex`, `smoker`, `day`, `time`, and `size`

---

    Code
      ggduo(tips, columnsX = c("tip", "day", "smoker"), columnsY = "not in tips")
    Condition
      Error in `fix_column_values()`:
      ! Columns in `columnsY` not found in data: `'not in tips'`.
      i Choices: `total_bill`, `tip`, `sex`, `smoker`, `day`, `time`, and `size`

---

    Code
      ggpairs(tips, columns = 1:10)
    Condition
      Error in `fix_column_values()`:
      ! Make sure your numeric "columns" values are less than or equal to 7.
      * columns = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

---

    Code
      ggduo(tips, columnsX = 1:10)
    Condition
      Error in `fix_column_values()`:
      ! Make sure your numeric "columnsX" values are less than or equal to 7.
      * columnsX = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

---

    Code
      ggduo(tips, columnsY = 1:10)
    Condition
      Error in `fix_column_values()`:
      ! Make sure your numeric "columnsY" values are less than or equal to 7.
      * columnsY = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

---

    Code
      ggpairs(tips, columns = -5:5)
    Condition
      Error in `fix_column_values()`:
      ! Make sure your numeric "columns" values are positive.
      * columnsName = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5)

---

    Code
      ggduo(tips, columnsX = -5:5)
    Condition
      Error in `fix_column_values()`:
      ! Make sure your numeric "columnsX" values are positive.
      * columnsName = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5)

---

    Code
      ggduo(tips, columnsY = -5:5)
    Condition
      Error in `fix_column_values()`:
      ! Make sure your numeric "columnsY" values are positive.
      * columnsName = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5)

---

    Code
      ggpairs(tips, columns = (2:10) / 2)
    Condition
      Error in `fix_column_values()`:
      ! Make sure your numeric "columns" values are integers.
      * columnsName = c(toString(columns))

---

    Code
      ggduo(tips, columnsX = (2:10) / 2)
    Condition
      Error in `fix_column_values()`:
      ! Make sure your numeric "columnsX" values are integers.
      * columnsName = c(toString(columns))

---

    Code
      ggduo(tips, columnsY = (2:10) / 2)
    Condition
      Error in `fix_column_values()`:
      ! Make sure your numeric "columnsY" values are integers.
      * columnsName = c(toString(columns))

---

    Code
      ggpairs(tips, columns = 1:3, columnLabels = c("A", "B", "C", "Extra"))
    Condition
      Error in `fix_column_values()`:
      ! The length of the `columnLabels` does not match the length of the `columns` being used.
      * Labels: c(A, B, C, Extra)
      * Columns: c(1, 2, 3)

---

    Code
      ggduo(tips, columnsX = 1:3, columnLabelsX = c("A", "B", "C", "Extra"))
    Condition
      Error in `fix_column_values()`:
      ! The length of the `columnLabelsX` does not match the length of the `columnsX` being used.
      * Labels: c(A, B, C, Extra)
      * Columns: c(1, 2, 3)

---

    Code
      ggduo(tips, columnsY = 1:3, columnLabelsY = c("A", "B", "C", "Extra"))
    Condition
      Error in `fix_column_values()`:
      ! The length of the `columnLabelsY` does not match the length of the `columnsY` being used.
      * Labels: c(A, B, C, Extra)
      * Columns: c(1, 2, 3)

---

    Code
      ggpairs(tips, upper = c("not_a_list"))
    Condition
      Error in `check_and_set_ggpairs_defaults()`:
      ! `upper` is not a list

---

    Code
      ggpairs(tips, diag = c("not_a_list"))
    Condition
      Error in `check_and_set_ggpairs_defaults()`:
      ! `diag` is not a list

---

    Code
      ggpairs(tips, lower = c("not_a_list"))
    Condition
      Error in `check_and_set_ggpairs_defaults()`:
      ! `lower` is not a list

---

    Code
      ggduo(tips, types = c("not_a_list"))
    Condition
      Error in `check_and_set_ggpairs_defaults()`:
      ! `types` is not a list

---

    Code
      ggpairs(tips, upper = list(aes_string = ggplot2::aes(color = .data$day)))
    Condition
      Error in `check_and_set_ggpairs_defaults()`:
      ! `aes_string()` is a deprecated element for the section upper. Please use 'mapping' instead.

---

    Code
      ggpairs(tips, lower = list(aes_string = ggplot2::aes(color = .data$day)))
    Condition
      Error in `check_and_set_ggpairs_defaults()`:
      ! `aes_string()` is a deprecated element for the section lower. Please use 'mapping' instead.

---

    Code
      ggpairs(tips, diag = list(aes_string = ggplot2::aes(color = .data$day)))
    Condition
      Error in `check_and_set_ggpairs_defaults()`:
      ! `aes_string()` is a deprecated element for the section diag. Please use 'mapping' instead.

---

    Code
      ggduo(tips, types = list(aes_string = ggplot2::aes(color = .data$day)))
    Condition
      Error in `check_and_set_ggpairs_defaults()`:
      ! `aes_string()` is a deprecated element for the section types. Please use 'mapping' instead.

# cardinality

    Code
      stop_if_high_cardinality(tips, 1:ncol(tips), "not numeric")
    Condition
      Error in `stop_if_high_cardinality()`:
      ! `cardinality_threshold` should be a numeric or `NULL`.

---

    Code
      stop_if_high_cardinality(tips, 1:ncol(tips), 2)
    Condition
      Error in `stop_if_high_cardinality()`:
      ! Column "day" has more levels (4) than the threshold (2) allowed.
      i Please remove the column or increase the 'cardinality_threshold' parameter. Increasing the cardinality_threshold may produce long processing times.

# mapping

    Code
      ggpairs(tips, columns = 1:3, mapping = 1:3)
    Condition
      Error in `stop_if_bad_mapping()`:
      ! `mapping` should not be numeric unless 'columns' is missing from function call.

