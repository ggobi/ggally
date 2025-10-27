# examples

    Code
      ggnet(n, group = NA)
    Condition
      Error in `set_node()`:
      ! incorrect node.group value

---

    Code
      ggnet(n, group = 1:3)
    Condition
      Error in `set_node()`:
      ! incorrect node.group length

---

    Code
      ggnet(n, label = TRUE, label.size = -10:-1)
    Condition
      Error in `set_node()`:
      ! incorrect label.size value

---

    Code
      ggnet(n, size = "phono")
    Condition
      Error in `ggnet()`:
      ! incorrect `size` value

---

    Code
      ggnet(n, segment.label = NA)
    Condition
      Error in `set_edge()`:
      ! incorrect segment.label value

---

    Code
      ggnet(n, segment.label = 1:3)
    Condition
      Error in `set_edge()`:
      ! incorrect segment.label length

---

    Code
      ggnet(n, segment.label = -11:-1)
    Condition
      Error in `set_edge()`:
      ! incorrect segment.label value

---

    Code
      ggnet(n, mode = c("xx", "yy"))
    Condition
      Error in `set_attr()`:
      ! vertex attribute xx was not found

---

    Code
      ggnet(n, mode = c("abc", "abc"))
    Condition
      Error in `set_attr()`:
      ! vertex attribute abc is not numeric

---

    Code
      ggnet(n, mode = matrix(1, ncol = 2, nrow = 9))
    Condition
      Error in `set_attr()`:
      ! incorrect coordinates length

---

    Code
      ggnet(n, arrow.size = -1)
    Condition
      Error in `ggnet()`:
      ! incorrect `arrow.size` value

---

    Code
      ggnet(n, arrow.size = 12, arrow.gap = -1)
    Condition
      Warning:
      network is undirected; `arrow.size` ignored
      Error in `ggnet()`:
      ! incorrect `arrow.gap` value

---

    Code
      ggnet(n, weight = "degree", weight.min = -1)
    Condition
      Error in `ggnet()`:
      ! incorrect `weight.min` value

---

    Code
      ggnet(n, weight = "degree", weight.max = -1)
    Condition
      Error in `ggnet()`:
      ! incorrect `weight.max` value

---

    Code
      ggnet(n, weight = "abc")
    Condition
      Error in `ggnet()`:
      ! incorrect `weight.method` value

---

    Code
      ggnet(n, weight.cut = NA)
    Condition
      Error in `ggnet()`:
      ! incorrect `weight.cut` value

---

    Code
      ggnet(n, weight.cut = "a")
    Condition
      Error in `ggnet()`:
      ! incorrect `weight.cut` value

---

    Code
      ggnet(n, label = letters[1:10], label.size = "abc")
    Condition
      Error in `ggnet()`:
      ! incorrect `label.size` value

---

    Code
      ggnet(n, mode = "xyz")
    Condition
      Error in `ggnet()`:
      ! unsupported placement method: "gplot.layout.xyz"

---

    Code
      ggnet(n, mode = letters[1:3])
    Condition
      Error in `ggnet()`:
      ! incorrect `mode` value

---

    Code
      ggnet(n, label = TRUE, label.trim = "xyz")
    Condition
      Error in `ggnet()`:
      ! incorrect `label.trim` value

---

    Code
      ggnet(n, layout.exp = "xyz")
    Condition
      Error in `ggnet()`:
      ! incorrect `layout.exp` value

---

    Code
      ggnet(1:2)
    Condition
      Error in `ggnet()`:
      ! could not coerce `net` to a network object

---

    Code
      ggnet(network(data.frame(1:2, 3:4), hyper = TRUE))
    Condition
      Error:
      ! If `hyper` is `TRUE`, the first two columns of `x` should be list columns.

---

    Code
      ggnet(network(data.frame(1:2, 3:4), multiple = TRUE))
    Condition
      Error in `ggnet()`:
      ! ggnet cannot plot multiplex graphs

