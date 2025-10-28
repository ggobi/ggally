# examples

    Code
      ggnet2(n, color = NA)
    Condition
      Error in `set_node()`:
      ! incorrect node.color value

---

    Code
      ggnet2(n, color = -1)
    Condition
      Error in `set_node()`:
      ! incorrect node.color value

---

    Code
      ggnet2(n, color = rep("red", network.size(n) - 1))
    Condition
      Error in `set_node()`:
      ! incorrect node.color length

---

    Code
      ggnet2(n, edge.color = NA)
    Condition
      Error in `set_edge()`:
      ! incorrect edge.color value

---

    Code
      ggnet2(n, edge.color = -1)
    Condition
      Error in `set_edge()`:
      ! incorrect edge.color value

---

    Code
      ggnet2(n, edge.color = rep("red", network.edgecount(n) - 1))
    Condition
      Error in `set_edge()`:
      ! incorrect edge.color length

---

    Code
      ggnet2(n, mode = c("xx", "yy"))
    Condition
      Error in `set_attr()`:
      ! vertex attribute xx was not found

---

    Code
      ggnet2(n, mode = c("phono", "phono"))
    Condition
      Error in `set_attr()`:
      ! vertex attribute phono is not numeric

---

    Code
      ggnet2(n, mode = matrix(1, ncol = 2, nrow = 9))
    Condition
      Error in `set_attr()`:
      ! incorrect coordinates length

---

    Code
      ggnet2(n, arrow.size = -1)
    Condition
      Error in `ggnet2()`:
      ! incorrect `arrow.size` value

---

    Code
      ggnet(n, arrow.size = 12, arrow.gap = -1)
    Condition
      Warning:
      `ggnet()` was deprecated in GGally 2.2.2.
      i Please use `ggnet2()` instead.
      Warning:
      network is undirected; `arrow.size` ignored
      Error in `ggnet()`:
      ! incorrect `arrow.gap` value

---

    Code
      ggnet2(n, max_size = NA)
    Condition
      Error in `ggnet2()`:
      ! incorrect `max_size` value

---

    Code
      ggnet2(n, na.rm = 1:2)
    Condition
      Error in `ggnet2()`:
      ! incorrect `na.rm` value

---

    Code
      ggnet2(n, na.rm = "xyz")
    Condition
      Error in `ggnet2()`:
      ! vertex attribute xyz was not found

---

    Code
      ggnet2(n, size = "degree", size.min = -1)
    Condition
      Error in `ggnet2()`:
      ! incorrect `size.min` value

---

    Code
      ggnet2(n, size = "degree", size.max = -1)
    Condition
      Error in `ggnet2()`:
      ! incorrect `size.max` value

---

    Code
      ggnet2(n, size = 1:10, size.cut = NA)
    Condition
      Error in `ggnet2()`:
      ! incorrect `size.cut` value

---

    Code
      ggnet2(n, size = 1:10, size.cut = "xyz")
    Condition
      Error in `ggnet2()`:
      ! incorrect `size.cut` value

---

    Code
      ggnet2(n, alpha = "phono", alpha.palette = c(vowel = 1))
    Condition
      Error in `ggnet2()`:
      ! no `alpha.palette` value for consonant

---

    Code
      ggnet2(n, color = factor(1:10), palette = "Set1")
    Condition
      Error in `ggnet2()`:
      ! too many node groups (10) for ColorBrewer palette Set1 (max: 9)

---

    Code
      ggnet2(n, color = "phono", color.palette = c(vowel = 1))
    Condition
      Error in `ggnet2()`:
      ! no `color.palette` value for consonant

---

    Code
      ggnet2(n, shape = "phono", shape.palette = c(vowel = 1))
    Condition
      Error in `ggnet2()`:
      ! no `shape.palette` value for consonant

---

    Code
      ggnet2(n, size = "phono", size.palette = c(vowel = 1))
    Condition
      Error in `ggnet2()`:
      ! no `size.palette` value for consonant

---

    Code
      ggnet2(n, label = TRUE, label.alpha = "xyz")
    Condition
      Error in `ggnet2()`:
      ! incorrect `label.alpha` value

---

    Code
      ggnet2(n, label = TRUE, label.color = "xyz")
    Condition
      Error in `ggnet2()`:
      ! incorrect `label.color` value

---

    Code
      ggnet2(n, label = TRUE, label.size = "xyz")
    Condition
      Error in `ggnet2()`:
      ! incorrect `label.size` value

---

    Code
      ggnet2(n, label = TRUE, label.trim = "xyz")
    Condition
      Error in `ggnet2()`:
      ! incorrect `label.trim` value

---

    Code
      ggnet2(n, mode = "xyz")
    Condition
      Error in `ggnet2()`:
      ! unsupported placement method: `gplot.layout.xyz`

---

    Code
      ggnet2(n, mode = letters[1:3])
    Condition
      Error in `ggnet2()`:
      ! incorrect `mode` value

---

    Code
      ggnet2(n, edge.color = "xyz")
    Condition
      Error in `ggnet2()`:
      ! incorrect `edge.color` value

---

    Code
      ggnet2(n, edge.label = "xyz", edge.label.alpha = "xyz")
    Condition
      Error in `ggnet2()`:
      ! incorrect `edge.label.alpha` value

---

    Code
      ggnet2(n, edge.label = "xyz", edge.label.color = "xyz")
    Condition
      Error in `ggnet2()`:
      ! incorrect `edge.label.color` value

---

    Code
      ggnet2(n, edge.label = "xyz", edge.label.size = "xyz")
    Condition
      Error in `ggnet2()`:
      ! incorrect `edge.label.size` value

---

    Code
      ggnet2(n, edge.size = "xyz")
    Condition
      Error in `ggnet2()`:
      ! incorrect `edge.size` value

---

    Code
      ggnet2(n, layout.exp = "xyz")
    Condition
      Error in `ggnet2()`:
      ! incorrect `layout.exp` value

---

    Code
      ggnet2(1:2)
    Condition
      Error in `ggnet2()`:
      ! could not coerce `net` to a network object

---

    Code
      ggnet2(network(data.frame(1:2, 3:4), hyper = TRUE))
    Condition
      Error:
      ! If `hyper` is `TRUE`, the first two columns of `x` should be list columns.

---

    Code
      ggnet2(network(data.frame(1:2, 3:4), multiple = TRUE))
    Condition
      Error in `ggnet2()`:
      ! `ggnet2()` cannot plot multiplex graphs

