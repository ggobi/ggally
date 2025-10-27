# labels

    Code
      ggnetworkmap(usa, flights, label.nodes = c("A", "B"))
    Condition
      Error in `ggnetworkmap()`:
      ! length(labels) == nrow(plotcord) is not TRUE

# arrow.size

    Code
      ggnetworkmap(net = flights, arrow.size = -1)
    Condition
      Error in `ggnetworkmap()`:
      ! incorrect `arrow.size` value

# network coercion

    Code
      ggnetworkmap(net = 1:2)
    Condition
      Error in `ggnetworkmap()`:
      ! could not coerce `net` to a network object

---

    Code
      ggnetworkmap(net = network(data.frame(1:2, 3:4), hyper = TRUE))
    Condition
      Error:
      ! If `hyper` is `TRUE`, the first two columns of `x` should be list columns.

---

    Code
      ggnetworkmap(net = network(data.frame(1:2, 3:4), multiple = TRUE))
    Condition
      Error in `ggnetworkmap()`:
      ! `ggnetworkmap()` cannot plot multiplex graphs

