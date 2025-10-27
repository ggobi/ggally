# stops

    Code
      ggsurv(lungNoCensor, surv.col = c("black", "red"))
    Condition
      Error in `ggsurv()`:
      ! length(surv.col) == 1 | length(surv.col) == strata is not TRUE

---

    Code
      ggsurv(lungNoCensor, lty.est = 1:2)
    Condition
      Error in `ggsurv()`:
      ! length(lty.est) == 1 | length(lty.est) == strata is not TRUE

---

    Code
      ggsurv(lungNoCensor, plot.cens = TRUE)
    Condition
      Error in `fn()`:
      ! There are no censored observations

---

    Code
      ggsurv(kidneyNoCensor, surv.col = c("black", "red", "blue"))
    Condition
      Error in `ggsurv()`:
      ! length(surv.col) == 1 | length(surv.col) == strata is not TRUE

---

    Code
      ggsurv(kidneyNoCensor, lty.est = 1:3)
    Condition
      Error in `ggsurv()`:
      ! length(lty.est) == 1 | length(lty.est) == strata is not TRUE

---

    Code
      ggsurv(kidneyNoCensor, plot.cens = TRUE)
    Condition
      Error in `fn()`:
      ! There are no censored observations

