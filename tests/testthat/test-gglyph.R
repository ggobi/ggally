
context("gglyph")

data(nasa)
nasaLate <- nasa[
  nasa$date >= as.POSIXct("1998-01-01") &
  nasa$lat >= 20 &
  nasa$lat <= 40 &
  nasa$long >= -80 &
  nasa$long <= -60
, ]

do_glyph <- function(...) {
  glyphs(
    nasaLate, # no lint
    "long", "day", "lat", "surftemp", height = 2.37, width = 2.38, ...
  )
}



do_gg <- function(dt) {
  ggplot2::ggplot(dt, ggplot2::aes(gx, gy, group = gid)) +
    add_ref_lines(dt, color = "red", size = 0.5) +
    add_ref_boxes(dt, color = "blue") +
    ggplot2::geom_path() +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "", y = "") +
    ggplot2::xlim(-80, -60) + ggplot2::ylim(20, 40)
}

test_that("examples", {

  dt <- do_glyph()

  expect_true(all(c("gx", "gy", "gid") %in% names(dt)))
  expect_true(all(names(nasaLate) %in% names(dt)))

  p <- do_gg(dt)

  expect_equal(length(p$layers), 3)
  expect_equal(as.character(get("aes_params", envir = p$layers[[1]])$colour), "red")
  expect_equal(as.character(get("aes_params", envir = p$layers[[2]])$colour), "blue")

})

test_that("message", {

  expect_message(glyphs(nasaLate, "long", "day", "lat", "surftemp", height = 1), "Using width 2.38")
  expect_message(glyphs(nasaLate, "long", "day", "lat", "surftemp", width = 1), "Using height 2.37")
})


test_that("scales", {

  dt <- do_glyph(x_scale = log)
  dt$dayLog <- dt$day
  dt$day <- NULL
  dtm <- merge(dt, nasaLate)
  expect_true(all(dtm$dayLog == log(dtm$day)))

  dt <- do_glyph(y_scale = log)
  dt$surftempLog <- dt$surftemp
  dt$surftemp <- NULL
  dtm <- merge(dt, nasaLate)
  expect_true(all(dtm$surftempLog == log(dtm$surftemp)))

  for (scale_fn in c(range01, max1, mean0, min0, rescale01, rescale11)) {
    dt <- do_glyph(y_scale = scale_fn)
    dt$surftempScaled <- dt$surftemp
    dt$surftemp <- NULL
    dtm <- merge(dt, nasaLate)
    expect_true(all(dtm$surftempScaled != dtm$surftemp))
  }


  for (scale_fn in c(rescale01, rescale11)) {
    scale_fn2 <- function(x) {
      scale_fn(x, xlim = c(1 / 4, 3 / 4))
    }
    dt <- do_glyph(y_scale = scale_fn2)
    dt$surftempScaled <- dt$surftemp
    dt$surftemp <- NULL
    dtm <- merge(dt, nasaLate)
    expect_true(all(dtm$surftempScaled != dtm$surftemp))
  }

})

test_that("polar", {
  dt <- do_glyph(polar = TRUE)

  expect_equal(attr(dt, "polar"), TRUE)

  # idk how to test that polar happened

  p <- do_gg(dt)
  expect_equal(length(p$layers), 3)


})

test_that("fill", {
  dt <- do_glyph()

  # idk how to test that polar happened
  do_gg_fill <- function(...){
    ggplot2::ggplot(dt, ggplot2::aes(gx, gy, group = gid)) +
      add_ref_lines(dt, color = "red", size = 0.5) +
      add_ref_boxes(dt, color = "blue", ...) +
      ggplot2::geom_path() +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "", y = "") +
      ggplot2::xlim(-80, -60) + ggplot2::ylim(20, 40)
  }

  p <- do_gg_fill(fill = "green")
  expect_equal(as.character(get("aes_params", envir = p$layers[[2]])$fill), "green")
  p <- do_gg_fill(var_fill = "gid")
  expect_equal(as.character(get("mapping", envir = p$layers[[2]])$fill), "fill")

})

test_that("print", {

  dt <- do_glyph()
  txt <- capture.output(print(dt))
  expect_equal(txt[length(txt) - 2], "Cartesian glyphplot: ")
  expect_equal(txt[length(txt) - 1], "  Size: [2.38, 2.37]")
  expect_equal(txt[length(txt) - 0], "  Major axes: long, lat" )

  dt <- do_glyph(polar = TRUE)
  txt <- capture.output(print(dt))
  expect_equal(txt[length(txt) - 2], "Polar glyphplot: ")
  expect_equal(txt[length(txt) - 1], "  Size: [2.38, 2.37]")
  expect_equal(txt[length(txt) - 0], "  Major axes: long, lat" )

  txt <- capture.output(print(rel(0.95)))
  expect_equal(txt, "[1] 0.95 *")

})
