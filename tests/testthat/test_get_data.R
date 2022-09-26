library(httr)
library(jsonlite)

test_that("get_data() rejects errounous input", {
  values_df <- data.frame(id="0580", title="Linköping",type="K")
  compare_lst <- list(count=c(1),values=values_df)
  expect_error(get_data(1234,5))
  expect_error(get_data("http://api.kolada.se/v2/municipality",5))
  expect_error(get_data(1234,"somePlace"))
  expect_error(get_data("http://api.kolada.se/v2/muniality",""))
  expect_equal(get_data("http://api.kolada.se/v2/municipality",
                        query="title=Linköping"),compare_lst)
})

test_that("get_muni_id() works", {
  expect_error(get_muni_id(1234))
  expect_match(get_muni_id("Linköping"), "0580")
  expect_equal(get_muni_id("Ale"), c("1440","2361","0023","0128","2039"))
})

test_that("get_figures() works", {
  compare_df <- data.frame(years=2005,N00401=5.759551, N85078=0.7506498,
                           N85072=0.1988583, N85075=0.2207173, N85077=0.337514,
                           N85073=2.029161, N85076=2.003366)
  rownames(compare_df) <- c(3L)
  expect_error(get_figures(1234))
  expect_true(is.data.frame(get_figures("Linköping")))
  expect_equal(round(get_figures("Linköping")[3,],6), round(compare_df,6))
})

test_that("plot_figures() works", {
  expect_error(plot_figures(1234))
})
