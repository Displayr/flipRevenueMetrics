context("Revenue Metric - subsets")
data(q.invoice.lines)
d <- q.invoice.lines
library(lubridate)
end <- ISOdate(2016, 2, 15, tz = tz(q.invoice.lines$ValidFrom))
start <- ISOdate(2012, 7, 1, tz = tz(q.invoice.lines$ValidFrom))



test_that("Create subsets",
          {
              # Country
              s <- flipRevenueMetrics:::createFilters(d[, "country", drop = FALSE], subset = NULL, id = d$name)
              expect_equal(length(names(s)), 5)
              expect_equal(names(s)[1], "Australia")
              expect_equal(sum(s[[1]]), 2386) # Observations, companies can have multiple observations
              
              # Country - with a filter
              f <- d$salesman == "4"
              s <- flipRevenueMetrics:::createFilters(d[, "country", drop = FALSE], subset = f, id = d$name)
              expect_equal(length(names(s)), 5)
              expect_equal(names(s)[1], "Australia")
              expect_equal(sum(s[[1]]), 504)
              
              # Saleman
              s <- flipRevenueMetrics:::createFilters(d[, "salesman", drop = FALSE], subset = NULL, id = d$name)
              expect_equal(length(names(s)), 4)
              expect_equal(names(s)[1], "MISSING DATA")
              
              # Country and salesman
              s <- flipRevenueMetrics:::createFilters(d[, c("country", "salesman")], subset = NULL, id = d$name)
              expect_equal(length(names(s)), 20)
              expect_equal(names(s)[1], "Australia + MISSING DATA")
          })

