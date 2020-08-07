context("Revenue Metric - results")
data(q.invoice.lines)
d <- q.invoice.lines
library(lubridate)
end <- ISOdate(2016, 2, 15, tz = tz(q.invoice.lines$ValidFrom))
start <- ISOdate(2012, 7, 1, tz = tz(q.invoice.lines$ValidFrom))
by = "year"

test_that("GrowthAccounting",
          {
              
              s = RevenueMetric("GrowthAccounting", output = "Table",  d$AUD, d$ValidFrom, d$ValidTo, end = as.Date("2016/12/31"), id = d$name,  by = by)
              expect_equal(s[, "2016"], c(New = 457832.625451466, Resurrection = 27522.7272727273, `Major Expansion` = 405035.003914683, 
                                          `Minor Expansion` = 10527.5750700948, Contraction = -431011.760536442, 
                                          Churn = -2059576.69060701))
              }
)


test_that("CustomerChurn",
          {

              s = RevenueMetric("CustomerChurn", output = "Table",  d$AUD, d$ValidFrom, d$ValidTo, end = as.Date("2016/12/31"), id = d$name,  by = by)
              expect_equal(s[, "2016"], c(New = 457832.625451466, Resurrection = 27522.7272727273, `Major Expansion` = 405035.003914683,
                                          `Minor Expansion` = 10527.5750700948, Contraction = -431011.760536442,
                                          Churn = -2059576.69060701))
          }
)