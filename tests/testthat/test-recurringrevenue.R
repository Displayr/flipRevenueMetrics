context("RecurringRevenue")
data(q.invoice.lines)
d <- q.invoice.lines
library(lubridate)
end <- ISOdate(2016, 12, 31, tz = tz(q.invoice.lines$ValidFrom))
start <- ISOdate(2012, 7, 1, tz = tz(q.invoice.lines$ValidFrom))
by = "week"
for (by in c("week", "month", "quarter", "year"))
    test_that(paste("Creating RevenueData", by),
              {
                  expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFro, d$ValidTo, start = start, end = end, id = d$name, subscription.length = by, subset = d$validInvoice == 1)), NA)
                  expect_error(capture.output(print(RecurringRevenue(rd, by = by))), NA)
              })


dollars <- c(100, 100, 100, 100)
from <- as.Date(c("2016/01/01", "2016/07/01", "2016/08/01", "2016/10/01"))
to <- as.Date(c("2017/01/01", "2017/01/01", "2017/08/01", "2017/11/01"))
name <- c("A", "A", "B", "C")
test_that("Recurring Revenue", {
    # Years ending the day before 12 months
    capture.output(rd <- RevenueData(dollars, from, to, id = name, end = Sys.Date(), subscription.length = "year"))
    r <- RecurringRevenue(rd, by = "year")
    expect_equivalent(r["2016-12-31"], 100 + 100 / 0.5 + 100 + 100 * 12 / 13, tolerance = .02) # 2017
    expect_equivalent(r["2017-12-31"], 0, tolerance = .02) # 2017
    r <- RecurringRevenue(rd, by = "month")
    expect_equivalent(r["2016-01-31"], 100) # 2016
    expect_equivalent(r["2016-06-30"], 100, tolerance = .02) # 2016
    expect_equivalent(r["2016-08-31"], 100 + 100 * (12/6) + 100, tolerance = .02) # 2016
    expect_equivalent(r["2017-01-31"], 100 + 100 * 12/13, tolerance = .02) # 2017
    r <- RecurringRevenue(rd, by = "quarter")
    expect_equivalent(r["2016-03-31"], 100) # 2016
    expect_equivalent(r["2016-06-30"], 100, tolerance = .02) # 2016
    expect_equivalent(r["2016-09-30"], 100 + 100 * (12/6) + 100, tolerance = .02) # 2016
    expect_equivalent(r["2016-12-31"], 100 + 100 * (12/6) + 100 + 100, tolerance = .02) # 2016
    expect_equivalent(r["2017-03-31"], 100 + 100 * 12/13, tolerance = .02) # 2017
    r <- RecurringRevenue(rd, by = "week")
    expect_equivalent(r["2016-01-02"], 100) # 2016
    expect_equivalent(r["2016-07-02"], 100 + 100 * (12/6), tolerance = .02) # 2016
    expect_equivalent(r["2016-10-01"], 100 + 100 * (12/6) + 100 + 100 * 12/13, tolerance = .02) # 2016
    expect_equivalent(r["2016-12-31"], 100 + 100 * (12/6) + 100 + 100 * 12/13, tolerance = .02) # 2016
    expect_equivalent(r["2017-03-25"], 100 + 100 * 12/13, tolerance = .02) # 2017
})


