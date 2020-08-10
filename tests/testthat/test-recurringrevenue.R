context("RecurringRevenue")

dollars <- c(100, 100, 100, 100)
from <- as.Date(c("2016/01/01", "2016/07/01", "2016/08/01", "2016/10/01"))
to <- as.Date(c("2017/01/01", "2017/01/01", "2017/08/01", "2017/11/01"))
name <- c("A", "A", "B", "C")
test_that("Recurring Revenue", {
    # Years ending the day before 12 months
    r <- RevenueMetric(FUN = "RecurringRevenue", "Table", 
                       dollars, from, to, id = name, end = Sys.Date(), subscription.length = "year", 
                       by = "year")
    expect_equivalent(r["2016"], 100 + 100 / 0.5 + 100 + 100 * 12 / 13, tolerance = .02) # 2017
    expect_equivalent(r["2017"], 0, tolerance = .02) # 2017

    r <- RevenueMetric(FUN = "RecurringRevenue", "Table", 
                       dollars, from, to, id = name, end = Sys.Date(), subscription.length = "year", 
                       by = "month")
    expect_equivalent(r["2016-01"], 100) # 2016
    expect_equivalent(r["2016-06"], 100, tolerance = .02) # 2016
    expect_equivalent(r["2016-08"], 100 + 100 * (12/6) + 100, tolerance = .02) # 2016
    expect_equivalent(r["2017-01"], 100 + 100 * 12/13, tolerance = .02) # 2017

    r <- RevenueMetric(FUN = "RecurringRevenue", "Table", 
                       dollars, from, to, id = name, end = Sys.Date(), subscription.length = "year", 
                       by = "quarter")
    expect_equivalent(r["2016-01"], 100) # 2016
    expect_equivalent(r["2016-04"], 100, tolerance = .02) # 2016
    expect_equivalent(r["2016-07"], 100 + 100 * (12/6) + 100, tolerance = .02) # 2016
    expect_equivalent(r["2016-10"], 100 + 100 * (12/6) + 100 + 100, tolerance = .02) # 2016
    expect_equivalent(r["2017-01"], 100 + 100 * 12/13, tolerance = .02) # 2017
    r <- RevenueMetric(FUN = "RecurringRevenue", "Table", 
                       dollars, from, to, id = name, end = Sys.Date(), subscription.length = "year", 
                       by = "week")
    expect_equivalent(r["2015-12-27"], 100) # 2016
    expect_equivalent(r["2016-06-26"], 100 + 100 * (12/6), tolerance = .02) # 2016
    expect_equivalent(r["2016-09-25"], 100 + 100 * (12/6) + 100 + 100 * 12/13, tolerance = .02) # 2016
    expect_equivalent(r["2016-12-25"], 100 + 100 * (12/6) + 100 + 100 * 12/13, tolerance = .02) # 2016
    expect_equivalent(r["2017-03-19"], 100 + 100 * 12/13, tolerance = .02) # 2017
})


