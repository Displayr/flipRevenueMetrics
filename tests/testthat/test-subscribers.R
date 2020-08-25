# context("subscribers")
# data(q.invoice.lines)
# d <- q.invoice.lines
# library(lubridate)
# end <-  ISOdate(2016, 12, 31, tz = tz(q.invoice.lines$ValidFrom))
# start <-  ISOdate(2012, 7, 1, tz = tz(q.invoice.lines$ValidFrom))
# by = "month"
# for (by in c("week", "month", "quarter", "year"))
#     test_that(paste("Creating RevenueData", by),
#               {
#                   expect_error(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, start = start, end = end, id = d$name, subscription.length = by, subset = d$validInvoice == 1), NA)
#                   expect_error(Subscribers(rd, by = by), NA)
#               })
# 
# 
