# context("retention")
# data(q.invoice.lines)
# d <- q.invoice.lines
# library(lubridate)
# end <-  ISOdate(2015, 12, 31, tz = tz(q.invoice.lines$ValidFrom))
# by = "year"
# for (by in c("week", "month", "quarter", "year"))
#     test_that(paste("Creating RevenueData", by),
#           {
#             expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = end, id = d$name, subscription.length = by, subset = d$validInvoice == 1)), NA)
#             expect_error(capture.output(r <- Retention(rd, by)), NA)
#             expect_error(capture.output(print(r)), NA)
# })
# 
# 
# # rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = end, id = d$name, by = by, subset = d$validInvoice == 1)
# # r <- Retention(rd)
# # apply(r$retention, 2, mean, na.rm = TRUE)
# # r
# #
# # expect_error(print(r), NA)
