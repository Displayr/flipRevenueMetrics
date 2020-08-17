context("Layercake")
data(q.invoice.lines)
d <- q.invoice.lines
library(lubridate)
end <-  ISOdate(2016, 12, 31,tz = tz(q.invoice.lines$ValidFrom))
start <-  ISOdate(2012, 7, 1, tz = tz(q.invoice.lines$ValidFrom))
by = "year"
for (by in c("week", "month", "quarter", "year"))
    test_that(paste("Layercake", by),
          {
              capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, start = start, end = end, id = d$name, subscription.length = by, subset = d$validInvoice == 1))
              lc <- LayerCake(rd)
              LayerCake(rd, as.table = TRUE)
              expect_error(capture.output(print(lc), NA))
          })


