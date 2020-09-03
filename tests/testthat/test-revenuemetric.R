context("Revenue Metric")
data(q.invoice.lines)
d <- q.invoice.lines
library(lubridate)
end <- ISOdate(2016, 2, 15, tz = tz(q.invoice.lines$ValidFrom))
start <- ISOdate(2012, 7, 1, tz = tz(q.invoice.lines$ValidFrom))

for (by in c("week", "month", "quarter"))
  test_that(paste("RecurringRevenue and GrowthAccounting are consistent", by),{
      rr = RevenueMetric("RecurringRevenue", output = "Table",  cohort.type = "None", value = d$AUD, from = d$ValidFrom, to = d$ValidTo, id = d$name,  by = by)
      ga  = RevenueMetric("GrowthAccounting", output = "Table",  value = d$AUD, from = d$ValidFrom, to = d$ValidTo, id = d$name,  by = by)
      aga = cumsum(colSums(ga))
      expect_equivalent(as.numeric(tail(aga, length(rr) - 1)), as.numeric(rr)[-1])
  })

data(q.invoice.lines.short)
d <- q.invoice.lines.short

data(metric.functions)

# Quick run through checking that the basic function works
by = "year"
for (fun in metric.functions)
  for (cohort.type in c("None", "New", "Calendar"))
    if (!(fun %in% c("NetRecurringRevenueRetention","NewRecurringRevenueGrowth","UnderlyingRecurringRevenueGrowth", "NumberofCustomers") & cohort.type == "Calendar" |
          (fun == "GrowthAccounting" & cohort.type != "None")))
    test_that(paste("metrics", fun, cohort.type),
              {
                s = RevenueMetric(FUN = fun, 
                                  output = "Plot", 
                                  cohort.type = cohort.type, 
                                  value = d$AUD, from = d$ValidFrom, to = d$ValidTo, id = d$name, by = by)
                expect_error(print(s), NA)
              }
    )

# Looping though arguments 
for (fun in metric.functions)
  for (out in c("Table", "Plot")) # Details tested in a different file
    for (by in c("month", "quarter", "year"))
    {
      #subscription.length <- if (fun %in%  c("RecurringRevenueByCohort", "MeanRecurringRevenueByCohort")) c("month", "quarter", "year") else NA
      for (subscription.length in "year")#c("month", "quarter", "year"))
      {
        #days <- if (fun %in%  c("MeanRecurringRevenue", "MeanRecurringRevenueByCohort")) c(0, 30, 90, 180, 365, 365 * 2) else NA
        #for (day in days)
        #{
        #descr <- paste("metrics", fun, out, by, subscription.length, "days", day)
        descr <- paste("metrics", fun, out, by, subscription.length)
        # Tests that the function works with a total data set
        test_that(paste("aggregate ", descr),{
          s = RevenueMetric(FUN = fun, output = out, value = d$AUD, from = d$ValidFrom, to = d$ValidTo, id = d$name, by = by, subscription.length = subscription.length)
          if (is.null(s)) expect_true(TRUE)
          else capture.output(expect_error(print(s), NA))
        })
      }
    }

