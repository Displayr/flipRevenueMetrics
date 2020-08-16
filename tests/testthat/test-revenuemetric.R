context("Revenue Metric")
data(q.invoice.lines)
d <- q.invoice.lines
library(lubridate)
end <- ISOdate(2016, 2, 15, tz = tz(q.invoice.lines$ValidFrom))
start <- ISOdate(2012, 7, 1, tz = tz(q.invoice.lines$ValidFrom))


  test_that(paste("Mean recurrent revenue consistency"), {
    by= "year"
    # Near-depricated
    rdd <- RevenueData(d$AUD,d$ValidFrom,d$ValidTo, id = d$name, subscription.length = by)
    r <- Lifetime(rdd)
    # These tests are checking that numer methods of computing churn give the same
    # answer as depricated methods
#warning("Test that need to add back in")
    rr = RevenueMetric("RecurringRevenueByCohort", output = "Table", d$AUD,d$ValidFrom,d$ValidTo, id = d$name, by = by, subscription.length = by)
    expect_equal(flipRevenueMetrics:::removeAttributesAndClass(rr)[rownames(r$total), ], r$total[, colnames(rr)])

  })

for (by in c("week", "month", "quarter", "year"))
test_that(paste("RecurringRevenue and GrowthAccounting are consistent", by),{
    rr = RevenueMetric("RecurringRevenue", output = "Table",  d$AUD, d$ValidFrom, d$ValidTo, id = d$name,  by = by)
#warning("Test that need to add back in")
    ga  = RevenueMetric("GrowthAccounting", output = "Table",  d$AUD, d$ValidFrom, d$ValidTo, id = d$name,  by = by)
    aga = cumsum(colSums(ga))
    expect_equal(as.numeric(rr[-1]), as.numeric(aga))
})


data(q.invoice.lines.short)
d <- q.invoice.lines.short

funcs <- c("GrowthAccounting",
           "Expansion",
           "Contraction",
           "InitialCustomerChurn",  
           "CustomerChurn",
           "CustomerRetentionByCohort",
           "RecurringRevenue",
           "InitialRecurringRevenueChurn",
           "RecurringRevenueChurn",
           "RecurringRevenueRetentionByCohort")#,
           #           "RecurringRevenueChurnByCohort",
#           "Customers", # Customers
 #          "NewCustomers",
  #         "CustomerGrowth",
           #"Revenue",                  # Revenue
          # "RecurringRevenueGrowth",
          # "MeanRecurringRevenue")#,
           #"MeanRecurringRevenueByCohort")

warning("Add test for MeanRecurringRevenue, RecurringRevenueByCohort, Customers, NewCustomers, CustomerGrowth")
# Quick run through checking that the basic function works
by = "year"
for (fun in funcs)
      test_that(paste("metrics", fun),
                {
                  s = RevenueMetric(FUN = fun, output = "Plot", d$AUD,d$ValidFrom,d$ValidTo, id = d$name, by = by)
                  expect_error(print(s), NA)
                }
      )

warning("Test other subscription lengths")
# Looping though arguments 
for (fun in funcs)
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
                        s = RevenueMetric(FUN = fun, output = out, d$AUD,d$ValidFrom,d$ValidTo, id = d$name, by = by, subscription.length = subscription.length)
                        if (is.null(s)) expect_true(TRUE)
                        else capture.output(expect_error(print(s), NA))
                      })
                }
        }
        
