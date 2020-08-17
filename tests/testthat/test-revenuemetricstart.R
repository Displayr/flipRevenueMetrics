context("Revenue Metric - start")

data(q.invoice.lines.short)
d <- q.invoice.lines.short

data(metric.functions)

# Checking that start parameter is taken into account 
library(flipTime)
strt = as.Date("2013-01-01")
nd = as.Date("2015-01-01")

for (fun in metric.functions)
    for (out in c("Table"))#
        for (by in c("month", "quarter", "year"))
        {
            for (subscription.length in "year")
            {
                # days <- if (fun %in%  c("MeanRecurringRevenue", "MeanRecurringRevenueByCohort")) c(0, 30, 90, 180, 365, 365 * 2) else NA
                # for (day in days)
                # {
                    descr <- paste("metrics", fun, out, by, subscription.length)#cohort.by, "days", day)
                    # Tests that the function works with a total data set
                    test_that(paste("Start and end ", descr),{
                        #print(fun)
                        s = RevenueMetric(FUN = fun, output = out, d$AUD,d$ValidFrom,d$ValidTo, 
                                          start = strt,
                                          end = nd,
                                          id = d$name, by = by, subscription.length = subscription.length)#cohort.by = cohort.by, days.to.count = day)
                        #print(s)
                        if (is.null(s))
                            expect_true(TRUE) # avoiding empty test
                        else {
                            
                            dts = if (any(grepl("Cohort", class(s)))) rownames(s) else 
                            {
                                if (is.matrix(s)) colnames(s) else names(s) 
                            }
                            #print(dts[1:4])
                            dts = AsDate(dts)
                            expect_true(all(dts >= strt))
                            expect_true(all(dts <= nd))
                            #  print(dts)
                        }
                    })
                }
            }
#        }
