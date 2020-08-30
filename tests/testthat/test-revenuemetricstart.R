context("Revenue Metric - start parameter")

data(q.invoice.lines.short)
d <- q.invoice.lines.short
data(metric.functions)
library(flipTime)
strt = as.Date("2013-01-01")
nd = as.Date("2015-01-01")

for (fun in metric.functions)
    for (out in c("Table"))#
        for (by in c("month", "quarter", "year"))
        {
            for (subscription.length in "year")
            {
                descr <- paste("metrics", fun, out, by, subscription.length)
                test_that(paste("Start and end ", descr),{
                s = RevenueMetric(FUN = fun, output = out, value=d$AUD,from=d$ValidFrom,to=d$ValidTo, 
                                  start = strt,
                                  end = nd,
                                  id = d$name, by = by, subscription.length = subscription.length)#cohort.by = cohort.by, days.to.count = day)
                if (is.null(s))
                    expect_true(TRUE) # avoiding empty test
                else {
                    
                    dts = if (any(grepl("Cohort", class(s)))) rownames(s) else 
                    {
                        if (is.matrix(s)) colnames(s) else names(s) 
                    }
                    dts = AsDate(dts)
                    expect_true(all(dts >= strt))
                    expect_true(all(dts <= nd))
                }
                })
            }
            }
#        }
