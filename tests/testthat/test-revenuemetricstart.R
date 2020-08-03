context("Revenue Metric - start")

data(q.invoice.lines.short)
d <- q.invoice.lines.short

funcs <- c("GrowthAccounting",
           "InitialCustomerChurn",  # Churn
           "CustomerChurn",
           "RecurringRevenueChurn",
           "RecurringRevenueChurnByCohort",
           "CustomerChurnByCohort",
           "Customers", # Customers
           "NewCustomers",
           "CustomerGrowth",
           #"Revenue",                  # Revenue
           "RecurringRevenue",
           "RecurringRevenueByCohort",
           # "RecurringRevenueGrowth",
           "MeanRecurringRevenue",
           "GrowthAccounting")#,
#"MeanRecurringRevenueByCohort")

# Checking that start parameter is taken into account 
library(flipTime)
strt = as.Date("2013-01-01")
nd = as.Date("2014-01-01")

for (fun in funcs)
    for (out in c("Table"))#
        for (by in c("month", "quarter", "year"))
        {
            cohort.bys <- if (fun %in%  c("RecurringRevenueByCohort", "MeanRecurringRevenueByCohort")) c("month", "quarter", "year") else NA
            for (cohort.by in cohort.bys)
            {
                days <- if (fun %in%  c("MeanRecurringRevenue", "MeanRecurringRevenueByCohort")) c(0, 30, 90, 180, 365, 365 * 2) else NA
                for (day in days)
                {
                    descr <- paste("metrics", fun, out, by, cohort.by, "days", day)
                    # Tests that the function works with a total data set
                    test_that(paste("Start and end ", descr),{
                        #print(fun)
                        s = RevenueMetric(FUN = fun, output = out, d$AUD,d$ValidFrom,d$ValidTo, 
                                          start = strt,
                                          id = d$name, by = by, cohort.by = cohort.by, days.to.count = day)
                        
                        dts = if (any(grepl("Cohort", class(s)))) rownames(s) else 
                        {
                            if (is.matrix(s)) colnames(s) else names(s) 
                        }
                        #print(dts[1:4])
                        dts = AsDate(dts)
                        expect_true(all(dts >= strt) & all(dts <= nd))
                      #  print(dts)
                    })
                }
            }
        }
