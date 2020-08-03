context("Revenue Metric - Profiling")
data(q.invoice.lines)
d <- q.invoice.lines
library(lubridate)
end <- ISOdate(2016, 2, 15, tz = tz(q.invoice.lines$ValidFrom))
start <- ISOdate(2012, 7, 1, tz = tz(q.invoice.lines$ValidFrom))


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

p.country <- d[, "country", drop = FALSE]
p.country.salesman <- d[, c("country", "salesman")]
# Splitting by profiling variables
for (fun in funcs)
    for (out in c("Table", "Plot"))
        for (by in c("quarter"))
        {
            cohort.bys <- if (fun %in%  c("RecurringRevenueByCohort", "MeanRecurringRevenueByCohort")) c("month", "quarter", "year") else NA
            for (cohort.by in cohort.bys)
            {
                days <- if (fun %in%  c("MeanRecurringRevenue", "MeanRecurringRevenueByCohort")) c(0, 30, 90, 180, 365, 365 * 2) else NA
                for (day in days)
                {
                    
                    descr <- paste("metrics", fun, out, by, cohort.by, "days", day)
                    
                    # # Tests that the function works with a total data set
                    # test_that(paste("aggregate ", descr),{
                    #     s = RevenueMetric(FUN = fun, output = out, d$AUD,d$ValidFrom,d$ValidTo, id = d$name, by = by, cohort.by = cohort.by, days.to.count = day)
                    #     if (is.null(s)) expect_true(TRUE)
                    #     else capture.output(expect_error(print(s), NA))
                    # })
                    # 
                    # Conducts the analysis split by a profiling variable
                    test_that(paste("profiling 1 ", descr),{
                        s = RevenueMetric(FUN = fun, output = out, d$AUD,d$ValidFrom,d$ValidTo, id = d$name, by = by, cohort.by = cohort.by, profiling = p.country, days.to.count = day)
                        if (is.null(s)) expect_true(TRUE)
                        else capture.output(expect_error(print(s), NA))
                    })
                    
                    # Conducts the analysis split by two profiling variables
                    test_that(paste("profiling 2 ", descr),{
                        s = RevenueMetric(fun, output = out, d$AUD, d$ValidFrom,d$ValidTo, id = d$name, by = by, cohort.by = cohort.by, profiling = p.country.salesman, days.to.count = day)
                        if (is.null(s)) expect_true(TRUE)
                        else capture.output(expect_error(print(s), NA))
                    })
                }
            }
        }
