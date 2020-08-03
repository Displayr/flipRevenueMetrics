context("Revenue Metric - Detail different to table")
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


          
for (fun in funcs)
    test_that(paste("Checking that Table, Plot, Detail, exist and are of the right classes",
                    fun),
    {
        tbl = RevenueMetric(FUN = fun, output = "Table", d$AUD,d$ValidFrom,d$ValidTo, id = d$name, by = "quarter")#, cohort.by = cohort.by)#, days.to.count = day)
        plt = RevenueMetric(FUN = fun, output = "Plot", d$AUD,d$ValidFrom,d$ValidTo, id = d$name, by = "quarter")#, cohort.by = cohort.by, days.to.count = day)
        expect_true(any(class(plt) %in% c("htmlwidget")))
        dtl = RevenueMetric(FUN = fun, output = "Detail", d$AUD,d$ValidFrom,d$ValidTo, id = d$name, by = "quarter")#, cohort.by = cohort.by, days.to.count = day)
        expect_false(class(tbl)[1] == class(dtl)[1])
    }) 