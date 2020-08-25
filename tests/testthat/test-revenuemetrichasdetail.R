context("Revenue Metric - Detail different to table")
data(q.invoice.lines)
d <- q.invoice.lines
library(lubridate)
end <- ISOdate(2016, 2, 15, tz = tz(q.invoice.lines$ValidFrom))
start <- ISOdate(2012, 7, 1, tz = tz(q.invoice.lines$ValidFrom))


data(metric.functions)
fun= "CustomerChurn"
for (fun in metric.functions)
    test_that(paste("Checking that Table, Plot, Detail, exist and are of the right classes",
                    fun),
    {
        tbl = RevenueMetric(FUN = fun, output = "Table", value=d$AUD,from=d$ValidFrom,to=d$ValidTo, id = d$name, by = "quarter", subscription.length = "year")#, cohort.by = cohort.by)#, days.to.count = day)
        plt = RevenueMetric(FUN = fun, output = "Plot", value=d$AUD,from=d$ValidFrom,to=d$ValidTo, id = d$name, by = "quarter", subscription.length = "year")#, cohort.by = cohort.by, days.to.count = day)
        expect_true(any(class(plt) %in% c("htmlwidget")))
        dtl = RevenueMetric(FUN = fun, output = "Detail", value=d$AUD,from=d$ValidFrom,to=d$ValidTo, id = d$name, by = "quarter", subscription.length = "year")#, cohort.by = cohort.by, days.to.count = day)
        expect_false(class(tbl)[1] == class(dtl)[1])
    })