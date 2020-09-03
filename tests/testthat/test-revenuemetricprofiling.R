context("Revenue Metric - Profiling")
data(q.invoice.lines)
d <- q.invoice.lines
library(lubridate)
end <- ISOdate(2016, 2, 15, tz = tz(q.invoice.lines$ValidFrom))
start <- ISOdate(2012, 7, 1, tz = tz(q.invoice.lines$ValidFrom))


data(metric.functions)

p.country <- d[, "country", drop = FALSE]
p.country.salesman <- d[, c("country", "salesman")]
# Splitting by profiling variables
for (fun in metric.functions)
    for (out in c("Table", "Plot"))
        for (by in c("quarter"))
        {
            for (subscription.length in "year")
            {
                descr <- paste("metrics", fun, out, by, subscription.length)#, "days", day)
                    # Conducts the analysis split by a profiling variable
                    test_that(paste("profiling 1 ", descr),{
                        s = RevenueMetric(FUN = fun, output = out, value=d$AUD,from=d$ValidFrom,to=d$ValidTo, id = d$name, by = by, subscription.length = subscription.length, profiling = p.country)
                        if (is.null(s)) expect_true(TRUE)
                        else capture.output(expect_error(print(s), NA))
                    })
                    
                    # Conducts the analysis split by two profiling variables
                    test_that(paste("profiling 2 ", descr),{
                        s = RevenueMetric(fun, output = out, value=d$AUD,from=d$ValidFrom,to=d$ValidTo, id = d$name, by = by, subscription.length = subscription.length, profiling = p.country)#cohort.by = cohort.by, profiling = p.country.salesman, days.to.count = day)
                        if (is.null(s)) expect_true(TRUE)
                        else capture.output(expect_error(print(s), NA))
                    })
                }
            }
 
test_that(paste("stackMatrices "),{
    
    s = RevenueMetric(FUN = "Contraction", 
                      output = "Table",
                      cohort.type = "Calendar",
                      value=d$AUD,from=d$ValidFrom,to=d$ValidTo, id = d$name, by = "year", 
                      subscription.length = "year", 
                      start = as.Date("2014-01-01"),
                      profiling = p.country)
    expect_equal(s, structure(c(0.0632014124792544, NaN, 0.377708171934232, NaN, 
                                          0.0564391440379688, NaN, 0, 0.13040103723447, NaN, 0.203038015936136, 
                                          0.0904112723467829, 0, 0.224546681181527, 0.91316311663968, 0.277940416868016, 
                                          0.666666666666667, 0.161819416727558, 0.173608215638123), .Dim = c(9L, 
                                                                                                             2L), .Dimnames = list(c("Australia 2014", "Australia 2015", "United Kingdom 2014", 
                                                                                                                                     "United Kingdom 2015", "Other 2014", "Other 2015", "New Zealand 2014", 
                                                                                                                                     "United States 2014", "United States 2015"), c("2015", "2016"
                                                                                                                                     ))))
})
