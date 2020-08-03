context("Revenue Metric")
data(q.invoice.lines)
d <- q.invoice.lines
library(lubridate)
end <- ISOdate(2016, 2, 15, tz = tz(q.invoice.lines$ValidFrom))
start <- ISOdate(2012, 7, 1, tz = tz(q.invoice.lines$ValidFrom))



test_that("Create subsets",
          {
              # Country
              s <- flipRevenueMetrics:::createFilters(d[, "country", drop = FALSE], subset = NULL, id = d$name)
              expect_equal(length(names(s)), 5)
              expect_equal(names(s)[1], "Australia")
              expect_equal(sum(s[[1]]), 2386) # Observations, companies can have multiple observations

              # Country - with a filter
              f <- d$salesman == "4"
              s <- flipRevenueMetrics:::createFilters(d[, "country", drop = FALSE], subset = f, id = d$name)
              expect_equal(length(names(s)), 5)
              expect_equal(names(s)[1], "Australia")
              expect_equal(sum(s[[1]]), 504)

              # Saleman
              s <- flipRevenueMetrics:::createFilters(d[, "salesman", drop = FALSE], subset = NULL, id = d$name)
              expect_equal(length(names(s)), 4)
              expect_equal(names(s)[1], "MISSING DATA")

              # Country and salesman
              s <- flipRevenueMetrics:::createFilters(d[, c("country", "salesman")], subset = NULL, id = d$name)
              expect_equal(length(names(s)), 20)
              expect_equal(names(s)[1], "Australia + MISSING DATA")
          })

# by= "quarter"
# for (by in c("month", "quarter", "year"))
#   test_that(paste("Churn consistency", by), {
#     # Near-depricated
#     rdd <- RevenueData(d$AUD,d$ValidFrom,d$ValidTo, id = d$name, subscription.length = "year")
#     r <- Retention(rdd, by = by)
#     # These tests are checking that numer methods of computing churn give the same
#     # answer as depricated methods
#     cc = RevenueMetric("CustomerChurn", output = "Table", d$AUD,d$ValidFrom,d$ValidTo, id = d$name, by = by)
#     expect_equal(flipRevenueMetrics:::removeAttributesAndClass(cc),
#                  1 - r$retention.rate.by.period[names(cc)])
#
#     rrc = RevenueMetric("RecurringRevenueChurn", output = "Table", d$AUD,d$ValidFrom,d$ValidTo, id = d$name, by = by)
#     expect_equal(flipRevenueMetrics:::removeAttributesAndClass(rrc),
#                  1 - r$retention.rate.volume.by.period[names(rrc)])
#
#     ccc <- RevenueMetric("CustomerChurnByCohort", "Table", d$AUD,d$ValidFrom,d$ValidTo, id = d$name, by = by)
#     expect_equal(flipRevenueMetrics:::removeAttributesAndClass(ccc),
#                  1 - r$retention.rate[rownames(ccc), colnames(ccc)])
#
#     rrcc <- RevenueMetric("RecurringRevenueChurnByCohort", "Table", d$AUD,d$ValidFrom,d$ValidTo, id = d$name, by = by)
#     expect_equal(flipRevenueMetrics:::removeAttributesAndClass(rrcc),
#                  1 - r$retention.rate.volume[rownames(rrcc), colnames(rrcc)])
#   })

by= "year"
  test_that(paste("Mean recurrent revenue consistency"), {
    # Near-depricated
    rdd <- RevenueData(d$AUD,d$ValidFrom,d$ValidTo, id = d$name, subscription.length = by)
    r <- Lifetime(rdd)
    # These tests are checking that numer methods of computing churn give the same
    # answer as depricated methods
    rr = RevenueMetric("RecurringRevenueByCohort", output = "Table", d$AUD,d$ValidFrom,d$ValidTo, id = d$name, by = by, cohort.by = by)
    expect_equal(flipRevenueMetrics:::removeAttributesAndClass(rr)[rownames(r$total), ], r$total[, colnames(rr)])

    # rr = RevenueMetric("MeanRecurringRevenueByCohort", output = "Table", d$AUD,d$ValidFrom,d$ValidTo, id = d$name, by = by)
    # expect_equal(flipRevenueMetrics:::removeAttributesAndClass(rr), r$revenue.per.subscriber[, colnames(rr)])

  })

for (by in c("week", "month", "quarter", "year"))
test_that(paste("RecurringRevenue and GrowthAccounting are consistent", by),{
    rr = RevenueMetric("RecurringRevenue", output = "Table",  d$AUD, d$ValidFrom, d$ValidTo, id = d$name,  by = by)
    ga  = RevenueMetric("GrowthAccounting", output = "Table",  d$AUD, d$ValidFrom, d$ValidTo, id = d$name,  by = by)
    aga = cumsum(colSums(ga))
    expect_equal(as.numeric(rr), as.numeric(aga))
})
  

data(q.invoice.lines.short)
d <- q.invoice.lines.short

# This is just checking for errors. Blog projects will be used for checking outputs.
# fun = "RecurringRevenueChurnByCohort"
out = "Table"#"Table"
# by = "month"
p.country <- d[, "country", drop = FALSE]
p.country.salesman <- d[, c("country", "salesman")]

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
#funcs <- "MeanRecurringRevenueByCohort"#, #RecurringRevenueChurnByCohort"
#fun = "RecurringRevenueChurnByCohort"
#fun = "MeanRecurringRevenueChurnByCohort"#MeanRecurringRevenueByCohort"
# Quick run through checking that the basic function works
# fun = "RecurringRevenueGrowth"
for (fun in funcs)
      test_that(paste("metrics", fun),
                {
                  s = RevenueMetric(FUN = fun, output = "Plot", d$AUD,d$ValidFrom,d$ValidTo, id = d$name, by = by)
                  expect_error(print(s), NA)
                }
      )

# Looping though arguments
# fun = "InitialCustomerChurn"
# out = "Detail"
# by = "year"
#funcs = c("MeanRecurringRevenueByCohort")
#funcs = c("RecurringRevenueByCohort")

for (fun in funcs)
    for (out in c("Table", "Plot"))#, "Detail"))
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
                      test_that(paste("aggregate ", descr),{
                        s = RevenueMetric(FUN = fun, output = out, d$AUD,d$ValidFrom,d$ValidTo, id = d$name, by = by, cohort.by = cohort.by, days.to.count = day)
                        if (is.null(s)) expect_true(TRUE)
                        else capture.output(expect_error(print(s), NA))
                      })
                }
            }
        }


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
          
          # Tests that the function works with a total data set
          test_that(paste("aggregate ", descr),{
            s = RevenueMetric(FUN = fun, output = out, d$AUD,d$ValidFrom,d$ValidTo, id = d$name, by = by, cohort.by = cohort.by, days.to.count = day)
            if (is.null(s)) expect_true(TRUE)
            else capture.output(expect_error(print(s), NA))
          })
          
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
