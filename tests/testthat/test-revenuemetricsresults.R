context("Revenue Metric - results")
library(lubridate)
library(flipTime)
data(q.invoice.lines)
d <- q.invoice.lines
end <- ISOdate(2016, 2, 15, tz = tz(q.invoice.lines$ValidFrom))
start <- ISOdate(2012, 7, 1, tz = tz(q.invoice.lines$ValidFrom))

by = "year"
sy = RevenueMetric("RecurringRevenue", output = "Table",  value=d$AUD, from=d$ValidFrom, to=d$ValidTo, id = d$name,  by = by)
test_that("Recurring Revenue",
          { 
              expect_equal(as.numeric(sy[-1]), c(36990.3305564501, 274498.441101142, 656939.30921227, 1011238.06310803, 
                                             1211959.22553159, 1804608.88256603, 2705382.99216918, 3677057.98312644, 
                                             4250691.85480842))
              # If the next few tests fail, it is likely that there is a misalignment of the period dates.
              # Checking that daily data matches yearly data
              by = "day"
              sd = RevenueMetric("RecurringRevenue", output = "Table",  value=d$AUD, from=d$ValidFrom, to=d$ValidTo, id = d$name,  by = by)
              # Matching to yearly. 
              m <- c(match(as.character(AsDate(names(sy))[-1]), names(sd)) , length(sd)) - 1
              expect_equal(as.numeric(sy), as.numeric(sd[m]))
              # Checking that daily data matches yearly data
              by = "month"
              sm = RevenueMetric("RecurringRevenue", output = "Table",  value=d$AUD, from=d$ValidFrom, to=d$ValidTo, id = d$name,  by = by)
              expect_equal(as.numeric(sy[c("2015", "2016")]), as.numeric(sm[c("2015-12", "2016-06")]))
              # Checking that daily data matches yearly data
              by = "quarter"
              sq = RevenueMetric("RecurringRevenue", output = "Table",  value=d$AUD, from=d$ValidFrom, to=d$ValidTo, id = d$name,  by = by)
              expect_equal(as.numeric(sy[c("2015", "2016")]), as.numeric(sq[c("2015-10", "2016-04")]))
          }
)


test_that("Recurring Revenue Churn",
          {
              by = "year"
              s = RevenueMetric("RecurringRevenueChurn", cohort.type = "None", output = "Table",  value=d$AUD, from=d$ValidFrom, to=d$ValidTo, id = d$name,  by = by)
              expect_equal(as.numeric(s[-1:-2]), c(0, 0.00503686058558867, 0.131656082941708, 0.0738085730159674, 
                                            0.0512936476901212, 0.0859822919426476, 0.142475132527322, 0.103565887))
              expect_equal(as.numeric(sy[2:(length(sy)-2)]), as.numeric(attr(s, "denominator")[3:(length(s)-1)]))

              sr = RevenueMetric("RecurringRevenueRetention", cohort.type = "Calendar", output = "Table",  value=d$AUD, from=d$ValidFrom, to=d$ValidTo, id = d$name,  by = by)
              ss = colSums(Numerator(sr), na.rm = TRUE) / colSums(Denominator(sr), na.rm = TRUE)
              expect_equivalent(1 - ss, s[-1:-2])

              s2 = RevenueMetric("RecurringRevenueChurn", cohort.type = "New", output = "Table",  value=d$AUD, from=d$ValidFrom, to=d$ValidTo, id = d$name,  by = by)
              sss2 <- diag(Denominator(sr)) - diag(Numerator(sr))
              k <- length(sss2)
              # Removing last element for reasons described in documentation of RevenueMetric
              expect_equal(as.numeric(sss2[-k]), as.numeric(Numerator(s2)[-1:-2][-k]))
              expect_equal(as.numeric(diag(Denominator(sr))[-k]), as.numeric(Denominator(s2)[-1:-2][-k]))
              expect_equal(1 - as.numeric(diag(sr))[-k], as.numeric(s2)[-1:-2][-k])
              
              by = "month"
              s = RevenueMetric("RecurringRevenueChurn", cohort.type = "New", output = "Plot",  value=d$AUD, from=d$ValidFrom, to=d$ValidTo, id = d$name,  by = by)
              sss2 <- diag(Numerator(sr))
              k <- length(sss2)
              
          }
)

test_that("GrowthAccounting",
          {
              by = "year"
              g = RevenueMetric("GrowthAccounting", output = "Table",  value=d$AUD, from=d$ValidFrom, to=d$ValidTo, id = d$name,  by = by)
              expect_equal(g[, "2016"], c(New = 457832.625451466, Resurrection = 27522.7272727273, `Major Expansion` = 524883.685489852, 
                                          `Minor Expansion` = 19531.1464753758, Contraction = -404788.339449469, 
                                          Churn = -51347.9735579736))
              # Checkingt that matches yearly recurring revenue data
              sum.g <- cumsum(colSums(g))
              expect_equal(as.numeric(sy[-1]), as.numeric(sum.g))
              
              # DS-3058
              data(q.invoice.lines)
              d <- q.invoice.lines
              library(lubridate)
              end <- ISOdate(2016, 2, 15, tz = tz(q.invoice.lines$ValidFrom))
              start <- ISOdate(2012, 7, 1, tz = tz(q.invoice.lines$ValidFrom))
              
              rr = RevenueMetric("RecurringRevenue", output = "Table",  cohort.type = "Calendar", value=d$AUD, from=d$ValidFrom, to=d$ValidTo, id = d$name,  by = "year")
              ga  = RevenueMetric("GrowthAccounting", output = "Table",  value=d$AUD, from=d$ValidFrom, to=d$ValidTo, id = d$name,  by = "year")
              aga = cumsum(colSums(ga, na.rm = TRUE))
              rrs <- colSums(rr, na.rm = TRUE)
              expect_equal(as.numeric(rrs), as.numeric(aga))
          }
)

test_that("Customer Churn and Retention",
          {
              by = "year"
              s.none = RevenueMetric("CustomerChurn", cohort.type = "None", output = "Table",  value=d$AUD, from=d$ValidFrom, to=d$ValidTo, id = d$name,  by = by)
              expect_equal(as.numeric(s.none[-1:-2]), c(0, 0.0344827586206896, 0.0973451327433629, 0.118343195266272, 
                                            0.118421052631579, 0.130718954248366, 0.241299303944316, 0.123076923076923))
              
              s = RevenueMetric("CustomerRetention", cohort.type = "Calendar", output = "Table",  value=d$AUD, from=d$ValidFrom, to=d$ValidTo, id = d$name,  by = by)
              expect_equal(s[2, -1], c(`2010` = 0.978260869565217, `2011` = 0.891304347826087, 
                                     `2012` = 0.923076923076923, `2013` = 0.945945945945946, `2014` = 1, 
                                     `2015` = 0.916666666666667, `2016` = 1))

              ss = RevenueMetric("CustomerChurn", cohort.type = "New", output = "Table",  value=d$AUD, from=d$ValidFrom, to=d$ValidTo, id = d$name,  by = by)
              expect_equal(as.numeric(ss[-1:-2]),c(0, 0.0217391304347826, 0.111111111111111, 0.123287671232877, 
                                           0.111111111111111, 0.09, 0.389221556886228, 0.06779661))                                 


          }
)

