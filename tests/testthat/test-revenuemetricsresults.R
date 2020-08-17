context("Revenue Metric - results")
library(lubridate)
library(flipTime)
data(q.invoice.lines)
d <- q.invoice.lines
end <- ISOdate(2016, 2, 15, tz = tz(q.invoice.lines$ValidFrom))
start <- ISOdate(2012, 7, 1, tz = tz(q.invoice.lines$ValidFrom))

by = "year"
sy = RevenueMetric("RecurringRevenue", output = "Table",  d$AUD, d$ValidFrom, d$ValidTo, id = d$name,  by = by)
test_that("Recurring Revenue",
          { 
              expect_equal(as.numeric(sy), c(36990.3305564501, 274498.441101142, 656939.30921227, 1011238.06310803, 
                                             1211959.22553159, 1804608.88256603, 2705382.99216918, 3677057.98312644, 
                                             4250691.85480842))
              # If the next few tests fail, it is likely that there is a misalignment of the period dates.
              # Checking that daily data matches yearly data
              by = "day"
              sd = RevenueMetric("RecurringRevenue", output = "Table",  d$AUD, d$ValidFrom, d$ValidTo, id = d$name,  by = by)
              # Matching to yearly. 
              m <- c(match(as.character(AsDate(names(sy))[-1]), names(sd)) , length(sd))
              expect_equal(as.numeric(sy), as.numeric(sd[m]))
              # Checking that daily data matches yearly data
              by = "month"
              sm = RevenueMetric("RecurringRevenue", output = "Table",  d$AUD, d$ValidFrom, d$ValidTo, id = d$name,  by = by)
              expect_equal(as.numeric(sy[c("2015", "2016")]), as.numeric(sm[c("2015-12", "2016-06")]))
              # Checking that daily data matches yearly data
              by = "quarter"
              sq = RevenueMetric("RecurringRevenue", output = "Table",  d$AUD, d$ValidFrom, d$ValidTo, id = d$name,  by = by)
              expect_equal(as.numeric(sy[c("2015", "2016")]), as.numeric(sq[c("2015-10", "2016-04")]))
          }
)


test_that("Recurring Revenue Churn",
          {
              by = "year"
              s = RevenueMetric("RecurringRevenueChurn", use = "Aggregate", output = "Table",  d$AUD, d$ValidFrom, d$ValidTo, id = d$name,  by = by)
              expect_equal(as.numeric(s), c(0, 0.00503686058558867, 0.131656082941708, 0.0738085730159674, 
                                            0.0512936476901212, 0.0859822919426476, 0.142475132527322, 0.0318116258932942))
              expect_equal(as.numeric(sy[1:(length(sy) - 2)]), as.numeric(attr(s, "denominator")[-length(s)]))

              s = RevenueMetric("RecurringRevenueRetention", use = "Cohort", output = "Table",  d$AUD, d$ValidFrom, d$ValidTo, id = d$name,  by = by)
              expect_equal(s[2, ], c(`2009` = NaN, `2010` = 0.994963139414411, `2011` = 0.947369370000059, 
                                     `2012` = 0.895820705115493, `2013` = 0.889920696404257, `2014` = 0.889920696404257, 
                                     `2015` = 0.818929400709358, `2016` = 0.829851138508574))
              
              sr = RevenueMetric("RecurringRevenueChurn", use = "Initial", output = "Table",  d$AUD, d$ValidFrom, d$ValidTo, id = d$name,  by = by)
              expect_equal(as.numeric(sr), c(0.0373773458628039, 0.0526306299999406, 0.199688856811766, 
                                             0.115366142841956, 0.162440774758907, 0.178098968441071, 0.157996829606064))                                 
          }
)

test_that("GrowthAccounting",
          {
              by = "year"
              g = RevenueMetric("GrowthAccounting", output = "Table",  d$AUD, d$ValidFrom, d$ValidTo, id = d$name,  by = by)
              expect_equal(g[, "2016"], c(New = 457832.625451466, Resurrection = 27522.7272727273, `Major Expansion` = 524883.685489852, 
                                          `Minor Expansion` = 19531.1464753758, Contraction = -404788.339449469, 
                                          Churn = -51347.9735579736))
              # Checkingt that matches yearly recurring revenue data
              sum.g <- cumsum(colSums(g))
              expect_equal(as.numeric(sy), as.numeric(sum.g))
              
              # DS-3058
              data(q.invoice.lines)
              d <- q.invoice.lines
              library(lubridate)
              end <- ISOdate(2016, 2, 15, tz = tz(q.invoice.lines$ValidFrom))
              start <- ISOdate(2012, 7, 1, tz = tz(q.invoice.lines$ValidFrom))
              
              rr = RevenueMetric("RecurringRevenue", output = "Table",  use = "Cohort", d$AUD, d$ValidFrom, d$ValidTo, id = d$name,  by = "year")
              ga  = RevenueMetric("GrowthAccounting", output = "Table",  d$AUD, d$ValidFrom, d$ValidTo, id = d$name,  by = "year")
              aga = cumsum(colSums(ga))
              expect_equal(as.numeric(colSums(rr)), as.numeric(aga))
          }
)

test_that("Customer Churn and Retention",
          {
              by = "year"
              s = RevenueMetric("CustomerChurn", use = "Aggregate", output = "Table",  d$AUD, d$ValidFrom, d$ValidTo, id = d$name,  by = by)
              expect_equal(as.numeric(s), c(0, 0.0344827586206896, 0.0973451327433629, 0.118343195266272, 
                                            0.118421052631579, 0.130718954248366, 0.241299303944316, 0.123076923076923))
              
              s = RevenueMetric("CustomerRetention", use = "Cohort", output = "Table",  d$AUD, d$ValidFrom, d$ValidTo, id = d$name,  by = by)
              expect_equal(s[2, ], c(`2009` = 1, `2010` = 0.978260869565217, `2011` = 0.891304347826087, 
                                     `2012` = 0.923076923076923, `2013` = 0.945945945945946, `2014` = 1, 
                                     `2015` = 0.916666666666667, `2016` = 1))
              
              
              ss = RevenueMetric("CustomerChurn", use = "Initial", output = "Table",  d$AUD, d$ValidFrom, d$ValidTo, id = d$name,  by = by)
              expect_equal(as.numeric(ss),c(0, 0.0217391304347826, 0.111111111111111, 0.123287671232877, 
                                           0.111111111111111, 0.09, 0.389221556886228, 0.0666666666666667))                                 
              expect_equal(1 - as.numeric(diag(s)), as.numeric(ss))
              
              by = "quarter"
              s = RevenueMetric("CustomerChurn", use = "Initial", output = "Table",  d$AUD, d$ValidFrom, d$ValidTo, id = d$name,  by = by)
              attr(ss, "denominator")
              attr(s, "denominator")
              
          }
)



              
                            # 
              # 
              # 
              # g
              # s = RevenueMetric("RecurringRevenueChurn", output = "Table",  d$AUD, d$ValidFrom, d$ValidTo, id = d$name,  by = by)
              # expect_equal(attr(s, "numerator"),  -g[6, -1])
              # 
              # 
              #               # Checking growwth accounting consistent with churh
              # arr = cumsum(colSums(g))
              # rc <- -g[6, ] / arr
              # 
              # s = RevenueMetric("RecurringRevenueRetentionByCohort", output = "Table",  d$AUD, d$ValidFrom, d$ValidTo, id = d$name,  by = by)
              # expect_equal(s[2, ], c(`2009` = NaN, `2010` = 0.994963139414411, `2011` = 0.947369370000059, 
              #                        `2012` = 0.895820705115493, `2013` = 0.889920696404257, `2014` = 0.889920696404257, 
              #                        `2015` = 0.818929400709358, `2016` = 0.829851138508574))
              # 
              # 
              # s = RevenueMetric("InitialRecurringRevenueChurn", output = "Table",  d$AUD, d$ValidFrom, d$ValidTo, id = d$name,  by = by)
              # expect_equal(as.numeric(s),c(0.0373773458628039, 0.0526306299999406, 0.199106589341175, 
              #                              0.115138533321218, 0.162709152884027, 0.178098968441071, 0.157996829606064))                                 



# 
# q <- q.data
# 
# RevenueMetric("InitialCustomerChurn", output = "Table",  q$value, q$from, q$to, id = q$id,  by = by, end = Sys.Date())
# RevenueMetric("GrowthAccounting", output = "Table",  q$value, q$from, q$to, id = q$id,  by = by, end = Sys.Date())
# # 
# # 
# #RevenueMetric("InitialRecurringRevenueChurn", output = "Table",  q$value, q$from, q$to, id = q$id,  by = by, end = Sys.Date())
# RevenueMetric("CustomerChurn", output = "Table",  q$value, q$from, q$to, id = q$id,  by = by, end = Sys.Date())
# #RevenueMetric("RecurringRevenueChurn", output = "Table",  q$value, q$from, q$to, id = q$id,  by = by, end = Sys.Date())
# # 
# d <- q.invoice.lines.short
# RevenueMetric("RecurringRevenueChurn", output = "Table",  q$value, q$from, q$to, id = q$id,  by = by, end = Sys.Date())
# 
# RevenueMetric("NetRecurringRevenueChurn", output = "Table",  q$value, q$from, q$to, id = q$id,  by = by, end = Sys.Date())
# RevenueMetric("Expansion", output = "Table",  q$value, q$from, q$to, id = q$id,  by = by, end = Sys.Date())
# # RevenueMetric("Contraction", output = "Table",  q$value, q$from, q$to, id = q$id,  by = by, end = Sys.Date())
# # 
# q = displayr.2020.aug.17
# # 
# # dputt <- function(x)
# # { out <- as.numeric(x)
# # names(out) <- names(x)
# # dput(out)}    
# # 
# # s = RevenueMetric("InitialRecurringRevenueChurn", output = "Table",  q$value, q$from, q$to, id = q$id,  by = "month", end = Sys.Date())
# # dputt(s)
# # 
# # 
# RevenueMetric("NetRecurringRevenueRetention", output = "Plot",  q$value, q$from, q$to, id = q$id,  by = "month", end = Sys.Date())
# # 
# # RevenueMetric("InitialRecurringRevenueChurn", output = "Plot",  q$value, q$from, q$to, id = q$id,  by = "month", end = Sys.Date())
# RevenueMetric("RecurringRevenueChurn", output = "Plot",  q$value, q$from, q$to, id = q$id,  by = "month", end = Sys.Date())
# RevenueMetric("Expansion", output = "Plot",  q$value, q$from, q$to, id = q$id,  by = "month", end = Sys.Date())
# s = RevenueMetric("RecurringRevenueChurn", output = "Plot",  q$value, q$from, q$to, id = q$id,  by = "month", end = Sys.Date())
# #RevenueMetric("RecurringRevenue", output = "Table",  q$value, q$from, q$to, id = q$id,  by = by, end = Sys.Date())
# RevenueMetric("RecurringRevenue", output = "Plot",  q$value, q$from, q$to, id = q$id,  by = "month", end = Sys.Date())
# RevenueMetric("Customers", output = "Plot",  q$value, q$from, q$to, id = q$id,  by = "month", end = Sys.Date())
# RevenueMetric("Customers", output = "Plot",  q$value, q$from, q$to, id = q$parent,  by = "month", end = Sys.Date())


q = displayr.2020.aug.17
q = q[q$Product == "Q",]
ID = q$id
RevenueMetric("Customers", output = "Plot",  q$value, q$from, q$to, id = ID,  by = "month", end = Sys.Date())

q = displayr.2020.aug.17
q = q[q$Product == "Displayr",]
ID = q$id
RevenueMetric("Customers", output = "Plot",  q$value, q$from, q$to, id = ID,  by = "month", end = Sys.Date())

q = displayr.2020.aug.17
#q = q[q$Product == "Displayr",]
ID = q$id
prof = q[, "Product", drop = FALSE]
RevenueMetric("Customers", output = "Plot",  q$value, q$from, q$to, id = ID,  by = "month", end = Sys.Date(), profiling = prof)
