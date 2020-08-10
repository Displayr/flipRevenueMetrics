context("Revenue Metric - start")

data(q.invoice.lines.short)
d <- q.invoice.lines.short
d$name <- factor(d$name)
levels(d$name) <- paste0("Firm", 1:nlevels(d$name))
churn.funcs <- c("GrowthAccounting",
           "InitialCustomerChurn", 
           "CustomerChurn",
           "RecurringRevenueChurn",
           "RecurringRevenueChurnByCohort",
           "CustomerChurnByCohort",
           "Customers", 
           "CustomerGrowth")

# Checking that start parameter is taken into account 
#library(flipTime)
test_that("Checking quality of id.merges",
          {
              out = "Table"
              fun = "CustomerChurn"
              by = "quarter"
              # Nothing
              expect_error(RevenueMetric(FUN = fun, output = out, d$AUD,d$ValidFrom,d$ValidTo, 
                    id = d$name, by = by), NA)
              # Null
              id.m <- NULL
              expect_error(RevenueMetric(FUN = fun, output = out, d$AUD,d$ValidFrom,d$ValidTo, 
                                         id = d$name, by = by, id.merges = id.m), NA)
              
              # Wrong class (should be a data.frame))
              id.m <- matrix(2,2,1)
              expect_error(RevenueMetric(FUN = fun, output = out, d$AUD,d$ValidFrom,d$ValidTo, 
                                         id = d$name, by = by, id.merges = id.m),
                           "'id.merges' needs to be a data frame")
              
              # IDs going to self
              id.m <- data.frame(id = d$name[1:3],
                                 id.to = d$name[1:3])
              expect_error(RevenueMetric(FUN = fun, output = out, d$AUD,d$ValidFrom,d$ValidTo, 
                                         id = d$name, by = by, id.merges = id.m),
                           "(contains same values as)")

              # Incorrect variable names
              id.m <- data.frame(id = d$name[1:3],
                                 id = d$name[1:3])
              expect_error(RevenueMetric(FUN = fun, output = out, d$AUD,d$ValidFrom,d$ValidTo, 
                                         id = d$name, by = by, id.merges = id.m),
                           "(must be a data)")

              # Incorrect values of id
              id.m <- data.frame(id = LETTERS[1:3],
                                 id.to = d$name[1:3])
              expect_error(RevenueMetric(FUN = fun, output = out, d$AUD,d$ValidFrom,d$ValidTo, 
                                         id = d$name, by = by, id.merges = id.m),
                           "(contains ids not in)")
              id.m <- data.frame(id.to = LETTERS[1:3],
                                 id = d$name[1:3])
              expect_error(RevenueMetric(FUN = fun, output = out, d$AUD,d$ValidFrom,d$ValidTo, 
                                         id = d$name, by = by, id.merges = id.m),
                           "(contains ids not in)")
              id.m <- data.frame(id.to = NA,
                                 id = NA)
              expect_error(RevenueMetric(FUN = fun, output = out, d$AUD,d$ValidFrom,d$ValidTo, 
                                         id = d$name, by = by, id.merges = id.m),
                           "(contains missing values)")
              
              id.m <- data.frame(id = d$name[c(1,1,2)],
                                 id.to = d$name[rep(3,3)])
              expect_error(RevenueMetric(FUN = fun, output = out, d$AUD,d$ValidFrom,d$ValidTo, 
                                         id = d$name, by = by, id.merges = id.m),
                           "(duplicates)")
          })


test_that("Checking calculations",
          {
              out = "Table"
              by = "year"
              data("fake.invoice.lines")
              d = fake.invoice.lines
              # Nothing
              s0 = RevenueMetric(FUN = "CustomerChurn", output = out, d$Value ,d$From, d$To, 
                                         id = d$Name, by = by)
              RevenueMetric(FUN = "GrowthAccounting", output = out, d$Value ,d$From, d$To, 
                                 id = d$Name, by = by)
              
              id.m <- data.frame(id = d$name[c(1)],
                                 id.to = d$name[c(2)])
warning("Turn test back on!!!")              
              # s1 <- RevenueMetric(FUN = fun, output = out, d$AUD, from = d$ValidFrom, to = d$ValidTo, 
              #                            id = d$name, by = by, id.merges = id.m)
          })

