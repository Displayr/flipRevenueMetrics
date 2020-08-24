context("Company mergers")

data(q.invoice.lines.short)
d <- q.invoice.lines.short
d$name <- factor(d$name)
levels(d$name) <- paste0("Firm", 1:nlevels(d$name))
#data(metric.functions)

# Checking that start parameter is taken into account 
#library(flipTime)
test_that("Checking quality of mergers",
          {
              out = "Table"
              fun = "CustomerChurn"
              by = "quarter"
              # Nothing
              expect_error(RevenueMetric(FUN = fun, output = out, value = d$AUD, from = d$ValidFrom, to = d$ValidTo, 
                                         id = d$name, by = by), NA)
              # Null
              id.m <- NULL
              expect_error(RevenueMetric(FUN = fun, output = out, value = d$AUD, from = d$ValidFrom, to = d$ValidTo, 
                                         id = d$name, by = by, mergers = id.m), NA)
              
              # Wrong class (should be a data.frame))
              id.m <- matrix(2,2,1)
              expect_error(RevenueMetric(FUN = fun, output = out, value = d$AUD, from = d$ValidFrom, to = d$ValidTo, 
                                         id = d$name, by = by, mergers = id.m),
                           "'mergers' needs to be a data frame")
              
              # IDs going to self
              id.m <- data.frame(id = d$name[1:3],
                                 id.to = d$name[1:3])
              expect_error(RevenueMetric(FUN = fun, output = out, value = d$AUD, from = d$ValidFrom, to = d$ValidTo, 
                                         id = d$name, by = by, mergers = id.m),
                           "(contains same values as)")
              
              # Incorrect variable names
              id.m <- data.frame(id = d$name[1:3],
                                 id = d$name[1:3])
              expect_error(RevenueMetric(FUN = fun, output = out, value = d$AUD, from = d$ValidFrom, to = d$ValidTo, 
                                         id = d$name, by = by, mergers = id.m),
                           "(must be a data)")
              
              # Incorrect values of id
              id.m <- data.frame(id = LETTERS[1:3],
                                 id.to = d$name[1:3])
              expect_error(RevenueMetric(FUN = fun, output = out, value = d$AUD, from = d$ValidFrom, to = d$ValidTo, 
                                         id = d$name, by = by, mergers = id.m),
                           "(contains ids not in)")
              id.m <- data.frame(id.to = LETTERS[1:3],
                                 id = d$name[1:3])
              expect_error(RevenueMetric(FUN = fun, output = out, value = d$AUD, from = d$ValidFrom, to = d$ValidTo, 
                                         id = d$name, by = by, mergers = id.m),
                           "(contains ids not in)")
              id.m <- data.frame(id.to = NA,
                                 id = NA)
              expect_error(RevenueMetric(FUN = fun, output = out, value = d$AUD, from = d$ValidFrom, to = d$ValidTo, 
                                         id = d$name, by = by, mergers = id.m),
                           "(contains missing values)")
              
              id.m <- data.frame(id = d$name[c(1,1,2)],
                                 id.to = d$name[rep(3,3)])
              expect_error(RevenueMetric(FUN = fun, output = out, value = d$AUD, from = d$ValidFrom, to = d$ValidTo, 
                                         id = d$name, by = by, mergers = id.m),
                           "(duplicates)")
          })


test_that("Checking calculations",
          {
              out = "Table"
              by = "year"
              data("fake.invoice.lines")
              d = fake.invoice.lines
              # Nothing
              s0 = RevenueMetric(FUN = "CustomerChurn", output = out, value = d$Value , from = d$From, to = d$To, 
                                 id = d$Name, by = by)
              
              s <- RevenueMetric(FUN = "CustomerChurn", output = out,value = d$Value , from = d$From, to = d$To,
                                 id = d$Name, by = by, mergers = NULL)
              expect_equal(s["2018"], c("2018" = 0.5))
              expect_equal(attr(s, "den")["2018"], c("2018" = 2))
              id.m <- data.frame(id = d$Name[c(1)],
                                 id.to = d$Name[c(7)])
              s1 <- RevenueMetric(FUN = "CustomerChurn", output =out, value = d$Value , from = d$From, to = d$To, #end = as.Date("2020/06/30"), 
                                  id = d$Name, by = by, mergers = id.m)
              expect_equal(s1["2018"], c("2018" = 0))
              expect_equal(attr(s1, "den")["2018"], c("2018" = 1))
              
              
              data(q.invoice.lines)
              q <- q.invoice.lines
              s <- RevenueMetric(FUN = "CustomerChurn", output = out, value = q$AUD , from = q$ValidFrom, to = q$ValidTo,
                                 id = q$name, by = by, mergers = NULL)
              expect_equal(s["2015"], c("2015" = 0.24129930))
              unique.ids = unique(q$name)
              id.m <- data.frame(id = unique.ids[1:300],
                                 id.to = unique.ids[301:600])
              s1 <- RevenueMetric(FUN = "CustomerChurn", output = out, value = q$AUD , from = q$ValidFrom, to = q$ValidTo,
                                  id = q$name, by = by, mergers = id.m)
              expect_equal(s1["2015"], c("2015" = 0.20437956))
              expect_true(all(s[-1:-2] >= s1[-1:-2]))
              
              
          })

