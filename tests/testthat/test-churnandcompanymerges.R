context("Company mergers")

data(q.invoice.lines.short)
d <- q.invoice.lines.short
d$name <- factor(d$name)
levels(d$name) <- paste0("Firm", 1:nlevels(d$name))

# Checking that start parameter is taken into account 
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


test_that("Fake invoice calculations",
          {
              out = "Table"
              by = "year"
              data("fake.invoice.lines")
              d = fake.invoice.lines
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
              
              s <- RevenueMetric(FUN = "Contraction", output = out,value = d$Value , from = d$From, to = d$To,
                                 id = d$Name, by = by, mergers = NULL)
              expect_equal(s["2018"], c("2018" = 0))
              s1 <- RevenueMetric(FUN = "Contraction", output =out, value = d$Value , from = d$From, to = d$To, #end = as.Date("2020/06/30"), 
                                  id = d$Name, by = by, mergers = id.m)
              expect_equal(s1["2018"], c("2018" = 0.09090909), tol = 0.0000001)
              
              s <- RevenueMetric(FUN = "RecurringRevenueChurn", output = out,value = d$Value , from = d$From, to = d$To,
                                 id = d$Name, by = by, mergers = NULL)
              expect_equal(s["2018"], c("2018" = 0.09090909))
              s1 <- RevenueMetric(FUN = "RecurringRevenueChurn", output =out, value = d$Value , from = d$From, to = d$To, #end = as.Date("2020/06/30"), 
                                  id = d$Name, by = by, mergers = id.m)
              expect_equal(s1["2018"], c("2018" = 0), tol = 0.0000001)

              s <- RevenueMetric(FUN = "NetRecurringRevenueRetention", output = out,value = d$Value , from = d$From, to = d$To,
                                 id = d$Name, by = by, mergers = NULL)
              expect_equal(s["2018"], c("2018" = 1 - 0.09090909), tol = 0.0000001)
              s1 <- RevenueMetric(FUN = "NetRecurringRevenueRetention", output =out, value = d$Value , from = d$From, to = d$To, #end = as.Date("2020/06/30"), 
                                  id = d$Name, by = by, mergers = id.m)
              expect_equal(s1["2018"], c("2018" = 1- 0.09090909), tol = 0.0000001)
              
              s <- RevenueMetric(FUN = "GrowthAccounting", output = out,value = d$Value , from = d$From, to = d$To,
                                 id = d$Name, by = by, mergers = NULL)
              expect_equal(s["Churn", "2018"], -1000, tol = 0.0000001)
              s1 <- RevenueMetric(FUN = "GrowthAccounting", output =out, value = d$Value , from = d$From, to = d$To, #end = as.Date("2020/06/30"), 
                                  id = d$Name, by = by, mergers = id.m)
              expect_equal(s1["Contraction", "2018"], -1000, tol = 0.0000001)
})

test_that("No customers churn in 2014", {
    # These tests compare some real data, with data that's been modified so that 
    # any churners in 2014 are treated as mergers. Organization 672 does churn, \
    # but resurrects, which is why they show up in the data    
    data(q.invoice.lines)
    q <- q.invoice.lines
    unique.ids = unique(q$name)
    by = "year"
    out= "Table" 
    # Treating anybody that churned in 2014 as a merger
    z = aggregate(q$ValidTo, list(q$name), max)
    ids.churned.2014 <- z[z[, 2] < as.Date("2015-01-01") & z[, 2] >= as.Date("2014-01-01"), 1]
    ids.churned.2014 <- ids.churned.2014
    ids.alive.2015 <- z[z[, 2] >= as.Date("2015-01-01"), 1]
    # set.seed(1223)
    # id.m <- data.frame(id = ids.churned.2014,
    #                  id.to = sample(ids.alive.2015, length(ids.churned.2014), TRUE))
    id.m <- data.frame(id = ids.churned.2014,
                       id.to = structure(c(32L, 7L, 16L, 15L, 11L, 41L, 8L, 36L, 40L, 4L, 37L, 
                                           5L, 23L, 20L, 13L, 3L, 10L, 29L, 26L, 28L, 18L, 25L, 38L, 22L, 
                                           1L, 27L, 45L, 14L, 33L, 35L, 12L, 6L, 46L, 24L, 31L, 30L, 44L, 
                                           9L, 42L, 34L, 2L, 21L, 43L, 17L, 39L, 19L), .Label = c("Organization 119", 
                                                                                                  "Organization 130", "Organization 163", "Organization 17", "Organization 175", 
                                                                                                  "Organization 187", "Organization 19", "Organization 209", "Organization 212", 
                                                                                                  "Organization 227", "Organization 235", "Organization 243", "Organization 253", 
                                                                                                  "Organization 282", "Organization 301", "Organization 334", "Organization 336", 
                                                                                                  "Organization 362", "Organization 389", "Organization 397", "Organization 420", 
                                                                                                  "Organization 446", "Organization 449", "Organization 461", "Organization 467", 
                                                                                                  "Organization 474", "Organization 475", "Organization 521", "Organization 524", 
                                                                                                  "Organization 529", "Organization 531", "Organization 535", "Organization 581", 
                                                                                                  "Organization 593", "Organization 599", "Organization 606", "Organization 63", 
                                                                                                  "Organization 681", "Organization 684", "Organization 685", "Organization 686", 
                                                                                                  "Organization 693", "Organization 705", "Organization 746", "Organization 751", 
                                                                                                  "Organization 768"), class = "factor"))
    s <- RevenueMetric(FUN = "CustomerChurn", output = out, value = q$AUD , from = q$ValidFrom, to = q$ValidTo,
                       id = q$name, by = by, mergers = NULL)
    expect_equal(s["2014"], c("2014" = 0.13114754 ), tol = 0.000001)
    s1 <- RevenueMetric(FUN = "CustomerChurn", output = out, value = q$AUD , from = q$ValidFrom, to = q$ValidTo,
                        id = q$name, by = by, mergers = id.m)
    expect_equal(s1["2014"], c("2014" = 0.003759398 ), tol = 0.000001)
    
    sc <- RevenueMetric(FUN = "RecurringRevenueChurn", output = out, value = q$AUD , from = q$ValidFrom, to = q$ValidTo,
                       id = q$name, by = by, mergers = NULL)
    expect_equal(sc["2014"], c("2014" = 0.08624760  ), tol = 0.000001)
    s0 <- RevenueMetric(FUN = "RecurringRevenueChurn", output = out, value = q$AUD , from = q$ValidFrom, to = q$ValidTo,
                        id = q$name, by = by, mergers = id.m)
    expect_equal(s0["2014"], c("2014" = 0.03561232), tol = 0.000001)
    
    s <- RevenueMetric(FUN = "CustomerRetention", output = out, value = q$AUD , from = q$ValidFrom, to = q$ValidTo,
                       id = q$name, by = by, mergers = NULL)
    expect_equal(s["2014"], c("2014" = 0.8688525 ), tol = 0.000001)
    s1 <- RevenueMetric(FUN = "CustomerRetention", output = out, value = q$AUD , from = q$ValidFrom, to = q$ValidTo,
                        id = q$name, by = by, mergers = id.m)
    expect_equal(s1["2014"], c("2014" = 0.9962406), tol = 0.000001)
    
    s <- RevenueMetric(FUN = "RecurringRevenueRetention", output = out, value = q$AUD , from = q$ValidFrom, to = q$ValidTo,
                     id = q$name, by = by, mergers = NULL)
    expect_equal(s["2014"], c("2014" = 0.9137524), tol = 0.00001)
    s1 <- RevenueMetric(FUN = "RecurringRevenueRetention", output = out, value = q$AUD , from = q$ValidFrom, to = q$ValidTo,
                      id = q$name, by = by, mergers = id.m)
    expect_equal(s1["2014"], c("2014" = 0.9643877), tol = 0.000001)
    
    s <- RevenueMetric(FUN = "NetRecurringRevenueRetention", output = out, value = q$AUD , from = q$ValidFrom, to = q$ValidTo,
                     id = q$name, by = by, mergers = NULL)
    expect_equal(s["2014"], c("2014" = 1.058812), tol = 0.00001)
    s2 <- RevenueMetric(FUN = "NetRecurringRevenueRetention", output = out, value = q$AUD , from = q$ValidFrom, to = q$ValidTo,
                      id = q$name, by = by, mergers = id.m)
    expect_equal(s2["2014"], c("2014" = 1.075586 ), tol = 0.000001)
    
    s <- RevenueMetric(FUN = "Expansion", output = out, value = q$AUD , from = q$ValidFrom, to = q$ValidTo,
                     id = q$name, by = by, mergers = NULL)
    expect_equal(s["2014"], c("2014" = 0.3474605), tol = 0.00001)
    s3 <- RevenueMetric(FUN = "Expansion", output = out, value = q$AUD , from = q$ValidFrom, to = q$ValidTo,
                      id = q$name, by = by, mergers = id.m)
    expect_equal(s3["2014"], c("2014" = 0.3449015), tol = 0.000001)
    
    s <- RevenueMetric(FUN = "Contraction", output = out, value = q$AUD , from = q$ValidFrom, to = q$ValidTo,
                     id = q$name, by = by, mergers = NULL)
    expect_equal(s["2014"], c("2014" = 0.2024007 ), tol = 0.00001)
    s4 <- RevenueMetric(FUN = "Contraction", output = out, value = q$AUD , from  = q$ValidFrom, to = q$ValidTo,
                      id = q$name, by = by, mergers = id.m)
    expect_equal(s4["2014"], c("2014" = 0.2337031), tol = 0.000001)
    z = s2 - (s1 - s4 + s3)
    expect_equal(mean(z, na.rm = TRUE), 0)
    expect_equal(sd(z, na.rm = TRUE), 0)
    
    ss <- RevenueMetric(FUN = "GrowthAccounting", output = out, value = q$AUD , from = q$ValidFrom, to = q$ValidTo,
                     id = q$name, by = by, mergers = NULL)
    k <- NCOL(ss) 
    expect_equal(Numerator(sc)[-1][-k], -ss["Churn", -k]) # The last period is different: see documentation
    
    ss1 <- RevenueMetric(FUN = "GrowthAccounting", output = out, value = q$AUD , from = q$ValidFrom, to = q$ValidTo,
                        id = q$name, by = by, mergers = id.m)
    k <- NCOL(ss) 
    expect_equal(colSums(ss), colSums(ss1))
    expect_equal(Numerator(s0)[-1][-k], -ss1["Churn", -k]) # The last period is different: see documentation
    expect_equal(Numerator(s3)[-1][-k], colSums(ss1[c("Major Expansion", "Minor Expansion"), -k])) # The last period is different: see documentation
    expect_equal(Numerator(s4)[-1][-k], -ss1["Contraction", -k]) # The last period is different: see documentation


    rr <- RevenueMetric(FUN = "RecurringRevenue", output = out, value = q$AUD , from = q$ValidFrom, to = q$ValidTo,
                         id = q$name, by = by, mergers = id.m)
    
    expect_equal(as.numeric(diff(rr)), as.numeric(colSums(ss1)))
          })



test_that("Mergers and profiling", {

    data(q.invoice.lines.short)
    d = q.invoice.lines.short
    by = "year"
    id.m <- data.frame(id = d$name[1],
                       id.to = d$name[7])
    expect_error(RevenueMetric(FUN = "NumberofCustomers", output = "Table", value = d$AUD, from = d$ValidFrom, to = d$ValidTo,
                               id = d$name, by = by, mergers = id.m, profiling = data.frame(d$currency)),
                 NA)
    
    data(q.invoice.lines)
    q <- q.invoice.lines
    unique.ids = unique(q$name)
    by = "year"
    out= "Table" 
    # Treating anybody that churned in 2014 as a merger
    z = aggregate(q$ValidTo, list(q$name), max)
    ids.churned.2014 <- z[z[, 2] < as.Date("2015-01-01") & z[, 2] >= as.Date("2014-01-01"), 1]
    ids.churned.2014 <- ids.churned.2014
    ids.alive.2015 <- z[z[, 2] >= as.Date("2015-01-01"), 1]
    id.m <- data.frame(id = ids.churned.2014,
                       id.to = structure(c(32L, 7L, 16L, 15L, 11L, 41L, 8L, 36L, 40L, 4L, 37L, 
                                           5L, 23L, 20L, 13L, 3L, 10L, 29L, 26L, 28L, 18L, 25L, 38L, 22L, 
                                           1L, 27L, 45L, 14L, 33L, 35L, 12L, 6L, 46L, 24L, 31L, 30L, 44L, 
                                           9L, 42L, 34L, 2L, 21L, 43L, 17L, 39L, 19L), .Label = c("Organization 119", 
                                                                                                  "Organization 130", "Organization 163", "Organization 17", "Organization 175", 
                                                                                                  "Organization 187", "Organization 19", "Organization 209", "Organization 212", 
                                                                                                  "Organization 227", "Organization 235", "Organization 243", "Organization 253", 
                                                                                                  "Organization 282", "Organization 301", "Organization 334", "Organization 336", 
                                                                                                  "Organization 362", "Organization 389", "Organization 397", "Organization 420", 
                                                                                                  "Organization 446", "Organization 449", "Organization 461", "Organization 467", 
                                                                                                  "Organization 474", "Organization 475", "Organization 521", "Organization 524", 
                                                                                                  "Organization 529", "Organization 531", "Organization 535", "Organization 581", 
                                                                                                  "Organization 593", "Organization 599", "Organization 606", "Organization 63", 
                                                                                                  "Organization 681", "Organization 684", "Organization 685", "Organization 686", 
                                                                                                  "Organization 693", "Organization 705", "Organization 746", "Organization 751", 
                                                                                                  "Organization 768"), class = "factor"))
    
    expect_error(RevenueMetric(FUN = "RecurringRevenueChurn", output = "Table", value = d$AUD, from = d$ValidFrom, to = d$ValidTo,
                               id = d$name, by = by, mergers = id.m, profiling = data.frame(d$country_cat)),
                 NA)
    
})

test_that("Mergers and subset", {

    by = "year"
    data(q.invoice.lines.short)
    d = q.invoice.lines.short
    id.m <- data.frame(id = d$name[1],
                       id.to = d$name[7])
    f <- d$name != id.m$id[1]
    d$name <- as.character(d$name)
    expect_error(RevenueMetric(FUN = "NumberofCustomers", output = "Table", value = d$AUD, from = d$ValidFrom, to = d$ValidTo,
                               id = d$name, by = by,
                               mergers = id.m, profiling = data.frame(d$currency),
                               subset = f),
                 NA)

})