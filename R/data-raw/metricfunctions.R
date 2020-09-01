metric.functions <-  c("GrowthAccounting", 
                       "RecurringRevenue", 
                       "NewRecurringRevenue",
                       "NumberofCustomers",
                       "NewRecurringRevenueGrowth",
                       "UnderlyingRecurringRevenueGrowth",
                       #"RecurringRevenueGrowth",
                       #"Customers",
                       "CustomerChurn",
                       "CustomerRetention",
                       "RecurringRevenueChurn",
                       "Expansion",
                       "Contraction",
                       "RecurringRevenueRetention",
                       "NetRecurringRevenueRetention",
                       "AverageRecurringRevenue")

library(devtools)
use_data(metric.functions, internal = FALSE, overwrite = TRUE)




