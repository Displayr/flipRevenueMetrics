metric.functions <-  c("GrowthAccounting", 
                       "RecurringRevenue", 
                       "NumberofCustomers",
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




