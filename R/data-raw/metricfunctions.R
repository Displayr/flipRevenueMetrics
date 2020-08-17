metric.functions <-  c("RecurringRevenue", 
                       "Customers",
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




