#' \code{RecurringRevenue}
#'
#' @description Computes recurring revenue at the end of the period (i.e., as of
#' the instant before the next time periods starts). Where the time period isn't complete, 
#' the recurring revenue is calculated up until the instant before attr(data, "end").
#' @inheritParams CustomerChurn
#' @details For example, if with yearly data, if a customer has an annual license for $1,000
#' from 1 February to 1 February of the next year, and a second license which starts on 1 January of the last year
#' with a prorated price of $1000/12, then the recurring revenue is $2,000.
#' @return A vector showing the recurring revenue by time points.
#' @importFrom lubridate as_datetime
RecurringRevenue <- function(data)
{
    # calculateUnivariate(data, 
    #                     ratio = FALSE, 
    #                     volume = TRUE,
    #                     name = "Recurring Revenue",
    #                     statistic = "numerator")
    # calculateUnivariate <- function(data, ratio, volume, name, statistic)
    # {
        calc <- calculate(data, components = "current", volume = TRUE)
        stat <- calc[["numerator"]]
        plt <- if (!singleSeries(data)) "Heatmap" else  {
                if  (newCohort(data)) "OneDimensionalWithTrend" else "OneDimensionalWithoutTrend"
            }
        createOutput(stat, plt, calc, "Recurring Revenue")
    # }    
    
}
# tidyingDetailForCustomerChurn <- function(detail, subscription.length, by)
# {
#     #detail <- detail[, -2]
#     colnames(detail)[1] <- "Period"
#     #detail$Period <- addSubscriptionLengthToName(detail$Period, subscription.length, by)
#     detail
# }

#' \code{NumberofCustomers}
#'
#' @description Number of customers by time period..
#' @inheritParams CustomerChurn
#' @details For example, if with yearly data, if a customer has an annual license for $1,000
#' from 1 February to 1 February of the next year, and a second license which starts on 1 January of the last year
#' with a prorated price of $1000/12, then the recurring revenue is $2,000.
#' @return A vector showing the recurring revenue by time points.
#' @importFrom lubridate as_datetime
NumberofCustomers <- function(data)
{
    calc <- calculate(data, components = "number of customers", volume = FALSE)
    plt <- if (newCohort(data)) "OneDimensionalWithTrend" else "OneDimensionalWithoutTrend"
    createOutput(calc$denominator, plt, calc, "Customers")
}    
    



#    calculateUnivariate(data, ratio = FALSE, volume = TRUE, cohort.type = cohort.type,
#                        name = "Customers", "denominator")
#calculateRatio(data, ratio = FALSE, components = "customers", cohort.type = cohort.type,
#               name = "Customers")
#}


#        calculateRecurringRevenueOrCustomers(data, recurring.revenue = FALSE)


# from <- floor_date(AsDate(attr(data, "start")), unit = by)
# end <- floor_date(AsDate(attr(data, "end")), unit = by)
# dts <- seq.Date(from, end, by = by)
# m <- matrix(dts, nrow(data), length(dts), byrow = TRUE)
# m <- sweep(m, 1, as.numeric(as.Date(data$from)), ">=") & sweep(m, 1, as.numeric(as.Date(data$to)), "<")  
# out <- apply(m, 2, function(x) nUnique(data$id[x]))
# names(out) <- Period(dts, by)
# detail <- data[data$observation == 0 &  data$to >= attr(data, "start") & data$to <= attr(data, "end"), 
#                c("id", "subscriber.from.period", "subscriber.to.period")]
# addAttributesAndClass(out, "Customers", by, detail)