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
#' @export
RecurringRevenue <- function(data, use = "Aggregate")
{
    calculateCurrent(data, ratio = FALSE, volume = TRUE, use = use)
}
tidyingDetailForCustomerChurn <- function(detail, subscription.length, by)
{
    #detail <- detail[, -2]
    colnames(detail)[1] <- "Period"
    #detail$Period <- addSubscriptionLengthToName(detail$Period, subscription.length, by)
    detail
}

