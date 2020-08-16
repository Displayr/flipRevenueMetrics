#' \code{CustomerChurn}
#' 
#' The percentage of customers who could churn in a period that did churn.
#' @param data A \code{MetricData} object.
#' @param use The data to be used in the calculation: 'aggregate, "initial', or 'chort'.
#' @details The definition of churn is is not the most widely used definition.
#' A more common definition is the proportion of customers that churned. 
#' These two measures differ in some situations:
#' \enumerate{
#'   \item Incomplete periods. The traditional metric can only be used with completed metrics.
#' This function computes churn for periods that are not yet complete, by using the dat
#' for the customers that were have been up for renewal prior to the point at which the
#' analysis is conducted.
#'   \item Contracts that extend over multiple subsscription lengths. E.g., a two year
#' contract for a product that typically has a one year subscription. This function
#' ignores such contracts in their first year (as the customer cannot churn). Thus, 
#' this function will show a higher churn rate than the more traditional churn.
#'   \item Contracts that start and end with a subscription-length period. For example, 
#' if subscriptions are yearly, and a customer starts on the 1st of Jan and churns on the 
#' 30th of December, they will not appear in the churn statistics in the traditional
#' metric. They do appear in the calculations in this function.
#' }
#' @return A named vector if \code{use = 'aggregate'} or \code{use = 'initial'}, or,
#'   a \code{matrix}. This will contain a number of attributes includeing:
#' \enumerate{
#'   \item The \code{denominator}. Where the statistic is a ratio, this is it's
#'   denominator. Otherwise, it is typically total recurring revenue.
#'   \item The \code{numerator}. Where the statistic is a ratio, this is it's
#'   numerator Otherwise, it is typically the number of customers.
#'   }
#' @importFrom flipTime Period AsDate
#' @export
CustomerChurn <- function(data, use = "aggregate")
{
    calculateChurn(data, components = "churn", volume = FALSE, use = use)
}



#' \code{RecurringRevenueChurn}
#' 
#' Lost revenue due to churned customers as a percentage of total recurring revenue.
#' @inherit CustomerChurn
#' @details The calculation is based on all customers priorimmedidately prior to the 
#' end of the period. Note that this definition is not merely the recurrning revenue-weighted
#' equivalent of [CustomerChurn()], as [CustomerChurn()] is based on customers that could
#' have churned at any stage in the period.
#' @importFrom flipTime Period
#' @export
RecurringRevenueChurn <- function(data, use)
{
    calculateChurn(data, components = "churn", volume = TRUE, use = use)
}

#' \code{NetRecurringRevenueChurn}
#' 
#' The change in the recurring revenue over time. A positive value inducates
#' that [Expansion()] + [Contraction()] > [RecurringRevenueChurn()]
#' @inherit CustomerChurn
#' @param data A \code{MetricData} object.
#' @importFrom flipTime Period AsDate
#' @details Calculated based on all the customer immediately prior to the end of the
#' previous period.
#' @export
NetRecurringRevenueChurn <- function(data, use = "aggregate")
{
    calculateChurn(data, use = use, volume = TRUE, components = c("expansion", "contraction", "churn"))
}

#' \code{Contraction}
#' 
#' The percentage of customers whose recurring revenue declined relative to the previous
#' period.
#' @inherit CustomerChurn
#' @importFrom flipTime Period AsDate
#' @details Calculated based on all the customer immediately prior to the end of the
#' previous period.
#' @export
Contraction <- function(data, use = "aggregate")
{
    calculateChurn(data, use = use, volume = TRUE, components = c("contraction"))
}


#' \code{Expansion}
#' 
#' The percentage of customers whose recurring revenue increased relative to the previous
#' period.
#' @inherit CustomerChurn
#' @importFrom flipTime Period AsDate
#' @details Calculated based on all the customer immediately prior to the end of the
#' previous period.
#' @export
Expansion <- function(data, use = "aggregate")
{
    calculateChurn(data, use = use, volume = TRUE, components = c("expansion"))
}






#' @export
plot.Churn <- function(x, ...)
{
    smooth <- if (length(x) < 4) "None" else "Friedman's super smoother"
    y.title <- if (attr(x, "volume")) "Recurring Revenue Churn Rate" else "Customer Churn Rate"
    columnChart(x, 
                fit.type = smooth,
                fit.ignore.last = TRUE,
                y.title = y.title, 
                y.tick.format = "%", ...)
}

calculateChurn <- function(data, components, volume, use)
{
    calc <- calculate(data, components, volume, use)
    stat <- calc$numerator / calc$denominator
    if (asRetention(components, use))
        stat <- 1 - stat
    class.name <- paste0("Churn", if (use == "cohort") "ByCohort" else "")
    createOutput(stat, class.name, calc)
}    




asRetention <- function(components, use)
{
    if (use == "cohort")
        return(TRUE)
    all(c("churn", "expansion", "contraction") %in% components)
}

tidyingDetailForCustomerChurn <- function(detail, subscription.length, by)
{
    #detail <- detail[, -2]
    colnames(detail)[1] <- "Period"
    #detail$Period <- addSubscriptionLengthToName(detail$Period, subscription.length, by)
    detail
}

addSubscriptionLengthToName <- function(x, subscription.length, by)
{
    Period(AsDate(x) + Periods(1, subscription.length), by)
}    