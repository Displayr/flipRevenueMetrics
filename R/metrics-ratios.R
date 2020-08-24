# This file contains metrics that can either be expressed as a ratio, or, as just the numerator.

#' \code{CustomerChurn}
#' 
#' The percentage or number of customers who could churn in a period that did churn.
#' @param data A \code{MetricData} object.
#' @param ratio If \code{TRUE}, the statistic is returned as a ratio.
#' @details The definition of churn is is not the most widely cohort.typed definition.
#' A more common definition is the proportion of customers that churned. 
#' These two measures differ in some situations:
#' \enumerate{
#'   \item Incomplete periods. The traditional metric can only be cohort.typed with completed metrics.
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
#' @return A named vector if \code{cohort.type} is set to \code{"None"} or \code{"Preceding"}, or,
#'   a \code{matrix}. This will contain a number of attributes includeing:
#' \enumerate{
#'   \item \code{denominator} cohort.typed when \code{ratio} is \code{TRUE}.
#'   \item \code{numerator} cohort.typed when \code{ratio} is \code{TRUE}.
#'   \item The \code{detail} cohort.typed when \code{ratio} is \code{TRUE}.
#'   }
#' @importFrom flipTime Period AsDate
CustomerChurn <- function(data, ratio = TRUE)
{
    calculateRatio(data, ratio = ratio, volume = FALSE, components = "churn",
                   name = "Customer Churn")
}

#' \code{CustomerRetention}
#' 
#' The number of proportion of customers that are retained.
#' @inherit CustomerChurn
#' @details Based on those whose contracts were
#' up for renewal.
#' @importFrom flipTime Period
CustomerRetention <- function(data, ratio = TRUE)
{
    calculateRatio(data, ratio = ratio, volume = FALSE, components = "retention",
                   name = "Customer Retention")
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
RecurringRevenueChurn <- function(data, ratio = TRUE)
{
    calculateRatio(data, ratio = ratio, volume = TRUE, components = "churn",
                   name = "Recurring Revenue Churn")
}

#' \code{RecurringRevenueRetention}
#' 
#' The amount, or percent, of recurring revenue retained over time.
#' @inherit CustomerChurn
#' @param data A \code{MetricData} object.
#' @importFrom flipTime Period AsDate
#' @details 1 -  [RecurringRevenueChurn()].
#' includes [Expansion()] and [Contraction()]
RecurringRevenueRetention <- function(data,  ratio = TRUE)
{
    calculateRatio(data, ratio = ratio, volume = TRUE, components = "retention",
                   name = "Recurring Revenue Retenton")
}

#' \code{NetRecurringRevenueRetention}
#' 
#' The change in the recurring revenue over time. A positive value inducates
#' that [Expansion()] + [Contraction()] > [RecurringRevenueChurn()].
#' @inherit CustomerChurn
#' @param data A \code{MetricData} object.
#' @importFrom flipTime Period AsDate
#' @details Calculated based on all the customer immediately prior to the end of the
#' previous period. Note that this is not the commplement of [RecurringRevenueChurn()],
#' as that metric only takes into account churn, whereas this metric also 
#' includes [Expansion()] and [Contraction()]
NetRecurringRevenueRetention <- function(data, ratio = TRUE)
{
    calculateRatio(data, ratio = ratio, volume = TRUE, components = "net retention",
                   "Net Recurring Revenue Retention")
}

#' \code{Contraction}
#' 
#' The percentage of customers whose recurring revenue declined relative to the previous
#' period.
#' @inherit CustomerChurn
#' @importFrom flipTime Period AsDate
#' @details Calculated based on all the customer immediately prior to the end of the
#' previous period.
Contraction <- function(data,  ratio = TRUE)
{
    calculateRatio(data, ratio = ratio, volume = TRUE, components = "contraction",
                   name = "Contraction")
}


#' \code{Expansion}
#' 
#' The percentage of customers whose recurring revenue increased relative to the previous
#' period.
#' @inherit CustomerChurn
#' @importFrom flipTime Period AsDate
#' @details Calculated based on all the customer immediately prior to the end of the
#' previous period.
Expansion <- function(data,  ratio = TRUE)
{
    calculateRatio(data, ratio = ratio, volume = TRUE, components = "expansion",
                   name = "Expansion")
}


#' \code{AverageRecurringRevenue}
#' 
#' The average recurring revenue provided by each customer.
#' @inherit CustomerChurn
#' @importFrom flipTime Period AsDate
#' @details Calculated based on all the customer immediately prior to the end of the
#' previous period.
AverageRecurringRevenue <- function(data, ratio = TRUE)
{
    calculateRatio(data, ratio = ratio, volume = TRUE, components = "current",
                   name = "Average Recurring Revenue")
}

calculateRatio <- function(data, ratio, components, volume, name)
{
    calc <- calculate(data, components, volume)
    stat <- if (ratio) calc$numerator / calc$denominator else calc$numerator
    if (components == "retention")
        stat <- 1 - stat
    class.name <- if (singleSeries(data)) "OneDimensionalWithTrend"  else "Heatmap" 
    createOutput(stat, class.name, calc, name)
}    

