#' \code{CustomerChurn}
#' 
#' The percentage of customers who could churn in a period that did churn.
#' Note that this is not the most widely used definition. A more common definition
#' is the proportion of customers that churned. These two measures differ in some  situations:
#' 1. Multi-year contracts.
#' 2. Incomplete periods (e.g., the conventional metric can't be used in a quarter until it
#' has been completed.
#' 3. Where a customer joins and churns withhin a single period (ignored in the traditional definition)
#' @param data A \code{MetricData} object.
#' @return A named vector showing churn.
#' @importFrom flipTime Period AsDate
#' @export
CustomerChurn <- function(data)
{
    calculateChurn(data, components = "churn", volume = FALSE, use = "aggregate")
}



#' \code{RecurringRevenueChurn}
#' 
#' Lost revenue duee to churned customers as a percentage of total recurring revenue.
#' @param data A \code{RevenuMetric} object.
#' @return A named vector showing churn, a plot, or an object showing detail.
#' @importFrom flipTime Period
#' @export
RecurringRevenueChurn <- function(data)
{
    calculateChurn(data, components = "churn", volume = TRUE, use = "aggregate")
}
#' \code{NetRecurringRevenueChurn}
#' 
#' The percentage of customers who could churn in a period that did churn.
#' Note that this is not the most widely used definition. A more common definition
#' is the proportion of customers that churned. These two measures differ in some  situations:
#' 1. Multi-year contracts.
#' 2. Incomplete periods (e.g., the conventional metric can't be used in a quarter until it
#' has been completed.
#' 3. Where a customer joins and churns withhin a single period (ignored in the traditional definition)
#' @param data A \code{MetricData} object.
#' @return A named vector showing churn.
#' @importFrom flipTime Period AsDate
#' @export
NetRecurringRevenueChurn <- function(data)
{
    calculateChurn(data, use = "aggregate", volume = TRUE, components = c("expansion", "contraction", "churn"))
}

#' \code{Contraction}
#' 
#' xxx
#' @param data A \code{MetricData} object.
#' @return A named vector showing churn.
#' @importFrom flipTime Period AsDate
#' @export
Contraction <- function(data)
{
    calculateChurn(data, use = "aggregate", volume = TRUE, components = c("contraction"))
}


#' \code{Expansion}
#' 
#' xxx
#' @param data A \code{MetricData} object.
#' @return A named vector showing churn.
#' @importFrom flipTime Period AsDate
#' @export
Expansion <- function(data)
{
    calculateChurn(data, use = "aggregate", volume = TRUE, components = c("expansion"))
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