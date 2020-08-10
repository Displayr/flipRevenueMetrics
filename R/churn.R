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
    calculateChurn(data, volume = FALSE, components = "churn")
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
    calculateChurn(data, volume = TRUE, components = "churn")
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
    calculateChurn(data, volume = TRUE, components = c("expansion", "contraction", "churn"))
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
    calculateChurn(data, volume = TRUE, components = c("contraction"))
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
    calculateChurn(data, volume = TRUE, components = c("expansion"))
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

calculateChurn <- function(data, volume = FALSE,  components = "churn")
{
    r <- calculate(data, initial.only = FALSE, by.period = FALSE, volume = volume, components = components)
    subscription.length <- attr(data, "subscription.length")
    by <- attr(data, "by")
    churn <- calcChurn(r, subscription.length, by, components)
    detail <- attr(r, "detail")
    if (!volume)
        detail <- tidyingDetailForCustomerChurn(detail, subscription.length, by)
    out <- addAttributesAndClass(churn, "Churn", by, detail)
    attr(out, "numerator") <- attr(r, "numerator")[, 1]
    attr(out, "denominator") <- attr(r, "denominator")[, 1]
    attr(out, "volume") <- FALSE
    out
}    

calcChurn <- function(r, subscription.length, by, components)
{
    churn <- 1 - r[, 1]
    if (all(c("churn", "expansion", "contraction") %in% components))
        churn <- churn - 1
    churn
    # names(churn) <- addSubscriptionLengthToName(names(churn), 
    #                                             subscription.length, 
    #                                             by)
    churn    
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