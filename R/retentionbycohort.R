#' \code{CustomerRetentionByCohort}
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
#' @importFrom flipTime Period
#' @export
CustomerRetentionByCohort <- function(data)
{
    calculateChurn(data, components = "churn", use = "cohort", volume = FALSE)
}
# 
# zeroRowsAtTopAndBottom <- function(x)
# {
#     rs <- rowSums(x)
#     cumsum(rs) > 0 & rev(cumsum(rev(rs))) > 0
# }

#' \code{RecurringRevenueChurn}
#' 
#' Lost revenue duee to churned customers as a percentage of total recurring revenue.
#' @param data A \code{RevenuMetric} object.
#' @return A named vector showing churn, a plot, or an object showing detail.
#' @importFrom flipTime Period
#' @export
RecurringRevenueRetentionByCohort <- function(data)
{
    calculateChurn(data, components = "churn", use = "cohort", volume = TRUE)
}


#' @importFrom flipTables Cbind
asMatrix <- function(list.of.lists, FUN, fill.with = 0)
{
    x <- lapply(list.of.lists, function(x) sapply(x, FUN))
    m <- t(do.call("Cbind", x)) #Using t() to hack around DS-3041
    m[is.na(m)] <- fill.with
    m
}    


#' @importFrom plotly plot_ly layout `%>%`
#' @importFrom flipFormat FormatAsPercent
#' @export
plot.RetentionByCohort <- function(x, ...)
{
    churn.type <- if(attr(x, "volume")) "Recurring Revenue " else "Customer "
    series.hover <- paste0(churn.type, "Retention Rate: ", FormatAsPercent(x, decimals = 1))
    cohortHeatmap(x, series.hover = series.hover, ...)
}