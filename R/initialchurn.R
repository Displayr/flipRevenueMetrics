#' \code{InitialChurn}
#'
#' @description Computes the churn rate in the initial time period
#' @param churn.cohort A \code{mattrix} showing churn by cohort.
#' @param remove.last Remove the final period (as usually is incomplete).
#' @param volume Weights the results by volume. Does nothing in this case.
#' @param by The time period to aggregate the dates by: 
#' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' and \code{"day"}.
#' @param ... Additional arguments to be passed to lower level functions.
#' @return A \code{\link{matrix}}
#' @importFrom flipTime Period Periods AsDate
#' @export
InitialChurn <- function(churn.cohort, remove.last = TRUE, volume = FALSE, by, ...)
{
    if (is.null(churn.cohort))
        return(NULL)
    if (!"1" %in% colnames(churn.cohort))
        return(NULL)
    churn <- churn.cohort[, "1"]#selectChurnData(churn.cohort, attr(churn.cohort, "subscription.length"), by)#[diag(k)[, k:1] == 1]
    subscription.length <- attr(churn.cohort, "subscription.length")
    names(churn) <- Period(AsDate(names(churn)) + Periods(1, subscription.length), by)
    detail <- attr(churn.cohort, "detail")
    detail <- detail[detail[, 2] == 0, -2] # Filtering for people in their first period
    churn <- addAttributesAndClass(churn, "InitialChurn", by, detail)
    attr(churn, "subscription.length") <- subscription.length
    attr(churn, "volume") <- volume
    churn
}

#' InitialCustomerChurn
#' 
#' @description Computes the churn rate for people in their first completed
#' renewal period.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time period to aggregate the dates by: 
#' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' and \code{"day"}.
#' @param remove.last Remove the final period (as usually is incomplete).
#' @param ... Additional arguments to be passed to lower level functions.
#' @details For example, if the subscriptions are annual, computes 
#' proportion of people in 2012 that churned in 2013. Customers that had
#' an incomplete year that falls within a calendar year will be excluded
#' from this metric (e.g., a license from 1 January to 1 December 2012  will
#' not appear in this calculation, but their renewal year may instead). 
#' This calculation is taken from the one-off diagonal of 
#' \code{CustomerChurnByCohort}.
#' @export
InitialCustomerChurn <- function(data, by, remove.last, ...)
{
    churn.cohort <- CustomerChurnByCohort(data, by, remove.last, )
    if (is.null(churn.cohort))
        return(NULL)
    InitialChurn(churn.cohort, remove.last = remove.last, volume = FALSE, by = by, ...)
}

#' InitialRecurringRevenueChurn
#' 
#' @description Computes the recurrent revenue churn rate for people in their first completed
#' renewal period.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time period to aggregate the dates by: 
#' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' and \code{"day"}.
#' @param remove.last Remove the final period (as usually is incomplete).
#' @param ... Additional arguments to be passed to lower level functions.
#' 
#' @details For example, if the subscriptions are annual, computes 
#' proportion of people in 2012 that churned in 2013. Customers that had
#' an incomplete year that falls within a calendar year will be excluded
#' from this metric (e.g., a license from 1 January to 1 December 2012  will
#' not appear in this calculation, but their renewal year may instead). 
#' This calculation is taken from the one-off diagonal of \code{RecurringRevenueChurnByCohort}.
#' 
#' @export
InitialRecurringRevenueChurn <- function(data, by, remove.last, ...)
{
    churn.cohort <- RecurringRevenueChurnByCohort(data, by, remove.last, ...)
    if (is.null(churn.cohort))
        return(NULL)
    InitialChurn(churn.cohort, remove.last = remove.last, volume = TRUE, by = by, ...)
}

#' @export
plot.InitialChurn <- function(x, ...)
{
    smooth <- if (length(x) < 4) "None" else "Friedman's super smoother"
    y.title <- if(attr(x, "volume")) "Year 1 Recurring Revenue Churn Rate" else "Year 1 Customer Churn Rate"
    columnChart(x, fit.type = smooth, 
              #  x.title = "Customer since",
                y.title = y.title, 
                y.tick.format = "%",
                ...)
}
