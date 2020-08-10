#' \code{InitialCustomerChurn}
#' 
#' The percentage of customers in their first period of a subscription who don't renew for a 
#' second subscription. See \code{CustomerChurn}.
#' @param data A \code{MetricData} object.
#' @return A named vector showing churn.
#' @importFrom flipTime Period
#' @export
InitialCustomerChurn <- function(data)
{
    calcInitialChurn(data, FALSE)
}

#' InitialRecurringRevenueChurn
#' 
#' @description Computes the recurrent revenue churn rate for people in their first completed
#' renewal period.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @details For example, if the subscriptions are annual, computes 
#' proportion of people in 2012 that churned in 2013. Customers that had
#' an incomplete year that falls within a calendar year will be excluded
#' from this metric (e.g., a license from 1 January to 1 December 2012  will
#' not appear in this calculation, but their renewal year may instead). 
#' This calculation is taken from the one-off diagonal of \code{RecurringRevenueChurnByCohort}.
#' 
#' @export
InitialRecurringRevenueChurn <- function(data)
{
    # churn.cohort <- RecurringRevenueChurnByCohort(data, by, remove.last, ...)
    # if (is.null(churn.cohort))
    #     return(NULL)
    # InitialChurn(churn.cohort, remove.last = remove.last, volume = TRUE, by = by, ...)
    calcInitialChurn(data, TRUE)
}


calcInitialChurn <- function(data, volume)
{
    r <- calculate(data, initial.only = TRUE, by.period = TRUE, volume = volume)
    by <- attr(data, "by")
    s.l <- attr(data, "subscription.length")
    n.churned <- diagRectangular(attr(r, "numerator"), by, s.l)
    cohort.size <- diagRectangular(attr(r, "denominator"), by, s.l)
    churn <- n.churned / cohort.size
    detail <- attr(r, "detail")
    detail <- if (!volume)
        initialDetail(detail, s.l)
    out <- addAttributesAndClass(churn, "InitialChurn", by, detail)
    attr(out, "volume") <- FALSE
    attr(out, "numerator") <- n.churned
    attr(out, "denominator") <- cohort.size
    attr(out, ("by")) <- attr(data, ("by"))
    out
    
}    
#' @importFrom flipTime AsDate Periods
#' @importFrom lubridate floor_date
initialDetail <- function(x, subscription.length)
{
    cohort <- AsDate(x[, 1])
    cohort.renewal <- floor_date(cohort, subscription.length) + Periods(1, subscription.length)
    period <- AsDate(x[, 2])
    f <- cohort.renewal == period
    x[f, ]    
}    


#' @importFrom flipTime AsDate Period Periods
#' @importFrom lubridate floor_date
diagRectangular <- function(x, by, subscription.length)
{
    rows <- floor_date(AsDate(rownames(x)), subscription.length) + Periods(1, subscription.length)
    rowm <- matrix(rows, nrow(x), ncol(x))                   
    cols <- AsDate(colnames(x))
    colm <- matrix(cols, nrow(x), ncol(x), byrow = TRUE)    
    m <- colm == rowm
    out <- x[m]
    names(out) <- colnames(x)[rowSums(m) != 0]
    out
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


#' #' \code{InitialChurn}
#' #'
#' #' @description Computes the churn rate in the initial time period
#' #' @param churn.cohort A \code{mattrix} showing churn by cohort.
#' #' @param remove.last Remove the final period (as usually is incomplete).
#' #' @param volume Weights the results by volume. Does nothing in this case.
#' #' @param by The time period to aggregate the dates by: 
#' #' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' #' and \code{"day"}.
#' #' @param ... Additional arguments to be passed to lower level functions.
#' #' @return A \code{\link{matrix}}
#' #' @importFrom flipTime Period Periods AsDate
#' #' @export
#' InitialChurn <- function(churn.cohort, remove.last = TRUE, volume = FALSE, by, ...)
#' {
#'     if (is.null(churn.cohort))
#'         return(NULL)
#'     if (!"1" %in% colnames(churn.cohort))
#'         return(NULL)
#'     churn <- churn.cohort[, "1"]#selectChurnData(churn.cohort, attr(churn.cohort, "subscription.length"), by)#[diag(k)[, k:1] == 1]
#'     subscription.length <- attr(churn.cohort, "subscription.length")
#'     names(churn) <- Period(AsDate(names(churn)) + Periods(1, subscription.length), by)
#'     detail <- attr(churn.cohort, "detail")
#'     detail <- detail[detail[, 2] == 0, -2] # Filtering for people in their first period
#'     churn <- addAttributesAndClass(churn, "InitialChurn", by, detail)
#'     attr(churn, "subscription.length") <- subscription.length
#'     attr(churn, "volume") <- volume
#'     churn
#' }
#' 
