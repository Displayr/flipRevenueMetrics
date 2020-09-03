#' @importFrom flipTime Period
calculateNumberCustomers <- function(in.cohort, period.start, data)
{
    if (!is.null(in.cohort))
       stop("There is no way of looking at number of customer by cohort. Perhaps Customer Retention is what you are after.")
    customers <- numberOrNewCustomers(data, period.start)
    list(detail = selectedIDs(data, customers),
                denominator = nUnique(data$id[customers]),
                numerator = NULL)
}

numberOrNewCustomers <- function(data, period.start)
{
    if (newCohort(data))
        return(newCustomers(data, period.start)) 
    boundary.date <- nextDate(data, period.start)
    customerAt(data, boundary.date)
}

newCustomers <- function(data, period.start)
{
    next.period.start <- period.start + byUnit(data)
    dateVariableInWindow(data$subscribed.from, period.start, next.period.start)
}

#' #' @importFrom flipTime Period
#' calculateNumberCustomers <- function(data)
#' {
#'     if (!singleSeries(data))
#'         stop("There is no way of looking at number of customer by cohort. Perhaps Customer Retention is what you are after.")
#'     statistic <- byPeriodVector(data)
#'     detail <- byPeriodList(data)
#'     n <- length(statistic)
#'     id <- data$id
#'     for (i in 1:n)
#'     {
#'         customers <- numberOrNewCustomers(data, i)
#'         statistic[i] <- nUnique(id[customers])
#'         detail[[i]] <- selectedIDs(data, customers)
#'     }        
#'     out <- list(detail = detail,
#'                 denominator = statistic,
#'                 numerator = NULL)
#'     addAttributesToList(out, data)
#' }
