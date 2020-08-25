#' @importFrom flipTime Period
calculateNumberCustomers <- function(data)
{
    if (!singleSeries(data))
        stop("There is no way of looking at number of customer by cohort. Perhaps Customer Retention is what you are after.")
    statistic <- byPeriodVector(data)
    detail <- byPeriodList(data)
    n <- length(statistic)
    id <- data$id
    for (i in 1:n)
    {
        customers <- numberOrNewCustomers(data, i)
        statistic[i] <- nUnique(id[customers])
        detail[[i]] <- selectedIDs(data, customers)
    }        
    out <- list(detail = detail,
                denominator = statistic,
                numerator = NULL)
    
    addAttributesToList(out, data)
}

numberOrNewCustomers <- function(data, i)
{
    if (newCohort(data))
        return(newCustomers(data, i)) 
    boundary.date <- nextPeriodStart(data, i)
    customerAtPeriodEnd(data, boundary.date)
}

newCustomers <- function(data, i)
{
    period.start <- periodStart(data, i)
    next.period.start <- period.start + byUnit(data)
    dateVariableInWindow(data$subscribed.from, period.start, next.period.start)
}

