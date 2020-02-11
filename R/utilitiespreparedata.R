setCohortPeriod <- function(data, cohort.by)
{
    data$cohort <- Period(data$subscriber.from, cohort.by)
    data
}

setPeriodCounter <- function(data, by)
{
    units <- Periods(1, by)
    data$period.counter <- interval(data$subscriber.from, data$from) %/% units
    data
}

updatePeriod <- function(data, by)
{
    data$period <- Period(data$from, by)
    data
}

#' @importFrom flipTime Period
prepareDataForChurn <- function(data, by)
{
    data <- removeIncompleteSubscriptions(data)
    data$to.renewal.period <- Period(data$to.renewal, by)
    data
}

#' @importFrom flipTime Period
#' @importFrom lubridate interval
prepareDataForChurnByCohort <- function(data, by)
{
    data <- removeIncompleteSubscriptions(data)
    data <- setCohortPeriod(data, by)
    units <- Periods(1, attr(data, "subscription.length"))
    data$to.renewal.period.counter <- interval(data$subscriber.from, data$to.renewal) %/% units
    data
}
