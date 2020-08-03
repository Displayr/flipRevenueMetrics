#' \code{CustomerChurnByCohort}
#'
#' @description Computes the customer churn by time period and time-based cohort, where the time
#' periods are determined by the \code{attr(data, "subscription.length")}.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time period to aggregate the dates by: 
#' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' and \code{"day"}.
#' @param remove.last Remove the final period (this is useful if it is likely incomplete.).
#' @param ... Additional arguments to be passed to lower level functions.
#' @return A \code{\link{matrix}} 
#' @importFrom flipTime Period
#' @export
CustomerChurnByCohort <- function(data, by, remove.last = FALSE, ...)
{
    data <- prepareDataForChurnByCohort(data, by)
    if (nrow(data) == 0)
        return(NULL)
    n.subscribers <- subscribersByCohortAndPeriod(data, by, remove.last)
    n.churn <- calculateNChurn(data, n.subscribers)
    rate <- churnRate(data, n.churn, n.subscribers, n.subscribers, by, remove.last)
    #rate <- removeStartEndFromColumnsOfMatrix(rare, attr(data, "start"), attr(data, "end"), remove.last)
    attr(rate, "volume") <- FALSE
    rate
}


calculateNChurn <- function(data, n.subscribers)
{
    churn.data <- data[data$churn, ]
    if (nrow(churn.data) == 0)
        return(matrix(0, nrow(n.subscribers), ncol(n.subscribers),
                      dimnames = dimnames(n.subscribers)))
    Table(id ~ cohort + to.renewal.period.counter, data = churn.data, FUN = nUnique)
}

#' \code{RecurringRevenueChurnByCohort}
#'
#' @description Computes the recurring revenue churn by time period and time-based cohort, where the time
#' periods are determined by the \code{attr(data, "subscription.length")}.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time period to aggregate the dates by: 
#' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' and \code{"day"}.
#' @param remove.last Remove the final period (this is useful if it is likely incomplete.).
#' @param ... Additional arguments to be passed to lower level functions.
#' @return A \code{\link{matrix}} 
#' @importFrom flipTime Period
#' @export
RecurringRevenueChurnByCohort <- function(data, by, remove.last = FALSE, ...)
{
    data <- prepareDataForChurnByCohort(data, by)
#    data <- updateDataForPeriodAndChurn(data, cohort.by, by)
    if (nrow(data) == 0)
        return(NULL)
    n.subscribers <- subscribersByCohortAndPeriod(data, by, remove.last)
    total <- Table(recurring.value ~ cohort + to.renewal.period.counter, 
                   data = data, 
                   FUN = sum)
    lost <- calculateRecurringRevenueNumerator(data, n.subscribers)
    rate <- churnRate(data, lost, total, n.subscribers, by, remove.last)
    #rate <- addAttributesAndClass(rate, "RecurringRevenueChurnByCohort", by, data[, c("recurring.value", "cohort", "to.renewal.period")])
    
    attr(rate, "volume") <- TRUE
    rate
}


calculateRecurringRevenueNumerator <- function(data, n.subscribers)
{
    churn.data <- data[data$churn, ]
    if (nrow(churn.data) == 0)
        return(matrix(0, nrow(n.subscribers), ncol(n.subscribers),
                      dimnames = dimnames(n.subscribers)))
    Table(recurring.value ~ cohort + to.renewal.period.counter, 
                  data = churn.data, 
                  FUN = sum)
}


#' @importFrom flipTime AsDate
subscribersByCohortAndPeriod <- function(data, by, remove.last)
{
    n.subscribers <- Table(id ~ cohort + to.renewal.period.counter, 
                           data = data, 
                           FUN = nUnique)
    # all.periods <- unique(unlist(dimnames(n.subscribers)))
    # dt <- CompleteListPeriodNames(all.periods, by)
    # n.subscribers <- FillInMatrix(n.subscribers, dt, dt, value = 0)
    # keep <- periodsToKeep(dt, attr(data, "start"), attr(data, "end"), remove.last)
    # n.subscribers <- n.subscribers[, keep, drop = FALSE]
    # names(dimnames(n.subscribers)) <- c("Commenced", properCase(by))
    n.subscribers
}

#' @importFrom flipTime AsDate
churnRate <- function(data, numerator, denominator, n.subscribers, by , remove.last)
{
    #dt <- rownames(n.subscribers)
    #num <- FillInMatrix(numerator, dt, dt, value = 0)
    num <- FillInMatrix(numerator, rownames(denominator), colnames(denominator), value = 0)
    rate <- num / denominator
   # names(rate) <- names(n.subscribers)
#    rate <- rate[rownames(n.subscribers), colnames(n.subscribers), drop = FALSE]
    rate <- tidyCohortTable(rate, by, attr(data, "subscription.length"), attr(data, "start"), attr(data, "end"), remove.last, NaN)
    n.subscribers <- FillInMatrix(n.subscribers, rownames(rate), colnames(rate), NaN)
    detail <- churnByCohortDetail(data, by)
    rate <- addAttributesAndClass(rate, "ChurnByCohort", by, detail)
    attr(rate, "n.subscribers") <- n.subscribers
    attr(rate, "subscription.length") <- attr(data, "subscription.length")
    dimnames(n.subscribers) <- dimnames(rate) 
    rate
}

churnByCohortDetail <- function(data, by)
{
    if(sum(data$churn) == 0)
        return(NULL)
    detail <- aggregate(recurring.value ~ cohort + period.counter + id, data, sum,
                        subset = data$churn & as.Date(data$to) >= attr(data, "start") & as.Date(data$to) <= attr(data, "end"))
    colnames(detail) <- c("Recurring Revenue", "Customer since", properCase(by))
    detail
}



#' @importFrom plotly plot_ly layout `%>%`
#' @importFrom flipFormat FormatAsPercent
#' @export
plot.ChurnByCohort <- function(x, ...)
{
    churn.type <- if(attr(x, "volume")) "Recurring Revenue " else "Customer "
    series.hover <-paste0(churn.type, "Churn Rate: ", FormatAsPercent(x, decimals = 1))
    cohortHeatmap(x, series.hover = series.hover, ...)
}