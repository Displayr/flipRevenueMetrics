#' \code{MetricData}
#'
#' @description Cleans and tidies data for use in revenue metric calculations.
#' @param value A vector of containing the revenue per transaction.
#' @param from A vector of class \code{POSIXct} or \code{POSIXt},
#'     recording the date and time each subscription commences.
#' @param to A vector of class \code{POSIXct} or \code{POSIXt},
#'     recording the date and time each subscription ends
#' @param start The date at which the analysis outputs should
#'     commence. By default, the earliest date recorded in
#'     \code{from}.
#' @param end The date at which the analysis ends, which is used to
#'     determine churn.  By default, the most recent date recorded in
#'     \code{from}.
#' @param id A vector of \code{character}, unique identifier for
#'     subscribers that made the transactions (e.g., email addresses,
#'     names, subscriber keys).
#' @param subscription.length The time unit that describes the
#'     subscription length: \code{year} to view the data by year,
#'     \code{quarter}, and \code{month}. This is assumed to be the
#'     billing period when determining if subscribers have churned or
#'     not.
#' @param by The time unit that describes the
#'     subscription length: \code{year} to view the data by year,
#'     \code{quarter}, and \code{month}. This is assumed to be the
#'     billing period when determining if subscribers have churned or
#'     not.
#' @param mergers A data frame with two variables 'id' and 'id.to'. 
#' 'id' contains the ids of companies that appeara, based on their data,
#' to have churned, but have in fact merged with the corresponding 'id.to'.
#' @param trim.id The maximum length of the strings to be used showing
#'     ID names (used to avoid situations where string names are so
#'     long as to make reading of tables impossible.
#' @return A \code{\link{data.frame}} where the rows represent invoice lines
#'     and the variables are: 
#'     \itemize{
#'        \code{id} {The unique identifier.}  
#'        \code{value} {The total fee/price for a subscription.}
#'        \code{from} {The commencement date of a subscription.}
#'        \code{recurring.value} {The value divided by proportion of the typicaly invoice period
#'        that was covered by the invoice. There are some rounding error issues (e.g., leap years,
#'        inconsistencies in how people enter data)}.
#'    }
#'    Additional information is included in attributes
#' @importFrom lubridate floor_date
#' @importFrom stats aggregate
#' @importFrom flipTime AsDate
#' @export
MetricData <- function(value, 
                       from, 
                       to, 
                       start,
                       end, 
                       id,
                       subscription.length,
                       by,
                       mergers,
                       trim.id) #, tolerance = .01)
{
    id <- as.character(id)
    # Checking the input variables.
    n <- length(value)
    from <- AsDate(from)
    to <- AsDate(to)
    checkVariableForLengthAndMissingData(value, n)
    checkVariableForLengthAndMissingData(from, n)
    checkVariableForLengthAndMissingData(to, n)
    checkVariableForLengthAndMissingData(id, n)
    # # aggregating time periods
    # from <- floor_date(from, by)
    # to <- floor_date(to, by)
    # aggregating unique combinations of to, from, and id
    data <- aggregate(value, list(from, to, id), sum)
    data <- as.data.frame(data)
    names(data) <- c("from", "to", "id", "value")
    
    # Removing cases with $0
    data <- data[data$value > 0, ]
    # Sorting so that other calculations can be made easier
    data <- data[with(data, order(id, from, to)), ]
    
    # appending other info
    data$recurring.value <- recurringValue(data$value, data$from, data$to, subscription.length)
    attr(data, "mergers") <- mergers
    attr(data, "start") <- start
    attr(data, "end") <- end
    attr(data, "subscription.length") <- subscription.length
    attr(data, "by") <- by
    
    unit <- Periods(1, by)
    start <- floor_date(attr(data, "start"), by)# + unit
    ceil.end <- as.Date(ceiling_date(end, by, change_on_boundary = NULL))
    attr(data, "previous.date") <- previous.date <- start - unit # Used in growth accounting
    attr(data, "previous.period") <- Period(previous.date, by)
    
    dts <- seq.Date(start, ceil.end, by)
    attr(data, "by.period.sequence") <- Period(dts, by)
    dts[length(dts)] <- end
    attr(data, "by.sequence") <- dts
    
    start <- floor_date(attr(data, "start"), subscription.length)# + unit
    ceil.end <- as.Date(ceiling_date(end, subscription.length, change_on_boundary = NULL))
    dts <- seq.Date(start, by = subscription.length, length.out = length(dts))
    attr(data, "subscription.period.sequence") <- Period(dts, subscription.length)
    attr(data, "subscription.sequence") <- c(dts[-length(dts)], end)
    
    class(data) <- c(class(data), "MetricData")
    data
}

# Computing recurring.revenue

#' @importFrom lubridate as.duration
#' @importFrom flipTime Periods
recurringValue <- function(value, from, to, subscription.length)
{
    units <- Periods(1, subscription.length)
    period.proportion <- as.numeric(to - from, "days") /  as.numeric(as.duration(units), "days")
    rounded.period.proportion <- round(period.proportion, 2)
    rnd <- rounded.period.proportion %in% c(.25,.5, .75, 1, 2, 3, 4, 5, 6)
    period.proportion[rnd] <- rounded.period.proportion[rnd]
    value / period.proportion
}    

checkVariableForLengthAndMissingData <- function(x, n)
{
    if (any(is.na(x)))
        stop("'", deparse(substitute(x)), "' contains missing values.")
    if (length(x) != n)
        stop("'" , deparse(substitute(x)), "' contains ", deparse(substitute(x)), " observations, but 'value' contains ", n, ".")
}


filterMetricData <- function(metric.data, subset)
{
    
    atr <- attributes(metric.data)
    out <- subset(metric.data, subset)
    
    # Re-appending attributes
    to.replace <- names(atr)
    to.replace <- to.replace[!to.replace %in% c("row.names", "class", "dim", "dimnames", "names")]
    for (a in to.replace)
        attr(out, a) <- atr[[a]]
    class(out) <- atr[["class"]]
    out
}    

#' @importFrom flipStatistics Table
#' @importFrom flipTime Periods AsDate
#' @importFrom plyr mapvalues
filterMetricDataByRelationshipLength <- function(metric.data, n.subscriptions)
{
    start.by.id <- aggregate(from ~ id, data = metric.data, FUN = min)
    #start.by.id <- ag[, 2]
    #names(start.by.id) <- ag[, 1]
    time.to.add <- Periods(n.subscriptions + 0, attr(metric.data, "subscription.length"))
    start.by.case <- mapvalues(metric.data$id, 
                               start.by.id[,1], as.character(start.by.id[, 2]))#(start.by.id + time.to.add)[metric.data$id]
    cutoff <- AsDate(start.by.case) + time.to.add
    subset <- metric.data$from < cutoff
    out <- filterMetricData(metric.data, subset)
    out
}    

