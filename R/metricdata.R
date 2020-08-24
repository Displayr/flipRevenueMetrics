#' \code{MetricData}
#'
#' @description Cleans and tidies data for cohort.type in revenue metric calculations.
#' @inheritParams RevenueMetric     
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
#' @importFrom lubridate floor_date time_length
#' @importFrom stats aggregate
#' @importFrom flipTime AsDate
MetricData <- function(value, 
                       from, 
                       to, 
                       start,
                       end, 
                       id,
                       subscription.length,
                       by,
                       cohort.type,
                       cohort.period,
                       mergers,
                       trim.id) #, tolerance = .01)
{
    id <- as.character(id)
    # Checking the input variables.
    n <- length(value)
    from <- AsDate(from)
    to <- AsDate(to)
    checkCohortType(cohort.type)
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
    
    # Adding data for processing cohorts, if required
    data$subscribed.from <- subscribedFrom(data$from, data$id, cohort.type)
    
    # appending other info
    data$recurring.value <- recurringValue(data$value, data$from, data$to, subscription.length)
    attr(data, "mergers") <- mergersWithDates(data, mergers)
    attr(data, "start") <- start
    attr(data, "end") <- end
    attr(data, "by") <- by
    attr(data, "subscription.length") <- subscription.length
    attr(data, "cohort.type") <- cohort.type
    attr(data, "cohort.period") <- cohort.period
    by.dates <- seq.Date(floor_date(start, by),
                         floor_date(end, by),
                         by = by)
    attr(data, "by.date.names") <- Period(by.dates, by)
    if(!end %in% by.dates)
        by.dates <- c(by.dates, end)
    attr(data, "by.dates") <- by.dates
    
    if (cohort.type %in% c("Calendar", "Tenure"))
    {
        strt <- start
        if (calendarCohort(data))
        {
            strt <- floor_date(start, cohort.period)
            attr(data, "calendar.start") <- strt
        }
        u <- cohortUnit(data) 
        n <- ceiling(time_length(end - strt) / time_length(u))
        nms <- if (tenureCohort(data)) 0:n else {
            dts <- seq.Date(strt, by = cohort.period, length.out = n)
            Period(dts, cohort.period)
        }
        attr(data, "cohort.names") <- nms
    }
    
    
    # unit <- Periods(1, by)
    # start <- floor_date(attr(data, "start"), by)# + unit
    # ceil.end <- as.Date(ceiling_date(end, by, change_on_boundary = NULL))
    # #attr(data, "previous.date") <- previous.date <- start - unit # Used in growth accounting
    # #attr(data, "previous.period") <- Period(previous.date, by)
    # 
    # dts <- seq.Date(start, ceil.end, by)
    # #attr(data, "by.period.sequence") <- Period(dts, by)
    # dts[length(dts)] <- end
    # attr(data, "by.dates") <- dts
    # # The last date is excluded as it is only ever used to define the end of a period
    # attr(data, "by.date.names") <- Period(dts, by)[-length(dts)]
    # 
    # # 
    # # start <- floor_date(attr(data, "start"), subscription.length)# + unit
    # # ceil.end <- as.Date(ceiling_date(end, subscription.length, change_on_boundary = NULL))
    # # dts <- seq.Date(start, to = ceil.end, by = subscription.length)
    # # attr(data, "subscription.period.sequence") <- Period(dts, subscription.length)
    # # attr(data, "subscription.sequence") <- c(dts[-length(dts)], end)
    # # 
    class(data) <- c(class(data), "MetricData")
    data
}


cohortNames <- function(data)
{
    attr(data, "cohort.names")
}

nCohorts <- function(data)
{
    length(cohortNames(data))
}

calendarStart <- function(data)
{
    attr(data, "calendar.start")
}

start <- function(data)
{
    attr(data, "start")
}

cohortType <- function(data)
{
    attr(data, "cohort.type")
}

calendarCohort <- function(data)
{
    cohortType(data) == "Calendar"
}

tenureCohort <- function(data)
{
    cohortType(data) == "Tenure"
}

newCohort <- function(data)
{
    cohortType(data) == "New"
}

subscribedFrom <- function(from, id, cohort.type)
{
    if (cohort.type == "None")
        return(NULL)
    start.by.id <- aggregate(from ~ id, FUN = min)
    m <- match(id, start.by.id[, 1])
    start.by.id[m, 2]
}

periodStart <- function(data, i)
{
    attr(data, "by.dates")[i]
}

nextDate <- function(data, date)
{
    dates <- attr(data, "by.dates")
    dates[1 + match(date, dates)]
}


nextPeriodStart <- function(data, i)
{
    periodStart(data, i + 1)
}

nextCohortPeriodStart <- function(data, start.date)
{
    end.date <- start.date + cohortUnits(2, data)
    end.date <- min(end.date, attr(data, "end"))
    end.date <- end.date - cohortUnit(data)
}


subscriptionLength <- function(data)
{
    attr(data, "subscription.length")
}    

subscriptionUnit <- function(data)
{
    subscriptionUnits(1, data)
}    

cohortUnit <- function(data)
{
    cohortUnits(1, data)
}    

#' @importFrom flipTime Periods
cohortUnits <- function(x, data)
{
    Periods(x, attr(data, "cohort.period"))
}    



#' @importFrom flipTime Periods
subscriptionUnits <- function(x, data)
{
    Periods(x, attr(data, "subscription.length"))
}    

#' @importFrom flipTime Periods
byUnit <- function(data)
{
    Periods(1, attr(data, "by"))
}    

nPeriods <- function(data)
{
    length(attr(data, "by.dates")) - 1
}

periodNames <- function(data)
{
    attr(data, "by.date.names")
}

periodName <- function(data, i)
{
    periodNames(data)[i]
}

cohortStartDate <- function(data, cohort)
{
    attr(data, "by.sequence")[cohort]
}    
 
checkCohortTypes <- function(cohort.type)
{
    if (!cohort.type %in% c("None", "New", "Calendar", "Tenure"))
        stop("Unknown cohort.type: ", paste(cohort.type, separate = ","))
}            


# cohortNames <- function(data, cohort.type)
# {
#     switch(cohort.type,
#            "Cohort" = attr(data, "subscription.period.sequence"),
#            "Initial" = paste("Initial", subscriptionLength(data)),
#            "Aggregate" = "All")
# }    

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

checkCohortType <- function(cohort.type)
{
    if (!cohort.type %in% c("None", "New", "Calendar", "Tenure"))
        stop("Unknown cohort.type: ", paste(cohort.type, separate = ","))
}            

mergersWithDates <- function(data, mergers)
{   #' Adds a column of the tim period at which the mergers occurred
    if (is.null(mergers) || nrow(mergers) == 0)
        return(NULL)
    by <- attr(data, "by")
    id <- data$id
    mergers$date <- rep(as.Date("2999-12-31"), NROW(mergers)) # Lazy way of dealing with situation where churn doesn't occur
    m <- id %in% mergers$id
    ag <- aggregate(data$to[m], list(id[m]), FUN = max)
    m <- ag[, ] %in% id
    mergers$date[match(ag[,1], mergers$id)] <- ag[, 2]
    mergers
}
