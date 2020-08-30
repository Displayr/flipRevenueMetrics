#' \code{calculate}
#' 
#' Calculates main metrics
#' @param data A \code{MetricData} object.
#' @param components A character indicating the revenue metric component to cohort.type; one
#' of 'churn', 'expansion', 'contraction', 'retention', and 'net retention'.
#' @param volume If TRUE, recurring revenue rather than customers is cohort.typed in calculations.
#' @return A named vector showing churn.
#' @importFrom flipTime Period
calculate <- function(data, components, volume)
{
    checkComponents(components)
    results <- if(singleSeries(data)) 
        calculateNoCohort(data, components, volume)
    else
        calculateByCohort(data, components, volume)
    tidyResults(results, volume, components, data)
}

calculateNoCohort <- function(data, components, volume)
{
    fake.date <- start(data) - byUnit(data)
    loopByPeriod(NULL, fake.date, data, components, volume)
}

calculateByCohort <- function(data, components, volume)
{
    results <- namedList(cohortNames(data))
    for (cohort in 1:nCohorts(data))
    {
        cohort.start <- cohortStart(data, cohort)
        in.cohort <- inCohort(data, cohort.start, components, volume)
        results[[cohort]] <- loopByPeriod(in.cohort, cohort.start, data, components, volume)
    }
    results
}


loopByPeriod <- function(in.cohort, cohort.start, data, components, volume)
{
    results <- byPeriodList(data)
    for (period in 1:nPeriods(data))
    {
        period.start <- periodStart(data, period)
        if (validPeriod(data, components, cohort.start, period.start))
            results[[period]] <- doCalculations(in.cohort, period.start, components, volume, data)
    }
    results
}


doCalculations <- function(in.cohort, period.start, components, volume, data)
{
    if (components == "number of customers")
        return(calculateNumberCustomers(in.cohort, period.start, data))
    if (components == "current")
        return(revenueCalculation(in.cohort, period.start, data))
    if (volume)
        return(comparingTwoPointsInTime(in.cohort, period.start, data, components))
    timeWindowCalculation(in.cohort, period.start, data)
}


inCohort <- function(data, cohort.start, components, volume)
{
    u <- cohortUnit(data)
    cohort.boundary <- if (volume | components %in% c("number of customers", "current"))
        min(attr(data, "end"), cohort.start + u)
    else
        nextCohortPeriodStart(data, cohort.start)
#    new.cohort.start <- cohort.boundary - u
    # # Dealing with edge case of churn among people that start in last period
    # if (new.cohort.start + u < cohort.start)
    # {
    #     cohort.boundary <- cohort.boundary + u
    #     new.cohort.start <- cohort.start
    # }
    #     
    #print(paste("cohort window", cohort.start, cohort.boundary))
    data$subscribed.from >= cohort.start & data$subscribed.from < cohort.boundary
}



cohortStart <- function(data, cohort)
{
    start <- if (calendarCohort(data)) calendarStart(data) else start(data)
    start + cohortUnits(cohort - 1, data)
}    

startPeriod <- function(data, components)
{
    # if(components %in% c("current", "customers"))
    #     return(1)
    # With calculations that look at data in one time pepriod relative to earlier time periods, we can't start at period 
    # 1 as their's no relevant history
    ceiling(1 + subscriptionUnit(data) / byUnit(data))
}
# cohortID <- function(cohort.ids, date, data)
# {
#     if (is.null(cohort.ids)) # All
#         return(NULL)
#     period.name <- Period(date, attr(data, "by"))
#     cohort.ids[[period.name]]
# }

customerAtPeriodEnd <- function(data, period.date)
{
    period.date > data$from  & period.date <= data$to
}

checkComponents <- function(components)
{
    if (!components %in% c("number of customers", 
                           "current", 
                           "expansion", 
                           "contraction", 
                           "churn", 
                           "retention", 
                           "net retention", 
                           "customers"))
        stop("Unknown components: ", paste(components, separate = ","))
}            



selectedIDs <- function(data, subset)
{
    unique(data$id[subset])
}

byPeriodVector <- function(data)
{
    nms <- attr(data, "by.date.names")
    namedVector(nms)
}

byPeriodList <- function(data)
{
    nms <- attr(data, "by.date.names")
    namedList(nms)
}

validPeriod <- function(data, components, cohort.start, period.start)
{
    # if (components == "current")
    #     return(period.start >= cohort.start)
    period.start >= cohort.start
}




# customerAtEarlierPeriodEnd <- function(data, period.dt, later.period.being.compared.to)
# {
#     previous <- customerAtPeriodEnd(data, period.dt)
#     if (later.period.being.compared.to - period.dt < attr(data, "subscription.length")) # Dealing with final incomplete period
#         previous <- previous & data$to < attr(data, "end")
#     previous
# }


#' createMatrix <- function(value, rownames, colnames)
#' {
#'     matrix(value, 
#'            length(rownames),
#'            length(colnames), 
#'            dimnames = list(rownames, colnames))
#'     
#' }    
#' 
#' idCohort <- function(cohort, cohort.ids, cohort.type, volume)
#' {
#'     if (cohort.type != "None")
#'         return(cohort.ids[[cohort]])
#'     unlist(cohort.ids[1:cohort])
#' }
#' 
#' subscriptionPeriods <- function(data, cohort.type)
#' {
#'     if (cohort.type == "None") 
#'         return("None" )
#'     seq <- attr(data, "subscription.period.sequence")
#'     return(seq[-length(seq)])
#' }
#' 
#' 
#' #' @importFrom flipTime Period AsDate
#' cohortIDs <- function(data)
#' {
#'     start.by.id <- All(from ~ id, data = data, FUN = min)
#'     period <- Period(start.by.id[, 2], attr(data, "by"))
#'     r <- tapply(start.by.id[, 1], list(period), c)
#'     periods <- attr(data, "by.period.sequence")
#'     out <- vector("list", length(periods))
#'     names(out) <- periods
#'     out[names(r)] <- r
#'     out
#' }
#' 






# asRetention <- function(components, cohort.type)
# {
#     if (cohort.type == "By year")
#         return(TRUE)
#     all(c("churn", "expansion", "contraction") %in% components)
# }

# 
# periodLabels <- function(data, cohort.type, components)
# {
#     periods <- attr(data, "by.period.sequence")
#     periods <- periods[-length(periods)]
#     if ("current" %in% components & attr(data, "by") != "day" & cohort.type == "None")
#         periods <- c(attr(data, "previous.period"), periods)
#     periods
# }
# 
# 
# 
# matrixToVectorForInitialPeriod <- function(calculation, by, subscription.length)
# {
#     calculation$denominator <- diagRectangular(calculation$denominator, by, subscription.length)
#     calculation$numerator <- diagRectangular(calculation$numerator, by, subscription.length)
#     calculation
# }    
#' 
#' 
#' #' @importFrom flipTime AsDate Period Periods
#' #' @importFrom lubridate floor_date
#' diagRectangular <- function(x, by, subscription.length)
#' {
#'     subscription.unit <- Periods(1, subscription.length)
#'     row.dts <- AsDate(rownames(x))
#'     rows <- floor_date(row.dts, subscription.length) + subscription.unit
#'     rowm <- matrix(rows, nrow(x), ncol(x))                   
#'     cols <- AsDate(colnames(x))
#'     colm <- matrix(cols, nrow(x), ncol(x), byrow = TRUE)    
#'     m <- colm == rowm
#'     out <- x[m]
#'     names(out) <- Period(row.dts [rowSums(m) != 0] + subscription.unit, by)
#'     out
#' }   
#' 
