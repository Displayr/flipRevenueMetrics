#' @importFrom flipTime Period
comparingTwoPointsInTime <- function(in.cohort, period.start, data, components)
{
    # We compare points in time by looking at the statistic of interest at thee end of the periiod,
    # compared to the end of the preceding period. The period.start.date is the actual beginning
    # of the peeriod. The period.end.boundary is the beginning of the following period.
    # That is, in terms of mathematical notation for intervals: [period)
    current <- currentCustomers(in.cohort, period.start, data)
    earlier <- earlierCustomers(in.cohort, period.start, data)
    
    id <- idsReflectingMergers(data, period.start)
    rr <- data$recurring.value
    earlier.id <- unique(id[earlier])
    #merger.in.period <- mergerInPeriod(data, start.dt, next.dt)
    #merger
#    earlier <- id %in% earlier.id & current
    if (any(c("contraction", "expansion") == components) & !"churn" %in% components)
        detail <- expansionOrContraction(components, current, earlier, period.start, rr, id)
    else
    {
        m <- if ("net retention" == components)
            current & id %in% earlier.id # Net Recurring Revenue Churn
        else # churn or retention
            earlier & id %in% setdiff(earlier.id, id[current])
        detail <- tapply(rr[m], list(id[m]), sum)
    }
    list(denominator = sum(rr[earlier]), numerator = sum(detail), detail = detail)
}

currentCustomers <- function(in.cohort, period.start, data)
{
    period.boundary <- nextDate(data, period.start)
    m <- customerAtPeriodEnd(data, period.boundary)
    andSubsetIfItExists(m, in.cohort)
}

earlierCustomers <- function(in.cohort, period.start, data)
{
    period.boundary <- nextDate(data, period.start)
    unit <- cohortUnit(data)#if(newCohort(data)) byUnit(data) else cohortUnit(data)
    earlier.boundary <- period.boundary - unit
    m <- customerAtPeriodEnd(data, earlier.boundary)
    if (newCohort(data)) # Should only occur when is.null(in.cohort)
    {
        in.cohort <- dateVariableInWindow(data$subscribed.from, 
                                          earlier.boundary - unit, 
                                          earlier.boundary)
        #print(paste("new", earlier.boundary - unit, earlier.boundary))
    }
    andSubsetIfItExists(m, in.cohort)
}

expansionOrContraction <- function(components, current, earlier, period.start,  value, id)
{
    rr.change.by.id <- changeInRecurringRevenueByID(current, earlier, value, id)
    if (components == "contraction") 
        return(-rr.change.by.id[rr.change.by.id < 0])
    rr.change.by.id[rr.change.by.id > 0]
    
}

idsReflectingMergers <- function(data, period.start)
{
    merger.info <- mergerInfo(data, period.start, nextDate(data, period.start))
    id <- data$id    
    if (length(merger.info$from.id) == 0)
        return(id)
    mtch <- match(id, merger.info$from.id)
    m <- !is.na(mtch)
    id[m] <- merger.info$to.id[mtch[m]] 
    id
}


changeInRecurringRevenueByID <- function(current, earlier, value, id)
{
    id.earlier <- unique(id[earlier])
    id.current <- unique(id[current])
    id.retained <- intersect(id.earlier, id.current)
    retained <- id %in% id.retained
    rr.current <- recurringRevenueByID(current, retained, value, id)
    rr.earlier <- recurringRevenueByID(earlier, retained, value, id)
    rr.current - rr.earlier
}


recurringRevenueByID <- function(x, y, value, id)
{
    subset <- x & y
    tapply(value[subset],
           list(id[subset]), 
           sum)
}