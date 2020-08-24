#' @importFrom flipTime Period
comparingTwoPointsInTime <- function(in.cohort, period.start, data, components)
{
    # We compare points in time by looking at the statistic of interest at thee end of the periiod,
    # compared to the end of the preceding period. The period.start.date is the actual beginning
    # of the peeriod. The period.end.boundary is the beginning of the following period.
    # That is, in terms of mathematical notation for intervals: [period)
    current <- currentCustomers(in.cohort, period.start, data)
    earlier <- if (newCohort(data))
        dateVariableInWindow(data$subscribed.from, period.start - byUnit(data), period.start)
    else
        earlierCustomers(in.cohort, period.start, data)
    
    id <- data$id
    rr <- data$recurring.value
    earlier.id <- unique(id[earlier])
    if (any(c("contraction", "expansion") == components) & !"churn" %in% components)
        detail <- expansionOrContraction(components, current, earlier, data)
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
    earlier.boundary <- period.boundary - subscriptionUnit(data)
    m <- customerAtPeriodEnd(data, earlier.boundary)
    andSubsetIfItExists(m, in.cohort)
}

expansionOrContraction <- function(components, current, earlier, data)
{
    
    rr.change.by.id <- changeInRecurringRevenueByID(current, earlier, data)
    if (components == "contraction") 
        return(-rr.change.by.id[rr.change.by.id < 0])
    rr.change.by.id[rr.change.by.id > 0]
    
}

changeInRecurringRevenueByID <- function(current, earlier, data)
{
    id <- data$id
    id.earlier <- unique(id[earlier])
    id.current <- unique(id[current])
    id.retained <- intersect(id.earlier, id.current)
    retained <- id %in% id.retained
    rr.current <- recurringRevenueByID(current, retained, data)
    rr.earlier <- recurringRevenueByID(earlier, retained, data)
    rr.current - rr.earlier
}


recurringRevenueByID <- function(x, y, data)
{
    subset <- x & y
    tapply(data$recurring.value[subset],
           list(data$id[subset]), 
           sum)
}