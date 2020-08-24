timeWindowCalculation <- function(in.cohort, period.start, data)
{
    from <- data$from
    to <- data$to
    id <- data$id
    
    # Working out who was due to churn
    # <- periodStart(data, period)
    next.period.start  <- nextDate(data, period.start)# + byUnit(data)#endDate(start.date, period, data, period, cohort.type)
    #    next.dt <- if (cohort.type != "None") subscription.dts[i + 1] else min(start.dt + subscription.unit, attr(data, "end"))
    
#    print(c("cur", as.character(period.start), as.character(next.period.start)))
    to.renew <- dateVariableInWindow(data$to, period.start, next.period.start)
    if (newCohort(data))
        in.cohort <- newCusts(data, period.start, next.period.start)
    to.renew <- andSubsetIfItExists(to.renew, in.cohort)
    id.to.renew <- unique(id[to.renew])
    
    # Working out who churned
    subscribed.in.period <- from >= period.start & to > next.period.start
    subscribed.in.period <- andSubsetIfItExists(subscribed.in.period, in.cohort)
    id.subscribed.in.period <- unique(id[subscribed.in.period])
    
    id.churned <- setdiff(id.to.renew, id.subscribed.in.period)
    
    #Taking mergers ito account
    calculateRatioNumbers(data, period.start, next.period.start, id.to.renew, id.churned)
}


newCusts <- function(data, period.start, next.period.start)
{
    u <- subscriptionUnit(data)
 #   print(c("prev", as.character(period.start - u), as.character(next.period.start - u)))
    dateVariableInWindow(data$subscribed.from, period.start - u, next.period.start - u)
}

calculateRatioNumbers <- function(data, start.dt, next.dt, id.to.renew, id.churned)
{
    mergers <- attr(data, "mergers")
    merger.in.period <- mergers$date >= start.dt & mergers$date < next.dt
    merger.after.period <- mergers$date <= start.dt
    relevant.mergers <- merger.in.period | merger.after.period
    if (any(relevant.mergers))
    {
        mergers.from <- mergers$id[relevant.mergers]
        id.to.renew <- setdiff(id.to.renew, mergers.from)
        if (any(merger.in.period))
            id.churned <- setdiff(id.churned, mergers.from) 
    }
#    print(length(id.to.renew))
    list(denominator = length(id.to.renew),
         numerator = length(id.churned),
         detail = id.churned)
}


