timeWindowCalculation <- function(in.cohort, period.start, components, data)
{
    from <- data$from
    to <- data$to
    id <- data$id
    
    # Working out who was due to churn
    # <- periodStart(data, period)
    next.period.start  <- nextDate(data, period.start)# + byUnit(data)#endDate(start.date, period, data, period, cohort.type)
    #    next.dt <- if (cohort.type != "None") subscription.dts[i + 1] else min(start.dt + subscription.unit, attr(data, "end"))
    
#    print(c("cur", as.character(period.start), as.character(next.period.start)))
    to.renew <- toRenew(to, from, id, period.start, next.period.start)
    if (newCohort(data))
        in.cohort <- newCusts(data, period.start, next.period.start)
    to.renew <- andSubsetIfItExists(to.renew, in.cohort)
    id.to.renew <- unique(id[to.renew])
    
    # Working out who churned
    subscribed.in.period <- from >= period.start & to > next.period.start
    subscribed.in.period <- andSubsetIfItExists(subscribed.in.period, in.cohort)
    id.subscribed.in.period <- unique(id[subscribed.in.period])
    
    ids <- if (components == "churn")
        setdiff(id.to.renew, id.subscribed.in.period)
    else # retention
        intersect(id.to.renew, id.subscribed.in.period)

    #Taking mergers ito account
    calculateRatioNumbers(data, period.start, next.period.start, id.to.renew, ids)
}



toRenew <- function(to, from, id, period.start, period.end)
{
    id %in% toRenewID(to, from, id, period.start, period.end)
}


toRenewID <- function(to, from, id, period.start, period.end)
{
    in.window <- dateVariableInWindow(to, period.start, period.end)
    crossing.window <- from < period.start & period.end < to
    setdiff(id[in.window], id[crossing.window]) 
}

newCusts <- function(data, period.start, next.period.start)
{
    u <- subscriptionUnit(data)
 #   print(c("prev", as.character(period.start - u), as.character(next.period.start - u)))
    dateVariableInWindow(data$subscribed.from,
                         period.start - u, 
                         period.start + byUnit(data) - u)
}

calculateRatioNumbers <- function(data, start.date, next.start.date, id.to.renew, id.churned)
{
    merge.info <- mergerInfo(data, start.date, next.start.date)
    if (any(merge.info$relevant))
    {
        mergers.from <- merge.info$from.id
        id.to.renew <- setdiff(id.to.renew, mergers.from)
        if (any(merge.info$in.period))
            id.churned <- setdiff(id.churned, mergers.from)
    }
    list(denominator = length(id.to.renew),
         numerator = length(id.churned),
         detail = id.churned)
}


mergerInfo <- function(data, start.date, next.start.date)
{
    mergers <- attr(data, "mergers")
    in.period <- mergers$date >= start.date & mergers$date < next.start.date
    later.periods <- mergers$date <= start.date #Must be a smarter way...
    relevant <- in.period | later.periods    
    list(from.id = as.character(mergers$id[relevant]),
         to.id = as.character(mergers$id.to[relevant]),
         in.period = in.period, 
         relevant = relevant)
}
