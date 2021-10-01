#' @importFrom flipTime Period
#' @importFrom verbs Sum
comparingTwoPointsInTime <- function(in.cohort, period.start, data, components)
{
    # We compare points in time by looking at the statistic of interest at thee end of the periiod,
    # compared to the end of the preceding period. The period.start.date is the actual beginning
    # of the peeriod. The period.end.boundary is the beginning of the following period.
    # That is, in terms of mathematical notation for intervals: [period)
    
    # Can be made a lot faster by having the loop inside the function so calculations
    # can be re-used from pepriod to period
    period.end <- nextDate(data, period.start)
    invoice.end <- customerAtTime(data, period.end, in.cohort)
    invoice.start <- customerAtTime(data, period.start, in.cohort)
    if (newCohort(data))
    {
        new <- newCusts(data, period.start, period.end)
        invoice.end <- invoice.end & new
        invoice.start <- invoice.start & new
    }
    
#    invoice.end <- invoice.end & to.renew
#    invoice.start <- invoice.start & to.renew
    
    id <- idsReflectingMergers(data, period.start)
    #to.renew.id <- toRenewID(data$to, data$from, id, period.start, period.end)
    to.renew <- toRenew(data$to, data$from, id, period.start, period.end)
    #to.renew <- to.renew.id %in% to.renew.id
    
    rr <- data$recurring.value
    rr.end.by.id <- sumBy(rr[invoice.end], id[invoice.end])
    rr.start.by.id <- sumBy(rr[invoice.start], id[invoice.start])
    id.end <- names(rr.end.by.id)
    id.start <- names(rr.start.by.id)
    retention.id <- retentionID(id.start, id.end, id, to.renew)
    if (period.start == as.Date("2016-01-01"))
    {
        period.end = period.start
        
        
    }
    detail <- switch(components,
                  "new" = rr.end.by.id[setdiff(id.end, id.start)],
                  "contraction" = contraction(rr.start.by.id, rr.end.by.id),
                  "expansion" = expansion(rr.start.by.id, rr.end.by.id),
                  "churn" = rr.start.by.id[setdiff(id.start, id.end)],
                  "retention" = rr.start.by.id[retention.id])
    den <- if (components %in% c("churn", "retention", "contraction"))
                       sum(rr[to.renew & invoice.start])
    else
    {
        subscription.length.correction <- byUnit(data) / subscriptionUnit(data)
        final.period.correction <- finalPeriodCorrection(data, period.start, period.end)
        sum(rr[invoice.start])) * final.period.correction * subscription.length.correction
    }
    list(denominator = den, 
         numerator = sum(detail),
         detail = detail)
}


#' @importFrom flipTime Periods 
finalPeriodCorrection <- function(data, period.start, period.end)
{
    if (period.end != lastDate(data))
        return(1)
    days.left <- as.numeric(period.end - period.start)
    days.if.complete <- as.numeric(period.start + byUnit(data) - period.start)#  / Periods(1, "day"))
    days.left / days.if.complete
}
retentionID <- function(id.start, id.end, id, to.renew)
{
    renew.id <- unique(id[to.renew])
    intersect(renew.id,intersect(id.start, id.end))
}

customerAtTime <- function(data, date, in.cohort)
{
    customer <- customerAt(data, date)
    andSubsetIfItExists(customer, in.cohort)
}


differences <- function(x, y)
{
    int <- intersect(names(x), names(y))
    y[int] - x[int]
}

expansion <- function(x, y)
{
    d <- differences(x, y)
    d[d > 0]    
}

contraction <- function(x, y)
{
    d <- differences(x, y)
    -d[d < 0]    
}

# 
# currentCustomers <- function(in.cohort, period.start, data)
# {
#     period.boundary <- nextDate(data, period.start)
#     m <- customerAtPeriodEnd(data, period.boundary)
#     andSubsetIfItExists(m, in.cohort)
# }
# 
# earlierCustomers <- function(in.cohort, period.start, data)
# {
#     period.boundary <- nextDate(data, period.start)
#     unit <- byUnit(data) #cohortUnit(data)#if(newCohort(data)) else cohortUnit(data)
#     earlier.boundary <- period.boundary - unit
#     m <- customerAtPeriodEnd(data, earlier.boundary)
#     if (newCohort(data)) # Should only occur when is.null(in.cohort)
#         in.cohort <- dateVariableInWindow(data$subscribed.from, 
#                                           earlier.boundary - unit, 
#                                           earlier.boundary)
#         #print(paste("new", earlier.boundary - unit, earlier.boundary))
#     andSubsetIfItExists(m, in.cohort)
# }
# # 
# # expansionOrContraction <- function(components, current, earlier, period.start,  value, id)
# # {
# #     rr.change.by.id <- changeInRecurringRevenueByID(current, earlier, value, id)
# #     if (components == "contraction") 
# #         return(-rr.change.by.id[rr.change.by.id < 0])
# #     rr.change.by.id[rr.change.by.id > 0]
# #     
# # }
# 
idsReflectingMergers <- function(data, period.start)
{
    changingMergedIDs(data, period.start, data$id)
}

changingMergedIDs <- function(data, period.start, id)
{
    merger.info <- mergerInfo(data, period.start, nextDate(data, period.start))
    if (length(merger.info$from.id) == 0)
        return(id)
    mtch <- match(id, merger.info$from.id)
    m <- !is.na(mtch)
    id[m] <- merger.info$to.id[mtch[m]]
    id
}
# 
# changeInRecurringRevenueByID <- function(current, earlier, value, id)
# {
#     id.earlier <- unique(id[earlier])
#     id.current <- unique(id[current])
#     id.retained <- intersect(id.earlier, id.current)
#     retained <- id %in% id.retained
#     rr.current <- recurringRevenueByID(current, retained, value, id)
#     rr.earlier <- recurringRevenueByID(earlier, retained, value, id)
#     rr.current - rr.earlier
# }
# 
# 
# recurringRevenueByID <- function(x, y, value, id)
# {
#     subset <- x & y
#     tapply(value[subset],
#            list(id[subset]), 
#            sum)
# }