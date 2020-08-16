#' \code{calculate}
#' 
#' The percentage of customers who could churn in a period that did churn.
#' Note that this is not the most widely used definition. A more common definition
#' is the proportion of customers that churned. These two measures differ in some  situations:
#' 1. Multi-year contracts.
#' 2. Incomplete periods (e.g., the conventional metric can't be used in a quarter until it
#' has been completed.
#' 3. Where a customer joins and churns withhin a single period (ignored in the traditional definition)
#' @param data A \code{MetricData} object.
#' @param components A vector of characters indicating the components of churn to use.
#' Contains any of 'churn', 'expansion', and 'contraction'.
#' @param use The data to be used in the calculation: 'aggregate, "first period', or 'chort'.
#' @param volume If TRUE, recurring revenue rather than customers is used in calculations.
#' @return A named vector showing churn.
#' @importFrom flipTime Period
calculate <- function(data, 
                      components = c("current", "expansion", "contraction", "churn")[3],
                      volume = TRUE, 
                      use = c("aggregate", "initial", "cohort")[1])
{
    period.dts <- attr(data, "by.sequence")
    periods <- periodLabels(data, components)

    subscription.dts <- attr(data, "subscription.sequence")
    subscription.dts <- c(subscription.dts[-length(subscription.dts)], attr(data, "end"))
    subscription.periods <- subscriptionPeriods (data, use)
    unit <- Periods(1, attr(data, "subscription.length"))

    cohort.ids <- if (!volume) cohortIDs(data)
    mergers <- mergersWithDates(data)
    
    results <-  matrixLikeList(periods, subscription.periods)
    for (period in 1:length(periods))
    {
        period.dt <- period.dts[period]
        cohort.id <- if (volume) NULL else idCohort(period, cohort.ids, use)

        for (i in seq_along(subscription.periods))
        {
            start.dt <- if (use == "aggregate") period.dt else subscription.dts[i]
            if (start.dt >= period.dt)
            {
                is.first.period <- start.dt > period.dt & period.dt + unit >= start.dt
                if (is.first.period | use != "first period")
                {
                    results[[period]][[i]] <- doCalculations(volume, i, period.dt, start.dt, subscription.dts, unit, cohort.id, use, components, data, mergers)
                }
                
            }
        }
    }
    tidyResults(results, use, volume, attr(data, "by"), attr(data, "subscription.length"))
}

periodLabels <- function(data, components)
{
    periods <- attr(data, "by.period.sequence")
    if ("current" %in% components)
        return(periods)
    periods[-length(periods)]
}

tidyResults <- function(results, use, volume, by, subscription.length)
{
    
    denominator <- convertToMatrix(results, "denominator")
    numerator <- convertToMatrix(results, "numerator")
    detail <- lapply(results, function(x) lapply(x, function(x) x[["detail"]]))
    out <- tidyNumeratorAndDenominator(numerator, denominator)
    if (use == "initial")
        out <- matrixToVectorForInitialPeriod(out, by, subscription.length)
    out$detail <- tidyDetail(volume, numerator, denominator, detail)
    out$by <- by
    out$volume <- volume
    out
}


matrixToVectorForInitialPeriod <- function(calculation, by, subscription.length)
{
    calculation$denominator <- diagRectangular(calculation$denominator, by, subscription.length)
    calculation$numerator <- diagRectangular(calculation$numerator, by, subscription.length)
    calculation
}    


#' @importFrom flipTime AsDate Period Periods
#' @importFrom lubridate floor_date
diagRectangular <- function(x, by, subscription.length)
{
    unit <- Periods(1, subscription.length)
    row.dts <- AsDate(rownames(x))
    rows <- floor_date(row.dts, subscription.length) + unit
    rowm <- matrix(rows, nrow(x), ncol(x))                   
    cols <- AsDate(colnames(x))
    colm <- matrix(cols, nrow(x), ncol(x), byrow = TRUE)    
    m <- colm == rowm
    out <- x[m]
    names(out) <- Period(row.dts [rowSums(m) != 0] + unit, by)
    out
}   

#' #' @importFrom flipTime AsDate Period Periods
#' #' @importFrom lubridate floor_date
#' diagRectangular <- function(x, data)
#' {
#'     by <- attr(data, "by")
#'     subscription.length <- attr(data, "subscription.length")
#'     unit <- Periods(1, subscription.length)
#'     row.dts <- AsDate(rownames(x))
#'     rows <- floor_date(row.dts, subscription.length) + unit
#'     rowm <- matrix(rows, nrow(x), ncol(x))                   
#'     cols <- AsDate(colnames(x))
#'     colm <- matrix(cols, nrow(x), ncol(x), byrow = TRUE)    
#'     m <- colm == rowm
#'     out <- x[m]
#'     names(out) <- Period(row.dts [rowSums(m) != 0] + unit, by)
#'     out
#' }   

convertToMatrix <- function(results, statistic)
{
    out <- sapply(results, function(x) sapply(x, function(x) {
            r <- x[[statistic]]
            if (is.null(r)) 0 else r}))
    if (is.matrix(out))
        out <- t(out)
    if (!is.matrix(out))
        out <- matrix(out, ncol = 1, dimnames = list(names(results), names(results[[1]])))
    out
}


tidyDetail <- function(volume, numerator, denominator, detail)
{
    if (volume) 
        return(detail)
    cohort.matrix <- matrix(rownames(denominator),  nrow(denominator), ncol(denominator))
    subscription.matrix <- matrix(colnames(denominator),  nrow(denominator), ncol(denominator))
    data.frame(Cohort = rep(cohort.matrix, numerator),
               "Rewewal Period" = rep(subscription.matrix, numerator),
               Churned = unlist(unlist(detail)),
               row.names = NULL)

    
}
    
    
doCalculations <- function(volume, i, period.dt, start.dt, subscription.dts, unit, cohort.id, use, components, data, mergers)
{
    if (volume | use == "current")
        return(volumeOrCurrentCalculation(period.dt, start.dt, unit, use, components, data))
    customerCalculation(i, start.dt, end.dt, subscription.dts, unit, cohort.id, use, components, data, mergers)
    
}

customerCalculation <- function(i, start.dt, end.dt, subscription.dts, unit, cohort.id, use, components, data, mergers)
{
    from <- data$from
    to <- data$to
    id <- data$id
    # Working out who was due to churn
    next.dt <- if (use != "aggregate") subscription.dts[i + 1] else min(start.dt + unit, attr(data, "end"))
    to.renew <- to >= start.dt & to < next.dt
    id.to.renew <- intersect(unique(id[to.renew]), cohort.id)
    
    # Working out who churned
    subscribed.in.period <- from >= start.dt & to > next.dt
    id.subscribed.in.period <- intersect(id[subscribed.in.period], cohort.id)
    id.churned <- setdiff(id.to.renew, id.subscribed.in.period)
    
    #Taking mergers ito account
    calculateChurnNumbers(mergers, start.dt, next.dt, id.to.renew, id.churned)
}

calculateChurnNumbers <- function(mergers, start.dt, next.dt, id.to.renew, id.churned)
{
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
    list(denominator = length(id.to.renew),
         numerator = length(id.churned),
         detail = id.churned)
}



volumeOrCurrentCalculation <- function(period.dt, start.dt, unit, use, components, data)
{
    id <- data$id
    rr <- data$recurring.value
    start.dt <- min(start.dt + unit, attr(data, "end"))
    current <- customerAtPeriodEnd(data, start.dt)
    if (use == "current") # recurring revenue and customer numbers
        return(currentRevenueAndCustomers(current, id, rr))
     #churn/retention
    previous <- customerAtEarlierPeriodEnd(data, period.dt, start.dt)
    previous.id <- unique(id[previous])
    if (any(c("contraction", "expansion") %in% components) & !"churn" %in% components)
        detail <- expansionOrContraction(components, current, previous, id, rr, previous.id)
    else
    {
        m <- if (all(c("churn", "contraction", "expansion") %in% components))
            current & id %in% previous.id # Net Recurring Revenue Churn
        else # churn
            previous & id %in% setdiff(previous.id, id[current])
        detail <- tapply(rr[m], list(id[m]), sum)
    }
    list(denominator = sum(rr[previous]), numerator = sum(detail), detail = detail)
}

currentRevenueAndCustomers <- function(current, id, rr)
{
    value <- rr[current]
    id <- id[current]
    numerator <- sum(value)
    list(numerator = value, denominator = length(id), detail = data.frame(id = id, value = value))
}    

expansionOrContraction <- function(components, current, previous, id, rr, previous.id)
{
    
    rr.change.by.id <- changeInRecurringRevenueByID(current, previous, id, rr, previous.id)
    if (components == "contraction") 
        return(-rr.change.by.id[rr.change.by.id < 0])
    rr.change.by.id[rr.change.by.id > 0]
    
}
changeInRecurringRevenueByID <- function(current, previous, id, rr, previous.id)
{
    retained.id <- intersect(previous.id, id[current])
    id.m <- id %in% retained.id
    m <- previous & id.m
    rr.prev.by.id <- tapply(rr[m], list(id[m]), sum)
    m <- current & id.m
    rr.curr.by.id <- tapply(rr[m], list(id[m]), sum)
    rr.curr.by.id - rr.prev.by.id
}

customerAtPeriodEnd <- function(data, period.date)
{
    period.date >= data$from  & period.date < data$to
}

customerAtEarlierPeriodEnd <- function(data, period.dt, later.period.being.compared.to)
{
    previous <- customerAtPeriodEnd(data, period.dt)
    if (later.period.being.compared.to - period.dt < attr(data, "subscription.length")) # Dealing with final incomplete period
        previous <- previous & data$to < attr(data, "end")
    previous
}

matrixLikeList <- function(rownames, colnames)
{
    column <- vector("list", length(colnames))
    names(column) <- colnames
    m <- rep(list(column), length(rownames))
    names(m) <- rownames
    m
}

createMatrix <- function(value, rownames, colnames)
{
    matrix(value, 
           length(rownames),
           length(colnames), 
           dimnames = list(rownames, colnames))
    
}    

idCohort <- function(cohort, cohort.ids, use, volume)
{
    if (use != "aggregate")
        return(cohort.ids[[cohort]])
    unlist(cohort.ids[1:cohort])
}

subscriptionPeriods <- function(data, use)
{
    if (use == "aggregate") 
        return("All" )
    seq <- attr(data, "subscription.period.sequence")
    return(seq[-length(seq)])
}


#' @importFrom flipTime Period AsDate
cohortIDs <- function(data)
{
    start.by.id <- aggregate(from ~ id, data = data, FUN = min)
    period <- Period(start.by.id[, 2], attr(data, "by"))
    r <- tapply(start.by.id[, 1], list(period), c)
    periods <- attr(data, "by.period.sequence")
    out <- vector("list", length(periods))
    names(out) <- periods
    out[names(r)] <- r
    out
}


#' @importFrom flipTime Period
tidyNumeratorAndDenominator <- function(numerator, denominator)#, mergers, by, volume, rr)
{
    # Removing zero rows and columns
    rf <- zeroRowsAtTopAndBottom(denominator)
    cf <- zeroRowsAtTopAndBottom(t(denominator))
    denominator <- denominator[rf, cf]
    numerator <- numerator[rf, cf]
    list(denominator = denominator, numerator = numerator)
}


#' Adds a column of the tim period at which the mergers occurred
mergersWithDates <- function(data)
{
    mergers <- attr(data, "mergers")
    by <- attr(data, "by")
    id <- data$id
    if (is.null(mergers) || nrow(mergers) == 0)
        return(NULL)
    mergers$date <- rep(as.Date("2999-12-31"), NROW(mergers)) # Lazy way of dealing with situation where churn doesn't occur
    m <- id %in% mergers$id
    ag <- aggregate(data$to[m], list(id[m]), FUN = max)
    m <- ag[, ] %in% id
    mergers$date[match(ag[,1], mergers$id)] <- ag[, 2]
    mergers
}
#mergers$date
    # m <- id %in% mergers$id
    # tmp <- aggregate(to[m], list(id[m]), FUN = max)
    # merge.period <- Period(tmp[, 2], "by")
    # merge.id <- tmp[, 1]
    # 
    # increment <- if (volume) 
    #     tapply(rr[m], list(id[m]), FUN = sum)
    # else
    #     rep(1, NROW(merge.period))
    # 
    # # Applying to customer churn
    # # applying to numertor
    # m <- match(names(increment), names(numerator))
    # diffs <- tapply(increment, list(merge.id), sum)
    #  <- if (volume) 
    
    

zeroRowsAtTopAndBottom <- function(x)
{
    rs <- rowSums(x)
    cumsum(rs) > 0 & rev(cumsum(rev(rs))) > 0
}
