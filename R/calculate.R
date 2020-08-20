calculateRatio <- function(data, ratio, components, volume, use, name)
{
    calc <- calculate(data, components, volume, use)
    stat <- if (ratio) calc$numerator / calc$denominator else calc$numerator
    if (components == "retention")
        stat <- 1 - stat
    class.name <- if (use == "Cohort") "MetricCohort" else "MetricRatio" 
    createOutput(stat, class.name, calc, name)
}    


calculateUnivariate <- function(data, ratio, volume, use, name, statistic)
{
    calc <- calculate(data, components = "current", volume, use)
    stat <- calc[[statistic]]
    class.name <- if (use == "Cohort") "MetricCohort" else "MetricUnivariate"
    createOutput(stat, class.name, calc, name)
}    


#' \code{calculate}
#' 
#' Calculates main metrics
#' @param data A \code{MetricData} object.
#' @param components A character indicating the revenue metric component to use; one
#' of 'churn', 'expansion', 'contraction', 'retention', and 'net retention'.
#' @param use The data to be used in the calculation: 'aggregate, "initial', or 'chort'.
#' @param volume If TRUE, recurring revenue rather than customers is used in calculations.
#' @return A named vector showing churn.
#' @importFrom flipTime Period
calculate <- function(data, 
                      components,
                      volume, 
                      use)
{
    mergers <- mergersWithDates(data)

    if (components == "customers" & use != "Aggregate") 
        return(calculateNumberCustomers(use, data, mergers))
    calculateUsingLoop(data, components, use, mergers, volume)
}

#' @importFrom flipTime Period
calculateNumberCustomers <- function(use, data, mergers)
{
    if (use == "Cohort")
        stop("There is no way of looking at number of customer by cohort. Perhaps Customer Retention is what you are after.")
    cohort.ids <- cohortIDs(data)    
    out <- list(detail = cohort.ids,
                denominator = sapply(cohort.ids, length),
                numerator = NULL)
    out$by <- attr(data, "by")
    out$use <- use
    out$subscription.length <- attr(data, "subscription.length")
    out$volume <- attr(data, "volume")
    out
}
    
    
#' @importFrom flipTime Period
calculateUsingLoop <- function(data, components, use, mergers, volume)
{
    checkInputs(components, use)
    cohort.ids <- if (!volume | use != "Aggregate") cohortIDs(data)
    period.dts <- attr(data, "by.sequence")
    periods <- periodLabels(data, use,  components)
    subscription.dts <- attr(data, "subscription.sequence")
    subscription.dts <- c(subscription.dts[-length(subscription.dts)], attr(data, "end"))
    subscription.periods <- subscriptionPeriods(data, use)
    subscription.unit <- Periods(1, attr(data, "subscription.length"))
    results <-  matrixLikeList(periods, subscription.periods)
    for (period in 1:length(periods))
    {
        period.dt <- period.dts[period]
        cohort.id <- if (volume & use == "Aggregate") NULL else idCohort(period, cohort.ids, use)
        
        for (i in seq_along(subscription.periods))
        {
            start.dt <- if (use == "Aggregate") period.dt else period.dt + subscription.unit * (i - 1)# subscription.dts[i]
            if (start.dt >= period.dt)
            {
                is.first.period <- start.dt > period.dt & period.dt + subscription.unit >= start.dt
                if (is.first.period | use != "Initial")
                    results[[period]][[i]] <- doCalculations(volume, i, period.dt, start.dt, subscription.dts, cohort.id, use, components, data, mergers)
                
            }
        }
    }
    tidyResults(results, use, volume, attr(data, "by"), attr(data, "subscription.length"), components)
}


    
doCalculations <- function(volume, i, period.dt, start.dt, subscription.dts, cohort.id, use, components, data, mergers)
{
    if (volume | components %in%  c("current", "customers"))
        return(pointInTimeCalculation(period.dt, start.dt, cohort.id, use, components, data))
    timeWindowCalculation(i, start.dt, subscription.dts, cohort.id, use, components, data, mergers)
    
}

timeWindowCalculation <- function(i, start.dt, subscription.dts, cohort.id, use, components, data, mergers)
{
    subscription.unit <- Periods(1, attr(data, "subscription.length"))
    from <- data$from
    to <- data$to
    id <- data$id
    # Working out who was due to churn
    next.dt <- if (use != "Aggregate") subscription.dts[i + 1] else min(start.dt + subscription.unit, attr(data, "end"))
    to.renew <- to >= start.dt & to < next.dt
    id.to.renew <- intersect(unique(id[to.renew]), cohort.id)
    
    # Working out who churned
    subscribed.in.period <- from >= start.dt & to > next.dt
    id.subscribed.in.period <- intersect(id[subscribed.in.period], cohort.id)
    id.churned <- setdiff(id.to.renew, id.subscribed.in.period)
    
    #Taking mergers ito account
    calculateRatioNumbers(mergers, start.dt, next.dt, id.to.renew, id.churned)
}

calculateRatioNumbers <- function(mergers, start.dt, next.dt, id.to.renew, id.churned)
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

#' @importFrom flipTime Period
pointInTimeCalculation <- function(period.dt, start.dt, cohort.id, use, components, data)
{
    by.unit <- Periods(1, attr(data, "by"))
    subscription.unit <- Periods(1, attr(data, "subscription.length"))
    id <- data$id
    rr <- data$recurring.value
    if (!components %in% c("current", "customers") | use != "Aggregate")
        start.dt <- min(start.dt + by.unit, attr(data, "end"))
    current <- customerAtPeriodEnd(data, start.dt)
    if (components %in% c("current", "customers")) # recurring revenue and customer numbers
        return(currentRevenueAndCustomers(current, cohort.id, id, rr, use))
     #churn/retention
    previous <- customerAtPeriodEnd(data, start.dt - subscription.unit)
    previous.id <- unique(id[previous])
    if (any(c("contraction", "expansion") == components) & !"churn" %in% components)
        detail <- expansionOrContraction(components, current, previous, id, rr, previous.id)
    else
    {
        m <- if ("net retention" == components)
            current & id %in% previous.id # Net Recurring Revenue Churn
        else # churn or retention
            previous & id %in% setdiff(previous.id, id[current])
        detail <- tapply(rr[m], list(id[m]), sum)
    }
    # print(paste("previous", as.character(period.dt),
    #         as.character(start.dt),
    #         "customers: ", sum(nUnique(id[previous])),
    #         "base rr: ", round(sum(rr[previous])),
    #         "expanders n: ", length(detail),
    #         "%: ", round(length(detail)/nUnique(id[previous]), 2),
    #         "expansion $: ", round(sum(detail)),
    #         "%: ", round(sum(detail)/sum(rr[previous]), 2)))
    list(denominator = sum(rr[previous]), numerator = sum(detail), detail = detail)
}

currentRevenueAndCustomers <- function(current, cohort.id, id, rr, use)
{
    m <- current
    if (use != "Aggregate")
        m <- m & id %in% cohort.id # Can be made more efficient by moving up
    value <- rr[m]
    id <- id[m]
    list(numerator = sum(value), 
         denominator = nUnique(id),
         detail = data.frame(id = as.character(id), 
                             value = value, 
                             stringsAsFactors = FALSE))
}    

expansionOrContraction <- function(components, current, previous, id, rr, previous.id)
{
    
    rr.change.by.id <- changeInRecurringRevenueByID(current, previous, id, rr, previous.id)
    #print(rr.change.by.id)
    #print(c(Contraction = mean(rr.change.by.id < 0),
    ##        Same = mean(rr.change.by.id == 0),
     #       Expansion = mean(rr.change.by.id > 0)))
    #print(c("Mean Expansion" = mean(rr.change.by.id[rr.change.by.id > 0])))
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
    period.date > data$from  & period.date <= data$to
}

# customerAtEarlierPeriodEnd <- function(data, period.dt, later.period.being.compared.to)
# {
#     previous <- customerAtPeriodEnd(data, period.dt)
#     if (later.period.being.compared.to - period.dt < attr(data, "subscription.length")) # Dealing with final incomplete period
#         previous <- previous & data$to < attr(data, "end")
#     previous
# }


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
    if (use != "Aggregate")
        return(cohort.ids[[cohort]])
    unlist(cohort.ids[1:cohort])
}

subscriptionPeriods <- function(data, use)
{
    if (use == "Aggregate") 
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
    denominator <- denominator[rf, cf, drop = FALSE]
    numerator <- numerator[rf, cf, drop = FALSE]
    if (ncol(denominator) == 1)
    {
        if (nrow(denominator) == 1)
        {
            rn <- rownames(denominator)
            denominator <- denominator[1, 1]
            numerator <- numerator[1, 1]
            names(denominator) <- names(numerator) <- rn
            
        } else
        {
            numerator <- numerator[, 1]
            denominator <- denominator[, 1]
        }
    }        
    list(denominator = denominator, numerator = numerator)
}


mergersWithDates <- function(data)
{   #' Adds a column of the tim period at which the mergers occurred
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

    

zeroRowsAtTopAndBottom <- function(x)
{
    rs <- rowSums(x)
    cumsum(rs) > 0 & rev(cumsum(rev(rs))) > 0
}


#' @importFrom flipTables Cbind
asMatrix <- function(list.of.lists, FUN, fill.with = 0)
{
    x <- lapply(list.of.lists, function(x) sapply(x, FUN))
    m <- t(do.call("Cbind", x)) #Using t() to hack around DS-3041
    m[is.na(m)] <- fill.with
    m
}    
tidyResults <- function(results, use, volume, by, subscription.length, components)
{
    
    denominator <- convertToMatrix(results, "denominator")
    numerator <- convertToMatrix(results, "numerator")
    detail <- lapply(results, function(x) lapply(x, function(x) x[["detail"]]))
    out <- tidyNumeratorAndDenominator(numerator, denominator)
    if (use == "Initial")
        out <- matrixToVectorForInitialPeriod(out, by, subscription.length)
    out$detail <- tidyDetail(volume, numerator, denominator, detail, components)
    out$by <- by
    out$use <- use
    out$subscription.length <- subscription.length
    out$volume <- volume
    out
}


checkInputs <- function(components, use)
{
    if (!components %in% c("current", "expansion", "contraction", "churn", "retention", "net retention", "customers"))
        stop("Unknown components: ", paste(components, separate = ","))
    if (!use %in% c("Initial", "Aggregate", "Cohort"))
        stop("Unknown use: ", paste(use, separate = ","))
}            



# asRetention <- function(components, use)
# {
#     if (use == "Cohort")
#         return(TRUE)
#     all(c("churn", "expansion", "contraction") %in% components)
# }


periodLabels <- function(data, use, components)
{
    periods <- attr(data, "by.period.sequence")
    periods <- periods[-length(periods)]
    if ("current" %in% components & attr(data, "by") != "day" & use == "Aggregate")
        periods <- c(attr(data, "previous.period"), periods)
    periods
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
    subscription.unit <- Periods(1, subscription.length)
    row.dts <- AsDate(rownames(x))
    rows <- floor_date(row.dts, subscription.length) + subscription.unit
    rowm <- matrix(rows, nrow(x), ncol(x))                   
    cols <- AsDate(colnames(x))
    colm <- matrix(cols, nrow(x), ncol(x), byrow = TRUE)    
    m <- colm == rowm
    out <- x[m]
    names(out) <- Period(row.dts [rowSums(m) != 0] + subscription.unit, by)
    out
}   


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

#' @importFrom dplyr bind_rows 
tidyDetail <- function(volume, numerator, denominator, detail, components)
{
    if (volume) 
        return(detail)
    if (components == "customers")# This could probably can be combined into the code below
    { 
        detail <- lapply(detail, function(x) x[['All']])
        return(bind_rows(detail))
    }
    
    cohort.matrix <- matrix(rownames(denominator),  nrow(denominator), ncol(denominator))
    subscription.matrix <- matrix(colnames(denominator),  nrow(denominator), ncol(denominator))
    data.frame(Cohort = rep(cohort.matrix, numerator),
               "Rewewal Period" = rep(subscription.matrix, numerator),
               Churned = unlist(unlist(detail)),
               row.names = NULL)
    
    
}

