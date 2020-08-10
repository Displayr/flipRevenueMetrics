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
#' @param initial.only If TRUE, only calculates retention for the first opportunity for
#' renewal
#' @param by.period If TRUE, calculates one figure for each cohort (rather than by period)
#' @return A named vector showing churn.
#' @importFrom flipTime Period
#' @export
calculate <- function(data, initial.only = FALSE, by.period = FALSE, volume = TRUE, components = c("churn"))
{

    id <- data$id
    to <- data$to
    from <- data$from
    rr <- if (volume) data$recurring.value else NULL
    ids.by.cohort <- if (!volume) idsByFirstPeriod(data)
    end.date <- attr(data, "end")

    s.l <- attr(data, "subscription.length")
    subscription.dts <- attr(data, "subscription.sequence")
    n.subscription.dates <- length(subscription.dts)
    subscription.dts <- c(subscription.dts[-n.subscription.dates], attr(data, "end"))
    subscription.periods <- if (!by.period) "All" 
    else {
        seq <- attr(data, "subscription.period.sequence")
        seq[-length(seq)]
    }
    n.subscription.periods <- length(subscription.periods)
    unit <- Periods(1, s.l)

    cohort.dts <- attr(data, "by.sequence")
    cohorts <- attr(data, "by.period.sequence")
    #if (!volume)
    cohorts <- cohorts[-length(cohort.dts)]
    n.cohorts <- length(cohorts)
        
    # objects to save intermedia results
    dt.list <- vector("list", n.subscription.dates)
    names(dt.list) <- subscription.periods
    id.num.matrix <- rep(list(dt.list), n.cohorts)
    names(id.num.matrix) <- cohorts
   # id.den.matrix <- id.num.matrix
    
    den <- num <- matrix(0, n.cohorts, n.subscription.periods,
                         dimnames = list(Cohort = cohorts,
                                         Period =  subscription.periods))
    
    for (cohort in 1:n.cohorts)
    {
        cohort.dt <- cohort.dts[cohort]
        if (!volume)
            id.cohort <- if(by.period) ids.by.cohort[[cohort]] else unlist(ids.by.cohort[1:cohort])
        
        iterate.through <- if (by.period) 1:n.subscription.periods else 1
        for (i in iterate.through) # The initial part of the sequence can be set a bit smarter
        {
            start.dt <- if (by.period) subscription.dts[i] else cohort.dt# + unit
            if (start.dt >= cohort.dt)
            {
                if (!initial.only | start.dt > cohort.dt & cohort.dt + unit >= start.dt)
                {
                    if (volume)
                    {
                        start.dt <- min(start.dt + unit, end.date)
                        previous <- cohort.dt >= from  & cohort.dt < to 
                        if (start.dt - cohort.dt < s.l) # Dealing with final incomplete period
                             previous <- previous & to < end.date
                        
                            
                        id.cohort <- unique(id[previous])
                        den[cohort, i] <- sum(rr[previous])
    
                        current <- start.dt >= from  & start.dt < to# & id %in% id.cohort
                        if (any(c("contraction", "expansion") %in% components) & 
                            !"churn" %in% components)
                        {
                            id.retained <- intersect(id.cohort, id[current])
                            id.m <- id %in% id.retained
                            m <- previous & id.m
                            rr.prev.by.id <- tapply(rr[m], list(id[m]), sum)
                            m <- current & id.m
                            rr.curr.by.id <- tapply(rr[m], list(id[m]), sum)
                            rr.change.by.id <- rr.curr.by.id - rr.prev.by.id
                            det <- if (components == "contraction") 
                                 -rr.change.by.id[rr.change.by.id < 0]
                            else 
                                rr.change.by.id[rr.change.by.id > 0]
                        }
                        else
                        {
                            m <- if (all(c("churn", "contraction", "expansion") %in% components))
                            {   # Net Recurring Revenue Churn
                                current & id %in% id.cohort
                            } 
                            else 
                            {
                                id.churned <- setdiff(id.cohort, id[current])
                                previous & id %in% id.churned
                            }                            
                            det <- tapply(rr[m], list(id[m]), sum)
                        }
                        id.num.matrix[[cohort]][[i]] <- dt
                        num[cohort, i] <- sum(det)
                    }
                    else 
                    {
                        next.dt <- if (by.period) subscription.dts[i + 1] else min(start.dt + unit, attr(data, "end"))
                        to.renew <- to >= start.dt & to < next.dt
                        id.to.renew <- intersect(unique(id[to.renew]), id.cohort)
                        #id.den.matrix[[cohort]][[i]] <- id.to.renew
                        den[cohort, i] <- length(id.to.renew)
                        
                        subscribed.in.period <- from >= start.dt & to > next.dt
                        id.subscribed.in.period <- intersect(id[subscribed.in.period], id.cohort)
                        id.num.matrix[[cohort]][[i]] <- churned <- setdiff(id.to.renew, id.subscribed.in.period)
                        num[cohort, i] <- length(churned)
                    }    
                }
            }
        }
    }
    # Removing periods at start or end with no possibility of churn
    rf <- zeroRowsAtTopAndBottom(den)
    cf <- zeroRowsAtTopAndBottom(t(den))
    den <- den[rf, cf, drop = FALSE]
    num <- num[rf, cf, drop = FALSE]
    
#    names(dimnames(den)) <- names(dimnames(den)) <- c("Cohort", "Period")
 #   retained <- 1 - n.churned / den
    detail <- if (volume) id.num.matrix  else 
    {
        cohort.matrix <- matrix(rownames(den),  nrow(den), ncol(den))
        subscription.matrix <- matrix(colnames(den),  nrow(den), ncol(den))
        data.frame(Cohort = rep(cohort.matrix, num),
                   "Rewewal Period" = rep(subscription.matrix, num),
                   Churned = unlist(unlist(id.num.matrix)),
                   row.names = NULL)
    }
    
    
    out <- 1 - num / den
    out <- addAttributesAndClass(out, "RetentionByCohort", by, detail)
    attr(out, "volume") <- FALSE
    attr(out, "numerator") <- num
    attr(out, "denominator") <- den
    attr(out, ("by")) <- attr(data, ("by"))
    out
}

zeroRowsAtTopAndBottom <- function(x)
{
    rs <- rowSums(x)
    cumsum(rs) > 0 & rev(cumsum(rev(rs))) > 0
}
