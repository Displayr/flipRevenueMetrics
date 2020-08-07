#' \code{Churn}
#'
#' @description Computes retention, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param volume Weights the results by volume.
#' @param by The time period to aggregate the dates by: 
#' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' and \code{"day"}.
#' @param error.if.no.data If TRUE and the data contains no valid cases, an error is thrown.
#' @details Where subscribers suspends their purchasing for a period, 
#' but purchases again later, the subscriber
#' is included in the churn. Churn is show for all periods that have 
#' data in \code{data$to.period}, even if they occur in the future.
#' @return A named vector showing churn.
#' @importFrom flipTime AsDate Period
#' @export
Churn <- function(data, volume = FALSE, by = "quarter", error.if.no.data = FALSE)
{
    data <- prepareDataForChurn(data, by)
    #data <- data[data$from >= attr(data, "start"), ]
    if (nrow(data) == 0)
    {
        if (error.if.no.data)
            stop("No subscriptions have had an opportunity to churn.")
        return(NULL)
    }
    counts <- churnCountsByTime(data, volume)
    out <-  prop.table(counts, 2)[2, ]
    if (length(out) == 1) # Dealing with scalars losing names
        names(out) <- colnames(counts)
    to <- as.Date(data$to)
    dat <- data[data$churn & to >= attr(data, "start") & to <= attr(data, "end"),, drop = FALSE]
    detail <- idByPeriod(dat, time = "to.renewal.period")#sapply(id, paste, collapse = ",")
    out <- addAttributesAndClass(out, "Churn", by, detail)
    attr(out, "volume") <- volume
    out
}

#' \code{CustomerChurn}
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time period to aggregate the dates by: 
#' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' and \code{"day"}.
#' @param ... Additional arguments to be passed to lower level functions.
#' @return A named vector showing churn.
#' @importFrom flipTime AsDate
#' @export
CustomerChurn <- function(data)
{
    dts <- attr(data, "date.sequence")
    n <- length(dts)
    dts <- c(dts[-n], attr(data, "end"))
    id <- data$id
    to <- data$to
    from <- data$from
    recurring.value<- data$recurring.value
    #to.floor <- floor_date(to, attr(data, "by"))
    rr.to.renew <- vector("numeric", n - 1)
    id.to.renew <- vector("list", n - 1)
    names(id.renewed) <- names(rr.to.renew) <- dts[-1]
    n.renewed <- n.to.renew <- rr.renewed <- rr.to.renew
    id.renewed <- id.to.renew
    for (i in 1:(n - 1))
    {
        dt <- dts[i]
        next.dt <- ts[i + 1]
        # The denominator is people that are customers
        # at the beginning of the period who 
        # must renew in the period
        available.for.renewal <- to >= dt & to < next.dt
        id.available.for.renewal <- unique(id[available.for.renewal])

        
        purchased.next.period <- to >= dt & to < next.dt
        id.available.for.renewal <- unique(id[available.for.renewal])

                
        renewed <- from < next.dt & to >= next.dt
        id.renewed <- unique(id[renewed])
        
        
        
        # Excluding any within-window renewals
        renewed <- renewed & !to.renew
        
        rr.to.renew <- sum(rr[to.renew])
        rr.renewed <- sum(rr[renewed])
    }
    n.to.renew <- sapply(id.to.renew, length)
    n.renewed <- sapply(id.renewed, length)
    churn <- 1 - n.renewed / n.to.renew
    net.churn <- rr.renewed / rr.to.renew
    print(net.churn)
    print(n.renewed)
#    out <- rep(NA, n)
#    for (i in 1:n)
#    ids.by.dts <- aggregate()
#    print(dts)
   # Churn(data, volume = FALSE, by = by, error.if.no.data = FALSE)
}

#' #' @importFrom flipTime AsDate Period
#' #' @importFrom flipTime AsDate Period
#' prepareDataForChurnCalculation <- function(data)
#' {
#'     data$to.floor <- floor_date(data$to, attr(data, "by"))
#'     
#'     
#}    


#' \code{RecurringRevenueChurn}
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time period to aggregate the dates by: 
#' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' and \code{"day"}.
#' @param ... Additional arguments to be passed to lower level functions.
#' @return A named vector showing churn.
#' @importFrom flipTime AsDate
#' @export
RecurringRevenueChurn <- function(data, by = "quarter", ...)
{
    Churn(data, volume = TRUE, by = by, error.if.no.data = FALSE)
}


#' @export
plot.Churn <- function(x, ...)
{
    smooth <- if (length(x) < 4) "None" else "Friedman's super smoother"
    y.title <- if (attr(x, "volume")) "Recurring Revenue Churn Rate" else "Customer Churn Rate"
    columnChart(x, 
                fit.type = smooth,
                fit.ignore.last = TRUE,
                y.title = y.title, 
                y.tick.format = "%", ...)
}

churnCountsByTime <- function(data, volume)
{
    counts <- if (volume) {
        v <- Table(recurring.value ~ churn + to.renewal.period, data = data, FUN = sum)
        v[round(v, 7) == 0] <- 0 # Dealing with numeric precision issue that can lead to negative churn
        v
    }
    else Table(id ~ churn + to.renewal.period, data = data, FUN = nUnique)
    if (nrow(counts) == 1)
    {
        counts <-if (rownames(counts) == "TRUE")
            rbind(counts, "FALSE" = 0)[2:1, , drop = FALSE]
        else
            rbind(counts, "TRUE" = 0)
    }
    #print(counts)
    counts
}

