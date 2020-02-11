#' \code{GrowthAccounting}
#'
#' @description Computes the growth within accounts
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time period used in defining the cohorts: "year", "quarter", "month", "week", day".
#' @param ... Additional arguments to be passed to lower level functions.
#' @return A matrix
#' @importFrom flipTime AsDate Period
#' @importFrom flipStatistics Table
#' @importFrom lubridate floor_date years
#' @export
GrowthAccounting <- function(data, by = "year", ...)
{
    unit <- Periods(1, by)

    
    
  
    start <- floor_date(attr(data, "start"), by)
    end <- floor_date(attr(data, "end"), by)
    dts <- seq.Date(start, end, by)
    periods <- Period(dts, by)
    n.dates <- length(dts)

    # Variables used in loop
    from <- data$from
    to <- data$to
    id <- data$id
    rr <- data$recurring.value

    # Calculations used in loop
    customers <- unique(id[from < start])
    ids.previous <- unique(id[invoice.previous])
    invoice.previous <- from <= dt & to >= dt 
    rr.by.id.previous <- tapply(rr[invoice.previous], list(id[invoice.previous]), sum)
    
    # Storing results of loop
    metrics <- c("New", "Resurrection", "Expansion", "Contraction", "Churn")
    counts <- accounting <- matrix(NA, 5, n.dates, dimnames = list(metrics, periods))
    names(dimnames(accounting)) <- c("Metric", properCase(by)) 
    n <- nrow(data) * n.dates 
    str <- rep("", n)
    details <- data.frame(Date = str, Metric = str, Name = str, Change = rep(NA, n))
    counter <- 0
    # dt.previous <- dt - unit
    # invoice.previous <- from <= dt.previous & to >= dt.previous
    # existing.previous <- invoice.previous & id %in% ids.previous
    # rr.previous <- sum(rr[invoice.previous])
    # rr.previous.existing <- sum(rr[existing])
    # rr.previous.new <- rr - rr.existing
    # 
    for (i in 1:n.dates)
    {
        invoice <- from <= dt & to >= dt 
        rr.by.id <- tapply(rr[invoice], list(id[invoice]), sum)
        
        ids <- names(rr.by.id)
        ids.new <- ids[!ids %in% customers]
        ids.resurrection <- ids[ids %in% customers & !ids %in% ids.previous]
        ids.churn <- names(rr.by.id == 0)
        ids.existing <- ids[ids %in% ids.previous]
        
        rr.previous.by.id <- rr.by.id.previous[ids.existing]
        rr.change.by.id <- rr.by.id[ids.existing] - rr.previous.by.id
        
        
        rr.metric.by.id <- list(New = rr.by.id[ids.new],
                                Resurrection = rr.by.id[ids.resurrection],
                                Expansion = rr.change.by.id[rr.change.by.id > 0],
                                Contraction <- rr.change.by.id[rr.change.by.id < 0],
                                Churn = -rr.by.id.previous[ids.churn])
        accounting[, i] <- sapply(rr.metric.by.id, sum)
        counts[, i] <- cnts <- sapply(rr.metric.by.id, length)
        lngth <- sum(cnts)
        rws <- counter + (1:lngth)
        details$Date[rws] <- rep(periods[i], lngth)
        rr.metric.by.id.vector <- unlist(rr.metric.by.id.vector)
        details$Name[rws] <- names(rr.metric.by.id.vector)
        details$Change[rws] <- rr.metric.by.id.vector
        accounting[, i] <- sapply(rr.metric.by.id, sum)
        
        # Incrementing thing that are used again
        counter <- counter + lngth
        rr.by.id.previous <- rr.by.id
    }
    accounting <- addAttributesAndClass(accounting, "GrowthAccounting", by, detail)
    attr(accounting, "counts") <- accounts
    accounting
}

#' @export
plot.GrowthAccounting <- function(x, ...)
{
  smooth <- if (length(x) < 4) "None" else "Friedman's super smoother"
  columnChart(x, 
              fit.type = smooth,
              fit.ignore.last = TRUE,
              y.title = "Net Recurring Revenue Retention", 
              y.tick.format = "%", ...)
}



