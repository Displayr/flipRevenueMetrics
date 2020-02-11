#' \code{MeanRecurringRevenueByCohort}
#'
#' @description Computes recurring revenue, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param days.to.count The number of days after the initial commencing to include in the Recurring Revenue
#' calculation. Defaults to 0 (i.e., so only the initial period is counted)
#' @param by The time period to aggregate the dates by: 
#' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' and \code{"day"}.
#' @param ... Additional arguments to be passed to lower level functions.
#' @return A matrix
#' @importFrom flipTime AsDate Period
#' @importFrom flipStatistics Table
#' @export
MeanRecurringRevenue <- function(data, days.to.count = 0, by, ...)
{
    start <- attr(data, "start")
    end <- attr(data, "end") - days.to.count
    data <- data[data$from <= data$subscriber.from + days.to.count, ]
    data$subscriber.from.period <- Period(data$subscriber.from, by)
    counts <- Table(id ~ subscriber.from.period, data = data, FUN = nUnique)
    value <- Table(recurring.value ~ subscriber.from.period, data = data, FUN = sum)
    out <- value / counts
    dates <- AsDate(names(out))
    out <- out[dates >= start & dates <= end]
    detail <- data[, c("subscriber.from", "from", "id", "recurring.value")]
    out <- addAttributesAndClass(out, "MeanRecurringRevenue", by, detail)
    attr(out, "days.to.count") <- days.to.count
    out
}

#' @export
plot.MeanRecurringRevenue <- function(x, ...)
{
    days <- attr(x, "days.to.count")
    y.title <- "Initial recurring revenue per customer"
    if (days > 0)
    {
        time <- if (days <= 360) paste(days, "days") else  paste(round(days/365.25), "years")
        y.title <- paste0("Recurring revenue per new customer (first ", time, ")")
    }
    smooth <- if (length(x) < 4) "None" else "Friedman's super smoother"
    columnChart(x, y.title = y.title, fit.type = smooth, y.tick.format = "$")
}
