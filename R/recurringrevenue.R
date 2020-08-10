#' \code{RecurringRevenue}
#'
#' @description Computes recurring revenue at the end of the period (i.e., as of
#' the instant before the next time periods starts). Where the time period isn't ocmplete, 
#' the recurring revenue is calculated up until the instant before attr(data, "end").
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @details For exmaple, if with yearly data, if a customer has an annual license for $1,000
#' from 1 February to 1 February of the next year, and a second license which starts on 1 January of the last year
#' with a prorated price of $1000/12, then the recurring revenue is $2,000.
#' @return A vector showing the recurring revenue by time points.
#' @importFrom lubridate as_datetime
#' @export
RecurringRevenue <- function(data)
{
    a.dts <- attr(data, "by.sequence")
    n <- length(a.dts)
    rr <- data$recurring.value
    from <- data$from
    to <- data$to
    out <- rep(0, n)
    names(out) <- c(attr(data, "previous.period"),
                    attr(data, "by.period.sequence")[-n])
    for (i in 1:n)
    {
        a.dt <- as_datetime(a.dts[i]) - 0.00001
        out[i] <- sum(rr[from <= a.dt & a.dt <= to])
    }
    detail <- data[from >= attr(data, "start") & from <= attr(data, "end"),
                   c("id", "value", "recurring.value","from", "to")]
    colnames(detail) <- c("Name", "Revenue", "Recurring Revenue", "From", "To")
    out <- addAttributesAndClass(out, "RecurringRevenue", by, detail)
    attr(out, "subscription.length") <- attr(data, "subscription.length")
    out
}



#' @export
plot.RecurringRevenue <- function(x, ...)
{
    title <- switch(attr(x, "subscription.length"),
                    week = "Weekly Recurring Revenue",
                    month = "Monthly Recurring Revenue",
                    quarter = "Quarterly Recurring Revenue",
                    year = "Annual Recurring Revenue")
    areaChart(x, y.title = title, ...)
}
