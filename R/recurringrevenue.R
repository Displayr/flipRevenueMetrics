#' \code{RecurringRevenue}
#'
#' @description Computes recurring revenue, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time period to aggregate the dates by: 
#' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' and \code{"day"}.
#' @param ... Additional arguments to be passed to lower level functions.
#' @details Computed based on being a subscribed on the last second of the time period. 
#' Partial revenue is multipled out. For exmaple, if with yearly data, if a customer has an annual license for $1,000
#' from 1 February to 1 February of the next year, and a second license which starts on 1 January of the last year
#' with a prorated price of $1000/12, then the recurring revenue is $2,000.
#' @return A vector showing the recurring revenue by time points.
#'
#' @export
RecurringRevenue <- function(data, by = "day", ...)
{
    end <- attr(data, "end")
    x <- Subscribers(data, end = end, by = by, volume = TRUE, recurring = TRUE)
    keep <- periodsToKeep(names(x), attr(data, "start"), attr(data, "end"), FALSE)
    x <- x[keep]
    detail <- data[data$from >= attr(data, "start") & data$from <= attr(data, "end"),
                   c("id", "value", "recurring.value","from", "to")]
    colnames(detail) <- c("Name", "Revenue", "Recurring Revenue", "From", "To")
    out <- addAttributesAndClass(x, "RecurringRevenue", by, detail)
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
