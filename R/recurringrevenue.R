#' 
#' #' @export
#' plot.RecurringRevenue <- function(x, ...)
#' {
#'     title <- switch(attr(x, "subscription.length"),
#'                     week = "Weekly Recurring Revenue",
#'                     month = "Monthly Recurring Revenue",
#'                     quarter = "Quarterly Recurring Revenue",
#'                     year = "Annual Recurring Revenue")
#'     areaChart(x, y.title = title, ...)
#' }

#' #' \code{RecurringRevenue}
#' #'
#' #' @description Computes recurring revenue at the end of the period (i.e., as of
#' #' the instant before the next time periods starts). Where the time period isn't ocmplete, 
#' #' the recurring revenue is calculated up until the instant before attr(data, "end").
#' #' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' #' @details For exmaple, if with yearly data, if a customer has an annual license for $1,000
#' #' from 1 February to 1 February of the next year, and a second license which starts on 1 January of the last year
#' #' with a prorated price of $1000/12, then the recurring revenue is $2,000.
#' #' @return A vector showing the recurring revenue by time points.
#' #' @importFrom lubridate as_datetime
#' #' @export
#' RecurringRevenue <- function(data)
#' {
#'     calculateRecurringRevenueOrCustomers(data, recurring.revenue = TRUE)
#' 
#' }

# calculateRecurringRevenueOrCustomers <- function(data, recurring.revenue)
# {
#     # r <- calculate(data, components = "current", volume = recurring.revenue, use = "aggregate")
#     # r
#     dts <- attr(data, "by.sequence")
#     n <- length(dts)
#     rr <- data$recurring.value
#     out <- setNames(rep(0, n), c(attr(data, "previous.period"), attr(data, "by.period.sequence")[-n]))
#     for (i in 1:n)
#     {
# #        dt #<- as_datetime(dts[i]) - 0.00001
#         #f <- from <= a.dt & a.dt <= to
#         f <- customerAtPeriodEnd(data, dts[i])
#         out[i] <- if (recurring.revenue)
#             sum(rr[f])
#         else
#             nUnique(data$id[rr > 0 & f])
# 
#     }
#     detail <- data[data$from >= attr(data, "start") & data$from <= attr(data, "end"),
#                    c("id", "value", "recurring.value","from", "to")]
#     colnames(detail) <- c("Name", "Revenue", "Recurring Revenue", "From", "To")
#     out <- addAttributesAndClass(out, if (recurring.revenue) "RecurringRevenue" else "Customers", by, detail)
#     attr(out, "subscription.length") <- attr(data, "subscription.length")
#     out
# }

