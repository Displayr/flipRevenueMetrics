#' #' \code{NetRecurringRevenueRetention}
#' #'
#' #' @description Computes recurring revenue, by cohort.
#' #' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' #' @param by The time period used in defining the cohorts: "year", "quarter", "month", "week", day".
#' #' @param ... Additional arguments to be passed to lower level functions.
#' #' @return A matrix
#' #' @importFrom flipTime AsDate Period
#' #' @importFrom flipStatistics Table
#' #' @importFrom lubridate floor_date years
#' #' @export
#' NetRecurringRevenueRetention <- function(data, by = "year", ...)
#' {
#'     
#'     start <- floor_date(attr(data, "start"), by)
#'     end <- floor_date(attr(data, "end"), by)
#'     dts <- seq.Date(start, end, by)
#'     n <- length(dts)
#'     revenue.retention <- rep(NA, n)
#'     names(revenue.retention) <- Period(dts, by)
#'     from <- data$from
#'     to <- data$to
#'     id <- data$id
#'     rr <- data$recurring.value
#'     for (i in seq_along(dts))
#'     {
#'         a.dt <- dts[i]
#'         a.dt.year.ago <- a.dt - years(1)
#'         invoice.year.ago <- from <= a.dt.year.ago & to >= a.dt.year.ago
#'         ids.year.ago <- unique(id[invoice.year.ago])
#'         invoice.this.year <- from <= a.dt & to >= a.dt & id %in% ids.year.ago
#'         #print(c(sum(rr[invoice.this.year]),sum(rr[invoice.year.ago])))
#'         #print(a.dt)
#'         revenue.retention[i] <- sum(rr[invoice.this.year]) / sum(rr[invoice.year.ago]) - 1
#'     }
#'     detail <- data[, c("from", "to", "id", "recurring.value")]
#'     detail <- detail$from >= attr(data, "start") & detail$from <= attr(data, "end")
#'     revenue.retention <- addAttributesAndClass(revenue.retention, "NetRecurringRevenueRetention", by, detail)
#'     revenue.retention
#' }
#' 
#' #' @export
#' plot.NetRecurringRevenueRetention <- function(x, ...)
#' {
#'     smooth <- if (length(x) < 4) "None" else "Friedman's super smoother"
#'     columnChart(x, 
#'                 fit.type = smooth,
#'                 fit.ignore.last = TRUE,
#'                 y.title = "Net Recurring Revenue Retention", 
#'                 y.tick.format = "%", ...)
#' }
#' 
