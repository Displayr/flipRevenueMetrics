# Need to maybe move to a grid or something
#' #' \code{MeanRecurringRevenueByCohort}
#' #'
#' #' @description Computes recurring revenue, by cohort.
#' #' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' #' @param days.to.count The number of days after the initial commencing to include in the Recurring Revenue
#' #' calculation. Defaults to 0 (i.e., so only the initial period is counted)
#' #' @param cohort.by The time period used in defining the cohorts: "
#' #' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' #' and \code{"day"}.
#' #' @param by The time period to aggregate the dates by: 
#' #' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' #' and \code{"day"}.
#' #' @param remove.last Remove the final period (this is useful if it is likely incomplete.).
#' #' @param ... Additional arguments to be passed to lower level functions.
#' #' @return A matrix
#' #' @importFrom flipTime AsDate Period
#' #' @importFrom flipStatistics Table
#' #' @export
#' MeanRecurringRevenueByCohort <- function(data, days.to.count = 0, cohort.by = "year", by = "quarter", remove.last = FALSE, ...)
#' {
#'     data = data
#'     data = data
#'     data <- setCohortPeriod(data, cohort.by)
#'     data <- setPeriodCounter(data, by)
#'     end <- attr(data, "end") - days.to.count
#'     data <- data[data$from <= data$subscriber.from + days.to.count, ]
#'     n.subscribers <- Table(id ~ cohort + period.counter, data = data, FUN = nUnique)
#'     value <- Table(recurring.value ~ cohort + period.counter, data = data, FUN = sum)
#'     mn <- value / n.subscribers
#'     mn <- tidyCohortTable(mn, cohort.by, by, attr(data, "start"), attr(data, "end"), remove.last, NaN)
#'     detail <- data[, c("cohort", "period.counter", "id", "value", "recurring.value")]
#'     colnames(detail) <- c(names(dimnames(table)), "Name", "Value", "Recurring Value")
#'     mn <- addAttributesAndClass(mn, "MeanRecurringRevenueByCohort", by, detail)
#'     attr(mn, "cohort.by") = cohort.by
#'     attr(mn, "n.subscribers") = n.subscribers
#'     attr(mn, "subscription.length") = attr(data, "subscription.length")
#'     mn
#' }
#' 
#' 
#' #' Plot Mean Recurring Revenue by Cohort
#' #' 
#' #' @importFrom plotly plot_ly layout `%>%`
#' #' @importFrom flipFormat  FormatAsReal
#' #' @export
#' plot.MeanRecurringRevenueByCohort <- function(x, ...)
#' {
#'     series.hover <- paste0("Mean Recurring Revenue: $", FormatAsReal(x, decimals = 0))
#'     cohortHeatmap(x, series.hover, ...)
#'         
#' }
