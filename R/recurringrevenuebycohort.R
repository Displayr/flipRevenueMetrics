#' #' \code{RecurringRevenueByCohort}
#' #'
#' #' @description Revenue by year and start year, as a stacked revenue chart.
#' #' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' #' @param cohort.by The time period used in defining the cohorts: "
#' #' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' #' and \code{"day"}.
#' #' @param by The time period to aggregate the dates by: 
#' #' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' #' and \code{"day"}.
#' #' @param remove.last Remove the final period (this is useful if it is likely incomplete.).
#' #' @param ... Additional arguments to be passed to lower level functions.
#' #' @return A table.
#' #' @importFrom flipStandardCharts Area
#' #' @importFrom flipStatistics Table
#' #' @importFrom scales col_numeric
#' #' @export
#' RecurringRevenueByCohort <- function(data, cohort.by = "year", by = "year", remove.last = TRUE, ...)
#' {
#'     data <- setCohortPeriod(data, cohort.by)
#'     data <- setPeriodCounter(data, by)
#'     table <- Table(recurring.value ~ cohort + period.counter, data = data, FUN = sum)
#'     table <- tidyCohortTable(table, cohort.by, by, attr(data, "start"), attr(data, "end"), remove.last, 0)
#'     detail <- data[data$from >= attr(data, "start") & data$from <= attr(data, "end"),
#'                    c("cohort", "period.counter", "id", "value", "recurring.value")]
#'     colnames(detail) <- c(names(dimnames(table)), "Name", "Value", "Recurring Value")
#'     table <- addAttributesAndClass(table, "RecurringRevenueByCohort", by, detail)
#'     attr(table, "date.format") <- switch(attr(data, "subscription.length"),
#'                                          "year" = "%Y", "%b %Y")
#'     table
#' }
#' 
#' 
#' #' @export
#' plot.RecurringRevenueByCohort <- function(x, ...)
#' {
#'     k <- NROW(x)
#'     if (NCOL(x) <= 1)
#'         return(NULL)
#'     Area(t(x), type = "Stacked Area",
#'           y.title = "Revenue",
#'           x.tick.format = attr(table, "date.format"),
#'           colors = col_numeric("Blues", domain = NULL)(1:(k + 3))[-1:-3],
#'           legend.ascending = FALSE)$htmlwidget
#' }