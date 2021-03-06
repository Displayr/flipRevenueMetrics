#' #' \code{NewCustomers}
#' #'
#' #' @description Computes NewCustomers, by cohort.
#' #' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' #' @param by The time period to aggregate the dates by: 
#' #' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' #' and \code{"day"}.
#' #' @param ... Additional arguments to be passed to lower level functions.
#' #' @return A \code{\link{list}} containing the following elements:
#' #'   \item{id}{The \code{id} values of subscribers to churn.}
#' #'   \item{base}{The number of subscribers to renew or churn in the time period.}
#' #'   \item{counts}{Number of NewCustomerss by period (or in $ if \code{volume} is \code{TRUE}}.
#' #'   \item{rates}{The percentage to churn (weighted if the counts are weighted)}.
#' #'
#' #' @importFrom flipStatistics Table
#' #' @importFrom flipTime Period
#' NewCustomers <- function(data, by = "quarter", ...)
#' {
#'     data$subscriber.from.period <- Period(data$subscriber.from, by)
#'     x <- quantityByTime(data, FALSE, "subscriber.from.period", by)
#'     detail <- idByPeriod(data, "subscriber.from.period")#data[, "subscriber.from", "id"]#sapply(id, paste, collapse = ", ")
#'     addAttributesAndClass(x, "NewCustomers", by, detail)
#' }
#' 
#' print.NewCustomers <- function(x, ...)
#' {
#'     printWithoutAttributes(x, ...)
#' }
#' 
#' 
#' 
#' idByPeriod <- function(data, time)
#' {
#'     data$time <- data[, time]
#'     if (nrow(data) == 0)
#'         return(NULL)
#'     idag <- aggregate(id ~ time, data = data, FUN = unique)
#'     id <- idag[, 2]
#'     names(id) <- idag[, 1]
#'     id
#' }   
#' 
#' 
#' #' @export
#' plot.NewCustomers <- function(x, ...)
#' {
#'     smooth <- if (length(x) < 4) "None" else "Friedman's super smoother"
#'     columnChart(x, fit.type = smooth, y.title = "New customers", ...)
#' }
