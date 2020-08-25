#' removeLastPeriodFromMatrix <- function(x)
#' {
#'     n.rows <- NROW(x)
#'     for (r in n.rows:(n.rows - NCOL(x) + 1))
#'         x[r, n.rows - r + 1] <- NA
#'     x
#' }
#' 
#' #' @importFrom flipTime AsDate
#' removeStartEndLastVector <- function(x, start, end, remove.last)
#' {
#'     x[periodsToKeep(names(x), start, end, remove.last)]
#' }
#' 
#' removeStartEndFromColumnsOfMatrix <- function(x, start, end, remove.last)
#' {
#'     x[, periodsToKeep(colnames(x), start, end, remove.last)]
#' }
#' 
#' removeStartEndFromRowsOfMatrix <- function(x, start, end, remove.last)
#' {
#'     t(removeStartEndFromColumnsOfMatrix(t(x), start, end, remove.last))
#' }
#' 
#' periodsToKeep <- function(nms, start, end, remove.last)
#' {
#'     dts <- AsDate(nms)
#'     keep <- rep(TRUE, length(dts))
#'     names(dts) <- nms
#'     keep[dts < start] <- FALSE
#'     if (missing(end))
#'         keep[dts > end] <- FALSE
#'     if (remove.last)
#'     {
#'         last <- nms[keep][sum(keep)]
#'         keep[nms == last] <- FALSE
#'     }
#'     nms[keep]
#' }
#' 
#' removeStartEndLastSymmetricMatrix <- function(x, start, end, remove.last)
#' {
#'     keep <- periodsToKeep(names(x), start, end, remove.last)
#'     x[keep, keep]
#' }
#' 
#' 
#' #' \code{Triangle}
#' #'
#' #' @description Lower and Upper Triangle  (left and right) part of a matrix.
#' #' @param x A matrix.
#' #' @param position One of \code{"lower left"}, \code{"lower right"},
#' #' \code{"upper left"}, or \code{"upper right"}.
#' #' @param diag Logical. If \code{TRUE}, the diagonal is included in the triangle.
#' #'
#' #' @export
#' Triangle <- function(x, position = "lower right", diag = FALSE)
#' {
#'     switch(position,
#'            "lower left" = lower.tri(x, diag),
#'            "lower right" = lower.tri(x, diag)[, ncol(x):1],
#'            "upper right" = upper.tri(x, diag),
#'            "upper left" = upper.tri(x, diag)[, ncol(x):1])
#' }
#' 
#' #' \code{Diagonal}
#' #'
#' #' @description Extract or replace the diagonal of a matrix, or construct a diagonal matri.
#' #' @param x A \code{matrix} or a number indicating the number of rows in a square matrix.
#' #' @param off \code{logical}. Operates on the off diagonal if selected.
#' #'
#' #' @export
#' Diagonal <- function(x, off = FALSE)
#' {
#'     if (is.matrix(x)){
#'         if (off)
#'             x <- x[,ncol(x):1]
#'         return(diag(x))
#'     }
#'     d <- diag(x)
#'     if (off)
#'         d <- d[,ncol(d):1]
#'     d
#' }
#' 
#' 
#' #' \code{removeLast}
#' #'
#' #' @description Removes the final data period from a \code{RevenueData} object.
#' #' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' #' @importFrom flipTime AsDate
#' #' @export
#' removeLast <- function(data)
#' {
#'     subscription.length <- attr(data, "subscription.length")
#'     end <- attr(data, "end")
#'     period.date <- AsDate(data$from.period, on.parse.failure = "silent")
#'     max.from <- max(period.date)
#'     data <- subset(data, period.date != max.from)
#'     attr(data, "end") <- end
#'     attr(data, "subscription.length") <- subscription.length
#'     data
#' }
#' 
#' #' \code{removeIncompleteSubscriptions}
#' #'
#' #' @description Removes from the data any subscriptions that have yet to be
#' #' completed.
#' #' @param data A \code{data.frame} that has the same variables as a
#' #' \code{RevenueData} object.
#' #' @export
#' removeIncompleteSubscriptions <- function(data)
#' {
#'     subscription.length <- attr(data, "subscription.length")
#'     end <- attr(data, "end")
#'     data <- subset(data, data$to.renewal <= end)
#'     attr(data, "subscription.length") <- subscription.length
#'     attr(data, "end") <- end
#'     data
#' }
