#' \code{Subscribers}
#'
#' @description Computes acquisition, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param end The date on and after which which revenue is ignored.
#' Note that the default value, \code{Sys.time()} may not be in the same time zone as your other data, and this
#' can cause unexpected results.
#' @param by The time period to aggregate the dates by: 
#' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' and \code{"day"}.
#' @param volume The number of subscribers in terms of their value.
#' @param recurring If TRUE, and \code{volume} is also TRUE, computes the recurring revenue.
#' @details Computed based on being a subscribed on the last second of the time period.
#' @return A vector showing number of subscribers over time.
#'
#' @importFrom lubridate '%within%' floor_date ceiling_date days
#' @importFrom flipTime Periods Period
#' @export
Subscribers <- function(data, end = Sys.Date(), by = "month", volume = FALSE, recurring = FALSE)
{
    true.end <- end
    end <- ceiling_date(end, unit = by, change_on_boundary = FALSE)
    start <- ceiling_date(attr(data, "start"), by, change_on_boundary = FALSE) 
    n <- interval(start, end) %/% Periods(1, by) + 1
    result <- rep(NA, n)
    starts <- start + Periods(0:(n - 1), by) - days(1) # Last day of the time period
    starts <- ensureDatesArentInFuture(starts, true.end)    
    names(result) <- as.character(starts)
    count <- 0
    value <- if (recurring) data$recurring.value else data$value
#    prev <- c()
    for (i in 1:n)
    {
        start <- starts[i]
        filt <- start >= data$from & start < data$to
# print(start)
# ids <-  unique(data$id[filt])       
# print("Organization 140" %in% ids)
# print(setdiff(ids,prev))
# 
# prev <- ids
        result[i] <- if(volume) sum(value[filt]) else nUnique(data$id[filt])
    }
    attr(result, "by") = by
    result
}
