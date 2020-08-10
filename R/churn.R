#' \code{CustomerChurn}
#' 
#' The percentage of customers who could churn in a period that did churn.
#' Note that this is not the most widely used definition. A more common definition
#' is the proportion of customers that churned. These two measures differ in some  situations:
#' 1. Multi-year contracts.
#' 2. Incomplete periods (e.g., the conventional metric can't be used in a quarter until it
#' has been completed.
#' 3. Where a customer joins and churns withhin a single period (ignored in the traditional definition)
#' @param data A \code{MetricData} object.
#' @return A named vector showing churn.
#' @importFrom flipTime Period AsDate
#' @export
CustomerChurn <- function(data)
{
    calculateChurn(data, volume = FALSE)
}

calculateChurn <- function(data, volume = FALSE)
{
    r <- calculate(data, initial.only = FALSE, by.period = FALSE, volume = volume)
    subscription.length <- attr(data, "subscription.length")
    by <- attr(data, "by")
    churn <- calcChurn(r, subscription.length, by)
    detail <- attr(r, "detail")
    if (!volume)
        detail <- tidyingDetailForCustomerChurn(detail, subscription.length, by)
    out <- addAttributesAndClass(churn, "Churn", by, detail)
    attr(out, "numerator") <- attr(r, "numerator")[, 1]
    attr(out, "denominator") <- attr(r, "denominator")[, 1]
    attr(out, "volume") <- FALSE
    out
}    

calcChurn <- function(r, subscription.length, by)
{
    churn <- 1 - r[, 1]
    # names(churn) <- addSubscriptionLengthToName(names(churn), 
    #                                             subscription.length, 
    #                                             by)
    churn    
}    

tidyingDetailForCustomerChurn <- function(detail, subscription.length, by)
{
    #detail <- detail[, -2]
    colnames(detail)[1] <- "Period"
    #detail$Period <- addSubscriptionLengthToName(detail$Period, subscription.length, by)
    detail
}

addSubscriptionLengthToName <- function(x, subscription.length, by)
{
    Period(AsDate(x) + Periods(1, subscription.length), by)
}    

#' \code{RecurringRevenueChurn}
#' 
#' Lost revenue duee to churned customers as a percentage of total recurring revenue.
#' @param data A \code{RevenuMetric} object.
#' @return A named vector showing churn, a plot, or an object showing detail.
#' @importFrom flipTime Period
#' @export
RecurringRevenueChurn <- function(data)
{
    calculateChurn(data, volume = TRUE)
    # print(r)
    # dts <- attr(data, "by.sequence")
    # n <- length(dts)
    # dts <- c(dts[-n], attr(data, "end"))
    # id <- data$id
    # to <- data$to
    # from <- data$from
    # rr <- data$recurring.value
    # detail <- vector("list", n - 1)
    # rr.previous <- vector("numeric", n - 1)
    # names(rr.previous) <- names(detail) <- Period(dts[-n], attr(data, ("by")))
    # rr.churned <- rr.previous
    # for (i in 2:n)
    # {
    #     prev.dt <- dts[i - 1]
    #     dt <- dts[i]
    #     # Customers on previous date (and it wasn't their final date)
    #     previous <- prev.dt >= from  & prev.dt < to 
    #     id.previous <- unique(id[previous])
    #     rr.previous[i - 1] <- sum(rr[previous])
    #     
    #     current <- dt >= from  & dt < to 
    #     id.churned <- setdiff(id.previous, id[current])
    #     m <- previous & id %in% id.churned
    #     rr.by.id <- tapply(rr[m], list(id[m]), sum)
    #     
    #     rr.churned[i - 1] <- sum(rr.by.id)
    #     detail[[i]] <- rr.by.id
    # }
    # churn <- rr.churned / rr.previous
    # out <- addAttributesAndClass(churn, "Churn", by, detail)
    # attr(out, "volume") <- TRUE
    # out
}


#' @export
plot.Churn <- function(x, ...)
{
    smooth <- if (length(x) < 4) "None" else "Friedman's super smoother"
    y.title <- if (attr(x, "volume")) "Recurring Revenue Churn Rate" else "Customer Churn Rate"
    columnChart(x, 
                fit.type = smooth,
                fit.ignore.last = TRUE,
                y.title = y.title, 
                y.tick.format = "%", ...)
}

#' #' \code{Churn}
#' #'
#' #' @description Computes retention, by cohort.
#' #' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' #' @param volume Weights the results by volume.
#' #' @param by The time period to aggregate the dates by: 
#' #' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' #' and \code{"day"}.
#' #' @param error.if.no.data If TRUE and the data contains no valid cases, an error is thrown.
#' #' @details Where subscribers suspends their purchasing for a period, 
#' #' but purchases again later, the subscriber
#' #' is included in the churn. Churn is show for all periods that have 
#' #' data in \code{data$to.period}, even if they occur in the future.
#' #' @return A named vector showing churn.
#' #' @importFrom flipTime AsDate Period
#' #' @export
#' Churn <- function(data, volume = FALSE, by = "quarter", error.if.no.data = FALSE)
#' {
#'     data <- prepareDataForChurn(data, by)
#'     #data <- data[data$from >= attr(data, "start"), ]
#'     if (nrow(data) == 0)
#'     {
#'         if (error.if.no.data)
#'             stop("No subscriptions have had an opportunity to churn.")
#'         return(NULL)
#'     }
#'     counts <- churnCountsByTime(data, volume)
#'     out <-  prop.table(counts, 2)[2, ]
#'     if (length(out) == 1) # Dealing with scalars losing names
#'         names(out) <- colnames(counts)
#'     to <- as.Date(data$to)
#'     dat <- data[data$churn & to >= attr(data, "start") & to <= attr(data, "end"),, drop = FALSE]
#'     detail <- idByPeriod(dat, time = "to.renewal.period")#sapply(id, paste, collapse = ",")
#'     out <- addAttributesAndClass(out, "Churn", by, detail)
#'     attr(out, "volume") <- volume
#'     out
#' }
#' 
#' 
#' churnCountsByTime <- function(data, volume)
#' {
#'     counts <- if (volume) {
#'         v <- Table(recurring.value ~ churn + to.renewal.period, data = data, FUN = sum)
#'         v[round(v, 7) == 0] <- 0 # Dealing with numeric precision issue that can lead to negative churn
#'         v
#'     }
#'     else Table(id ~ churn + to.renewal.period, data = data, FUN = nUnique)
#'     if (nrow(counts) == 1)
#'     {
#'         counts <-if (rownames(counts) == "TRUE")
#'             rbind(counts, "FALSE" = 0)[2:1, , drop = FALSE]
#'         else
#'             rbind(counts, "TRUE" = 0)
#'     }
#'     #print(counts)
#'     counts
#' }
#' 
