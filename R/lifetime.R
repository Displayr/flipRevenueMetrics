#' #' \code{Lifetime}
#' #'
#' #' @description Calculations of the lifetime revenue and length of a customers' relationships.
#' #' @param data A \code{RevenueData} object.
#' #' @param end The date at which the last observation in the data as
#' #' @return A \code{list} containing the following elements:
#' #' \item{total}{The total value of transactions, by \code{subscriber.from.period} and \code{period.counter}.}
#' #' \item{subscribers}{The number of subscribers by cohort.}
#' #' \item{revenue.per.subscriber}{The revenue per subsciber.}
#' #' \item{mean}{The average value of transactions, where the base is the number of subscribers
#' #' in the \code{subscriber.from.period}, by \code{subscriber.from.period} and \code{period.counter}.}
#' #' \item{cumulative}{The cumulative means.}
#' #' \item{index}{The \code{cumulative} means divided by the mean from the first period.}
#' 
#' #'
#' #' @importFrom flipStatistics Table
#' #' @importFrom flipTime CompleteListPeriodNames 
#' #' @export
#' Lifetime <- function(data, end = attr(data, "end"))
#' {
#'     subscription.length <- attr(data, "subscription.length")
#'     ns <- Table(id ~ subscriber.from.period, data = data, FUN = nUnique)
#'     end.numeric <- as.numeric(as.Date(end))
#'     data$to.as.numeric <- as.numeric(as.Date(data$to))
#'     data <- subset(data, data$from >= attr(data, "start"))
#'    # incomplete <- Table(to.as.numeric ~ subscriber.from.period + period.counter, data, FUN = max) < end.numeric
#'     total <- Table(recurring.value ~ subscriber.from.period + period.counter, data, sum)
#'     counts <- Table(id ~ subscriber.from.period + period.counter, data, nUnique)
#'     # Filling in missing row and column totals
#'     row.names <- CompleteListPeriodNames(rownames(total), subscription.length)
#'     col.names <- 0:max(length(row.names) - 1, as.numeric(colnames(total)))
#'     total <- FillInMatrix(total, row.names, col.names, 0)
#'     counts <- FillInMatrix(counts, row.names, col.names, 0)
#'     ns <- FillInVector(ns, row.names, 0)
#'     total[Triangle(total, position = "lower right")] <- NA
#'     counts[Triangle(total, position = "lower right")] <- NA
#'     names(dimnames(total)) <- c("Customer since", paste0(properCase(subscription.length), "s since commencing"))
#'     value <- sweep(total, 1, ns, "/")
#'     di <- Diagonal(value, off = TRUE)
#'     names(di) <- rownames(value)
#'     index <- Index(value, STATS = value[, 1], remove = "lower right", remove.diag = FALSE)
#'     cumulative <- t(apply(value, 1, cumsum))
#'     churn <- 1 - Retention(data, subscription.length)$retention.rate.volume.by.period
#'     churn <- churn[match(names(di), names(churn))]
#'     future.revenue <- di / churn
#'     future.revenue[!is.finite(future.revenue)] <- NA
#'     lifetime.revenue <- Diagonal(cumulative, off = TRUE) + future.revenue
#'     lifetime.revenue.per.customer <- sum(lifetime.revenue * prop.table(ns), na.rm = TRUE)
#'     revenue.per.subscriber <- total / counts
#'     revenue.per.subscriber[total == 0] <- NaN
#'     total[is.na(total)] <- 0
#'     result <- list(total = total,
#'                    subscribers = counts,
#'                    revenue.per.subscriber = revenue.per.subscriber, 
#'                    mean = value,
#'                    cumulative = cumulative,
#'                    index = index,
#'                    lifetime.revenue = lifetime.revenue,
#'                    lifetime.revenue.per.customer = lifetime.revenue.per.customer)
#'     class(result) <- c("LifetimeValue", class(result))
#'     result
#' }
#' 
