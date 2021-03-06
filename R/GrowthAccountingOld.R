#' #' \code{GrowthAccountingOld}
#' #'
#' #' @description Computes statistics for use in growth accounting of a startup.
#' #' @param data A \code{data.frame} that has variables: \code{id}, \code{period},
#' #' and \code{value} (e.g., a \code{RevenueData} object).
#' #' @param volume Weights the results by volume; doesn't currently work.
#' #' @param remove.last Remove the final period (as usually is incomplete).
#' #' @param tol The tolerance used in calculations of differences. This defaults to 1 (e.g., $1).
#' #' Values or differences less than this amount are treated as being equivalent. E.g., if revenue for
#' #' an entity this period is less than \code{tol} higher than in the previous period, it is treated as being constant.
#' #' @param ... Additional arguments to be passed to lower level functions.
#' #' @details Small differences in the percentages shown here versus those computed by other means may occur, due
#' #' to: (1) how churn is defined (e.g., the point at time when a customer churns, vs, takes a hieateous); (2) the precision
#' #' used to determine whether a subscriber is considered to have contracted/expanded or not.
#' #' @importFrom flipStatistics Table
#' #' @importFrom stats aggregate xtabs
#' #' @importFrom flipTime AsDate
#' #' @importFrom methods is
#' #' @export
#' GrowthAccountingOld <- function(data, start, end, remove.last = TRUE, tol = 1, ...)
#' {
#'     if (volume)
#'         stop("'volume' not currently supported")
#'     period.date <- AsDate(data$from.revenue.period, on.parse.failure = "silent")
#'     if (remove.last)
#'         data <- removeLast(data)
#'     id <- data$id
#'     period <- data$from.period
#'     value <- data$value
#'     id.by.period <- paste(id, period)
#'     aggregated <- aggregate(value ~ id.by.period, FUN = sum)
#'     ids <- sort(unique(id))
#'     periods <- sort(unique(period))
#'     n.periods <- length(periods)
#'     n.id <- length(ids)
#'     id.by.period <- data.frame(id = rep(ids, rep(n.periods + 1, n.id)))
#'     id.by.period$id.by.period <-  paste(rep(ids, rep(n.periods + 1, n.id)),
#'                                         c(0, periods))
#'     data <- merge(as.data.frame(id.by.period), as.data.frame(aggregated),
#'                   by = "id.by.period", all.x = TRUE, sort = TRUE)
#'     data$value[is.na(data$value)] <- 0
#'     data$diff <- c(0, data$value[-1] - data$value[-length(data$value)])
#'     data$cum <- cumsum(data$value)
#'     data$cum <- data$cum - rep(data$cum[c(TRUE, rep(FALSE, n.periods))],
#'                                rep(n.periods + 1, n.id))
#'     #print(data[, c("id", "value", "diff", "cum")])
#'     data <- data[c(FALSE, rep(TRUE, n.periods)), ]
#'     data$period <- periods
#'     data$status = "Unchanged"
#'     data$status[data$diff >= tol] <- "Expansion"
#'     data$status[data$value > tol & abs(data$diff - data$cum) < tol] <- "Acquisition"
#'     data$status[abs(data$value - data$diff) < tol & data$cum >= data$value + tol] <- "Resurrected" #+1 is to take numeric precision of cumsum into account.
#'     data$status[data$diff <= -tol] <- "Contraction"
#'     data$status[abs(data$value) < tol & data$diff <= -tol] <- "Churned"
#'     data$status[abs(data$diff) < tol] <- "Unchanged"
#'     tab <- aggregate(diff ~ status + period, data = data, sum)
#'     tab$period.by.status <- paste(tab$period,tab$status)
#'     stati <- c("Acquisition", "Expansion", "Resurrected", "Contraction", "Churned", "Unchanged")
#'     full.tab <- data.frame(period = rep(periods, rep(6, n.periods)), status = stati)
#'     full.tab$period.by.status <-  paste(rep(periods, rep(6, n.periods)), stati)
#'     tab <- merge(full.tab, tab[,-1:-2], by = "period.by.status", all.x = TRUE, sort = TRUE)
#'     diff <- tab$diff
#'     diff[is.na(diff)] <- 0
#'     tab <- as.data.frame(matrix(diff, nrow = n.periods,byrow = TRUE, dimnames = list(periods, sort(stati))))
#'     #print(tab)
#'     tab$Quick.Ratio <- -apply(tab[, -1:-2], 1, sum) / apply(tab[, 1:2], 1, sum)
#'     results <- list(n.id = n.id, n.periods = n.periods, periods = periods, data = data[, -1], Table = tab,
#'                     Revenue = aggregateAsVector(aggregate(value ~ period, data = data, FUN = sum)),
#'                     Growth = aggregateAsVector(aggregate(diff ~ status, data = data, FUN = sum)))
#'     class(results) <- append(class(results), "GrowthAccountingOld")
#'     results
#' }
#' 
#' #' \code{GrowthAccountingOld}
#' #'
#' #' @description Computes statistics for use in growth accounting of a startup.
#' #' @param data A \code{data.frame} that has variables: \code{id}, \code{period},
#' #' and \code{value} (e.g., a \code{RevenueData} object).
#' #' @param volume Weights the results by volume; doesn't currently work.
#' #' @param remove.last Remove the final period (as usually is incomplete).
#' #' @param tol The tolerance used in calculations of differences. This defaults to 1 (e.g., $1).
#' #' Values or differences less than this amount are treated as being equivalent. E.g., if revenue for
#' #' an entity this period is less than \code{tol} higher than in the previous period, it is treated as being constant.
#' #' @param ... Additional arguments to be passed to lower level functions.
#' #' @details Small differences in the percentages shown here versus those computed by other means may occur, due
#' #' to: (1) how churn is defined (e.g., the point at time when a customer churns, vs, takes a hieateous); (2) the precision
#' #' used to determine whether a subscriber is considered to have contracted/expanded or not.
#' #' @importFrom flipStatistics Table
#' #' @importFrom stats aggregate xtabs
#' #' @importFrom flipTime AsDate
#' #' @importFrom methods is
#' #' @export
#' GrowthAccountingOld <- function(data, volume = FALSE, remove.last = TRUE, tol = 1, ...)
#' {
#'     if (volume)
#'         stop("'volume' not currently supported")
#'     period.date <- AsDate(data$from.revenue.period, on.parse.failure = "silent")
#'     if (remove.last)
#'         data <- removeLast(data)
#'     id <- data$id
#'     period <- data$from.period
#'     value <- data$value
#'     id.by.period <- paste(id, period)
#'     aggregated <- aggregate(value ~ id.by.period, FUN = sum)
#'     ids <- sort(unique(id))
#'     periods <- sort(unique(period))
#'     n.periods <- length(periods)
#'     n.id <- length(ids)
#'     id.by.period <- data.frame(id = rep(ids, rep(n.periods + 1, n.id)))
#'     id.by.period$id.by.period <-  paste(rep(ids, rep(n.periods + 1, n.id)),
#'                                         c(0, periods))
#'     data <- merge(as.data.frame(id.by.period), as.data.frame(aggregated),
#'                   by = "id.by.period", all.x = TRUE, sort = TRUE)
#'     data$value[is.na(data$value)] <- 0
#'     data$diff <- c(0, data$value[-1] - data$value[-length(data$value)])
#'     data$cum <- cumsum(data$value)
#'     data$cum <- data$cum - rep(data$cum[c(TRUE, rep(FALSE, n.periods))],
#'                                rep(n.periods + 1, n.id))
#'     #print(data[, c("id", "value", "diff", "cum")])
#'     data <- data[c(FALSE, rep(TRUE, n.periods)), ]
#'     data$period <- periods
#'     data$status = "Unchanged"
#'     data$status[data$diff >= tol] <- "Expansion"
#'     data$status[data$value > tol & abs(data$diff - data$cum) < tol] <- "Acquisition"
#'     data$status[abs(data$value - data$diff) < tol & data$cum >= data$value + tol] <- "Resurrected" #+1 is to take numeric precision of cumsum into account.
#'     data$status[data$diff <= -tol] <- "Contraction"
#'     data$status[abs(data$value) < tol & data$diff <= -tol] <- "Churned"
#'     data$status[abs(data$diff) < tol] <- "Unchanged"
#'     tab <- aggregate(diff ~ status + period, data = data, sum)
#'     tab$period.by.status <- paste(tab$period,tab$status)
#'     stati <- c("Acquisition", "Expansion", "Resurrected", "Contraction", "Churned", "Unchanged")
#'     full.tab <- data.frame(period = rep(periods, rep(6, n.periods)), status = stati)
#'     full.tab$period.by.status <-  paste(rep(periods, rep(6, n.periods)), stati)
#'     tab <- merge(full.tab, tab[,-1:-2], by = "period.by.status", all.x = TRUE, sort = TRUE)
#'     diff <- tab$diff
#'     diff[is.na(diff)] <- 0
#'     tab <- as.data.frame(matrix(diff, nrow = n.periods,byrow = TRUE, dimnames = list(periods, sort(stati))))
#'     #print(tab)
#'     tab$Quick.Ratio <- -apply(tab[, -1:-2], 1, sum) / apply(tab[, 1:2], 1, sum)
#'     results <- list(n.id = n.id, n.periods = n.periods, periods = periods, data = data[, -1], Table = tab,
#'                     Revenue = aggregateAsVector(aggregate(value ~ period, data = data, FUN = sum)),
#'                     Growth = aggregateAsVector(aggregate(diff ~ status, data = data, FUN = sum)))
#'     class(results) <- append(class(results), "GrowthAccountingOld")
#'     results
#' }
#' 
#' 
#' #' @export
#' print.GrowthAccountingOld <- function(x, ...)
#' {
#'     cat('Growth Accounting calculations\n\n')
#'     cat(paste0('\nNumber of entities: ', x$n.id, '\n'))
#'     cat('\nRevenue\n')
#'     print(x$Revenue)
#'     cat('\nRevenue Growth\n')
#'     pre <- x$Revenue[-x$n.periods]
#'     revenue.growth <- round((x$Revenue[-1] - pre) / pre * 100, 0)
#'     names(revenue.growth) <- x$periods[-1]
#'     print(revenue.growth)
#'     cat('\nGrowth\n')
#'     print(x$Growth)
#'     cat('\nTable\n')
#'     print(x$Table)
#'     #cat('\n')
#' }
#' 
#' #' @importFrom ggplot2 ggplot geom_bar aes scale_y_continuous ggtitle theme_bw
#' #' @importFrom scales comma
#' #' @importFrom reshape2 melt
#' #' @export
#' plot.GrowthAccountingOld <- function(x, ...)
#' {
#'     t <- x$Table[, c(2, 3, 5, 4, 1)]
#'     x <- rownames(t)
#'     #numeric.periods <- all(as.character(as.numeric(x)) == x)
#'     categories <- colnames(t)
#'     y <- t[,"Churned"] + t[,"Contraction"]
#'     p <- plot_ly(
#'         x = x,
#'         y = y,
#'         showlegend = TRUE,
#'         marker = list(color = "white"),
#'         #hoverinfo='none',
#'         type = "bar")
#'     colors <- c("red", "orange", "teal", "turquoise", "blue")
#'     for (i in 1:5)
#'         p <- add_trace(#evaluate = TRUE,
#'             p,
#'             x = x,
#'             y = abs(t[, i]),
#'             marker = list(color = colors[i]),
#'             name = categories[eval(i)],
#'             type = "bar")
#'     p <- config(p, displayModeBar = FALSE)
#'     layout(p, barmode = "stack", showlegend = TRUE,
#'            xaxis = list(title = "",
#'                         zeroline = FALSE,
#'                         showticklabels = TRUE,
#'                         #range = if(numeric.periods) range(as.numeric(x) + c(-.5, .5)) else NULL
#'                         showgrid = FALSE),
#'            yaxis = list(title = "Change in revenue ($)",
#'                         zeroline = FALSE,
#'                         showticklabels = TRUE,
#'                         showgrid = FALSE))
#' }
#' 
#' #' @export
#' print.GrowthAccountingOld <- function(x, ...)
#' {
#'     cat('Growth Accounting calculations\n\n')
#'     cat(paste0('\nNumber of entities: ', x$n.id, '\n'))
#'     cat('\nRevenue\n')
#'     print(x$Revenue)
#'     cat('\nRevenue Growth\n')
#'     pre <- x$Revenue[-x$n.periods]
#'     revenue.growth <- round((x$Revenue[-1] - pre) / pre * 100, 0)
#'     names(revenue.growth) <- x$periods[-1]
#'     print(revenue.growth)
#'     cat('\nGrowth\n')
#'     print(x$Growth)
#'     cat('\nTable\n')
#'     print(x$Table)
#'     #cat('\n')
#' }
