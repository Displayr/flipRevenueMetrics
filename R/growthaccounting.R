#' \code{GrowthAccounting}
#'
#' @description Computes the growth within accounts
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time period to aggregate the dates by: 
#' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' and \code{"day"}.
#' @param small A proportion. Expansion less than or equal to this  is classified as minor expansion (e.g., price rises) or
#' an increase in license sales in a larger company.
#' @param ... Additional arguments to be passed to lower level functions.
#' @return A matrix
#' @importFrom flipTime AsDate Period
#' @importFrom flipStatistics Table
#' @importFrom lubridate floor_date years
#' @export
GrowthAccounting <- function(data, by = "year", small = 0.1,  ...)
{
    unit <- Periods(1, by)

    start <- floor_date(attr(data, "start"), by)
    end <- floor_date(attr(data, "end"), by)
    dts <- seq.Date(start, end, by)
    periods <- as.character(dts)
    n.dates <- length(dts)

    # Variables used in loop
    from <- data$from
    to <- data$to
    id <- data$id
    rr <- data$recurring.value

    # Calculations used in loop
    customers <- unique(id[from < start])
    previous.date <- start - unit
    invoice.previous <- from <= previous.date & to >= previous.date 
    ids.previous <- unique(id[invoice.previous])
    rr.by.id.previous <- tapply(rr[invoice.previous], list(id[invoice.previous]), sum)
    
    # Storing results of loop
    metrics <- c("New", "Resurrection", "Major Expansion", "Minor Expansion", "Contraction", "Churn")
    counts <- accounting <- matrix(0, 6, n.dates, dimnames = list(metrics, periods))
    names(dimnames(accounting)) <- c("Metric", "Date") 
    n <- nrow(data) * n.dates 
    str <- rep("", n)
    detail <- data.frame(Date = str, Metric = str, Name = str, Change = rep(NA, n), stringsAsFactors = FALSE)
    counter <- 0
    
    for (i in 1:n.dates)
    {
        dt <- dts[i]
        invoice <- from <= dt & to > dt 
        rr.by.id <- tapply(rr[invoice], list(id[invoice]), sum)
        
        ids <- names(rr.by.id)
        ids.new <- ids[!ids %in% customers]
        ids.resurrection <- ids[ids %in% customers & !ids %in% ids.previous]
        ids.churn <- ids.previous[!ids.previous %in% ids]
        ids.existing <- ids[ids %in% ids.previous]
        
        rr.previous.by.id <- rr.by.id.previous[ids.existing]
        rr.change.by.id <- rr.by.id[ids.existing] - rr.previous.by.id
        expansion <- rr.change.by.id > 0
        minor <- expansion & rr.change.by.id / rr.previous.by.id <= small
        major <- expansion & !minor
        
        rr.metric.by.id <- list(New = rr.by.id[ids.new],
                                Resurrection = rr.by.id[ids.resurrection],
                                "Major Expansion" = rr.change.by.id[major],
                                "Minor Expansion" = rr.change.by.id[minor],
                                Contraction = rr.change.by.id[rr.change.by.id < 0],
                                Churn = -rr.by.id.previous[ids.churn])
        
        accounting[, i] <- sapply(rr.metric.by.id, sum)
        counts[, i] <- cnts <- sapply(rr.metric.by.id, length)
        
        lngth <- sum(cnts)
        if (lngth > 0)
        {
            rws <- counter + (1:lngth)
            detail$Date[rws] <- rep(periods[i], lngth)
            rr.metric.by.id.vector <- unlist(rr.metric.by.id)
            detail$Name[rws] <- names(rr.metric.by.id.vector)
            detail$Change[rws] <- rr.metric.by.id.vector
            detail$Metric[rws] <- rep(metrics, cnts)
            
            # Incrementing thing that are used again
            counter <- counter + lngth
            ids.previous <- ids
            rr.by.id.previous <- rr.by.id
            customers <- union(customers, ids.new)
        }
    }
    
    accounting <- addAttributesAndClass(accounting, "GrowthAccounting", by, detail[1:counter, ])
    attr(accounting, "counts") <- counts
    accounting
}

#' @importFrom ggplot2 ggplot geom_bar aes scale_y_continuous ggtitle theme_bw
#' @importFrom scales comma
#' @importFrom reshape2 melt
#' @export
plot.GrowthAccounting <- function(x, ...)
{
    colors <- c(New = "#3e7dcc",
                Resurrection = "#6490C5",
                "Major Expansion" = "#82A5CB",
                "Minor Expansion" = "#A9C0DA",
                Contraction = "#E99589",
                Churn = "#FA614B")
  t <- x[c(5:6, 1:4), ]#$Table[, c(2, 3, 5, 4, 1)]
  x <- colnames(t)
  #numeric.periods <- all(as.character(as.numeric(x)) == x)
  categories <- rownames(t)
  y <- t["Churn",] + t["Contraction",]
  p <- plot_ly(
    x = x,
    y = y,
    showlegend = TRUE,
    marker = list(color = "white"),
    #hoverinfo='none',
    type = "bar")
  #colors <- c("red", "orange", "teal", "turquoise", "blue")
  for (i in 1:6)
    p <- add_trace(#evaluate = TRUE,
      p,
      x = x,
      y = abs(t[i, ]),
      marker = list(color = colors[i]),
      name = categories[eval(i)],
      type = "bar")
  p <- config(p, displayModeBar = FALSE)
  layout(p, barmode = "stack", showlegend = TRUE,
         xaxis = list(title = "",
                      zeroline = FALSE,
                      showticklabels = TRUE,
                      #range = if(numeric.periods) range(as.numeric(x) + c(-.5, .5)) else NULL
                      showgrid = FALSE),
         yaxis = list(title = "Change in revenue ($)",
                      zeroline = FALSE,
                      showticklabels = TRUE,
                      showgrid = FALSE))
}

