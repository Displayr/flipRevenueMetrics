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
#' @importFrom lubridate floor_date years ceiling_date
#' @export
GrowthAccounting <- function(data, by = "year", small = 0.1,  ...)
{
    unit <- Periods(1, by)
    true.end <- attr(data, "end")
    start <- floor_date(attr(data, "start"), by)# + unit
    end <- as.Date(ceiling_date(true.end, by, change_on_boundary = NULL))
    subscription.length <- attr(data, "subscription.length")
    subscription.unit <- Periods(1, subscription.length)
    
    #data <- prepareDataForChurn(data, by)
  
    previous.date <- start - unit
    dts <- seq.Date(start, end, by)
    n.dates <- length(dts)
    periods <- Period(c(previous.date, dts[-length(dts)]), by)
    
    # Ensuring dates used in calculations don't go past the 'end'
    dts <- ensureDatesArentInFuture(dts, true.end + 1) # +1 due to the < operator below
    
    # Variables used in loop
    from <- data$from
    to <- data$to
    id <- data$id
    rr <- data$recurring.value

    # Calculations used in loop
    ids.ever.customers <- unique(id[from <= previous.date])
    invoice.previous <- from <= previous.date & to > previous.date 
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
        invoice <- from < dt & to >= dt 
        rr.by.id <- tapply(rr[invoice], list(id[invoice]), sum)
        
        ids <- names(rr.by.id)
        ids.new <- ids[!ids %in% ids.ever.customers]
        ids.resurrection <- ids[ids %in% ids.ever.customers & !ids %in% ids.previous]
        ids.churn <- ids.previous[!ids.previous %in% ids]
        # Deakug 
        # if (true.end - subscription.unit < dt)
        # {
        #     f <- data$subscriber.to > true.end
        #     ids.not.churned <- unique(data$id[f])
        #     ids.churn <- setdiff(ids.churn, ids.not.churned)
        # }
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
            rr.metric.by.id.vector <- unlist(unname(rr.metric.by.id), use.names = TRUE)
            detail$Name[rws] <- names(rr.metric.by.id.vector)
            detail$Change[rws] <- rr.metric.by.id.vector
            detail$Metric[rws] <- rep(metrics, cnts)
            
            # Incrementing thing that are used again
            counter <- counter + lngth
            ids.previous <- ids
            rr.by.id.previous <- rr.by.id
            ids.ever.customers <- union(ids.ever.customers, ids.new)
        }
    }
    # Dropping the first period
    detail <- detail[1:counter, ] #Right-sizing the data frame
    detail <- detail[detail$Date != colnames(accounting)[1], ]
    accounting <- accounting[, -1, drop = FALSE]
    counts <- counts[, -1, drop = FALSE]
    accounting <- addAttributesAndClass(accounting, "GrowthAccounting", by, detail)
    attr(accounting, "counts") <- counts
    attr(accounting, "by") <- by
    accounting
}

#' plot.GrowthAccounting
#' 
#' @param x The metric to be plotted.
#' @param ... Othe parameters.
#' @importFrom ggplot2 ggplot geom_bar aes scale_y_continuous ggtitle theme_bw
#' @importFrom scales comma
#' @importFrom reshape2 melt
#' @importFrom flipFormat FormatAsReal
#' @export
plot.GrowthAccounting <- function(x, ...)
{
    # Extracting data to plot
    t <- x[6:1, ]#[c(5:6, 1:4), ]#$Table[, c(2, 3, 5, 4, 1)]
    by <- attr(x, "by")
    periods <- colnames(t)
    x.periods <- switch(by,
                      day = ,
                      week = ,
                      month = ,
                      quarter = AsDate(periods),
                      year = as.numeric(periods))
    ct <- attr(x, "counts")[6:1, ]
    detail <- attr(x, "detail")
    colors <- growthColors()
    metrics <- names(colors)
    
    #
    # Sorting based on biggest to smallest
    detail <- detail[order(abs(detail$Change), decreasing = TRUE), ] 
    detail$Name <- paste0("$", FormatAsReal(detail$Change), " ", detail$Name)

  #numeric.periods <- all(as.character(as.numeric(x)) == x)
  y <- t["Churn",] + t["Contraction",]
  p <- plot_ly(
    x = x.periods,
    y = y,
    showlegend = TRUE,
    marker = list(color = "white"),
    #hoverinfo='none',
    type = "bar")
  #colors <- c("red", "orange", "teal", "turquoise", "blue")
  for (i in 1:6)
  {
    y.values <- abs(t[i, ])
    m <- metrics[i]
#    hover.text = paste("$", FormatAsReal(y.values))
    det <- detail[detail$Metric == m, ]
    dd <- tapply(det$Name, list(det$Date), function(x) {
      l <- length(x)
      if (l== 0)
        out <- ""
      else {
        out <- paste(x[1:min(l, 5)], collapse = "<br>")
        if (l > 5)
          out <- paste0(out, "<br>+ ",  l - 5, " more")
      }
      out})
    d <- rep("", length(periods))
    names(d) <- periods
    d[names(dd)] <- dd
  ##  print(d)
#d <- "dog"
    p <- add_trace(#evaluate = TRUE,
      p,
      text = paste0("Number of accounts: ", ct[i,]),
      #hoverinfo = "text",
      x = x.periods,
      y = y.values,
      #hoverlabel.align = "left",
      hovertemplate = paste(
        " %{xaxis.title.text}: %{x}<br>",
        "%{yaxis.title.text}: %{y:$,.0f}<br>",
        "%{text}",
        "<extra>", d, "</extra>"
      ),      
      marker = list(color = colors[i]),
      name = metrics[eval(i)],
      type = "bar")
  }

  p <- config(p, displayModeBar = FALSE)
  x.title <- switch(by,
                    day = "Day",
                    week = "Week Commencing",
                    quarter = "Quarter Commencing",
                    year = "Year")
  layout(p, barmode = "stack",
         showlegend = TRUE,
         hoverlabel = list(align = "left"),
         xaxis = list(title = x.title,
                      zeroline = FALSE,
                      showticklabels = TRUE,
                      #range = if(numeric.periods) range(as.numeric(x) + c(-.5, .5)) else NULL
                      showgrid = FALSE),
         yaxis = list(title = "Change in Recurring Revenue",
                      zeroline = FALSE,
                      showticklabels = TRUE,
                      showgrid = FALSE))
}

growthColors <- function()
{
    c(New = "#04827b",
      Resurrection = "#345e8c",
      "Major Expansion" = "#3e7dcc",
      "Minor Expansion" = "#8cc0ff",
      Contraction = "#ff905a",
      Churn = "#c44e41")[6:1]
}