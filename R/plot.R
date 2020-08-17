addSubscriptionLengthToName <- function(x, subscription.length, by)
{
    Period(AsDate(x) + Periods(1, subscription.length), by)
}    

#' @export
plot.MetricRatio <- function(x, ...)
{
    smooth <- if (length(x) < 4) "None" else "Friedman's super smoother"
    y.title <- attr(x, "y.title")
    ytickformat <- if (grepl("Average", y.title)) "" else "%"
    columnChart(x, 
                fit.type = smooth,
                fit.ignore.last = TRUE,
                y.title = y.title, 
                y.tick.format = ytickformat, ...)
}


#' @export
plot.MetricUnivariate <- function(x, ...)
{
    y.title <- attr(x, "y.title")
    if (length(x) > 20)
        areaChart(x, y.title = y.title, ...)
    else
        columnChart(x, 
                    fit.ignore.last = TRUE,
                    y.title = y.title, 
                    ...)
}



#' @importFrom plotly plot_ly layout `%>%`
#' @importFrom flipFormat FormatAsPercent
#' @export
plot.MetricCohort <- function(x, ...)
{
    churn.type <- if(attr(x, "volume")) "Recurring Revenue " else "Customer "
    y.title <- attr(x, "y.title")
    vals <- if (grepl("Average", y.title)) FormatAsReal(x) else FormatAsPercent(x, decimals = 1)
    series.hover <-paste0(y.title, ": ", vals)
    cohortHeatmap(x, series.hover = series.hover, ...)
}


requiresHeatmap <- function(x)
{
    required.for <- c("Cohort")#, "RevenuePerSubscriberByCohortByTime")
    any(required.for %in% class(x[[1]]))
}

#' @importFrom scales colour_ramp
colorRamp <- function(local.y.max, global.y.max){
    global.color.ramp <- colour_ramp(c("white", "#3E7DCC"))
    if (is.null(global.y.max))
        return(global.color.ramp)
    colour_ramp(c("white", global.color.ramp(local.y.max / global.y.max)))
}

#' @importFrom flipStandardCharts Column
columnChart <- function(x,  ...)
{
    suppressWarnings(Column(x,  x.tick.angle = 0,
                            colors = "#3e7dcc",
                            #                fit.line.type = "solid", 
                            fit.line.width = 4, 
                            fit.line.type = "dot",
                            fit.line.colors = "#f5c524",
                            ...))$htmlwidget
}

#' @importFrom flipStandardCharts Column
areaChart <- function(x,  ...)
{
    Area(x,  x.tick.angle = 0,
         colors = "#3e7dcc",
         fit.ignore.last = TRUE,
         #         x.tick.format = "%d %b %Y",
         fit.line.type = "solid", fit.line.width = 2, 
         fit.line.colors = "#f5c524", ...)$htmlwidget
}




#' \code{cohortHeatmap}
#'
#' @description Internal heatmap function.
#' @param x A \code{matrix} 
#' @param series.hover The description of the value that appears in each cell;
#' used in the hover tooltips 
#' @param ... Additional arguments to be passed to lower level functions.
#' @importFrom plotly plot_ly layout
#' @return A plotly heatmap
cohortHeatmap <- function(x, series.hover, ...)
{
    rn <- matrix(rownames(x), nrow(x), ncol(x), byrow = FALSE)
    cn <- matrix(colnames(x), nrow(x), ncol(x), byrow = TRUE)
    hover.text <- matrix(paste0("Customer since: ", rn, "<br>",
                                properCase(attr(x, "by")), ": ", cn, "<br>",
                                series.hover, "<br>",
                                "Cohort size: ", c(attr(x, "denominator"))), nrow(x))
    plot_ly(x = colnames(x),
            y = rownames(x),
            z = x, 
            zauto = FALSE,
            zmin = 0, 
            zmax = max(x),
            colors = colorRamp(max(x, na.rm = TRUE), list(...)$y.max), 
            text = hover.text,
            hoverinfo = "text",
            type = "heatmap", 
            showscale = FALSE) %>% config(displayModeBar = FALSE) %>% 
        layout("Hello", xaxis = list(title = "Period"), yaxis = list(title = "Cohort")) 
    
    
}