
#' @export
plot.OneDimensionalWithTrend <- function(x, ...)
{
    smooth <- if (length(x) < 4) "None" else "Friedman's super smoother"
    y.title <- attr(x, "y.title")
    numerics <- c("Average Recurring Revenue", "Recurring Revenue", "Customers")
    numerics <- c(numerics, paste("New", numerics))
    is.numeric <- y.title %in% numerics
    ytickformat <- if (is.numeric) "" else "%"
    columnChart(x, 
                fit.type = smooth,
                fit.ignore.last = is.numeric,
                y.title = y.title, 
                y.tick.format = ytickformat, ...)
}


#' @export
plot.OneDimensionalWithoutTrend <- function(x, ...)
{
    y.title <- attr(x, "y.title")
    areaChart(x, y.title = y.title, ...)
}



#' @importFrom plotly plot_ly layout `%>%`
#' @importFrom flipFormat FormatAsPercent
#' @export
plot.Heatmap <- function(x, ...)
{
    volume <- attr(x, "volume")
    churn.type <- if(volume) "Recurring Revenue " else "Customer "
    y.title <- attr(x, "y.title")
    real <- volume | grepl("Average", y.title) | grepl("Customers", y.title) 
    vals <- if (real) FormatAsReal(x) else FormatAsPercent(x, decimals = 1)
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

#' @importFrom flipStandardCharts Area
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
#' @importFrom plotly plot_ly layout config
#' @return A plotly heatmap
cohortHeatmap <- function(x, series.hover, ...)
{
    rn <- matrix(rownames(x), nrow(x), ncol(x), byrow = FALSE)
    cn <- matrix(colnames(x), nrow(x), ncol(x), byrow = TRUE)
    by <- properCase(attr(x, "by"))
    y.title <- if(tenureCohort(x)) paste0(by, "s as customer") else "Customer since"
    hover.text <- matrix(paste0(y.title, ": ", rn, "<br>",
                                by, ": ", cn, "<br>",
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
        layout("Hello", xaxis = list(title = by), yaxis = list(title = y.title)) 
}

# addSubscriptionLengthToName <- function(x, subscription.length, by)
# {
#     Period(AsDate(x) + Periods(1, subscription.length), by)
# }    
