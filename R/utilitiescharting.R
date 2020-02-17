requiresHeatmap <- function(x)
{
    required.for <- c("ChurnByCohort")#, "RevenuePerSubscriberByCohortByTime")
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
#' @return A plotly heatmap
cohortHeatmap <- function(x, series.hover, ...)
{
    rn <- matrix(rownames(x), nrow(x), ncol(x), byrow = FALSE)
    cn <- matrix(colnames(x), nrow(x), ncol(x), byrow = TRUE)
    hover.text <- matrix(paste0("Customer since: ", rn, "<br>",
                                properCase(attr(x, "by")), "s since starting: ", cn, "<br>",
                                series.hover, "<br>",
                                "Number customers: ", c(attr(x, "n.subscribers"))), nrow(x))
    plot_ly(x = colnames(x),
            y = rownames(x),
            z = x, 
            colors = colorRamp(max(x, na.rm = TRUE), list(...)$y.max), 
            text = hover.text,
            hoverinfo = "text",
            type = "heatmap", 
            showscale = FALSE) %>% config(displayModeBar = FALSE)
    
}