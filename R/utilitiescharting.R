requiresHeatmap <- function(x)
{
    required.for <- c("ChurnByCohort", "RevenuePerSubscriberByCohortByTime")
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
         fit.line.type = "solid", fit.line.width = 2, 
         fit.line.colors = "#f5c524", ...)$htmlwidget
}
