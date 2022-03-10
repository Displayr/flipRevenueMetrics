#' \code{RevenueMetric}
#'
#' @description Creates a small multiple plot by sub-groups
#' @param FUN A function that calculates a metric
#' @param output Whether to output as a Plot, Table, or Detail 
#' @param by The unit of time to report on ("day", "month", "quarter", "year").
#' @param cohort.type How cohorts are to be used when performing the analysis
#' \code{by} setting.
#' \itemize{
#'  \code{"None"} {All customers are cohort.typed in calculations.}
#'  \code{"New"} {Customers added in the preceding \code{by}.}
#'  \code{"Calendar"} {Calender time periods are used. For example, if \code{cohort.period} is set
#'  to "year" then calendar years are used as cohorts.}
#'  \code{"Tenure"} {Tenure based time periods are used. For example, if \code{cohort.period} is set
#'  to "year", then cohorts are based on the number of years since a customeer first signed up.
#'  then calendar years are used as cohorts.}
#' }
#' @param cohort.period The period of aggregation to be used, with options of \code{"week"}, \code{"month"},
#' \code{"quarter"}, and \code{"year"}. This parameter is only used when \code{cohort.type} is \code{"Calendar"}
#' or \code{"Tenure"}
#' @param id A vector of \code{character}, unique identifier for
#'     subscribers that made the transactions (e.g., email addresses,
#'     names, subscriber keys).
#' @param value A vector of containing the revenue per transaction.
#' @param from A vector of class \code{POSIXct} or \code{POSIXt},
#'     recording the date and time each subscription commences.
#' @param to A vector of class \code{POSIXct} or \code{POSIXt},
#'     recording the date and time each subscription ends
#' @param start The date at which the analysis outputs should
#'     commence. By default, the earliest date recorded in
#'     \code{from}.
#' @param end The date at which the analysis ends, which is used to
#'     determine churn.  By default, the most recent date recorded in
#'     \code{from}.
#' @param profiling Separate analyses are conducted among each unique combination of these variables.
#' @param mergers A data frame with two variables 'id' and 'id.to'. 
#' 'id' contains the ids of companies that appeara, based on their data,
#' to have churned, but have in fact merged with the corresponding 'id.to'.
#' @param subscription.length The time unit that describes the
#'     subscription length: \code{year} to view the data by year,
#'     \code{quarter}, and \code{month}. This is assumed to be the
#'     billing period when determining if subscribers have churned or
#'     not.
#' @param subset An optional vector specifying a subset of
#'     observations to be used in the calculations
#' @param trim.id The maximum length of the strings to be used showing
#'     ID names (used to avoid situations where string names are so
#'     long as to make reading of tables impossible.
#' \code{"quarter"}, and \code{"year"}.
#' @param ... Additional arguments to be passed to lower level functions.
#' @details The \code{cohort.type} parameter does not have the transitive properties
#' that many assume. For example, it's not always the case that \code{"New"} is 
#' equivalent to a diagonal from \code{"Calendar"}, or that \code{"None"} is 
#' the column-sums of \code{"Calendar"}. 
#' 
#' Consider as an example \code{CustomerChurn} where
#' all analysis is being done with the unit of \code{"year"}. A customer that 
#' purchases a contract less than the length of a year will appear below the
#' (possibly offset) diagonal in \code{"Calendar"} and if they renew will be in 
#' the denominator for the following year of \code{"New"}, but not if they do 
#' not renew.
#' 
#' Similarly, with \code{RecurringRevenueChurn}, \code{"New"} in an incomplete final
#'  period will compare to the same period in the previous year, whereas the bottom
#'  row of \code{"Calendar"}  will use the entire calendar yearear as the base, and
#'  thus look much better (in a misleading way).
#' @importFrom plotly add_annotations subplot
#' @importFrom lubridate floor_date ceiling_date
#' @return A plotly plot#?
#' @export
RevenueMetric <- function(FUN = "Acquisition",
                          output = c("Plot", "Table", "Detail")[1],
                          # parameters from RevenueData
                          by  = c("day", "month", "quarter", "year")[4],
                          cohort.type = "None",
                          cohort.period = "year",
                          id,
                          value, 
                          from, 
                          to, 
                          start = as.Date(min(from)),
                          end = as.Date(max(from)), 
                          subscription.length = "year", 
                          subset = rep(TRUE, length(id)),
                          profiling = NULL, 
                          trim.id = 50,
                          mergers = NULL, ...)
{
    checkOutputArgument(output)
    start <- floor_date(start, by)

    filters <- createFilters(profiling, subset = subset, id)
    n.filters <- length(filters)
    out <- vector("list", n.filters)
    y.max <- 0
    y.min <- 0
    for (i in 1:n.filters)
    {
        # The start parameter is used later, so the data set isn't filted
        f <- filters[[i]]
        data <- MetricData(value[f], 
                           from[f], 
                           to[f], 
                           start,
                           end, 
                           id[f],
                           subscription.length,
                           by,
                           cohort.type,
                           cohort.period,
                           mergers,
                           trim.id)
        if (!is.null(data))
        {
            metric <- do.call(FUN, list(data, ...))
            if (!is.null(metric))
            {
                out[[i]] <- metric
                r <- yLim(metric)
                if (!is.na(r[1]))
                {
                    y.min <- min(y.min, r[1], na.rm = TRUE)
                    y.max <- max(y.max, r[2], na.rm = TRUE)
                }
            }
        }
    }
    names(out) <- names(filters)
    # Collect information for setting chart attribute data.
    # Info will be lost after creating plots
    y.title <- attr(out[[1]], "y.title")
    n.series <- length(out)
    transpose.chart.data <- FALSE
    if ("OneDimensionalWithoutTrend" %in% class(out[[1]])) {
        chart.type <- "Area"
    } else if ("GrowthAccounting" %in% class(out[[1]])) {
        chart.type <- "Column Stacked"
        transpose.chart.data <- TRUE
    } else {
        chart.type <- "Column Clustered"
    }
    out <- lapply(out, filterRange, start, end)
    out <- out[sapply(out, function(x) NROW(x) > 0 )] # Removing any empty strings
    if (length(out) == 0)
        return(NULL)

    chart.data <- createTable(out, start, end)
    out <- switch(output,
                  Plot = createPlots(out, start, end, y.min, y.max),
                  Table = chart.data,
                  Detail = createDetails(out, start, end))
    if (output == "Plot") {
        out <- addAttributesToRevenueMetricPlot(out, chart.data, chart.type, FUN, by, transpose.chart.data, y.title, n.series)
    }
    out
}

addAttributesToRevenueMetricPlot <- function(plot, chart.data, chart.type, FUN, by, transpose, y.title, n.series) {
    if (transpose){
        chart.data <- t(chart.data)
    }
    numeric.types <- c("GrowthAccounting", 
              "RecurringRevenue", 
              "NumberofCustomers", 
              "AverageRecurringRevenue")
    if (! FUN %in% numeric.types) {
        chart.data <- chart.data * 100
        attr(chart.data, "statistic") <- "%"
    }

    attr(plot, "ChartData") <- if (requiresHeatmap(plot) & length(plot) > 1) NULL else as.matrix(chart.data)
    attr(plot, "ChartType") <- chart.type

    chart.settings <- list()
    chart.settings$ShowLegend <- n.series > 1 | chart.type == "Column Stacked"
    chart.settings$ValueAxis <- list(NumberFormat = if (FUN %in% numeric.types) "General" else "0.0%",
                                    ShowTitle = TRUE)
    chart.settings$PrimaryAxis <- list(ShowTitle = TRUE, LabelPosition = "Low")
    chart.settings$TemplateSeries <- list()
    chart.settings$TemplateSeries[[1]] <- list(ShowDataLabels = FALSE)
    if (is.matrix(chart.data)) {
        for (j in seq_len(ncol(chart.data))) {
            chart.settings$TemplateSeries[[j]] <- list(ShowDataLabels = FALSE)
        }       
    }
    

    attr(plot, "ChartSettings") <- chart.settings
    x.title <- switch(by, 
                     day = "Day", 
                     month = "Month", 
                     quarter = "Quarter", 
                     year = "Year")
    attr(plot, "ChartLabels") <- list("PrimaryAxisTitle" = x.title, "ValueAxisTitle" = y.title)
    return(plot)
}

checkOutputArgument <- function(output)
{
    if (!output %in% c("Plot", "Table", "Detail"))
        stop("'output' was '", output, "'; it should be one of 'Plot', 'Table', or 'Detail'.")
}


filterRange <- function(x, start, end)
{
    if(is.null(x))
        return(x)
    atr <- attributes(x)
    if (any(grepl("Heatmap", class(x))))
    {
        sbs <- datesWithinRange(colnames(x), start, end)
        out <- x[, sbs, drop = FALSE]
    }   
    else if (is.matrix(x)) {
        sbs <- datesWithinRange(colnames(x), start, end)
        out <- x[, sbs, drop = FALSE]
    } else {
        sbs <- datesWithinRange(names(x), start, end)
        out <- x[sbs]
    }
    # Re-appending class information
    to.replace <- names(atr)
    to.replace <- to.replace[!to.replace %in% c("class", "dim", "dimnames", "names")]
    for (a in to.replace)
        attr(out, a) <- changeRangeOfAttribute(out, atr[[a]])
    class(out) <- atr[["class"]]
    out
}    

changeRangeOfAttribute <- function(filtered, original)
{
    if (length(original) == 1)
        return(original)
    if (is.character(original) | is.logical(original))
        return(original)
    if(is.vector(original) & !is.list(original))
        if (all(names(filtered) %in% names(original)))
            return(original[names(filtered)])
    if(is.matrix(original))
    {
        rn <- rownames(filtered)
        cn <- colnames(filtered)
        if(all(rn %in% rownames(original)) & all(cn %in% colnames(original)))
            return(original[rn, cn])
    }
    original
}

createDetails <- function(x, start, end)
{
    
    out = lapply(x, function(x) attr(x, "detail"))
    if (length(out) == 1)
        return(out[[1]])
    out
}

#' @importFrom flipTime AsDate Period
#' @importFrom stats sd
createTable <- function(x, start, end)
{
    
    if (length(x) == 1)
        return(x[[1]])
    by <- attr(x[[1]], "by")
    if (sd(sapply(x, NCOL)) > 0) # Inconsistent output sizes
        return(x)
    is.m <- is.matrix(x[[1]])
    dates <- if (any(class(x[[1]]) %in% c("RecurringRevenue", "Revenue")))
        names(x[[1]])
    else {
        rng <- if (is.m) sapply(x, function(x) colnames(x)[c(1, ncol(x))])
        else sapply(x, function(x) names(x)[c(1, length(x))])
        mn <- minDate(rng[1,])
        mx <- maxDate(rng[2,])
        Period(seq.Date(mn, mx, by = by), by)
    }
    if (is.m) stackMatrices(x, dates) else spliceVectors(x, dates)
}

#' @importFrom flipTime AsDate
datesWithinRange <- function(dt.as.character, start, end)
{
    dts <- AsDate(dt.as.character)
    dts >= start & dts <= end    
}    


# datesAreSame <- function(x)
# {
#     if (sd(sapply, nms) == 0)
#         return(FALSE)
#     nms <- lapply(x, names)
#     
#     
#     
# }



createPlots <- function(x, start, end, y.min, y.max)
{
    
    if (requiresHeatmap(x))
    {
        plotSubGroups(x, y.max)
    }
    else if ("RevenueByCohort" %in% class(x[[1]]))
        plotSubGroups(x)
    else plotSubGroups(x,
                       # need to specify bounds to ensure subplot share axis properly 
                       x.bounds.minimum = format(start, "%Y-%m-%d"), # pass date as a string
                       x.bounds.maximum = format(end, "%Y-%m-%d"),
                       x.tick.format = "%b %y",  # specify date format to help flipStandardChart figure out parsing
                       y.bounds.minimum = y.min, 
                       y.bounds.maximum = y.max,
                       opacity = 1.0)
}

#' @importFrom graphics plot
plotSubGroups <- function(x, ...)
{
    x <- x[sapply(x, canPlot)]
    if (length(x) == 0)
        return(NULL)
    if (length(x) == 1)
        return(print(plot(x[[1]])))
    plots <- lapply(x, FUN = plot, ...)
    plots <- plots[sapply(plots, FUN = function(x) !is.null(x))]
    n.plots <- length(plots)
    pp <- if (n.plots == 1) plots[[1]] else 
    {
        nr <- floor(sqrt(n.plots - 1))
        nc <- ceiling(n.plots/nr)
        pp <- subplot(plots, nrows = nr, shareY = nc > 1)# Bug in shareX, causes re-arrangement of series#, shareX = nr > 1)#, shareX = TRUE, )
        # Adding titles
        annotations <- list()
        titles.ypos <- rep((nr:1)/nr, each = nc)[1:n.plots]
        titles.xpos <- rep((1:nc - 0.5)/nc, nr)[1:n.plots]
        for (i in seq_along(plots))
        {
            annotations[[i]]  <- list(text = names(plots)[i],
                                      x = titles.xpos[i],
                                      y = titles.ypos[i],
                                      yref = "paper",
                                      xref = "paper",
                                      xanchor = "center",
                                      yanchor = "top",
                                      showarrow = FALSE,
                                      font = list(size = 15))
        }
        layout(pp, annotations = annotations)
    }
    print(pp)
}

yLim <- function(x)
{
    UseMethod("yLim", x)
}

yLim.default <- function(x, ...)
{
    if (all(is.na(x)))
        return(NA)
    range(x, na.rm = TRUE)
}

#' @importFrom verbs SumEmptyHandling
createFilters <- function(profiling, subset, id)
{
    if (is.null(subset))
        subset <- rep(TRUE, length(id))
    else if (any(is.na(subset)))
        subset[is.na(subset)] <- FALSE
    if (SumEmptyHandling(subset) == 0)
        stop("All data has been filtered out.")
    if (is.null(profiling))
        return(list(subset))
    subsets <- list()
    # Converting all variables to factors
    for (i in 1:NCOL(profiling))
    {
        p <- trimws(as.character(profiling[[i]]))
        p[is.na(p)] <- "MISSING DATA"
        profiling[[i]] <- p
    }
    #if (is.null(subset))
    #    subset <- TRUE
    n.profiling <- nrow(profiling)
    if (n.profiling != length(subset))
        stop("Profiling variables have the wrong number of observations.")
    levs <- lapply(profiling, unique)
    combs <- expand.grid(levs)
    n.combinations <- nrow(combs)
    for (i in 1:n.combinations)
    {
        filts <- combs[rep(i, n.profiling), , drop = FALSE]
        f <- apply(profiling == filts, 1, all) & subset
        f[is.na(f)] <- FALSE
        subsets[[i]] <- f
    }
    nms <- apply(combs,1, function(x) paste(as.character(x), collapse = " + "))
    nms <- trimws(nms)
    #nms <- paste0(nms, "\nn: ", sapply(subsets, function(x) length(unique(id[x]))))
    names(subsets) <- nms
    # Filtering out empty subsets
    subsets[sapply(subsets, function(x) length(x) > 0)]
}

#' @export
print.RevenueMetric <- function(x, ...)
{
    printWithoutAttributes(x)
}    

printWithoutAttributes <- function(x)
{
    print(removeAttributesAndClass(x))
}

removeAttributesAndClass <- function(x)
{
    for (a in c("detail", 
                "volume",
                "by",
                "numerator",
                "denominator",
                "n.retained",
                "n.churned",
                "churn",
                "retention",
                "cohort.size",
                "subscription.length",
                "y.title",
                "date.format",
                "cohort.type",
                "cohort.period",
                "cohort.by"))
    attr(x, a) <- NULL
    class(x) <- class(x)[-1:-2]
    x
}



#' Create a standardized object 
#' 
#' @param x The statistic to be displayed.
#' @param class.name Used to determine how to plot the data
#' @param calculation The detail
#' @param name Used in plotting
createOutput <- function(x, class.name, calculation, name)
{
    attr(x, "detail") <- calculation$detail
    attr(x, "cohort.type") <- calculation$cohort.type
    attr(x, "cohort.period") <- calculation$cohort.period
    attr(x, "by") <- calculation$by
    attr(x, "volume") <- calculation$volume
    attr(x, "numerator") <- calculation$numerator
    attr(x, "denominator") <- calculation$denominator
    attr(x, "subscription.length") <- s.l <- calculation$subscription.length
    attr(x, "y.title") <- paste0(if (newCohort(x)) "New " else "", name)
    class(x) <- c(class.name, "RevenueMetric", class(x))
    x    
}    



# licensed <- function(window.start, license.end.date, window.end)
# {
#     license.end.date >= window.start & license.end.date < window.end
# }
    
#' 
#' \code{Denominator}
#' 
#' Extracts the denominator from a RevenueMetric object (or a component of in some situations).
#' @param x A \code{RevenueMetric} object.
#' @export
Denominator <- function(x)
{
    attr(x, "denominator")
}

#' \code{Numerator}
#' 
#' Extracts the numerator from a RevenueMetric object (or a component of in some situations).
#' @param x A \code{RevenueMetric} object.
#' @export
Numerator <- function(x)
{
    attr(x, "numerator")
}

canPlot <- function(x)
{
    if (is.null(x))
        return(FALSE)
    if (all(is.na(x)))
        return(FALSE)
    TRUE
}

#' Detail
#'
#' @param x A RevenueMetric object
#' @description Creates a detailed description of the input data used to create the object.
#' @export
Detail <- function(x)
{
    UseMethod("Detail", x)
}

Detail.default <- function(x, ...)
{
    attr(x, "detail")
}
