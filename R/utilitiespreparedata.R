addAttributesToList <- function(list, object.with.attributes)
{
    attribute.names <- c("by", "cohort.type", "cohort.period", "subscription.length")
    current.attributes <- attributes(object.with.attributes)
    current.attributes <- current.attributes[!"names" %in% names(current.attributes)]
    attrs <- attributes(object.with.attributes)[attribute.names]
    #attrs <- lapply(attribute.names, function(x) attr(object.with.attributes, x))
    #names(attrs) <- attribute.names
    out <- c(list, attrs)
    setAttributes(out, current.attributes)
    out
}

getAttributes <- function(x)
{
    attributes(x)[c("start", "end", "mergers")]
}

setAttributes <- function(data, attr)
{
    attr(data, "start") <- attr[["start"]]
    attr(data, "end") <- attr[["end"]]
    attr(data, "mergers") <- attr[["mergers"]]
    data
}


# setCohortPeriod <- function(data, cohort.by)
# {
#     data$cohort <- Period(data$subscriber.from, cohort.by)
#     data
# }
# 
# setPeriodCounter <- function(data, by)
# {
#     units <- Periods(1, by)
#     data$period.counter <- interval(data$subscriber.from, data$from) %/% units
#     data
# }
# 
# updatePeriod <- function(data, by)
# {
#     data$period <- Period(data$from, by)
#     data
# }

#' #' @importFrom flipTime Period
#' prepareDataForChurn <- function(data, by)
#' {
#'     attr <- getAttributes(data)
#'     data <- removeIncompleteSubscriptions(data)
#'     data$to.renewal.period <- Period(data$to.renewal, by)
#'     data <- setAttributes(data, attr)
#'     data
#' }

#' 
#' #' @importFrom flipTime Period
#' #' @importFrom lubridate interval
#' prepareDataForChurnByCohort <- function(data, by)
#' {
#'     attr <- getAttributes(data)
#'     data <- removeIncompleteSubscriptions(data)
#'     data <- setCohortPeriod(data, by)
#'     units <- Periods(1, attr(data, "subscription.length"))
#'     data$to.renewal.period.counter <- interval(data$subscriber.from, data$to.renewal) %/% units
#'     data <- setAttributes(data, attr)
#'     data
#' }
