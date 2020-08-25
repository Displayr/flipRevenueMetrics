andSubsetIfItExists <- function(x, subset)
{
    if (is.null(subset))
        return(x)
    x & subset
}


singleSeries <- function(data)
{
    attr(data, "cohort.type") %in% c("None", "New")
}


dateVariableInWindow <- function(from, period.start, next.period.start)
{
    from >= period.start & from < next.period.start
}