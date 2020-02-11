#' \code{FillInDateRowsInMatrix}
#'
#' @description Fills in missing date rows in a matrix.
#' @param x The matrix.
#' @param by The aggregation of the dates (e.g., "month", "year")
#' @param value The value to fill in for cells that are not in the origial matrix.
#' @export

#' @importFrom flipTime AsDate Period CompleteListPeriodNames
FillInDateRowsInMatrix <- function(x, by, value = 0)
{
    dt <- CompleteListPeriodNames(rownames(x), by)
    FillInMatrix(x, dt, colnames(x))
}





tidyCohortTable <- function(x, cohort.by, by, start, end, remove.last, default.value = NA)
{
    x <- fillInAndSelectDates(x, cohort.by, by, start, end, remove.last, default.value)
    names(dimnames(x)) <- c("Customer since", paste0(properCase(by), "s since commencing"))
    x
}


#' @importFrom lubridate floor_date
#' @importFrom flipTime Periods AsDate
fillInAndSelectDates <- function(x, cohort.by, by, start, end, remove.last, value = NA)
{
    dt.range <- range(AsDate(rownames(x)))
    if (is.null(start))
        start <- dt.range[1]
    start <- AsDate(floor_date(start, cohort.by))
    if (is.null(end))
    {
        end <- dt.range[2]
        if (remove.last) 
            end <- end - Periods(1, cohort.by)
        
    }
    end <- AsDate(floor_date(end, cohort.by))
    mx <- max(as.numeric(colnames(x)))
    rnames <- Period(seq.Date(start, end, by = cohort.by), cohort.by)
    FillInMatrix(x, rnames, seq(0, mx, by = 1), value)
}