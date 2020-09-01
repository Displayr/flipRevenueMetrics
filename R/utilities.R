SumMatchingNames <- function(...)
{
    lst <- list(...)
    sm <- lst[[1]]
    for (i in 2:length(lst))
    {
        sm <- if (is.vector(sm))
            sumMatchingNames.Vector(sm, lst[[i]])
        else 
            sumMatchingNames.Matrix(sm, lst[[i]])
        
    }
    sm
}


sumMatchingNames.Matrix <- function(a, b)
{
    rn <- intersect(rownames(a), rownames(b))
    cn <- intersect(colnames(a), colnames(b))
    a[rn, cn] + b[rn, cn]
}

sumMatchingNames.Vector <- function(a, b)
{
    nm <- intersect(names(a), names(b))
    a[nm] + b[nm]
}


aggregateByNames <- function(x, FUN = sum)
{
    x <- aggregate(x, list(names(x)), FUN = FUN)
    result <- x[, 2]
    names(result) <- x[, 1]
    result
}


sumBy <- function(x, by)
{
    statisticBy(x, by)
}

statisticBy <- function(x, by, FUN = sum)
{
    tapply(x, list(by), FUN)
}
    
properCase <- function(x)
{
    paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
}

maxDate <- function(x)
{
    x <- x[!is.na(x)]
    max(AsDate(x, on.parse.failure = "ignore"), na.rm = TRUE)
}

minDate <- function(x)
{
    x <- x[!is.na(x)]
    min(AsDate(x, on.parse.failure = "ignore"), na.rm = TRUE)
}

nUnique <- function(x)
{
    length(unique(x))
}

namedVector <- function(names)
{
    out <- rep(NA, length(names))
    names(out) <- names
    out
    
}

namedList <- function(names)
{
    out <- vector("list", length(names))
    names(out) <- names
    out
}

spliceVectors <- function(x, dates)
{
    k <- length(x)
    m <- matrix(0, length(dates), k, dimnames = list(dates, names(x)))
    for (i in 1:k)
        m[names(x[[i]]), i] <- x[[i]]
    m
}

stackMatrices <- function(x, dates)
{
    nr <- sum(sapply(x, nrow))
    m <- matrix(NA, nr, length(dates), dimnames = list(1:nr, dates))
    counter <- 0
    for (i in 1:length(x))
    {
        t <- x[[i]]
        rows <- counter + 1:nrow(t)
        m[rows, colnames(t)] <- t
        nm <- strsplit(names(x)[i], split = "\n", fixed = TRUE)[[1]][1]
        rownames(m)[rows] <- paste(nm,rownames(t))
        counter <- counter + nrow(t)
    }
    m    
}
#' #' \code{FillInMatrix}
#' #'
#' #' @description Fills in missing rows and/or columns in a matrix.
#' #' @param x The matrix.
#' #' @param row.names The required row names, which includes the current row names
#' #' as a subset.
#' #' @param col.names The required colum names, which includes the current column names
#' #' as a subset.
#' #' @param value The value to fill in for cells that are not in the origial matrix.
#' #' @details No warning is provided if x contains row or column names not in row.names
#' #' and col.names; they are automaticaly removed.
#' FillInMatrix <- function(x, row.names, col.names, value = 0)
#' {
#'     new.dimnames <- list(row.names, col.names)
#'     names(new.dimnames) <- names(dimnames(x))
#'     new.x <- matrix(value, length(row.names), length(col.names), dimnames = new.dimnames)
#'     rn <- rownames(x)[rownames(x) %in% row.names]
#'     cn <- colnames(x)[colnames(x) %in% col.names]
#'     new.x[rn, cn] <- x[rn, cn]
#'     new.x
#' }
#' 
#' ensureDatesArentInFuture <- function(dates, end)
#' {
#'     future.dates <- dates > end
#'     if (sum(future.dates) > 1) # We permit 1, due to incomplete time periods
#'         stop("The specified end date is in the future.")
#'     dates[future.dates] <- end # Should just fix end
#'     dates
#' }
#' 
#' 
#' #' \code{FillInVector}
#' #'
#' #' @description Fills in missing rows and/or columns in a matrix.
#' #' @param x The vector
#' #' @param element.names The required names of the elements of the vector.
#' #' @param value The value to fill in for cells that are not in the origial matrix.
#' #' @export
#' FillInVector <- function(x, element.names, value = 0)
#' {
#'     n <- length(element.names)
#'     new.x <- rep(value, n)
#'     names(new.x) <- element.names
#'     new.x[match(names(x), element.names)] <- x
#'     new.x
#' }
#' 
#' 
#' #' \code{FillInDateVector}
#' #'
#' #' @description Fills in missing date rows in a matrix.
#' #' @param x The vector
#' #' @param by The time period to aggregate the dates by: 
#' #' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' #' and \code{"day"}.
#' #' @param value The value to fill in for cells that are not in the origial matrix.
#' #' @export
#' 
#' FillInDateVector <- function(x, by, value = 0)
#' {
#'     a.dt <- CompleteListPeriodNames(names(x), by)
#'     FillInVector(x, a.dt)
#' }
#' 
#' 
#' 
#' 
#' 
#' 
#' #' @importFrom flipStatistics Table
#' #' @importFrom stats as.formula
#' quantityByTime <- function(data, volume, time, by)
#' {
#'     form <- if (volume) paste0("value ~ ", time) else paste0("id ~ ", time)
#'     func <- if (volume) sum else nUnique
#'     t <- Table(as.formula(form), data = as.data.frame(data), FUN = func) 
#'     FillInDateVector(t, by)
#' }
#' 
#' 
#' 
#' 
#' 
#' #' \code{FillInDateRowsInMatrix}
#' #'
#' #' @description Fills in missing date rows in a matrix.
#' #' @param x The matrix.
#' #' @param by The aggregation of the dates (e.g., "month", "year")
#' #' @param value The value to fill in for cells that are not in the origial matrix.
#' #' @importFrom flipTime AsDate Period CompleteListPeriodNames
#' FillInDateRowsInMatrix <- function(x, by, value = 0)
#' {
#'     a.dt <- CompleteListPeriodNames(rownames(x), by)
#'     FillInMatrix(x, a.dt, colnames(x))
#' }
#' 
#' 
#' 
#' 
#' 
#' tidyCohortTable <- function(x, cohort.by, by, start, end, remove.last, default.value = NA)
#' {
#'     x <- fillInAndSelectDates(x, cohort.by, by, start, end, remove.last, default.value)
#'     names(dimnames(x)) <- c("Customer since", paste0(properCase(by), "s since commencing"))
#'     x
#' }
#' 
#' 
#' #' @importFrom lubridate floor_date
#' #' @importFrom flipTime Periods AsDate
#' fillInAndSelectDates <- function(x, cohort.by, by, start, end, remove.last, value = NA)
#' {
#'     dt.range <- range(AsDate(rownames(x)))
#'     if (is.null(start))
#'         start <- dt.range[1]
#'     start <- AsDate(floor_date(start, cohort.by))
#'     if (is.null(end))
#'     {
#'         end <- dt.range[2]
#'         if (remove.last) 
#'             end <- end - Periods(1, cohort.by)
#'         
#'     }
#'     end <- AsDate(floor_date(end, cohort.by))
#'     mx <- max(as.numeric(colnames(x)))
#'     rnames <- Period(seq.Date(start, end, by = cohort.by), cohort.by)
#'     FillInMatrix(x, rnames, seq(0, mx, by = 1), value)
#' }

