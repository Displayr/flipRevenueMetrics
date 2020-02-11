#' \code{FillInMatrix}
#'
#' @description Fills in missing rows and/or columns in a matrix.
#' @param x The matrix.
#' @param row.names The required row names, which includes the current row names
#' as a subset.
#' @param col.names The required colum names, which includes the current column names
#' as a subset.
#' @param value The value to fill in for cells that are not in the origial matrix.
#' @details No warning is provided if x contains row or column names not in row.names
#' and col.names; they are automaticaly removed.
#' @export
FillInMatrix <- function(x, row.names, col.names, value = 0)
{
    new.dimnames <- list(row.names, col.names)
    names(new.dimnames) <- names(dimnames(x))
    new.x <- matrix(value, length(row.names), length(col.names), dimnames = new.dimnames)
    rn <- rownames(x)[rownames(x) %in% row.names]
    cn <- colnames(x)[colnames(x) %in% col.names]
    new.x[rn, cn] <- x[rn, cn]
    new.x
}




nUnique <- function(x)
{
    length(unique(x))
}
#' \code{FillInVector}
#'
#' @description Fills in missing rows and/or columns in a matrix.
#' @param x The vector
#' @param element.names The required names of the elements of the vector.
#' @param value The value to fill in for cells that are not in the origial matrix.
#' @export
FillInVector <- function(x, element.names, value = 0)
{
    n <- length(element.names)
    new.x <- rep(value, n)
    names(new.x) <- element.names
    new.x[match(names(x), element.names)] <- x
    new.x
}


#' \code{FillInDateVector}
#'
#' @description Fills in missing date rows in a matrix.
#' @param x The vector
#' @param by The time period to aggregate the dates by: 
#' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' and \code{"day"}.
#' @param value The value to fill in for cells that are not in the origial matrix.
#' @export

#' @importFrom flipTime AsDate Period
FillInDateVector <- function(x, by, value = 0)
{
    dt <- CompleteListPeriodNames(names(x), by)
    FillInVector(x, dt)
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


#' @importFrom flipStatistics Table
#' @importFrom stats as.formula
quantityByTime <- function(data, volume, time, by)
{
    form <- if (volume) paste0("value ~ ", time) else paste0("id ~ ", time)
    func <- if (volume) sum else nUnique
    t <- Table(as.formula(form), data = as.data.frame(data), FUN = func) 
    FillInDateVector(t, by)
}


aggregateAsVector <- function(x)
{
    #print(x)
    result <- x[, 2]
    names(result) <- x[, 1]
    result
}