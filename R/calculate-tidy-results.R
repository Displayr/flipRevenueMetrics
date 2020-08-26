tidyResults <- function(results, volume, components, data)
{
    is.matrix <- !singleSeries(data)
    denominator <- deList(results, "denominator", is.matrix)
    numerator <- deList(results, "numerator", is.matrix)
    detail <- deListDetails(results, is.matrix)
    out <- tidyNumeratorAndDenominator(numerator, denominator)
    out$detail <- tidyDetail(volume, numerator, denominator, detail, components)
    out$volume <- volume
    out <- addAttributesToList(out, data)
    out
}

#' @importFrom flipTime Period
tidyNumeratorAndDenominator <- function(numerator, denominator)#, mergers, by, volume, rr)
{
    if (!is.vector(numerator))
    {    
        # Removing zero rows and columns
        rf <- zeroRowsAtTopAndBottom(denominator)
        cf <- zeroRowsAtTopAndBottom(t(denominator))
        denominator <- denominator[rf, cf, drop = FALSE]
        numerator <- numerator[rf, cf, drop = FALSE]
        # if (ncol(denominator) == 1)
        # {
        #     if (nrow(denominator) == 1)
        #     {
        #         rn <- rownames(denominator)
        #         denominator <- denominator[1, 1]
        #         numerator <- numerator[1, 1]
        #         names(denominator) <- names(numerator) <- rn
        #         
        #     } else
        #     {
        #         numerator <- numerator[, 1]
        #         denominator <- denominator[, 1]
        #     }
        # }
    }
    list(denominator = denominator, numerator = numerator)
}

zeroRowsAtTopAndBottom <- function(x)
{
    rs <- rowSums(x, na.rm = TRUE)
    cumsum(rs) > 0 & rev(cumsum(rev(rs))) > 0
}

deListDetails <- function(results, is.matrix)
{
    if (!is.matrix)
        return(lapply(results, function(x) x[["detail"]]))        
    lapply(results, function(x) lapply(x, function(x) x[["detail"]]))
}


deList <- function(results, statistic, is.matrix)
{
    if (is.matrix)
        return(convertToMatrix(results, statistic))        
    sapplyStatistic(results, statistic)
}



convertToMatrix <- function(results, statistic)
{
    out <- sapply(results, sapplyStatistic, statistic = statistic)
    if (is.matrix(out))
        out <- t(out)
    if (!is.matrix(out))
        out <- matrix(out, ncol = 1, dimnames = list(names(results), names(results[[1]])))
    out
}

sapplyStatistic <- function(x, statistic)
{
    sapply(x, function(x)
    {
        if (!is.list(x))
            return(NA)
        r <- x[[statistic]]
        if (is.null(r)) NA else r
    } )
}

#' @importFrom dplyr bind_rows 
tidyDetail <- function(volume, numerator, denominator, detail, components)
{
    if (volume) 
        return(detail)
    # if (components == "number of customers")# This could probably can be combined into the code below
    # { 
    #     detail <- lapply(detail, function(x) x[['All']])
    #     return(bind_rows(detail))
    # }
    rn <- if (is.matrix(denominator)) rownames(denominator) else names(denominator)
    cn <- if (is.matrix(denominator)) colnames(denominator) else "All"
    cohort.matrix <- matrix(rn,  length(rn), length(cn))
    subscription.matrix <- matrix(cn,  length(rn), length(cn))
    churned <- unlist(unlist(detail))
    n <- length(churned)
    rep.by <- if(sum(numerator, na.rm = TRUE) == n) numerator else {
        if (sum(denominator, na.rm = TRUE) == n) denominator else NULL }
    if (is.null(rep.by))
        return(churned)
    else
        rep.by[is.na(rep.by)] <- 0
    data.frame(Cohort = rep(cohort.matrix, rep.by),
               "Rewewal Period" = rep(subscription.matrix, rep.by),
               Churned = churned,
               row.names = NULL)
}
#' 
#' #' @importFrom flipTables Cbind
#' asMatrix <- function(list.of.lists, FUN, fill.with = 0)
#' {
#'     x <- lapply(list.of.lists, function(x) sapply(x, FUN))
#'     m <- t(do.call("Cbind", x)) #Using t() to hack around DS-3041
#'     m[is.na(m)] <- fill.with
#'     m
#' }    
