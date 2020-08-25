revenueCalculation <- function(in.cohort, period.start, data)
{
    next.period.start <- nextDate(data, period.start) 
    m <- customerAtPeriodEnd(data, next.period.start) #attr(data, "by.dates")nextDate(data, period.start)
    m <- if (newCohort(data)) 
        m & dateVariableInWindow(data$subscribed.from, period.start, next.period.start)
    else # Cohorts or None
        andSubsetIfItExists(m, in.cohort)
    value <- data$recurring.value[m]
    ids <- data$id[m]
    list(numerator = sum(value), 
         denominator = nUnique(ids),
         detail = data.frame(id = as.character(ids), 
                             value = value, 
                             stringsAsFactors = FALSE))
}    


