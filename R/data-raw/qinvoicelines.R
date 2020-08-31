#Sys.setenv(TZ='GMT')
q.invoice.lines <- foreign::read.spss(system.file("extdata", "invoiceLines.sav", package = "flipRevenueMetrics"), to.data.frame = TRUE)
q.invoice.lines <- q.invoice.lines[, !(names(q.invoice.lines) %in% c("InvoiceID", "orgID","Edition"))]
q.invoice.lines$ValidFrom <- as.Date(ISOdate(1582,10,14)  +  q.invoice.lines$ValidFrom)
q.invoice.lines$ValidTo <- as.Date(ISOdate(1582,10,14)  +  q.invoice.lines$ValidTo)# - lubridate::seconds(1)
exchangeRates <- c(AUD = 1, CNY = 4.84, EUR = 0.66, GBP = 0.52, NZD = 1.05, USD = .74) #11 June 2016
q.invoice.lines$AUD <- q.invoice.lines$Amount / exchangeRates[q.invoice.lines$currency]
q.invoice.lines$ValidTo <- q.invoice.lines$ValidTo #- lubridate::seconds(1)
end <- as.Date(ISOdate(2016, 6, 14))
d <- q.invoice.lines[q.invoice.lines$ValidFrom < end, ]

# Cleaning out some poor data
d <- d[d$ValidFrom < d$ValidTo, ]

# merging categories
d$country <- as.character(d$country)
t <- table(d$country)
d$country[d$country %in% names(t[t < 100])] <- "Other"

d$salesman <- as.character(d$salesman)
t <- table(d$salesman)
d$salesman[d$salesman %in% names(t[t < 100])] <- "Other"
d$name = as.character(trimws(d$name))
q.invoice.lines <- d
library(devtools)
use_data(q.invoice.lines, internal = FALSE, overwrite = TRUE)

set.seed(1223)
d <- q.invoice.lines
d <- d[d$validInvoice == 1, ]
d <- d[sample(1:nrow(d), 100), ]
q.invoice.lines.short <- d
use_data(q.invoice.lines.short, internal = FALSE, overwrite = TRUE)
