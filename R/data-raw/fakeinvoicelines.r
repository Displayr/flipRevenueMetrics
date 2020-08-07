library(lubridate)
company.1.from <- c(as.Date("2012/01/01"),
                    as.Date("2013/01/01"),
                    as.Date("2014/01/01"),
                    as.Date("2015/01/15"), # 14 day gap
                    as.Date("2016/01/15"), 
                    as.Date("2017/06/15")) # 6 month gap
company.1 <- data.frame(From = company.1.from,
                        To = company.1.from + years(1),
                        Value = 1000,
                        Name = "A")                                 
company.2.from <- seq.Date(as.Date("2010/06/30"), by = "year", length.out = 10)
company.2 <- data.frame(From = company.2.from,
                        To = company.2.from + years(1),
                        Value = 10000,
                        Name = "B")                                 
fake.invoice.lines <- rbind(company.1, company.2)

library(devtools)
use_data(fake.invoice.lines, internal = FALSE, overwrite = TRUE)

