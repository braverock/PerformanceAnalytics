
### Update EDHEC Data ###

# Load previous .rda(.xts) file
load("~/R/PerformanceAnalytics/data/edhec.rda")

# Get new data from https://risk.edhec.edu/indices-investment-solutions#tab_374
edhec.path <- "https://risk.edhec.edu/sites/risk/files/indices/Indices/Edhec%20Alternative%20Indices/Web/table/history.csv"
updated <- read.csv(edhec.path, sep=";", stringsAsFactors=FALSE)

# Format and prepare for xts
formated.date <- as.Date(updated$date, format="%d/%m/%Y")
formated.returns <- apply(as.matrix(updated[,-1], ncol=13), 2, function(x) as.numeric(gsub("\\%", "", x))/100)

## maintain column vector label continuity with the original edhec.xts object. 
# Note: labels from the original edhec.csv were transformed into the edhec.xts labels.
# Original .csv format is identical to current .csv format.
attr(formated.returns, "dimnames") <- list(NULL, 
                                           c("Convertible Arbitrage", "CTA Global", "Distressed Securities", 
                                             "Emerging Markets", "Equity Market Neutral", "Event Driven", 
                                             "Fixed Income Arbitrage", "Global Macro", "Long/Short Equity", 
                                             "Merger Arbitrage", "Relative Value", "Short Selling",
                                             "Funds of Funds"))

## create xts object with formatted date index and return matrix
library(xts)
updated.edhec <- xts(x = formated.returns, order.by = formated.date)
# Does it have NAs?
sum(is.na(updated.edhec))
# Zero fill and make sure to document this for dates and times!
updated.edhec <- PerformanceAnalytics::zerofill(updated.edhec)

# Test against previous edhec data for continuity
subset.updated.edhec <- updated.edhec[paste(index(edhec[1]), index(edhec[nrow(edhec)]), sep="/")]
all.equal(subset.updated.edhec, edhec)
all.equal(names(edhec), names(subset.updated.edhec)) 

## TRUE test passed ##

## rename and write to folder as .rda file.
edhec <- updated.edhec
save(edhec, file = "data/edhec.rda")
