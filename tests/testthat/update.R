##library(COVID19Erie)
library(devtools)
load_all()
## data("shapeData")
## data("pubExposed")
Counts <- caseCounts()
## pubExposed <- exposedPub()
## caseMap(counts, pubExposed)

counties <- read.csv("https://github.com/nytimes/covid-19-data/blob/master/us-counties.csv?raw=true")
erie <- counties %>% filter(county == "Erie", state == "New York")
if(nrow(erie) > nrow(historyCount)){
    e1 <- erie[(nrow(historyCount)+1):nrow(erie),,drop=F]
    e1 <- data.frame(date = e1$date, confirmed = e1$cases,
                     new = NA, active = NA, recovered = NA, deaths = e1$deaths)
    historyCount <- rbind(historyCount, e1)
    historyCount$new <- historyCount$confirmed - c(0, historyCount$confirmed[-nrow(historyCount)])
    use_data(historyCount, overwrite = TRUE)
}
## historyCount$confirmed <- erie$cases
## historyCount$deaths <- erie$deaths
## historyCount$new <- historyCount$confirmed - c(0, historyCount$confirmed[-243])

## if(format(Sys.time(), "%H:%M") > "22:30"){
##     historyCount <- countPlot(historyCount, update = TRUE)
##     ## rownames(historyCount) <- NULL
##     use_data(historyCount, overwrite=TRUE)
## }
## report <- caseReport(counts, pubExposed, historyCount)
## htmlwidgets::saveWidget(report, "index.html")
rmarkdown::render("vignettes/dashboard.Rmd", output_file = "index.html", output_dir = "docs")
