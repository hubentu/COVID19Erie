##library(COVID19Erie)
library(devtools)
load_all()
## data("shapeData")
## data("pubExposed")
Counts <- caseCounts()
## pubExposed <- exposedPub()
## caseMap(counts, pubExposed)

## counties <- read.csv("https://github.com/nytimes/covid-19-data/blob/master/us-counties.csv?raw=true")
## erie <- counties %>% filter(county == "Erie", state == "New York")
## historyCount$confirmed <- erie$cases
## historyCount$deaths <- erie$deaths
## historyCount$new <- historyCount$confirmed - c(0, historyCount$confirmed[-243])

if(format(Sys.time(), "%H:%M") > "22:30"){
    historyCount <- countPlot(historyCount, update = TRUE)
    ## rownames(historyCount) <- NULL
    use_data(historyCount, overwrite=TRUE)
}
## report <- caseReport(counts, pubExposed, historyCount)
## htmlwidgets::saveWidget(report, "index.html")
rmarkdown::render("vignettes/dashboard.Rmd")
