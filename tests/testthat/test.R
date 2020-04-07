
## data("shapeData")
## data("pubExposed")
Counts <- caseCounts()
## pubExposed <- exposedPub()
## caseMap(counts, pubExposed)
if(format(Sys.time(), "%H:%M") > "22:30"){
    historyCount <- countPlot(historyCount, update = TRUE)
    ## rownames(historyCount) <- NULL
    use_data(historyCount, overwrite=TRUE)
}
## report <- caseReport(counts, pubExposed, historyCount)
## htmlwidgets::saveWidget(report, "index.html")
rmarkdown::render("vignettes/dashboard.Rmd")

