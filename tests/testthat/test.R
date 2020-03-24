
## data("shapeData")
## data("pubExposed")
counts <- caseCounts()
pubExposed <- exposedPub()
## caseMap(counts, pubExposed)
historyCount <- countPlot(historyCount, update = TRUE)
rownames(historyCount) <- NULL
use_data(historyCount, overwrite=TRUE)
report <- caseReport(counts, pubExposed, historyCount)
htmlwidgets::saveWidget(report, "index.html")
