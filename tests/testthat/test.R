
data("shapeData")
data("pubExposed")
counts <- caseCounts()
## pubExposed <- exposedPub()
caseMap(counts, pubExposed)
countPlot(histoyCount)
report <- caseReport(counts, pubExposed, historyCount)
htmlwidgets::saveWidget(report, "index.html")
