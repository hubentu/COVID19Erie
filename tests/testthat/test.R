
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

rmarkdown::render("vignettes/dashboard.Rmd")

fig <- plot_ly(historyCount, x = ~date, y = ~confirmed, name = 'confirmed', type = 'scatter', mode = 'lines+markers') %>%
    add_trace(y = ~active, name = 'active', mode = 'lines+markers') %>%
    add_trace(y = ~recovered, name = 'recovered', mode = 'lines+markers') %>%
    add_trace(y = ~new, name = 'new', mode = 'lines+markers') %>%
    add_trace(y = ~deaths, name = 'deaths', mode = 'lines+markers')

fig <- fig %>%
    layout(yaxis = list(title = "counts"),
        updatemenus = list(list(
               active = 0,
               buttons= list(
                   list(label = 'linear',
                        method = 'update',
                        args = list(list(visible = c(T,T,T,T,T)),
                                    list(yaxis = list(type = 'linear')))),
                   list(label = 'log',
                        method = 'update', 
                        args = list(list(visible = c(T,T,T,T,T)),
                                    list(yaxis = list(type = 'log'))))))))
