#' combined report
#'
#' @import htmlTable
#' @import manipulateWidget
#' @export
caseReport <- function(counts, pubExposed, historyCount){
    Counts <- rbind(counts,
                    c(town = "Total", colSums(counts[,-1])))
    chtml <- htmlTable(Counts,
                   caption = txtMergeLines("COVID19 cases (Erie county)",
                                           paste("Last updated:", attributes(counts)$update.time),
                                           "<a href='https://erieny.maps.arcgis.com/apps/opsdashboard/index.html#/dd7f1c0c352e4192ab162a1dfadc58e1'>Data Source</a>"),
                   tfoot = "<a href='http://www2.erie.gov/health/index.php?q=public-advisories'>Public Advisories</a>",
                   col.rgroup = rep(c("none", "yellow"), c(nrow(Counts)-1, 1)))

    lf <- caseMap(counts, pubExposed)
    pl <- countPlot(historyCount)
    combineWidgets(lf, combineWidgets(chtml, pl, nrow = 2, rowsize = c(3, 2)),
               ncol = 2, colsize = c(2, 1))
}
