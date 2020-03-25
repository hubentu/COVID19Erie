#' Case Counts
#'
#' @import webdriver
#' @export
caseCounts <- function(){
    pjs <- run_phantomjs()
    ses <- Session$new(port = pjs$port)
    ses$go("https://erieny.maps.arcgis.com/apps/opsdashboard/index.html#/dd7f1c0c352e4192ab162a1dfadc58e1")
    Sys.sleep(5)
    recovered <- ses$findElement("#ember20")$getText()
    deaths <- ses$findElement("#ember43")$getText() # update to 43
    confirmed <- ses$findElement("#ember62")$getText() # 62
    updateT <- ses$findElement("#ember10")$getText()
    updateT <- strsplit(updateT, split = "\n")[[1]][2]
    
    town <- strsplit(confirmed, split = "\n")[[1]]
    len <- length(town) - 1
    town <- town[(seq(2, len, 2))]
    
    counts <- lapply(list(confirmed, recovered, deaths), function(x){
        count1 <- strsplit(x, split = "\n")[[1]]
        count1 <- data.frame(
            town = count1[seq(2, len, 2)],
            count = sub(" .*", "", count1[seq(1, len, 2)]),
            stringsAsFactors = FALSE)
        as.integer(count1$count[match(town, count1$town)])
    })
    counts <- data.frame(sub("\\/.*", "", town), do.call(cbind, counts),
                         stringsAsFactors = FALSE)
    colnames(counts) <- c("town", "confirmed", "recovered", "deaths")
    counts[is.na(counts)] <- 0
    attributes(counts)$update.time <- updateT
    return(counts)
}
