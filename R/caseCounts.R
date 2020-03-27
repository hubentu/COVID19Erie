#' Case Counts
#'
#' @import webdriver
#' @export
caseCounts <- function(){
    pjs <- run_phantomjs()
    ses <- Session$new(port = pjs$port)
    ses$go("https://erieny.maps.arcgis.com/apps/opsdashboard/index.html#/dd7f1c0c352e4192ab162a1dfadc58e1")
    Sys.sleep(10)
    ## contents <- ses$findElement("#ember6")$getText()
    contents <- ses$getActiveElement()$getText()
    
    ## dat <- strsplit(ses$findElement("#ember20")$getText()[[1]], split = "\n")[[1]]
    dat <- strsplit(contents, split = "\n")[[1]]
    t_active <- as.integer(dat[grep("Active Cases", dat) + 1])
    t_recovered <- as.integer(dat[grep("Recovered", dat) + 1])
    t_deaths <- as.integer(dat[grep("Total Deaths", dat) + 1])
    t_confirmed <- as.integer(dat[grep("Total Confirmed", dat) + 1])
    
    confirmed <- dat[(grep("Total Confirmed", dat) + 3):(length(dat)-1)]
    confirmed <- data.frame(
        town = sub("\\/.*", "", confirmed[seq(2, length(confirmed), 2)]),
        confirmed = as.integer(sub(" .*", "", confirmed[seq(1, length(confirmed), 2)])),
        stringsAsFactors = FALSE)
    counts <- data.frame(confirmed, recovered = NA, deaths = NA)
    
    ## updateT <- ses$findElement("#ember10")$getText()
    ## updateT <- strsplit(updateT, split = "\n")[[1]][2]
    updateT <- dat[grep("updated", dat)]
    
    ## recovered <- ses$findElement("#ember20")$getText()
    ## deaths <- ses$findElement("#ember43")$getText() # update to 43
    ## confirmed <- ses$findElement("#ember60")$getText() # 60

    ## ## total
    ## t_recovered <- ses$findElement("#ember80")$getText()
    ## t_deaths <- ses$findElement("#ember82")$getText()
    ## t_confirmed <- ses$findElement("#ember84")$getText() # 62
    
    ## town <- strsplit(confirmed, split = "\n")[[1]]
    ## len <- length(town) - 1
    ## town <- town[(seq(2, len, 2))]
    
    ## counts <- lapply(list(confirmed, recovered, deaths), function(x){
    ##     count1 <- strsplit(x, split = "\n")[[1]]
    ##     count1 <- data.frame(
    ##         town = count1[seq(2, len, 2)],
    ##         count = sub(" .*", "", count1[seq(1, len, 2)]),
    ##         stringsAsFactors = FALSE)
    ##     as.integer(count1$count[match(town, count1$town)])
    ## })
    ## counts <- data.frame(sub("\\/.*", "", town), do.call(cbind, counts),
    ##                      stringsAsFactors = FALSE)
    ## colnames(counts) <- c("town", "confirmed", "recovered", "deaths")
    ## counts[is.na(counts)] <- 0

    attributes(counts)$update.time <- updateT
    attributes(counts)$total.recovered <- t_recovered
    attributes(counts)$total.confirmed <- t_confirmed
    attributes(counts)$total.deaths <- t_deaths
    attributes(counts)$active.cases <- t_active
    
    return(counts)
}
