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
    ## contents <- ses$getActiveElement()$getText()
    contents <- ses$findElement(".dashboard-page")$getText()
    ## dat <- strsplit(ses$findElement("#ember20")$getText()[[1]], split = "\n")[[1]]
    dat <- strsplit(contents, split = "\n")[[1]]
    t_active <- as.integer(gsub(",", "", dat[grep("Active Cases", dat)[1] + 1]))
    t_recovered <- as.integer(gsub(",","",dat[grep("Recovered", dat)[1] + 1]))
    t_deaths <- as.integer(gsub(",","",dat[grep("Total Deaths", dat)[1] + 1]))
    t_confirmed <- as.integer(gsub(",", "", dat[grep("Confirmed Cases", dat)[1] + 1]))
    
    ## confirmed <- dat[(grep("Total Confirmed", dat) + 3):(length(dat)-1)]
    idx1 <- grep("[0-9] Confirmed", dat)[1]
    ## idx2 <- tail(grep("[0-9] Confirmed", dat), 1) + 1
    idx2 <- grep("Confirmed Cases by Zip Code", dat) - 1
    confirmed <- dat[idx1:idx2]
    confirmed <- data.frame(
        town = sub("\\/.*", "", confirmed[seq(2, length(confirmed), 2)]),
        confirmed = as.integer(sub(" .*", "", confirmed[seq(1, length(confirmed), 2)])),
        stringsAsFactors = FALSE)
    counts <- data.frame(confirmed, recovered = NA, deaths = NA)
    updateT <- dat[grep("updated", dat)]
    updateT <- paste0(sub("EST .*", "EST", updateT), ")")
    
    attributes(counts)$update.time <- updateT
    attributes(counts)$total.recovered <- t_recovered
    attributes(counts)$total.confirmed <- t_confirmed
    attributes(counts)$total.deaths <- t_deaths
    attributes(counts)$active.cases <- t_active

    ## zip
    dat1 <- dat[(idx2 + 2):length(dat)]
    idx3 <- grep("[0-9] Confirmed", dat1)
    zipCounts <- data.frame(zip=dat1[idx3 - 1],
                            confirmed=as.integer(sub(" Confirmed", "", dat1[idx3])))
    return(list(counts = counts, zipCounts = zipCounts))
}
