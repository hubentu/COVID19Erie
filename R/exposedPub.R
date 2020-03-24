#' public exposed
#'
#' @importFrom xml2 read_html
#' @import rvest
#' @importFrom ggmap geocode
#' @export
exposedPub <- function(google = TRUE){
    exposed <- read_html("http://www2.erie.gov/health/index.php?q=public-advisories")
    places <- exposed %>% html_nodes(".content p")
    places <- as.character(places[grep("/2020", places)])
    places <- unlist(strsplit(gsub("<p>|</p>", "", places), split = "<br>"))
    places <- places[places != ""]
    places <- sub("2020,", "2020:", places)
    addr <- sub(".*: |.*m., ", "", places)
    addr[grep("Flight", addr)] <- "Buffalo Niagara International Airport"
    addr[grep("Hotel Henry", addr)] <- "Hotel Henry, Buffalo"
    if(google){
        pubExposed <- geocode(addr)
        pubExposed$notes <- places
    }else{
        pubExposed <- places
    }
    return(pubExposed)
}
