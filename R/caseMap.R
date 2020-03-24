#' Plot leaflet Map
#'
#' @import leaflet
#' @import sp
#' @importFrom htmltools HTML
#' @export
caseMap <- function(counts, pubExposed, titlePos = c(10, 10)){
    fullName <- paste(counts$town, "town")
    fullName[fullName == "Buffalo town"] <- "Buffalo city"
    fullName[fullName == "Lackawanna town"] <- "Lackawanna city"
    idx <- match(fullName, shapeData@data$NAMELSAD)
    dat <- shapeData[idx, ]
    dat@data <- cbind(dat@data, counts)
    ## dat@data <- dat@data %>% inner_join(counts, by = c("NAME" = "town"))
    
    labs <- mapply(function(n, x, y, z){
        HTML(paste0(n, "<br>",
                    "confirmed: ", x, "<br>",
                    "recovered: ", y, "<br>",
                    "deaths: ", z, "<br>",
                    "updated: ", gsub("\\(updated |\\)", "", attributes(counts)$update.time)))
    }, counts$town, counts$confirmed, counts$recovered, counts$deaths,
    SIMPLIFY = FALSE, USE.NAMES = FALSE)
    
    bins <- c(1, 5, 10, 15, 20, 50, 100, Inf)
    pal <- colorBin("YlOrRd", domain = counts$confirmed, bins = bins)
    lf <- leaflet(dat)  %>% addTiles() %>%
        setView(-78.8, 42.8, 10) %>%
        addPolygons(
            fillColor = ~pal(confirmed),
            col = 'white',
            dashArray = "3",
            weight = 2,
            label = labs,
            fillOpacity = 0.5,
            group = "Case Counts") %>%
        addLegend("bottomright", pal = pal, values = ~confirmed,
                  title = "confirmed") %>%
        addMarkers(pubExposed$lon, pubExposed$lat,
                   label = pubExposed$notes,
                   group = "Public Advisories") %>%
        addLayersControl(
            overlayGroups =c("Case Counts", "Public Advisories"),
            options = layersControlOptions(collapsed=FALSE)
        )
    if(!is.null(titlePos)){
        lf <- lf %>% addTitle(paste("COVID19 cases (Erie county)<br>",
                              "Confirmed Count:", sum(counts$confirmed), "<br>",
                              "Recovered Count", sum(counts$recovered), "<br>",
                              "Deaths Count:", sum(counts$deaths), "<br>"),
                        fontSize = "18px",
                 leftPosition = titlePos[1], topPosition = titlePos[2])
    }
    return(lf)
}
