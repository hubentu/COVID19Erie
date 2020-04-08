#' Plot leaflet Map
#'
#' @import leaflet
#' @import sp
#' @importFrom htmltools HTML
#' @export
caseMap <- function(Counts, pubExposed, titlePos = c(10, 10)){
    counts <- Counts$counts
    fullName <- paste(counts$town, "town")
    fullName[fullName == "Buffalo town"] <- "Buffalo city"
    fullName[fullName == "Lackawanna town"] <- "Lackawanna city"
    fullName[fullName == "City of Tonawanda town"] <- "Tonawanda city"
    fullName[fullName == "Cattaraugus Indian Reservation town"] <- "Cattaraugus Reservation"
    
    ## idx <- match(fullName, shapeData@data$NAMELSAD)
    ## dat <- shapeData[idx, ]
    ## dat@data <- cbind(dat@data, counts)
    ## dat@data <- dat@data %>% inner_join(counts, by = c("NAME" = "town"))
    idx <- shapeData@data$NAMELSAD %in% fullName
    dat <- shapeData[idx, ]
    dat@data <- cbind(dat@data, counts[match(dat@data$NAMELSAD, fullName),])
    
    labs <- mapply(function(n, x, y, z){
        HTML(paste0(n, "<br>",
                    "confirmed: ", x, "<br>",
                    "recovered: ", y, "<br>",
                    "deaths: ", z, "<br>",
                    "updated: ", gsub("\\(updated |\\)", "", attributes(counts)$update.time)))
    }, dat@data$town, dat@data$confirmed, dat@data$recovered, dat@data$deaths,
    SIMPLIFY = FALSE, USE.NAMES = FALSE)
    
    bins <- c(1, 5, 10, 20, 50, 100, 500, Inf)
    pal <- colorBin("YlOrRd", domain = dat@data$confirmed, bins = bins)

    idx <- match(Counts$zipCounts$zip, zipDataBuf@data$ZCTA5CE10)
    dat1 <- zipDataBuf[na.omit(idx),]
    dat1@data$confirmed <- Counts$zipCounts$confirmed[!is.na(idx)]
    lab1 <- mapply(function(n, x){
        HTML(paste0(n, "<br>",
                    "confirmed: ", x, "<br>",
                    "updated: ", gsub("\\(updated |\\)", "", attributes(counts)$update.time)))
    }, dat1@data$ZCTA5CE10, dat1@data$confirmed, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    bins <- c(1, 5, 10, 20, 50, 100, 500, Inf)
    pal1 <- colorBin("YlOrRd", domain = dat1@data$confirmed, bins = bins)
    
    lf <- leaflet(dat)  %>% addTiles() %>%
        setView(-78.8, 42.8, 10) %>%
        addPolygons(
            fillColor = ~pal(confirmed),
            col = 'white',
            dashArray = "3",
            weight = 2,
            label = labs,
            fillOpacity = 0.5,
            group = "Town Counts") %>%
        addLegend("bottomright", pal = pal, values = ~confirmed,
                  title = "confirmed") %>%
        addMarkers(pubExposed$lon, pubExposed$lat,
                   label = pubExposed$notes,
                   group = "Public Advisories") %>%
        addPolygons(
            data = dat1,
            fillColor = ~pal1(confirmed),
            col = 'white',
            dashArray = "3",
            weight = 2,
            label = lab1,
            fillOpacity = 0.5,
            group = "Zip Counts") %>%
        addLayersControl(
            baseGroups = c("Zip Counts", "Town Counts"),
            overlayGroups =c("Public Advisories"),
            options = layersControlOptions(collapsed=FALSE)
        ) %>%
        hideGroup("Public Advisories")
    
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
