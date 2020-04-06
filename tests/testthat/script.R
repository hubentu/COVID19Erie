
library(leaflet)
library(rgdal)
library(dplyr)
library(htmltools)
library(ggmap)
library(rvest)
## devtools::install_github("lchiffon/leafletCN")
## library(leafletCN)
library(webdriver)

## install_phantomjs()
pjs <- run_phantomjs()
ses <- Session$new(port = pjs$port)
ses$go("https://erieny.maps.arcgis.com/apps/opsdashboard/index.html#/dd7f1c0c352e4192ab162a1dfadc58e1")
Sys.sleep(5)
recovered <- ses$findElement("#ember20")$getText()
deaths <- ses$findElement("#ember33")$getText()
confirmed <- ses$findElement("#ember52")$getText()
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
attributes(counts)$update.time <- updateT

## public exposed
exposed <- read_html("http://www2.erie.gov/health/index.php?q=public-advisories")
places <- exposed %>% html_nodes(".content p")
places <- as.character(places[-(1:3)])
places <- unlist(strsplit(gsub("<p>|</p>", "", places), split = "<br>"))
places <- places[places != ""]
places <- sub("2020,", "2020:", places)

addr <- sub(".*: |.*m., ", "", places)
addr[c(1, 7, 8, 26)] <- "Buffalo Niagara International Airport"
addr[20] <- "Hotel Henry, Buffalo"
pubExposed <- geocode(addr)
pubExposed$notes <- places
use_data(pubExposed)

## https://catalog.data.gov/dataset/tiger-line-shapefile-2013-state-new-york-current-county-subdivision-state-based
shapeData <- readOGR("/home/hq/Workspace/COVID19_Erie/shape")
shapeData <- spTransform(shapeData, CRS("+proj=longlat +ellps=GRS80"))
save_data(shapeData)

## https://catalog.data.gov/dataset/tiger-line-shapefile-2019-2010-nation-u-s-2010-census-5-digit-zip-code-tabulation-area-zcta5-na
zipData <- readOGR("~/Downloads/tl_2019_us_zcta510")
zipData <- spTransform(zipData, CRS("+proj=longlat +ellps=GRS80"))
head(zipData@data)
zipDataBuf <- zipData[grep("^14", zipData@data$ZCTA5CE10),]

## Leaflet
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
                "updated: ", Sys.Date()))
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
        fillOpacity = 0.5) %>%
    addLegend("bottomright", pal = pal, values = ~confirmed,
              title = "confirmed") %>%    
    addTitle(paste("COVID19 cases (Erie county)<br>",
                        "Confirmed Count:", sum(counts$confirmed), "<br>",
                        "Recovered Count", sum(counts$recovered), "<br>",
                        "Deaths Count:", sum(counts$deaths), "<br>"),
             fontSize = "14px",
             leftPosition = 50) %>%
    addMarkers(pubExposed$lon, pubExposed$lat,
               label = pubExposed$notes)

library(htmlwidgets)
library(manipulateWidget)

Counts <- rbind(counts,
                c(town = "Total", colSums(counts[,-1])))
chtml <- htmlTable(Counts,
                   caption = txtMergeLines("COVID19 cases (Erie county)",
                                            paste("Last updated:", attributes(counts)$update.time),
                                           "<a href='https://erieny.maps.arcgis.com/apps/opsdashboard/index.html#/dd7f1c0c352e4192ab162a1dfadc58e1'>Data Source</a>"),
                   tfoot = "<a href='http://www2.erie.gov/health/index.php?q=public-advisories'>Public Advisories</a>",
                   col.rgroup = rep(c("none", "yellow"), c(nrow(Counts)-1, 1)))

combineWidgets(lf, chtml, ncol=2, colsize = c(3, 1))

##
historyCount <- data.frame(date = c("2020-03-14",
                                    "2020-03-15",
                                    "2020-03-16",
                                    "2020-03-17",
                                    "2020-03-18",
                                    "2020-03-19",
                                    "2020-03-20",
                                    "2020-03-21",
                                    "2020-03-22"),
                           confirmed = c(3, 7, 7, 20, 27, 29, 47, 56, 64),
                           recovered = 0,
                           deaths = 0)
library(tidyr)
library(forcats)
hcounts <- historyCount %>% pivot_longer(-1, names_to="group", values_to="count") %>%
    mutate(group = fct_relevel(group, "confirmed", "recovered", "deaths"))

library(plotly)
p <- ggplot(hcounts, aes(x = date, y = count, group = group, colour = group)) +
    geom_line() + geom_point() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

pl <- ggplotly(p)

combineWidgets(lf, combineWidgets(chtml, pl, nrow = 2, rowsize = c(3, 2)),
               ncol = 2, colsize = c(2, 1))

##
library(flexdashboard)

##
dat1 <- zipDataBuf[match(Counts$zipCounts$zip, zipDataBuf@data$ZCTA5CE10),]
dat1@data$confirmed <- Counts$zipCounts$confirmed

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
    ) %>%
    hideGroup("Public Advisories") %>%
    addPolygons(
        map = dat1,
        fillColor = ~pal(confirmed),
        col = 'white',
        dashArray = "3",
        weight = 2,
        label = labs,
        fillOpacity = 0.5,
        group = "Zipcode Counts"
    ) %>%
    hideGroup("Zipcode Counts")


lab1 <- mapply(function(n, x){
    HTML(paste0(n, "<br>",
                "confirmed: ", x, "<br>",
                "updated: ", gsub("\\(updated |\\)", "", attributes(counts)$update.time)))
}, dat1@data$ZCTA5CE10, dat1@data$confirmed, SIMPLIFY = FALSE, USE.NAMES = FALSE)
bins <- c(1, 5, 10, 20, 50, 100, 500, Inf)
pal1 <- colorBin("YlOrRd", domain = dat1@data$confirmed, bins = bins)
leaflet(dat)  %>% addTiles() %>%
    setView(-78.8, 42.8, 10) %>%
    addPolygons(
        data = dat1,
        fillColor = ~pal1(confirmed),
        col = 'white',
        dashArray = "3",
        weight = 2,
        label = lab1,
        fillOpacity = 0.5,
        group = "Zip Counts") %>%
    addPolygons(
        fillColor = ~pal(confirmed),
        col = 'white',
        dashArray = "3",
        weight = 2,
        label = labs,
        fillOpacity = 0.5,
        group = "Town Counts") %>%
    addLayersControl(
        baseGroups =c("Town Counts", "Zip Counts"),
        options = layersControlOptions(collapsed=FALSE)
    ) %>%

