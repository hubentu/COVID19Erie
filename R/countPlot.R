#' history cases plot
#'
#' @import dplyr
#' @import tidyr
#' @importFrom forcats fct_relevel
#' @importFrom plotly ggplotly plot_ly
#' @import ggplot2
#' @export

countPlot <- function(historyCount, update = FALSE){
    if(update){
        if(!as.character(Sys.Date()) %in% historyCount$date){
            Counts <- caseCounts()
            counts <- Counts$counts
            ## current <- colSums(counts[,-1])
            attr <- attributes(counts)
            current <- data.frame(date = as.character(Sys.Date()),
                                  rbind(c(attr$total.confirmed,
                                          0,
                                          attr$total.confirmed - attr$total.recovered - attr$total.deaths,
                                          attr$total.recovered,
                                          attr$total.deaths)))
            colnames(current) <- colnames(historyCount)
            historyCount <- rbind(historyCount, current)
            
            historyCount$ycount <- c(0, historyCount$confirmed[-nrow(historyCount)])
            historyCount <- historyCount %>%
                mutate(active = confirmed - recovered - deaths,
                       new = confirmed - ycount) %>%
                select(date, confirmed, new, active, recovered, deaths)
            return(historyCount)
        }else{
            return(historyCount)
        }
    }
    ## historyCount$active <- historyCount$confirmed - historyCount$recovered - historyCount$deaths
    ## historyCount$ycount <- c(0, historyCount$confirmed[-nrow(historyCount)])

    ## hcounts <- historyCount %>%
    ##     mutate(active = confirmed - recovered - deaths,
    ##            new = confirmed - ycount) %>%
    ##     select(-ycount) %>%
    ##     pivot_longer(-1, names_to="group", values_to="count") %>%
    ##     mutate(group = fct_relevel(group, "confirmed", "active", "new", "recovered", "deaths"))

    ## hcounts <- historyCount %>%
    ##             pivot_longer(-1, names_to="group", values_to="count") %>%
    ##             mutate(group = fct_relevel(group, "confirmed", "new", "active", "recovered", "deaths"))
    ## p <- ggplot(hcounts, aes(x = date, y = count, group = group, colour = group)) +
    ##     geom_line() + geom_point() +
    ##     theme_bw() +
    ##     theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ## ggplotly(p)

    fig <- plot_ly(historyCount, x = ~date, y = ~confirmed, name = 'confirmed', type = 'scatter', mode = 'lines+markers') %>%
        add_trace(y = ~active, name = 'active', mode = 'lines+markers') %>%
        add_trace(y = ~recovered, name = 'recovered', mode = 'lines+markers') %>%
        add_trace(y = ~new, name = 'new', mode = 'lines+markers') %>%
        add_trace(y = ~deaths, name = 'deaths', mode = 'lines+markers')
    
    fig <- fig %>%
        layout(yaxis = list(title = "counts"),
               updatemenus = list(list(
                   active = 0,
                   x = 0.1,
                   y = 1,
                   buttons= list(
                       list(label = 'linear',
                            method = 'update',
                            args = list(list(visible = c(T,T,T,T,T)),
                                        list(yaxis = list(type = 'linear')))),
                       list(label = 'log',
                            method = 'update', 
                            args = list(list(visible = c(T,T,T,T,T)),
                                        list(yaxis = list(type = 'log'))))))))
    fig
}
