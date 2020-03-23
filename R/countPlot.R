#' history cases plot
#'
#' @import dplyr
#' @import tidyr
#' @importFrom forcats fct_relevel
#' @importFrom plotly ggplotly
#' @import ggplot2
#' @export

countPlot <- function(historyCount, update = FALSE){
    if(update){
        if(!as.character(Sys.Date()) %in% historyCount$date){
            counts <- caseCounts()
            tcount <- colSums(counts[,-1])
            historyCount <- rbind(historyCount,
                                  data.frame(date = as.character(Sys.Date()), rbind(tcount)))
        }
    }
    hcounts <- historyCount %>% pivot_longer(-1, names_to="group", values_to="count") %>%
        mutate(group = fct_relevel(group, "confirmed", "recovered", "deaths"))
    p <- ggplot(hcounts, aes(x = date, y = count, group = group, colour = group)) +
        geom_line() + geom_point() +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
}
