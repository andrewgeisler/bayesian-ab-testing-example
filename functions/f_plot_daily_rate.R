f_plot_daily_rate <- function(df, alpha, beta) {
  
  library(tidyverse)
  library(plotly)
  
  df_cume <- df %>%
    group_by(variation) %>%
    arrange(variation, date) %>%
    mutate(
      trials = cumsum(trials)+successes+beta,
      successes = cumsum(successes)+alpha, 
      failures = trials - successes, 
      rate = successes/trials
    )
  
  ylims <- df_cume %>%
    ungroup() %>%
    summarize(ymin=min(rate)*.95, ymax=max(rate)*1.05)
  
  plot <- ggplot(df_cume, aes(x=date, y=rate, color = variation)) +
    geom_line(alpha = .7) +
    geom_point(size=1) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_date(breaks = '1 days', date_labels = '%m-%d') +
    labs(x='', y='')
  
  plot <- ggplotly(plot) %>%
    layout(legend = list(orientation = "v", yanchor = 'top')) %>%
    style(hoverinfo = "none", traces = 2:3)
  
  return(plot)
  
}
