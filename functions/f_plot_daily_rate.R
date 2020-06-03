f_plot_daily_rate <- function(df, alpha, beta) {
  
  library(tidyverse)
  library(GoNoodleR)
  library(plotly)
  
  df_cume <- df %>%
    group_by(variation) %>%
    arrange(variation, date) %>%
    mutate(trials = cumsum(trials)+successes+beta,
           successes = cumsum(successes)+alpha, 
           failures = trials - successes, 
           rate = successes/trials)
  
  ylims <- df_cume %>%
    ungroup() %>%
    summarize(ymin=min(rate)*.95, ymax=max(rate)*1.05)
  
  gonoodle_theme()
  
  plot <- ggplot(df_cume, aes(x=date, y=rate, color = variation)) +
    geom_line(alpha = .7) +
    geom_point(size=1) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_date(breaks = '1 days', date_labels = '%m-%d') +
    labs(x='', y='')
  
  plot <- ggplotly(plot) %>%
    layout(legend = list(orientation = "h",   # show entries horizontally
                         xanchor = "center",  # use center of legend as anchor
                         x = 0.5,
                         yanchor = 'bottom'),
           margin = list(b=50)) 
  return(plot)
  
}
