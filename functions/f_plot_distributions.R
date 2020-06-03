f_plot_distributions <- function(data, alpha, beta) {

  chart_data <- data %>%
    crossing(tibble(x = seq(0,1,.0001))) %>% 
    mutate(
      d = dbeta(x, successes+alpha, failure+beta),
      lower = qbeta(.001, successes+alpha, failure+beta),
      upper = qbeta(.999, successes+alpha, failure+beta),
      z = qbeta(.5, successes+alpha, failure+beta)
    ) %>% 
    group_by(variation) %>%
    filter(x >= lower, x <= upper) %>%
    mutate(max_d = max(d)) %>%
    ungroup()
  
  rates <- chart_data %>%
    group_by(variation) %>%
    summarize(
      rate = mean(z),
      max_d = max(d)
    )
  
  plot <- chart_data %>%
    ggplot(
      aes(
        x = x,
        y = d, 
        fill = variation,
        color = variation
      )
    ) + 
    geom_line(alpha = .8) +
    theme(
      legend.position = 'None',
      axis.text.y = element_blank(),
      axis.title = element_blank()
    ) +
    scale_x_continuous(
      labels = scales::percent, 
      breaks = round(
        seq(from=min(chart_data$x), to=max(chart_data$x), by = (max(chart_data$x)-min(chart_data$x))/10),
        3
      )
    )
  plot <- ggplotly(plot) %>%
    layout(legend = list(orientation = "v", yanchor = 'top')) %>%
    style(hoverinfo = "none", traces = 2:3)
  
  return(plot)
}