### Evergy History Graphic


pacman::p_load(tidyverse, plotly, readxl, ggplot2, stringr, formattable)


# Import the data
kcpl <- read_excel("Evergy History/Evergy History Timelines.xlsx", sheet = "KCP&L")
wstr <- read_excel("Evergy History/Evergy History Timelines.xlsx", sheet = "Westar")

# Combine two datasets
df <- bind_rows(kcpl, wstr)

# Plot the interactive data
fig <- plot_ly(
  df,
  x = ~Year,
  y = ~`# Customers`,
  type = "scatter",
  mode = "lines",
  fill = "tozeroy",
  text = paste0(df$`New Name`,
               "<br>Year: ", df$Year,
               "<br>Customers: ", comma(df$`# Customers`, format = "d"),
               "<br>", df$`Other Notable Events`),
  hoverinfo = 'text',
  transforms = list(
    list(
      type = 'groupby',
      groups = df$Company,
      styles = list(
        list(target = "KCPL", value = list(line =list(color = 'green'),
                                           fillcolor = list(color = 'green'),
                                           marker = list(color = 'green'))),
        list(target = "Westar Energy", value = list(line =list(color = 'skyblue'),
                                                    fillcolor = list(color = 'skyblue'),
                                                    marker = list(color = 'skyblue')))
      )
    )
  )
) %>% 
  layout(showlegend = TRUE,
         title = "Evergy History Timeline",
         yaxis = list(title = "Number of Customers"),
         xaxis = list(nticks = 20))

fig




