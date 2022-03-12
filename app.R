library(dash)
# library(dashHtmlComponents)
library(tidyverse)
library(plotly)

app <- Dash$new(external_stylesheets = dbcThemes$DARKLY)

df <- read_csv("data/processed/processed.csv")

selected_genres <- as.list(unique(df$genres))

app$layout(
  dbcContainer(
    list(
      htmlP("Select Movie/ TV Show genres"),
      dccDropdown(
        id = "cat",
        options = selected_genres,
        value = "International",
        multi = T,
        style = list("color" = "black")
      ),
      htmlBr(),
      htmlH1("Top 10 Directors in Terms of Number of Content"),
      dccGraph(id = "plot_directors")
    )
  )
)

app$callback(
  output("plot_directors", "figure"),
  list(input("cat", "value")),
  function(cat) {
    plot_df <- df %>%
      filter(director != "Missing") %>%
      filter(genres %in% cat) %>%
      group_by(director) %>%
      summarise(Count = n_distinct(show_id))

    chart <- plot_df %>%
      arrange(desc(Count)) %>%
      slice(1:10) %>%
      ggplot() +
      geom_bar(aes(x = Count, y = reorder(director, Count)), stat = "identity", fill = "#b20710") +
      ylab("") +
      xlab("Number of Movies + TV shows") +
      theme_bw()

    ggplotly(chart, tooltip = "Count")
  }
)

app$run_server(host = "0.0.0.0")
