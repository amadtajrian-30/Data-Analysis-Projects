install.packages("shiny", dependencies = TRUE)
library(ggplot2)

pa_clean
pa_clean$week_start <- as.Date(pa_clean$week_start)
latest_week <- pa_clean[nrow(pa_clean), ]
prev_week <- pa_clean[nrow(pa_clean) - 1, ]

change <- latest_week$total_positive - prev_week$total_positive


ui <- fluidPage(
  titlePanel(
    "Pennsylvania Influenza Surveillance Dashboard (October-December 2025)",
    windowTitle = "PA Flu Dashboard"),
  tabsetPanel(
    tabPanel(
      "Overview",
      fluidRow(
        column(
          4,
          h4("Latest confirmed cases (week 52)"),
          h3(latest_week$total_positive)
        ),
        column(
          4,
          h4("Percent positive"),
          h3(paste0(round(latest_week$percent_positive, 1), "%"))
        ),
        column(
          4,
          h4("Week-over-week change in confirmed cases"),
          h3(change)
        )
      ),
      hr(),
      plotOutput("cases_plot")
    ),
    tabPanel("Methods",
             p("Outcome: weekly lab-confirmed influenza-positive specimens (A & B)."),
             p("Counts reflect laboratory-confirmed influenza-positive specimens and are influenced by testing volume and reporting practices."),
             p("Data: CDC FluView virologic surveillance via Delphi Epidata API."))
  )
)

server <- function(input, output, session) {
  output$cases_plot <- renderPlot({
    ggplot(pa_clean, aes(x = week_start, y = total_positive)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(x = "Month", y = "Confirmed Influenza A & B positives") +
      theme_minimal()
  })
}

shinyApp(ui, server)

