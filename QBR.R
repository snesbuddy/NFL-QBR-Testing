library(espnscrapeR)
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(ggimage)

weeks <- (1:18)
i <- 0

for (i in weeks){
assign(paste0("qbr",i), get_nfl_qbr(season = "2021", week = i) )
}

qbrList <- mget(ls(pattern = "qbr*"))
qbr <- bind_rows(qbrList)

teams <- get_nfl_teams()
teams <- within(teams, team_color[team_name == "Falcons"] <- "#A71930")

qbr <- merge(qbr, teams, by = "team_abb")


ui <- fluidPage(
  title = "NFL QBR, 2020",
  titlePanel(h1("Weekly QBR By Team, NFL 2020 Season", style = 'text-align: center ; color:white')),
  setBackgroundImage( src = "https://i.kym-cdn.com/photos/images/newsfeed/001/207/210/b22.jpg"),
  selectInput("week", HTML("<p><span style='color: white'>Week</span></p>"), choices = sort(unique(qbr$game_week))),
  plotOutput("qbrPlot", height = 1000, width = 1800)
)

server <- function(input, output, session){
  week <- reactive({
    subset(qbr,game_week == input$week )
  })
  output$qbrPlot <- renderPlot({
    ggplot(week(), aes(reorder(name_display, -qbr_total) , qbr_total)) +
      ggtitle(paste0("QBR By Player, Week ", input$week, " 2021 NFL Season")) +
      geom_col(colour = paste0(week()$team_alt_color), fill = paste0(week()$team_color), width = 0.5) +
      geom_image(aes(image = week()$headshot_href) , size = 0.05, by = "height", nudge_y = 1) +
      labs(x = "", y = "QBR") +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, max(week()$qbr_total) * 1.05))
  }, res = 0.96)

}


shinyApp(ui, server)


