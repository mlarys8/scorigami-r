library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(rvest)
library(stringr)

#get the data
url <- "https://pro-football-reference.com/boxscores/game-scores.htm"
nfl_data <- {
  read_html(url) %>%
    html_node("table") %>%
    html_table() %>%
    select(
      win_score = PtsW,
      lose_score = PtsL,
      num_games = Count,
      latest_game = 'Last Game',
    )
}

nfl_data$latest_game_year <- word(nfl_data$latest_game, -1)
nfl_data$latest_game_year <- as.numeric(nfl_data$latest_game_year)

#df of impossible NFL scores
impossible <- tibble(
  lose_score <- c(0, 1, 1, 1, 1, 1, 1),
  win_score <- c(1, 1, 2, 3, 4, 5, 7)
)

#add impossible scores based on win-loss relationship
for(i in 0:1000){
  temp <- tibble(
    win_score = i,
    lose_score = (i+1):100
  )
  impossible <- bind_rows(impossible,temp)
}

#set plot limits
mwin <- max(nfl_data$win_score) + 0.5
mloss <- max(nfl_data$lose_score) + 0.5

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      h2("NFL Scorigami"),
      tableOutput("data"),
      radioButtons("radio", label = "", choices = c("Gradient Off", "Gradient On"), selected = "Gradient Off"),
      selectInput("select",label = "", choices = c("Count", "Latest Game"), selected = "Count")
      ),
    mainPanel(
      plotOutput("plot", click = "plot_click")
    )
  )
  
  
  
)

server <- function(input, output){
  output$plot <- renderPlot({
    if (input$radio == "Gradient Off" & input$select == "Count"){
      ggplot(data = nfl_data) +
        aes(x = win_score, y = lose_score) + 
        geom_tile(data = impossible, color = "black") + 
        geom_tile(color = "darkgreen", fill = "chartreuse4") + 
        geom_text(aes(label=num_games), color = "white", size = 3) +
        coord_fixed(xlim = c(-0.5, mwin),ylim = c(mloss, -0.5),expand = FALSE) +
        scale_x_continuous(breaks = 0:100,minor_breaks = NULL,sec.axis = dup_axis()) +
        scale_y_continuous(breaks = 0:100,minor_breaks = NULL,sec.axis = dup_axis()) +
        theme_classic(base_line_size = 0) +
        theme(axis.text = element_text(size = 12),plot.caption = element_text(size = 16, color = "grey"),axis.title = element_text(size = 18)) + 
        labs(x = "Winning (or Tying) Score", y = "Losing Score")
    } else if (input$radio == "Gradient On" & input$select == "Count"){
      ggplot(data = nfl_data) +
        aes(x = win_score, y = lose_score) + 
        geom_tile(data = impossible, color = "black") + 
        geom_tile(aes(fill=num_games)) + 
        scale_fill_distiller(palette = "Spectral") +
        geom_text(aes(label=num_games), color = "white", size = 3) +
        coord_fixed(xlim = c(-0.5, mwin),ylim = c(mloss, -0.5),expand = FALSE) +
        scale_x_continuous(breaks = 0:100,minor_breaks = NULL,sec.axis = dup_axis()) +
        scale_y_continuous(breaks = 0:100,minor_breaks = NULL,sec.axis = dup_axis()) +
        theme_classic(base_line_size = 0) +
        theme(axis.text = element_text(size = 12),plot.caption = element_text(size = 16, color = "grey"),axis.title = element_text(size = 18)) + 
        labs(x = "Winning (or Tying) Score", y = "Losing Score")
    } else if (input$radio == "Gradient Off" & input$select == "Latest Game"){
      ggplot(data = nfl_data) +
        aes(x = win_score, y = lose_score) + 
        geom_tile(data = impossible, color = "black") + 
        geom_tile(color = "darkgreen", fill = "chartreuse4") +  
        geom_text(aes(label=latest_game_year), color = "white", size = 2.25) +
        coord_fixed(xlim = c(-0.5, mwin),ylim = c(mloss, -0.5),expand = FALSE) +
        scale_x_continuous(breaks = 0:100,minor_breaks = NULL,sec.axis = dup_axis()) +
        scale_y_continuous(breaks = 0:100,minor_breaks = NULL,sec.axis = dup_axis()) +
        theme_classic(base_line_size = 0) +
        theme(axis.text = element_text(size = 12),plot.caption = element_text(size = 16, color = "grey"),axis.title = element_text(size = 18)) + 
        labs(x = "Winning (or Tying) Score", y = "Losing Score")
    } else if (input$radio == "Gradient On" & input$select == "Latest Game"){
      ggplot(data = nfl_data) +
        aes(x = win_score, y = lose_score) + 
        geom_tile(data = impossible, color = "black") + 
        geom_tile(aes(fill=latest_game_year)) + 
        scale_fill_distiller(palette = "Spectral") +
        geom_text(aes(label=latest_game_year), color = "white", size = 2.25) +
        coord_fixed(xlim = c(-0.5, mwin),ylim = c(mloss, -0.5),expand = FALSE) +
        scale_x_continuous(breaks = 0:100,minor_breaks = NULL,sec.axis = dup_axis()) +
        scale_y_continuous(breaks = 0:100,minor_breaks = NULL,sec.axis = dup_axis()) +
        theme_classic(base_line_size = 0) +
        theme(axis.text = element_text(size = 12),plot.caption = element_text(size = 16, color = "grey"),axis.title = element_text(size = 18)) + 
        labs(x = "Winning (or Tying) Score", y = "Losing Score")
    }
    },  height = 1080)
  
  output$data <- renderTable({
    nearPoints(nfl_data, input$plot_click) %>%
    rename("Winning Score" = win_score, "Losing Score" = lose_score, "# Occurances" = num_games, "Latest Game" = latest_game, "Latest Year" = latest_game_year)
  })
}

shinyApp(ui, server)