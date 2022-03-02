library(shiny)
library(odds.converter)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Toy example of the contrarian strategy"),
    sidebarLayout(
        sidebarPanel(
          inputPanel(
            h3("Relative odds to win"),
          # Higher odds are worse chance to win
          # Should force odds to sum to probability of 1,
            # or add "the field" that takes the rest of the prob,
            # or use probs instead of odds and convert the other direction
            # or use "relative odds to win" 
            numericInput(inputId = "Adam_odds", label = "Adam", 
                         min = 1, value = 5, max = 100, step = 1, width = "30%"),
            numericInput(inputId = "Andy_odds", label = "Andy", 
                         min = 1, value = 3, max = 100, step = 1, width = "30%"),
            numericInput(inputId = "Connor_odds", label = "Connor", 
                         min =  1, value = 1, max = 100, step = 1, width = "30%"),
            numericInput(inputId = "Paul_odds", label = "Paul", 
                         min = 1, value = 4, max = 100, step = 1, width = "30%")
        ),
        inputPanel(
          h3("Number times picked"),
          # Higher odds are worse chance to win
          # Should force odds to sum to probability of 1,
          # or add "the field" that takes the rest of the prob,
          # or use probs instead of odds and convert the other direction
          numericInput(inputId = "Adam_pick", label = "Adam", 
                       min = 0, value = 20, max = 100, step = 1, width = "30%"),
          numericInput(inputId = "Andy_pick", label = "Andy", 
                       min = 0, value = 10, max = 100, step = 1, width = "30%"),
          numericInput(inputId = "Connor_pick", label = "Connor", 
                       min = 0, value = 2, max = 100, step = 1, width = "30%"),
          numericInput(inputId = "Paul_pick", label = "Paul", 
                       min = 0, value = 15, max = 100, step = 1, width = "30%")
        )
        ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("OddsPlot"),
           tableOutput("Table")
        )
    )
)

# Define server logic required to draw a histogram
# Add simulation of expected earnings from betting on 
server <- function(input, output) {
  output$OddsPlot <- renderPlot({
       data = data.frame(Golfer = c("Adam", "Andy", "Connor", "Paul"), 
                         Odds = c(input$Adam_odds, input$Andy_odds, 
                                  input$Connor_odds, input$Paul_odds),
                         Chosen = c(input$Adam_pick,input$Andy_pick,
                                    input$Connor_pick,input$Paul_pick)) %>%
         mutate(ProbToWin = Odds/sum(Odds),
                ProbPicked = Chosen/sum(Chosen),
                ProbWinWith = (ProbToWin/(Chosen + 1))/sum((ProbToWin/(Chosen + 1))),
                AmOddsToWin = odds.prob2us(ProbToWin),
                AmOddsOfPicked = odds.prob2us(ProbPicked),
                AmOddsToWinWith = odds.prob2us(ProbWinWith)) %>% 
         pivot_longer(., cols = starts_with("Prob"), 
                      names_to = "Type", values_to = "Probability")
       ggplot(data = data, aes(x = Golfer, y = Probability, fill = Type)) + 
         geom_col(position = "dodge") +
         theme_bw()
       })
  output$Table <- renderTable({
      data = data.frame(Golfer = c("Adam", "Andy", "Connor", "Paul"), 
                        RelativeOdds = c(input$Adam_odds, input$Andy_odds, 
                                 input$Connor_odds, input$Paul_odds),
                        TimesChosen = c(input$Adam_pick,input$Andy_pick,
                                   input$Connor_pick,input$Paul_pick)) %>%
        mutate(ProbToWin = RelativeOdds/sum(RelativeOdds),
               ProbPicked = TimesChosen/sum(TimesChosen),
               ProbWinWith = (ProbToWin/(TimesChosen + 1))/sum((ProbToWin/(TimesChosen + 1))),
               AmOddsToWin = odds.prob2us(ProbToWin)) %>%
        select(Golfer, RelativeOdds,TimesChosen, AmOddsToWin, ProbToWin, ProbWinWith)
      })
}
# Run the application 
shinyApp(ui = ui, server = server)
