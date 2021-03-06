
dfSentiment <- left_join(sentiment, df)
dfSentiment


ui <- shinyUI(navbarPage(theme = shinytheme("slate"),"Let's recommend a song for you!",
                         tabPanel("Popularity",
                                  sidebarPanel(
                                    # Genre Selection
                                    
                                    selectInput(inputId = "Columns", label = "Select Genre",
                                                unique(dfSentiment$grouped_genre), multiple = FALSE),
                                    verbatimTextOutput("pop"),
                                    
                                    sliderInput(inputId = "range", label = "Popularity",
                                                min = min(dfSentiment$pop),max = 100,value = c(55,100))
                                  ),
                                  mainPanel(
                                    h2("Top songs of the genre"),
                                    DT::dataTableOutput(outputId = "songsreco")
                                  )
                         ),
                         tabPanel("Sentiment",
                                  sidebarPanel(selectInput(inputId = "Columns", label = "Select Genre",
                                                           unique(dfSentiment$grouped_genre), multiple = FALSE),
                                               verbatimTextOutput("sentiment"),
                                               
                                               sliderInput(inputId = "range_2", label = "sentiment",
                                                           min = min(dfSentiment$afinn),max = max(dfSentiment$afinn),value = c(55,303))),
                                  mainPanel(
                                    h2("Top songs of the genre"),
                                    DT::dataTableOutput(outputId = "songsreco2")))))

#server <- function(input, output) {}

#

##recommendation engine server logic##

server <- function(input, output) {
  
  datasetInput <- reactive({
    
    # Filtering based on genre and rating
    dfSentiment %>% filter(grouped_genre %in% as.vector(input$Columns)) %>%
      group_by(title) %>% filter(pop >= as.numeric(input$range[1]), pop <= as.numeric(input$range[2])) %>%
      arrange(desc(pop)) %>%
      select(title, artist, pop, grouped_genre) %>%
      rename(`title` = title, `Genre(s)` = grouped_genre)
    
    
  })
  
  datasetInput2 <- reactive({
    
    # Filtering based on genre and sentiment
    dfSentiment %>% filter(grouped_genre %in% as.vector(input$Columns)) %>%
      group_by(title) %>% filter(afinn >= as.numeric(input$range[1]), afinn <= as.numeric(input$range[2])) %>%
      arrange(desc(afinn)) %>%
      select(title, artist, afinn, grouped_genre) %>%
      rename(`title` = title, `Genre(s)` = grouped_genre)
    
    
  })
  
  
  #Rendering the table
  output$songsreco <- DT::renderDataTable({
    
    DT::datatable(head(datasetInput(), n = 50), escape = FALSE, options = list(scrollX = '1000px'))
  })
  
  output$songsreco2 <- DT::renderDataTable({
    
    DT::datatable(head(datasetInput(), n = 50), escape = FALSE, options = list(scrollX = '1000px'))
  })
  
  
  output$songsreco_artist <- DT::renderDataTable({
    
    DT::datatable(head(datasetInput2(), n = 100), escape = FALSE, options = list(scrollX = '1000px'))
  })
}


shinyApp(ui = ui, server = server)



