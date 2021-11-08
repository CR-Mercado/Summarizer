

library(shiny)
source("global.R")

# Define UI for application
ui <- fluidPage(
  tags$style(type='text/css', '#combined_paragraph {white-space: pre-wrap;}'),
    # Application title
    titlePanel("SMMRY - Sentence Ranking Summarizer"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
           textAreaInput(inputId = "txt",
                         label = "Place Text Here", 
                         value = "Enjoy this default sentence. Here's a 2nd one."),
           sliderInput(inputId = "top_percentile", 
                       label = "Keep the top N percentile of sentences",
                       min = 0, max = .99,value = .2, step = .01)
        ),

        # Show outputs 
        mainPanel(
          h4("Your paragraph: "),
           verbatimTextOutput("combined_paragraph"),
           hr(),
          h4("Sentence Rankings"),
          dataTableOutput("sentence_rank_df"),
          hr(), 
          h4("Sentence Score CDF"),
          plotOutput("sentence_cdf")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  txt <- reactive({ 
    clean_txt( input$txt )
    })
  
  observe({
    txt1 <<- input$txt
    })
  
  sentences <- reactive({ 
    get_sentences( txt() )
    })
  
  pure_words <- reactive({ 
    get_pure_words( txt() )
    })
  
  words_ranking <- reactive({
    get_words_ranking( pure_words() )
  })
  
  sentence_ranking <- reactive({ 
    get_sentence_ranking( sentences = sentences(),
                          words_ranking = words_ranking() )
    })
  
  output$combined_paragraph <- renderText({ 
    paste0(
      sentence_ranking()$sentence[
      sentence_ranking()$score_percentile >= (1 - input$top_percentile)],
           collapse = ". ")
    })
  
  output$sentence_rank_df <- renderDataTable({ 
    sentence_ranking()
    })

  output$sentence_cdf <- renderPlot({ 
    plot(ecdf(sentence_ranking()$score),
         xlab = "Sentence Score",
         main = "CDF of Sentence Score")
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
