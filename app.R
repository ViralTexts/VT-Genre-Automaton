#devtools::install_github("daattali/shinyforms")
library(shiny)
library(shinyforms)
library(tidyverse)
library(shinyjs)
library(V8)
# Define the first form: basic information

basicInfoForm <- list(
  storage = list(
    type = STORAGE_TYPES$FLATFILE,
    path = "responses"
  )
)
# vtData <- read_csv("data/genreRank-8-28-17.csv")
# vtData$X1 <- NULL
jscode <- "shinyjs.refresh = function() { history.go(0); }"


server <- function(input, output) {
  # Pick a random row to display
  random <- round(runif(1, 1, nrow(vtData)),digits = 0)
  literaryExplan <- HTML("<h3>What we mean by Literary:</h3><p>In this experiment, literary texts can be poetry or prose e.g. sermons, sketches, vignettes, or essays.</p>")
  advertisementExplan <- HTML("<h3>What we mean by Advertisement:</h3><p>Advertisements are not often easy to distinguish from other kinds of prose in nineteenth century newspapers. A distinguishing factor will often be the mention of health or medicine as many ads are for patent medicines.</p>")
  newsExplan <- HTML("<h3>What we mean by News:</h3><p>A news article in our corpus will typically relate to politics, law, or speeches, and often features a dateline at the start of the article.</p>")
  informationalExplan <- HTML("<h3>What we mean by Informational:</h3><p>Informational pieces can be prose paragraphs or lists that offer facts or statistics from around the world.</p>")
  
  # Render main and sidebar UIs
  output$MainAction <- renderUI( {
    dynamicUi()
  })
  output$sideBar <- renderUI( {
    sidebarUi()
  })
  
  # Main UI
  dynamicUi <- reactive({
    

      div(style = 'overflow-x: scroll; height: 80vh; width: 100%; ', verbatimTextOutput("text"))

       })
  
  # Sidebar UI
  sidebarUi <- reactive({
    div(
      id = "form",
          # Top level selection
          radioButtons("genreYN", label = HTML(paste0("This text is classified as <b style='text-transform:uppercase;'>",vtData[random,3],"</b>.<br/>Does that seem right?")), 
                       choices = list("Yes" = vtData[random,3]$genre_1, "No" = "No", "Not sure" = "IDK")),
          
          # If the genre guess isn't right, show others
          conditionalPanel(
            condition = "input.genreYN == 'No'", radioButtons("survey", "What genre is this piece?", 
                                                              choices <- c(paste0(
                                                                vtData[random,4:ncol(vtData)]),"other" = "other"))),
          # If it's other
      conditionalPanel(
        condition = "input.survey == 'other'",  textInput("other_genre", "How would you classify this text?")),
          
          conditionalPanel(
            condition = "input.genreYN == 'IDK'", radioButtons("unknown", "What makes this difficul to classify?", 
                                                              choices <- list("The OCR is bad" = "bad_OCR", "None of the genres apply" = "no_applicable_genre", "There's a mix of genres" = "mixed_genres"))),
          # Show explanation text if the genre is right
      conditionalPanel(
        condition = "input.genreYN == 'literary'", div(class = "explainBox", literaryExplan)),
      conditionalPanel(
        condition = "input.genreYN == 'advertisement'", div(class = "explainBox", advertisementExplan)),
      conditionalPanel(
        condition = "input.genreYN == 'news'", div(class = "explainBox", newsExplan)),
      conditionalPanel(
        condition = "input.genreYN == 'informational'", div(class = "explainBox", informationalExplan)),
      
          # Show explanation text if the genre is wrong
      conditionalPanel(
        condition = "input.genreYN == 'No' && input.survey == 'literary'", div(class = "explainBox", literaryExplan)),
      conditionalPanel(
        condition = "input.genreYN == 'No' && input.survey == 'advertisement'", div(class = "explainBox", advertisementExplan)),
      conditionalPanel(
        condition = "input.genreYN == 'No' && input.survey == 'news'", div(class = "explainBox", newsExplan)),
      conditionalPanel(
        condition = "input.genreYN == 'No' && input.survey == 'informational'", div(class = "explainBox", informationalExplan)),
      
      actionButton("submit", "Submit") 
    )
        
      

  })
  
    
  # Survey completed page
  
  nextUi <- reactive({
    if (input$genreYN == "No" && input$survey == "other") {
      div(
        h4("Thanks for your feedback!"),
        p(HTML(paste0("This text is labeled <b>", as.character(vtData[random,3]), "</b>, but you classified it as <b>", as.character(input$other_genre),"</b>. We'll review it." ))),
        h5("Care to try another?"),
        actionButton("another", "Sure!")) 
    }
    
    else if (input$genreYN == "No") {
      div(
        h4("Thanks for your feedback!"),
        p(HTML(paste0("This text is labeled <b>", as.character(vtData[random,3]), "</b>, but you classified it as <b>", as.character(input$survey),"</b>. We'll review it." ))),
        h5("Care to try another?"),
        actionButton("another", "Sure!")) 
      }
    else if (input$genreYN == "IDK") {
      div(
        h4("Thanks for your feedback!"),
        p(HTML(paste0("This text is labeled <b>", as.character(vtData[random,3]), "</b>, but you weren't so sure. We'll have to take a closer look." ))),
        h5("Care to try another?"),
        actionButton("another", "Sure!")) 
    }
    
    else {
      
    div(
      h4("Thanks for your feedback!"),
      p(HTML(paste0("You agreed with the classifier and identified this as <b>", as.character(vtData[random,3]),"</b>." ))),
      h5("Care to try another?"),
      actionButton("another", "Sure!")) 
    }
  })

  # Render text
  output$text <- renderText({
    paste(
      vtData[random,2], sep="\n"
    )
  })
  
  #Save data to csv
  saveData <- function(data, storage) {

      saveDataFlatfile(data, storage)

  }
  
  # Function to create csv
  saveDataFlatfile <- function(data, storage) {
    fileName <- paste0(
      paste(
        format(Sys.time(), "%Y%m%d-%H%M%OS"),
        digest::digest(data, algo = "md5"),
        sep = "_"
      ),
      ".csv"
    )
    
    resultsDir <- storage$path
    
    # write out the results
    write.csv(x = data, file = file.path(resultsDir, fileName),
              row.names = FALSE, quote = TRUE)
  }
  
  # Create tbl to export
  
  formData <- reactive({
    data <- random
    data <- c(data, cluster = vtData[random,]$cluster)
    data <- c(data, text = as.character(vtData[random,2]))
    if (input$survey == "other") {
      data <- c(data, genre = as.character(input$other_genre))}
    else if (input$genreYN == "No") {
      data <- c(data, genre = as.character(input$survey))}
    else if (input$genreYN == "IDK") {
      data <- c(data, genre = "unknown")
      data <- c(data, reason = as.character(input$unknown ))}
    else {
      data <- c(data, genre = as.character(vtData[random,3]))}
    data <- c(data, timestamp = as.integer(Sys.time()))
    data <- t(data)
    data
  }) 
  
  # Run save data function and change UI to completed page on submit click
  
  observeEvent(input$submit, {
    saveDataFlatfile(formData(), basicInfoForm$storage)
    output$sideBar <- renderUI( {
      nextUi()
    })
    
  })
  
  # Refresh the app and return to form when user opts to do another
  
  observeEvent(input$another, {
    js$refresh();
    output$MainAction <- renderUI( {
      dynamicUi()
    })
    
  })
  

  
}

# Create the UI

ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(text = jscode),
  tags$head(
    tags$style(HTML("
                    
                    pre {border: none; font-family: Georgia, serif; font-size:16px;line-height:1.5em;}
                    .shiny-input-container:not(.shiny-input-container-inline) {width: 500px;}
                    .well {max-height: 90vh;}

                    .explainBox {background-color: #f5f5f5;border: 1px solid #e3e3e3;border-radius: 4px; width:300px; padding:0 10px 10px 10px; margin: 10px 0;}
                    
                    .container-fluid {width: 80%; height: 100vh}
                    h1 {margin: 20px 0;text-align:center;}
                    "))
  ),
  headerPanel("Viral Texts Genre Automaton, v1.0"),
  sidebarLayout(

    mainPanel(uiOutput("sideBar"), width = 5),    
    sidebarPanel(uiOutput("MainAction"), width = 7)


)
)

shinyApp(ui = ui, server = server)
