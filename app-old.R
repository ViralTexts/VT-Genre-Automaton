library(shiny)
#library(shinyforms)
library(tidyverse)
library(shinyjs)
source("functions.R")
random <- runif(1, 1, 10)

# Define the first form: basic information
basicInfoForm <- list(
  id = "basicinfo",
  questions = list(
    list(id = vtList$Answer1[random], type = "checkbox", title = vtList$Answer1[random]),
    list(id = vtList$Answer2[random], type = "checkbox", title = vtList$Answer2[random]),
    list(id = vtList$Answer3[random], type = "checkbox", title = vtList$Answer3[random]),
    list(id = vtList$Answer4[random], type = "checkbox", title = vtList$Answer4[random]),
    list(id = vtList$Answer5[random], type = "checkbox", title = vtList$Answer5[random]),
    list(id = vtList$Answer6[random], type = "checkbox", title = vtList$Answer6[random]),
    list(id = vtList$Answer7[random], type = "checkbox", title = vtList$Answer7[random])
  ),
  storage = list(
    type = STORAGE_TYPES$FLATFILE,
    path = "responses",
    id = HTML(paste0(vtList$Text[random]))
  ),
  name = HTML(paste0(vtList$Text[random])),
  password = "shinyforms",
  reset = TRUE
  # validations = list(
  #   
  # )
)



ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    
                    h2 {font-weight:normal; font-size:16px;}
                    
                    "))
    ),
  h1("shinyforms example"),
  tabsetPanel(
    tabPanel(
      "Basic info",
      formUI(basicInfoForm)
    )
  )
)

server <- function(input, output, session) {

  formServer(basicInfoForm)
}

shinyApp(ui = ui, server = server)