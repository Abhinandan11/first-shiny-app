library(shiny)
library(ggplot2)

outputDir <- "responses"

# Define the fields we want to save from the form
fields <- c("name", "fav_lang", "exp")

saveData <- function(input) {
  # put variables in a data frame
  data <- data.frame(matrix(nrow=1,ncol=0))
  for (x in fields) {
    var <- input[[x]]
    if (length(var) > 1 ) {
      # handles lists from checkboxGroup and multiple Select
      data[[x]] <- list(var)
    } else {
      # all other data types
      data[[x]] <- var
    }
  }
  data$submit_time <- date()
  
  # Create a unique file name
  fileName <- sprintf(
    "%s_%s.rds", 
    as.integer(Sys.time()), 
    digest::digest(data)
  )
  
  # Write the file to the local system
  saveRDS(
    object = data,
    file = file.path(outputDir, fileName)
  )
}

loadData <- function() {
  # read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  
  if (length(files) == 0) {
    # create empty data frame with correct columns
    field_list <- c(fields, "submit_time")
    data <- data.frame(matrix(ncol = length(field_list), nrow = 0))
    names(data) <- field_list
  } else {
    data <- lapply(files, function(x) readRDS(x)) 
    
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
  }
  
  data
}

deleteData <- function() {
  # Read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  
  lapply(files, file.remove)
}

resetForm <- function(session) {
  # reset values
  updateTextInput(session, "name", value = "")
  updateCheckboxInput(session, "fav_lang", value = FALSE)
  updateSliderInput(session, "exp", value = 0)
}

ui <- fluidPage(
  
  # App title ----
  titlePanel("Data Collection & Feedback"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      textInput("name", "Name", ""),
      select_demo <- selectInput(
        "fav_lang", 
        "My favourite programming language is:", 
        c("", 
          "Python", 
          "C++", 
          "JavaScript", 
          "Java"
        )
      ),
      sliderInput("exp", "My programming experience is",
                  0, 10, 0, ticks = FALSE),
      actionButton("submit", "Submit"),
      actionButton("clear", "Clear Form"),
      downloadButton("downloadData", "Download"),
      actionButton("delete", "Delete All Data")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput(outputId = "yearsPlot"),
      tags$hr(),
      dataTableOutput("responses")
    )
  )
)

server = function(input, output, session) {
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveData(input)
    resetForm(session)
  })
  
  observeEvent(input$clear, {
    resetForm(session)
  })
  
  # When the Delete button is clicked, delete all of the saved data files
  observeEvent(input$delete, {
    deleteData()
  })
  
  # Show the previous responses in a reactive table ----
  output$responses <- renderDataTable({
    # update with current response when Submit or Delete are clicked
    input$submit 
    input$delete
    
    loadData()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      write.csv(loadData(), file, row.names = FALSE, quote= TRUE)
    }
  )
  
  output$yearsPlot <- renderPlot({
    input$submit
    input$delete
    
    data <- loadData()
    
    ggplot(data) +
      geom_histogram(
        aes(exp), 
        binwidth = 1, 
        color = "black", 
        fill = "white"
      ) +
      scale_x_continuous(
        name = "Exp", 
        breaks = 0:10,
        limits = c(-0.5, 10.5)
      ) + 
      theme_minimal() +
      theme(
        text = element_text(family = "Helvetica", size = 20),
        plot.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title.y = element_blank()
      )
  })
}

shinyApp(ui, server)