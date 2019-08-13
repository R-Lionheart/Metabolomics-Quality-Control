csvFileInput <- function(id, label = "CSV file") {
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), label),
    checkboxInput(ns("heading"), "Has header", value = TRUE)
  )
}

promptForFile <- function(input, output, session) {
  return(reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  }))
}

# Module server function
csvFile <- function(input, output, session, stringsAsFactors) {
  userFile <- promptForFile(input, output, session)
  
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    read.csv(userFile()$datapath,
             header = input$heading,
             stringsAsFactors = stringsAsFactors)
  })
  
  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })
  
  # Return the reactive that yields the data frame
  return(dataframe)
  
}



