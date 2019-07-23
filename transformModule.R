# Module UI function
variableTransformUI <- function(id, label = "transform.button") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    actionButton(ns("transform"), label),
    checkboxInput(ns("list.variables"), "List variables")
    )
}



# Module server function
variableTransform <- function(input, output, session) {
  # The selected file, if any
  userFile <- data1
  
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    read.csv(userFile()$datapath,
             header = input$heading,
             quote = input$quote,
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