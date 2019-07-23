library(shiny)
library(shinyjs)
library(shinythemes)


ui <- fluidPage(useShinyjs(),
  theme = shinytheme("sandstone"),
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),

  # -----------------------------------------------------------------
  titlePanel("Quality Control for the Ingalls Laboratory"),
  
  # -----------------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      wellPanel(id = "tPanel", style = "overflow-y:scroll; max-height: 500",
      helpText("More info here about file input and parameter selection."),
      
      radioButtons("machine.type", h5("Which machine is the output file from?"),
                   choices = list("Xevo TQS" = "TQS", "QExactive (QE)" = "QE"), selected = "TQS"),
      hr(),
      helpText("If you are analyzing files produced by the QE, upload a blank matcher csv here.
               If you are analying files produced by the TQS, upload a master list of compounds here. 
               See Information tab for more details on files."),
      csvFileInput("skyline.file", h5("testing")),
      
      csvFileInput("supporting.file", h5("QE: Blank matcher csv. TQS: Master compound csv.")),
      hr(),
      textInput("std.tags", h5("Standard tag input (QE only)"), 
                value = "Enter samples..."),
      hr(),
      helpText("Pick the minimum height to be counted as a 'real' peak (QE suggestion: HILIC - 1000, Cyano - 5000)"),
      sliderInput("area.min", h5("Area Minimum"), 
                  min = 1000, step = 1000, max = 5000, value = 1000),
      hr(),
      helpText("Pick retention time (RT) flexibility (QE suggestion: +/- 0.4 min for HILIC, +/- 0.2 min for Cyano)"),
      sliderInput("RT.flex", h5("Retention Time Flexibility"),
                  min = 0.0, step = 0.1, max = 1.0, value = 0.2),
      hr(),
      helpText("Pick signal size comparison between sample and blank to merit inclusion (QE suggestion: +/- 0.2)"),
      sliderInput("blank.ratio.max", h5("Blank Ratio Maximum"),
                  min = 0.0, step = 0.1, max = 0.5, value = 0.3),
      hr(),
      helpText("Pick acceptable signal to noise ratio value. Note: broader peaks create more background noise(QE suggestion: 5 for Cyano, 4 for HILIC)"),
      sliderInput("SN.min", h5("Signal to Noise Ratio"),
                  min = 1, step = 1, max = 5, value = 3),
      hr(),
      helpText("Pick an absolute value for a cutoff for parts per million (ppm) (QE suggestion: 7)"),
      sliderInput("ppm.flex", h5("Parts per Million"),
                  min = 1, step = 1, max = 10, value = 5)
      )
    ),
    mainPanel(helpText("wtf"))
  ),
  
  # -----------------------------------------------------------------  
  mainPanel(
    tabsetPanel(type = "tabs",
      tabPanel("Information",
                           
        h3("How can YOU use the Ingalls Lab Quality Control?", align = "center"),
        div(p(HTML(paste0('This code, written in R, performs a user-defined quality-control check on output from the open-source mass spectrometer software ', a(href = 'https://skyline.ms/project/home/software/Skyline/begin.view', "Skyline.")))),
          style = "font-family: 'times'; font-sil6pt"),
        p("The application is split into two sections: targeted and untargeted metabolomic analysis, which can be accessed via the tabs at the top of the page (eventually).
           Within each section, choose between code for Thermo Q Exactive HF (Orbitrap) and a Waters Xevo TQ-S (triple quadrupole) mass spectrometers. The code will clean up your peaks.
           In fact, beneath this paragraph is a lovely visualization of that process.", style = "font-family: 'times'; font-sil6pt"),
        img(src = "QC.png", height = 200, width = 200),
        br(),
        br(),
        h4("LCMS Setup"),
        div(p(HTML(paste0("Samples should be run in the following manner for the quality control code and ", a(href = "https://github.com/IngallsLabUW/B-MIS-normalization", "B-MIS Normalization"), "- a process used for matching internal standards."))),
          style = "font-family: 'times'; font-sil6pt"),
        br(),
        p("Please label all samples in the following manner:", style = "font-family: 'times'; font-sil6pt", span(strong("Date_RunType_AdditionalID (e.g. 161018_Std_FirstStandardinH20)."), 
                                                                    ("RunType refers to whether the sample is a standard (Std), sample (Smp), pooled (poo), or blank (blk)."), 
                                                                    style = "font-family: 'times'; font-sil6pt"),
        p("- Standards run (all mixed) at least once at the beginning and end of the run", style = "font-family: 'times'; font-sil6pt"),
        p("- Standards run (in representative matrix, all mixed) at least once the beginning and end of the run. Example label: 161019_Std_FirstStandardinMatrix", style = "font-family: 'times'; font-sil6pt"),
        p("- Blanks run (preferably method/filter blanks) at least once. Example label: 161018_Blk_FirstBlank", style = "font-family: 'times'; font-sil6pt"),
        p("- A pooled sample run at least three times throughout the run. Example label:161018_Poo_PooledSample_1", style = "font-family: 'times'; font-sil6pt"),
        p("- Samples. Example label: Date_Smp_AdditionalID_Rep", style = "font-family: 'times'; font-sil6pt"))),
    
    tabPanel("Targeted",
      fixedPanel(top = 100, left = 100,  width = 400,
        draggable = TRUE,
        wellPanel(style = "border: 2px dashed black;", em("This window can be dragged around for easier viewing."),
                  strong("Your Quality Control Parameters are:"),   
        textOutput("machine"),
        textOutput("tags"),
        textOutput("minimum"),
        textOutput("retention"),
        textOutput("blank"),
        textOutput("signal"),
        textOutput("ppm"),
        tags$head(tags$style())
        )
      ),
      absolutePanel(style = "border: 2px dashed black;",
        dataTableOutput("data1"),
        dataTableOutput("data2")
      )
    ),

    tabPanel("Untargeted")
    )
  )
)

# -----------------------------------------------------------------
server = function(input, output, session) {
  output$machine <- renderText({paste("Your machine type is", input$machine.type, ".")})
  output$tags <- renderText({paste("Your tags for sample matching are: ", input$std.tags, ".")})
  output$minimum <- renderText({paste("You have selected", input$area.min, "as area.")})
  output$retention <- renderText({paste("You have selected", input$RT.flex, "as retention time flexibility.")})
  output$blank <- renderText({paste("You have selected", input$blank.ratio.max, "as the blank ratio maximum.")})
  output$signal <- renderText({paste("You have selected", input$SN.min, "as signal to noise flexibility.")})
  output$ppm <- renderText({paste("You have selected", input$ppm.flex, "as parts per million time flexibility.")})

  datafile1 <- callModule(csvFile, "skyline.file", stringsAsFactors = FALSE)
  datafile2 <- callModule(csvFile, "supporting.file", stringsAsFactors = FALSE)
  
  output$data1 <- renderDataTable({
    datafile1()
  })
  output$data2 <- renderDataTable({
    datafile2()
  })
  
}

# -----------------------------------------------------------------
shinyApp(ui, server)

