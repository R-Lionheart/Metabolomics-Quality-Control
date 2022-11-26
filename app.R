library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

options(scipen = 999)

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


# Page Setup-----------------------------------------------------------------
ui <- fluidPage(useShinyjs(),
                theme = shinytheme("yeti"),
                tags$head(
                  tags$style(HTML("hr {border-top: 1px solid #000000;}"))),

                # Title Panel -----------------------------------------------------------------
                h1(id = "big-heading", "Marine Microbial Metabolomics Lab: Quality Control"),
                hr(),
                tags$style(HTML("#big-heading{color: #26337a;}")),

                # Sidebar Panel -----------------------------------------------------------------
                sidebarLayout(
                  sidebarPanel(width = 4,
                               wellPanel(id = "tPanel", style = "overflow-y:scroll; max-height: 500",
                                         helpText("Upload your file(s) below. Please confirm if your file is MSDIAL or Skyline."),

                                         helpText("Select whether your file was run on the TQS or QE instrument.
                                                  See Information tab for more details."),

                                         csvFileInput("skyline.file", h5("Output file from Skyline.")),
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

                  # Information tabPanel -----------------------------------------------------------------
                  mainPanel(width = 10,
                            tabsetPanel(type = "tabs",
                                        tabPanel("Information", h2("Using the Ingalls Lab Quality Control", align = "center"),
                                                 br(),

                                                 img(src = "QC.png", height = 200, width = 200, style="display: block; margin-left: auto; margin-right: auto;"),
                                                 br(),

                                                 div(p(HTML(paste0("This code, written in R, performs a user-defined quality-control check on targeted output from the open-source mass spectrometer software ",
                                                                   a(href = "https://skyline.ms/project/home/software/Skyline/begin.view", "Skyline.")))), style = "font-family: 'times'; font-size:20px"),
                                                 p("This application contains three tabs: an Information tab, a QExactive tab, and a TQS (Triple Quadrupole Mass Spectrometer) tab.
               After ensuring you are using output from the correct machine, please complete the quality control", span(strong("in the following order. ")),
               "Each step has an information section below.",
               style = "font-family: 'times'; font-size:20px"),
               p("1. Check your LCMS setup.",
                 style = "font-family: 'times'; font-size:18px"),
               p("2. Switch to the appropriate tab.",
                 style = "font-family: 'times'; font-size:18px"),
               p("3. Upload the correct files for your machine type.",
                 style = "font-family: 'times'; font-size:18px"),
               p("4. Set your quality control parameters.",
                 style = "font-family: 'times'; font-size:18px"),
               h2("LCMS Setup"),
               hr(),

               div(p(HTML(paste0("Samples should be run in the following manner for the quality control code and ",
                                 a(href = "https://github.com/IngallsLabUW/B-MIS-normalization", "B-MIS Normalization"), "- a process used for matching internal standards."))),
                   style = "font-family: 'times'; font-size:18px"),
               br(),

               p("Please label all samples in the following manner:", style = "font-family: 'times'; font-size:18px",
                 span(strong("Date_RunType_AdditionalID (e.g. 161018_Std_FirstStandardinH20)."),
                      ("RunType refers to whether the sample is a standard (Std), sample (Smp), pooled (poo), or blank (blk)."),
                      style = "font-family: 'times'; font-size:18px"),
                 p("- Standards run (all mixed) at least once at the beginning and end of the run",
                   style = "font-family: 'times'; font-size:18px"),
                 p("- Standards run (in representative matrix, all mixed) at least once the beginning and end of the run. Example label: 161019_Std_FirstStandardinMatrix",
                   style = "font-family: 'times'; font-size:18px"),
                 p("- Blanks run (preferably method/filter blanks) at least once. Example label: 161018_Blk_FirstBlank",
                   style = "font-family: 'times'; font-size:18px"),
                 p("- A pooled sample run at least three times throughout the run. Example label:161018_Poo_PooledSample_1",
                   style = "font-family: 'times'; font-size:18px"),
                 p("- Samples. Example label: Date_Smp_AdditionalID_Rep",
                   style = "font-family: 'times'; font-size:18px")),
               h2("File Upload "),
               hr(),

               p("For QExactive: Two files are required, a raw output from Skyline and a blank matcher csv. The blank matcher is a user-made csv that matches the blanks with the appropriate samples
               for signal to noise and blank parameter flags.",
               style = "font-family: 'times'; font-size:18px"),
               p("For TQS: Stay tuned!",
                 style = "font-family: 'times'; font-size:18px"),
               h2("Quality Control Paramters"),
               hr(),

               p("Along the left-hand side panel, follow the steps for each parameter and pick the values that are appropriate for your data. These values are reactive and can be changed throughout
               the analysis process. For QE, you have the option of adding standard tags, which will filter standards for user-defined retention time. This can also be left blank if you would
               like to use all standards from the run.",
               style = "font-family: 'times'; font-size:18px")),


               # QE tabPanel -----------------------------------------------------------------
               tabPanel("QExactive",
                        fluidRow(
                          column(3, helpText("For best results, start on the right hand side where it says 'Start Analysis here!' and move downwards through each button."),
                                 br(),
                                 wellPanel(strong("Your run types are:"), textOutput("runtypes"), hr(), textOutput("std.status")),
                                 wellPanel(strong("Dataset Classes"),
                                           textOutput("classes_status"),
                                           verbatimTextOutput("classes"),
                                           tags$head(tags$style("#classes_status{color: #26337a; font-size: 17px;}")
                                           )
                                 )
                          ),

                          column(4,
                                 br(),
                                 wellPanel(strong("Retention Time Reference Table"), dataTableOutput("Retention.Time.References")),
                                 br()
                          ),

                          column(4,
                                 br(),
                                 wellPanel(strong("Blanks Reference Table"), dataTableOutput("Blank.Ratio.References"))
                          )
                        ),
                        hr(),

                        fluidRow(
                          column(10,
                                 absolutePanel(
                                   h3("Skyline File"),
                                   dataTableOutput("skyline1"),
                                   h3("Supporting File"),
                                   dataTableOutput("supporting1")
                                 )
                          ),

                          column(2,
                                 h3("Parameter Table"),
                                 dataTableOutput("parameterTable"),
                                 h3(id = "analysis-start", "Start analysis here!"),
                                 tags$style(HTML("#analysis-start{color: #26337a;}")),
                                 helpText("Transform original character column classes to numeric values. Drop unnecessary columns."),
                                 actionButton("transform", "Change variable classes"),
                                 br(),
                                 br(),

                                 helpText("Create a table of acceptable Retention Time Ranges"),
                                 actionButton("RT.Table", "Create Retention Time References"),
                                 br(),
                                 br(),

                                 helpText("Create a table of blank references for creation of blank flags."),
                                 actionButton("Blk", "Create Blank Area References"),
                                 br(),
                                 br(),

                                 helpText("Flag those rows that fall outside the user-defined boundaries for Signal-to-Noise minimums,
                      parts-per-million flexibility, and area minimums."),
                      actionButton("first.flags", "SN, PPM, Area flags"),
                      br(),
                      br(),

                      helpText("Flag any rows with values that fall outside of the given Retention Time Range."),
                      actionButton("RT.flags", "Retention Time flags"),
                      br(),
                      br(),

                      helpText("Flag any rows with values that are larger than the maximum blank value."),
                      actionButton("blk.flags", "Blank flags"),
                      br(),
                      br(),

                      helpText("Check if standards existed in the original set. If so, join those rows to the bottom of the modified dataset."),
                      actionButton("Stds", "Re-add standards"),
                      br(),
                      br(),

                      helpText("The new, QC'd file will be downloaded with the modifiers 'QEQC' and system date attached to the original filename."),
                      actionButton("addrows", "Add parameters directly to csv."),
                      downloadButton("QC_file", "Download your QC file here"),
                      downloadButton("Parameters", "Download your parameter file here"),
                      br(),
                      br()
                          )
                        )
               ),

               # TQS tabPanel -----------------------------------------------------------------
               tabPanel("Triple-Quadrupole",
                        helpText("Stay tuned for future developments!")
               )
                            )
                  )))

# Server function -----------------------------------------------------------------
server <- function(input, output, session) {

  # Paramter entry, transformation, runtype ID and initial flags -----------------------------------------------------------------
  output$tags      <- renderText({paste("Your tags for sample matching are (QE only): ", input$std.tags)})
  output$minimum   <- renderText({paste("You have selected", input$area.min, "as area")})
  output$retention <- renderText({paste("You have selected", input$RT.flex, "as retention time flexibility")})
  output$blank     <- renderText({paste("You have selected", input$blank.ratio.max, "as the blank ratio maximum")})
  output$signal    <- renderText({paste("You have selected", input$SN.min, "as signal to noise flexibility")})
  output$ppm       <- renderText({paste("You have selected", input$ppm.flex, "as parts per million time flexibility")})

  output$classes_status <- renderText({paste("Before transformation:")})
  output$classes        <- renderText({paste(colnames(skyline.file()), sapply(skyline.file(), class), " \n")})
  output$runtypes       <- renderText({paste(unique(tolower(str_extract(skyline.file()$Replicate.Name, "(?<=_)[^_]+(?=_)"))))})
  output$SN             <- renderText({"Add those flags"})

  # Data upload  -----------------------------------------------------------------
  skyline.filename <- callModule(promptForFile, "skyline.file")
  skyline.file     <- callModule(csvFile, "skyline.file", stringsAsFactors = FALSE)

  output$skyline1 <- renderDataTable({
    skyline.file()
  }, options = list(pageLength = 10))

  supporting.file <- callModule(csvFile, "supporting.file", stringsAsFactors = FALSE)
  output$supporting1 <- renderDataTable({
    supporting.file()
  }, options = list(pageLength = 10))


  # First transform event -----------------------------------------------------------------
  observeEvent(input$transform, {
    skyline.transformed <<- reactive({skyline.file() %>%
        select(-Protein.Name, -Protein) %>%
        mutate(Retention.Time = suppressWarnings(as.numeric(as.character(Retention.Time)))) %>%
        mutate(Area           = suppressWarnings(as.numeric(as.character(Area)))) %>%
        mutate(Background     = suppressWarnings(as.numeric(as.character(Background)))) %>%
        mutate(Mass.Error.PPM = suppressWarnings(as.numeric(as.character(Mass.Error.PPM)))) %>%
        rename(Mass.Feature   = Precursor.Ion.Name)
    })

    output$skyline1 <- renderDataTable({
      skyline.transformed()
    }, options = list(pageLength = 10))

    output$classes_status <- renderText({paste("After transformation:")})
    output$classes <- renderText({paste(colnames(skyline.transformed()), ":", sapply(skyline.transformed(), class), " \n")})
  })

  # Retention Time Table event -----------------------------------------------------------------
  observeEvent(input$RT.Table, {
    Stds.test <- grepl("_Std_", skyline.transformed()$Replicate.Name)
    if (any(Stds.test == TRUE)) {
      Retention.Time.References <<- reactive({skyline.transformed() %>%
          select(Replicate.Name, Mass.Feature, Retention.Time) %>%
          mutate(Run.Type = (tolower(str_extract(skyline.transformed()$Replicate.Name, "(?<=_)[^_]+(?=_)")))) %>%
          group_by(Mass.Feature) %>%
          filter(Run.Type == "std") %>%
          mutate(RT.min = min(Retention.Time, na.rm = TRUE)) %>%
          mutate(RT.max = max(Retention.Time, na.rm = TRUE)) %>%
          select(Mass.Feature, RT.min, RT.max) %>%
          unique()
      })
    } else {
      Retention.Time.References <<- reactive({skyline.transformed() %>%
          filter(Replicate.Name %in% input$std.tags) %>%
          select(Replicate.Name, Mass.Feature, Retention.Time) %>%
          group_by(Mass.Feature) %>%
          mutate(RT.min = min(Retention.Time, na.rm = TRUE)) %>%
          mutate(RT.max = max(Retention.Time, na.rm = TRUE)) %>%
          select(Mass.Feature, Replicate.Name, RT.min, RT.max)
      })
    }

    output$Retention.Time.References <- renderDataTable({
      Retention.Time.References()
    }, options = list(pageLength = 10))
  })

  # Blank Reference Table event -----------------------------------------------------------------
  observeEvent(input$Blk, {
    Blank.Ratio.References <<- reactive({skyline.file() %>%
        filter(Replicate.Name %in% supporting.file()$Blank.Name) %>%
        select(-Protein.Name, -Protein) %>%
        rename(Mass.Feature = Precursor.Ion.Name) %>%
        rename(Blank.Name = Replicate.Name,
               Blank.Area = Area) %>%
        select(Blank.Name, Mass.Feature, Blank.Area) %>%
        left_join(supporting.file(), by = "Blank.Name") %>%
        group_by(Mass.Feature, Replicate.Name) %>%
        filter(row_number() == 1) %>%
        unique()
      #select(Blank.Name, Mass.Feature, Blank.Area) %>%
    })

    output$Blank.Ratio.References <- renderDataTable({
      Blank.Ratio.References()
    }, options = list(pageLength = 10))
  })

  # First flags event -----------------------------------------------------------------
  observeEvent(input$first.flags, {
    skyline.first.flagged <<- reactive({skyline.transformed() %>%
        filter(Replicate.Name %in% supporting.file()$Replicate.Name) %>%
        mutate(SN.Flag       = ifelse(((Area / Background) < input$SN.min), "SN.Flag", NA)) %>%
        mutate(ppm.Flag      = ifelse(abs(Mass.Error.PPM) > input$ppm.flex, "ppm.Flag", NA)) %>%
        mutate(area.min.Flag = ifelse((Area < input$area.min), "area.min.Flag", NA))
    })
    output$skyline1 <- renderDataTable({
      skyline.first.flagged()
    }, options = list(pageLength = 10))
  })

  # RT flags event -----------------------------------------------------------------
  observeEvent(input$RT.flags, {
    skyline.RT.flagged <<- reactive({skyline.first.flagged() %>%
        left_join(Retention.Time.References(), by = "Mass.Feature") %>%
        mutate(RT.Flag = ifelse((Retention.Time >= (RT.max + input$RT.flex) | Retention.Time <= (RT.min - input$RT.flex)), "RT.Flag", NA)) %>%
        select(-RT.min, -RT.max)
    })
    output$skyline1 <- renderDataTable({
      skyline.RT.flagged()
    })
  })

  # Blank flags event -----------------------------------------------------------------
  observeEvent(input$blk.flags, {
    skyline.blk.flagged <<- reactive({skyline.RT.flagged() %>%
        # TODO (rlionheart): Same issue as Retention time. How to reference another table?
        # TODO (rlionheart): also double check this table itself- is it correct?
        left_join(Blank.Ratio.References(), by = c("Replicate.Name", "Mass.Feature")) %>%
        mutate(blank.Flag = suppressWarnings(ifelse((as.numeric(Area) / as.numeric(Blank.Area)) < input$blank.ratio.max, "blank.Flag", NA))) %>%
        select(-Blank.Name, -Blank.Area)
    })
    output$skyline1 <- renderDataTable({
      skyline.blk.flagged()
    })
  })

  # Re-adding stds event -----------------------------------------------------------------
  observeEvent(input$Stds, {
    Stds.test <- grepl("_Std_", skyline.file()$Replicate.Name)
    if (any(Stds.test == TRUE)) {
      output$std.status <- renderText({"Standards in set. Joining them to the bottom of the dataset!"})
      standards <- skyline.transformed()[grep("Std", skyline.transformed()$Replicate.Name), ]
      skyline.stds.added <<- reactive({rbind.fill((skyline.blk.flagged()), standards)})
    } else {
      output$std.status <- renderText({"No standards exist in this set. Table remains as is."})
      skyline.stds.added <<- reactive(skyline.blk.flagged())
    }

    output$skyline1 <- renderDataTable({
      skyline.stds.added()
    })
  })

  # Parameter table-----------------------------------------------------------------
  parametersReactive <- reactive({
    data.frame(Parameter= c("Area minimum", "Retention time flexibility", "Blank Ratio Maximum", "Signal to Noise Minimum", "Parts per million flexibility"),
               Values = c(input$area.min, input$RT.flex, input$blank.ratio.max, input$SN.min, input$ppm.flex))
  })

  output$parameterTable <- renderDataTable({
    parametersReactive()
  })

  observeEvent(input$addrows, {
    final.skyline <<- reactive({skyline.stds.added() %>%
        bind_rows(parametersReactive())
    })
    output$skyline1 <- renderDataTable({
      final.skyline()
    })
  })

  # Download both files-----------------------------------------------------------------
  output$QC_file <- downloadHandler(
    filename = function() {
      paste("QEQC_", Sys.Date(), skyline.filename(), sep = "")
    },
    content = function(file) {
      write.csv(final.skyline(), file)
    }
  )

  output$Parameters <- downloadHandler(
    filename = function() {
      paste("QE_Parameters_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(parametersReactive(), file)
    }
  )
}

shinyApp(ui = ui, server = server)
