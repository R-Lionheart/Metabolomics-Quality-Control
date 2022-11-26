library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

options(scipen = 999)

# Module functions
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
                h1(id = "big-heading", "Title Here"),
                hr(),
                tags$style(HTML("#big-heading{color: #26337a;}")),

                # Sidebar Panel -----------------------------------------------------------------
                sidebarLayout(
                  sidebarPanel(width = 4,
                               wellPanel(id = "tPanel", style = "overflow-y:scroll; max-height: 500",
                                         helpText("File upload info here."),

                                         helpText("More info here."),

                                         csvFileInput("Upload file here", h5("Output file from Skyline, peaks selected.")),
                                         hr(),

                                         helpText("Run type info here"),
                                         wellPanel("Your run types are:", textOutput("runtypes")),
                                         hr(), ##TODO

                                         helpText("Pick the minimum height to be counted as a 'real' peak."),
                                         sliderInput("area.min", h5("Area Minimum"),
                                                     min = 1000, step = 500, max = 5000, value = 1000),
                                         hr(),

                                         helpText("Pick retention time (RT) flexibility for an acceptable window cutoff."),
                                         sliderInput("RT.flex", h5("Retention Time Flexibility (in minutes)"),
                                                     min = 0.0, step = 0.1, max = 1.0, value = 0.2),
                                         hr(),

                                         helpText("Pick acceptable sample to blank ratio."),
                                         sliderInput("blank.ratio.max", h5("Blank Ratio Maximum"),
                                                     min = 0.0, step = 0.1, max = 1, value = 0.3),
                                         hr(),

                                         helpText("Pick acceptable signal to noise ratio value. Note: broader peaks create more background noise."),
                                         sliderInput("SN.min", h5("Signal to Noise Ratio"),
                                                     min = 1, step = 1, max = 5, value = 3),
                                         hr(),

                                         helpText("Pick an absolute value for a cutoff for parts per million (ppm) (QE suggestion: 7)"),
                                         sliderInput("ppm.flex", h5("Parts per Million"),
                                                     min = 1, step = 1, max = 10, value = 5)
                                         )
                               ),

                # Information tabPanel -----------------------------------------------------------------
                mainPanel(width = 7,
                          tabsetPanel(type = "tabs",
                                      tabPanel("Information", h2("View and QC Skyline Data", align = "center"),
                                               br(),
                                               img(src = "QC.png", height = 200, width = 200, style="display: block; margin-left: auto; margin-right: auto;"),
                                               br(),
                                               div(p(HTML(paste0("Click here for information on ",
                                                                 a(href = "https://skyline.ms/project/home/software/Skyline/begin.view", "Skyline.")))),
                                                   style = "font-family: 'times'; font-size:20px"),
                                               p("Information here", span(strong("bold.")), "Each step has an information section below.",
                                                 style = "font-family: 'times'; font-size:20px"),
                                               p("1. Step one.",
                                                 style = "font-family: 'times'; font-size:18px"),
                                               p("2. Step two",
                                                 style = "font-family: 'times'; font-size:18px"),
                                               p("3. Step three",
                                                 style = "font-family: 'times'; font-size:18px"),
                                               p("4. Step four",
                                                 style = "font-family: 'times'; font-size:18px"),
                                               h2("Step one info"),
                                               hr(),

                                               div(p(HTML(paste0("More info on B-MIS here ",
                                                                 a(href = "https://github.com/IngallsLabUW/B-MIS-normalization", "B-MIS Normalization"), "- info."))),
                                                   style = "font-family: 'times'; font-size:18px"),
                                               br(),

                                               p("Sample info:", style = "font-family: 'times'; font-size:18px",
                                                 span(strong("boldinfo"),
                                                      ("RunType refers to whether the sample is a standard (Std), sample (Smp), pooled (poo), or blank (blk)."),
                                                      style = "font-family: 'times'; font-size:18px"),
                                                 p("- List item here", style = "font-family: 'times'; font-size:18px")),
                                               h2("Step two info"),
                                               hr(),

                                               p("Info, long",
                                                 style = "font-family: 'times'; font-size:18px"),
                                               p("Info",
                                                 style = "font-family: 'times'; font-size:18px"),
                                               h2("Step 3 info"),
                                               hr(),

                                               p("Description here",
                                                 style = "font-family: 'times'; font-size:18px")
                                               ),
                                      # Browse Data tabPanel -----------------------------------------------------------------
                                      tabPanel("Browse data",
                                               fluidRow(
                                                 column(4, helpText("Text goes here."),
                                                        br(),
                                                        wellPanel(strong("Dataset Classes"),
                                                                  textOutput("classes_status"),
                                                                  verbatimTextOutput("classes"),
                                                                  tags$head(tags$style("#classes_status{color: #26337a; font-size: 17px;}"))
                                                                  )
                                                        ),

                                                 column(4, helpText("Second text goes here"),
                                                        br(),
                                                        wellPanel(strong("Retention Time Reference Table"), dataTableOutput("Retention.Time.References")),
                                                        br()
                                                        ),

                                                 column(4, helpText("Third text goes here"),
                                                        br(),
                                                        wellPanel(strong("Blanks Reference Table"), dataTableOutput("Blank.Ratio.References"))
                                                        )
                                                 ),
                                               hr(),

                                               ## Skyline file viewing
                                               fluidRow(
                                                 column(5,
                                                        absolutePanel(
                                                          h3("Skyline File"),
                                                          dataTableOutput("skyline1")
                                                          )
                                                        ),
                                                 column(5,
                                                        h3("Parameter Table"),
                                                        dataTableOutput("parameterTable")
                                                        )
                                                 )
                                               )
                                      )
                          )
                )
)
               #            ## Test break
               #                   h3(id = "analysis-start", "Start analysis here!"),
               #                   tags$style(HTML("#analysis-start{color: #26337a;}")),
               #                   helpText("Transform original character column classes to numeric values. Drop unnecessary columns."),
               #                   actionButton("transform", "Change variable classes"),
               #                   br(),
               #                   br(),
               #
               #                   helpText("Create a table of acceptable Retention Time Ranges"),
               #                   actionButton("RT.Table", "Create Retention Time References"),
               #                   br(),
               #                   br(),
               #
               #                   helpText("Create a table of blank references for creation of blank flags."),
               #                   actionButton("Blk", "Create Blank Area References"),
               #                   br(),
               #                   br(),
               #
               #                   helpText("Flag those rows that fall outside the user-defined boundaries for Signal-to-Noise minimums,
               #        parts-per-million flexibility, and area minimums."),
               #        actionButton("first.flags", "SN, PPM, Area flags"),
               #        br(),
               #        br(),
               #
               #        helpText("Flag any rows with values that fall outside of the given Retention Time Range."),
               #        actionButton("RT.flags", "Retention Time flags"),
               #        br(),
               #        br(),
               #
               #        helpText("Flag any rows with values that are larger than the maximum blank value."),
               #        actionButton("blk.flags", "Blank flags"),
               #        br(),
               #        br(),
               #
               #        helpText("Check if standards existed in the original set. If so, join those rows to the bottom of the modified dataset."),
               #        actionButton("Stds", "Re-add standards"),
               #        br(),
               #        br(),
               #
               #        helpText("The new, QC'd file will be downloaded with the modifiers 'QEQC' and system date attached to the original filename."),
               #        actionButton("addrows", "Add parameters directly to csv."),
               #        downloadButton("QC_file", "Download your QC file here"),
               #        downloadButton("Parameters", "Download your parameter file here"),
               #        br(),
               #        br()
               #        )
               #          )
               # )))))

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

  # # First transform event -----------------------------------------------------------------
  # observeEvent(input$transform, {
  #   skyline.transformed <<- reactive({skyline.file() %>%
  #       select(-Protein.Name, -Protein) %>%
  #       mutate(Retention.Time = suppressWarnings(as.numeric(as.character(Retention.Time)))) %>%
  #       mutate(Area           = suppressWarnings(as.numeric(as.character(Area)))) %>%
  #       mutate(Background     = suppressWarnings(as.numeric(as.character(Background)))) %>%
  #       mutate(Mass.Error.PPM = suppressWarnings(as.numeric(as.character(Mass.Error.PPM)))) %>%
  #       rename(Mass.Feature   = Precursor.Ion.Name)
  #   })
  #
  #   output$skyline1 <- renderDataTable({
  #     skyline.transformed()
  #   }, options = list(pageLength = 10))
  #
  #   output$classes_status <- renderText({paste("After transformation:")})
  #   output$classes <- renderText({paste(colnames(skyline.transformed()), ":", sapply(skyline.transformed(), class), " \n")})
  # })

  # # Retention Time Table event -----------------------------------------------------------------
  # observeEvent(input$RT.Table, {
  #   Stds.test <- grepl("_Std_", skyline.transformed()$Replicate.Name)
  #   if (any(Stds.test == TRUE)) {
  #     Retention.Time.References <<- reactive({skyline.transformed() %>%
  #         select(Replicate.Name, Mass.Feature, Retention.Time) %>%
  #         mutate(Run.Type = (tolower(str_extract(skyline.transformed()$Replicate.Name, "(?<=_)[^_]+(?=_)")))) %>%
  #         group_by(Mass.Feature) %>%
  #         filter(Run.Type == "std") %>%
  #         mutate(RT.min = min(Retention.Time, na.rm = TRUE)) %>%
  #         mutate(RT.max = max(Retention.Time, na.rm = TRUE)) %>%
  #         select(Mass.Feature, RT.min, RT.max) %>%
  #         unique()
  #     })
  #   } else {
  #     Retention.Time.References <<- reactive({skyline.transformed() %>%
  #         filter(Replicate.Name %in% input$std.tags) %>%
  #         select(Replicate.Name, Mass.Feature, Retention.Time) %>%
  #         group_by(Mass.Feature) %>%
  #         mutate(RT.min = min(Retention.Time, na.rm = TRUE)) %>%
  #         mutate(RT.max = max(Retention.Time, na.rm = TRUE)) %>%
  #         select(Mass.Feature, Replicate.Name, RT.min, RT.max)
  #     })
  #   }
  #
  #   output$Retention.Time.References <- renderDataTable({
  #     Retention.Time.References()
  #   }, options = list(pageLength = 10))
  # })
  #
  # # Blank Reference Table event -----------------------------------------------------------------
  # observeEvent(input$Blk, {
  #   Blank.Ratio.References <<- reactive({skyline.file() %>%
  #       filter(Replicate.Name %in% supporting.file()$Blank.Name) %>%
  #       select(-Protein.Name, -Protein) %>%
  #       rename(Mass.Feature = Precursor.Ion.Name) %>%
  #       rename(Blank.Name = Replicate.Name,
  #              Blank.Area = Area) %>%
  #       select(Blank.Name, Mass.Feature, Blank.Area) %>%
  #       left_join(supporting.file(), by = "Blank.Name") %>%
  #       group_by(Mass.Feature, Replicate.Name) %>%
  #       filter(row_number() == 1) %>%
  #       unique()
  #     #select(Blank.Name, Mass.Feature, Blank.Area) %>%
  #   })
  #
  #   output$Blank.Ratio.References <- renderDataTable({
  #     Blank.Ratio.References()
  #   }, options = list(pageLength = 10))
  # })
  #
  # # First flags event -----------------------------------------------------------------
  # observeEvent(input$first.flags, {
  #   skyline.first.flagged <<- reactive({skyline.transformed() %>%
  #       filter(Replicate.Name %in% supporting.file()$Replicate.Name) %>%
  #       mutate(SN.Flag       = ifelse(((Area / Background) < input$SN.min), "SN.Flag", NA)) %>%
  #       mutate(ppm.Flag      = ifelse(abs(Mass.Error.PPM) > input$ppm.flex, "ppm.Flag", NA)) %>%
  #       mutate(area.min.Flag = ifelse((Area < input$area.min), "area.min.Flag", NA))
  #   })
  #   output$skyline1 <- renderDataTable({
  #     skyline.first.flagged()
  #   }, options = list(pageLength = 10))
  # })
  #
  # # RT flags event -----------------------------------------------------------------
  # observeEvent(input$RT.flags, {
  #   skyline.RT.flagged <<- reactive({skyline.first.flagged() %>%
  #       left_join(Retention.Time.References(), by = "Mass.Feature") %>%
  #       mutate(RT.Flag = ifelse((Retention.Time >= (RT.max + input$RT.flex) | Retention.Time <= (RT.min - input$RT.flex)), "RT.Flag", NA)) %>%
  #       select(-RT.min, -RT.max)
  #   })
  #   output$skyline1 <- renderDataTable({
  #     skyline.RT.flagged()
  #   })
  # })
  #
  # # Blank flags event -----------------------------------------------------------------
  # observeEvent(input$blk.flags, {
  #   skyline.blk.flagged <<- reactive({skyline.RT.flagged() %>%
  #       # TODO (rlionheart): Same issue as Retention time. How to reference another table?
  #       # TODO (rlionheart): also double check this table itself- is it correct?
  #       left_join(Blank.Ratio.References(), by = c("Replicate.Name", "Mass.Feature")) %>%
  #       mutate(blank.Flag = suppressWarnings(ifelse((as.numeric(Area) / as.numeric(Blank.Area)) < input$blank.ratio.max, "blank.Flag", NA))) %>%
  #       select(-Blank.Name, -Blank.Area)
  #   })
  #   output$skyline1 <- renderDataTable({
  #     skyline.blk.flagged()
  #   })
  # })
  #
  # # Re-adding stds event -----------------------------------------------------------------
  # observeEvent(input$Stds, {
  #   Stds.test <- grepl("_Std_", skyline.file()$Replicate.Name)
  #   if (any(Stds.test == TRUE)) {
  #     output$std.status <- renderText({"Standards in set. Joining them to the bottom of the dataset!"})
  #     standards <- skyline.transformed()[grep("Std", skyline.transformed()$Replicate.Name), ]
  #     skyline.stds.added <<- reactive({rbind.fill((skyline.blk.flagged()), standards)})
  #   } else {
  #     output$std.status <- renderText({"No standards exist in this set. Table remains as is."})
  #     skyline.stds.added <<- reactive(skyline.blk.flagged())
  #   }
  #
  #   output$skyline1 <- renderDataTable({
  #     skyline.stds.added()
  #   })
  # })

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
