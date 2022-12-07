library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

options(scipen = 999)

# Modules -----------------------------------------------------------------

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
    })
  )
}

csvFile <- function(input, output, session, stringsAsFactors) {
  userFile <- promptForFile(input, output, session)

  # The user's data, parsed into a data frame
  dataframe <- reactive({
    read.csv(userFile()$datapath,
             header = input$heading,
             stringsAsFactors = stringsAsFactors)
  })

  # Run observer
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
                  sidebarPanel(width = 3,
                               wellPanel(id = "tPanel", style = "overflow-y:scroll; max-height: 500",
                                         helpText("Upload your files below. Your first file should be
                                                  from Skyline, and the second should be the latest
                                                  Ingalls Standards sheet. Please see the Information
                                                  tab for more details on both of these files."),

                                         csvFileInput("skyline.file", h5("Output file from Skyline")),
                                         csvFileInput("supporting.file", h5("Latest Ingalls Lab standards sheet")),
                                         hr(),

                                         wellPanel(helpText("Below are the run types included in your file.
                                                            'std' = standard run, 'smp' = sample run,
                                                            'poo' = pooled run, 'blk' = blank run."),
                                                   strong("Your run types are:"),
                                                   textOutput("runtypes")),
                                         hr(),

                                         helpText("Pick the minimum height to be counted as a 'real' peak
                                                  (Suggestion: HILIC - 1000, Cyano - 5000)"),
                                         sliderInput("area.min", h5("Area Minimum"),
                                                     min = 1000, step = 1000, max = 5000, value = 1000),
                                         hr(),

                                         helpText("Pick retention time (RT) flexibility
                                                  (Suggestion: +/- 0.4 min for HILIC, +/- 0.2 min for Cyano)"),
                                         sliderInput("RT.flex", h5("Retention Time Flexibility"),
                                                     min = 0.0, step = 0.1, max = 1.0, value = 0.2),
                                         hr(),

                                         helpText("Pick signal size comparison between sample and blank to merit inclusion
                                                  (Suggestion: +/- 0.2)"),
                                         sliderInput("blank.ratio.max", h5("Blank Ratio Maximum"),
                                                     min = 0.0, step = 0.1, max = 0.5, value = 0.3),
                                         hr(),

                                         helpText("Pick acceptable signal to noise ratio value. Note: broader peaks create more background noise
                                                  (Suggestion: 5 for Cyano, 4 for HILIC)"),
                                         sliderInput("SN.min", h5("Signal to Noise Ratio"),
                                                     min = 1, step = 1, max = 5, value = 3),
                                         hr(),

                                         helpText("Pick an absolute value for a cutoff for parts per million (ppm)
                                                  (Suggestion: 7)"),
                                         sliderInput("ppm.flex", h5("Parts per Million"),
                                                     min = 1, step = 1, max = 10, value = 5)
                               )
                  ),

                  # Information tabPanel -----------------------------------------------------------------
                  mainPanel(width = 8,
                            tabsetPanel(type = "tabs",
                                        tabPanel("Information", h2("Using the Ingalls Lab Quality Control", align = "center"),
                                                 br(),

                                                 img(src = "QC.png", height = 200, width = 200, style="display: block; margin-left: auto; margin-right: auto;"),
                                                 br(),

                                                 div(p(HTML(paste0("This Shiny app is a GUI (graphical user interface) for
                                                                   performing a user-defined quality-control check on output from
                                                                   the open-source mass spectrometry software software ",
                                                                   a(href = "https://skyline.ms/project/home/software/Skyline/begin.view",
                                                                     "Skyline.")))), style = "font-family: 'times'; font-size:20px"),
                                                 div(p(HTML(paste0("Combined with the latest Ingalls Standards sheet, this application will
                                                                   perform several basic QC steps, and create a modified csv which the user
                                                                   can download. The Ingalls standards sheet can be accessed ",
                                                                   a(href = "https://github.com/IngallsLabUW/Ingalls_Standards",
                                                                     "here.")))), style = "font-family: 'times'; font-size:20px"),
                                                 p("This application contains six tabs including this Information tab:
                                                   a Tidy Data tab, a Browse Data tab, a QC Data tab, a Flag Data tab, and a Download Data tab.
                                                   It is usually easiest to move through the tabs ", span(strong("left to right. ")),
                                                   "Below is a brief outline of the steps within this application,
                                                   with more detail following",
                                                   style = "font-family: 'times'; font-size:20px"),

                                                 # Steps overview -----------------------------------------------------------------
                                                 p("1. Import your data.",
                                                   style = "font-family: 'times'; font-size:18px"),
                                                 p("2. Set your QC parameters.",
                                                   style = "font-family: 'times'; font-size:18px"),
                                                 p("3. Tidy your data.",
                                                   style = "font-family: 'times'; font-size:18px"),
                                                 p("4. Browse data.",
                                                   style = "font-family: 'times'; font-size:18px"),
                                                 p("5. Create QC reference tables.",
                                                   style = "font-family: 'times'; font-size:18px"),
                                                 p("6. Flag data.",
                                                   style = "font-family: 'times'; font-size:18px"),
                                                 p("7. Download data.",
                                                   style = "font-family: 'times'; font-size:18px"),

                                                 # Data Import -----------------------------------------------------------------
                                                 h2("Data Import"),
                                                 hr(),
                                                 div(p(HTML(paste0("On the left hand side panel, upload your Skyline output file.
                                                                   The file should be in long format. Underneath, upload the latest
                                                                   Ingalls Standards sheet, available ",
                                                                   a(href = "https://github.com/IngallsLabUW/Ingalls_Standards", "here,"),
                                                                   " in order to ensure your compound names reflect the latest conventions."))),
                                                     style = "font-family: 'times'; font-size:18px"),
                                                 br(),

                                                 p("For the Skyline data, your runs must be labeled in the following format:",
                                                   style = "font-family: 'times'; font-size:18px",
                                                   span(strong("Date_RunType_AdditionalID (e.g. 161018_Std_FirstStandardinH20)."),
                                                        ("RunType refers to whether the sample is a standard (std), sample (smp), pooled (poo), or blank (blk)."),
                                                        style = "font-family: 'times'; font-size:18px")),
                                                 # QC Parameters -----------------------------------------------------------------
                                                 h2("QC Parameters"),
                                                 hr(),
                                                 p("On the left-hand side panel, enter the parameters that are appropriate for your data.
                                                   There is more detail on the panel about each parameter. Because the parameters are reactive,
                                                   you can change them throughout this process to see how your data is affected.",
                                                   style = "font-family: 'times'; font-size:18px"),
                                                 # Tidy Data -----------------------------------------------------------------
                                                 h2("Tidy Your Data"),
                                                 hr(),
                                                 p("Due to the way Skyline exports data, some munging is required. Underneath the Tidy Data tab,
                                                   there are two buttons (Change Columns and Classes, and Update Compound Names).
                                                   The Change Columns and Classes button will drop unnecessary columns, remove syntactically
                                                   incorrect placeholder values from the data, and adjust the classes of all columns so operations
                                                   can be performed on them. The Update Compound Names button will update any compound names to the
                                                   correct versions that are used in the Ingalls Lab.",
                                                   style = "font-family: 'times'; font-size:18px"),
                                                 # Browse Data -----------------------------------------------------------------
                                                 h2("View Your Data"),
                                                 hr(),
                                                 p("Under the Browse Data tab, you can see your uploaded Skyline file. This is a searchable,
                                                   reactive file that will change as you perform QC and tidying operations.",
                                                   style = "font-family: 'times'; font-size:18px"),
                                                 # QC Data -----------------------------------------------------------------
                                                 h2("Create QC Reference Tables"),
                                                 hr(),
                                                 p("This tab contains steps to create Reference Tables, which are then used to flag any
                                                   data that may fall outside of the user-defined QC parameters. On the right hand side
                                                   of the page, the Parameter table will show the user's entries from the sidebar. The
                                                   Create Retention Time References button will create a table displaying the maximum
                                                   and minimum values for each compound in the Skyline output data. The Create Blank Area
                                                   References button will display a table with the maximum and minimum blank values
                                                   for each compound.",
                                                   style = "font-family: 'times'; font-size:18px"),
                                                 # Flag Data -----------------------------------------------------------------
                                                 h2("Create Flags"),
                                                 hr(),
                                                 p("Under the Flag Data tab, there are buttons to add flags to the reactive Skyline data
                                                   viewable under the Browse Data tab. The Area Flags button will flag rows where the value
                                                   is less than the user-defined area minimym. The Retention Time Flags button will add a column
                                                   that flags any runs (per compound) that fall outside of the Retention Time Reference table,
                                                   plus or minus the user-defined parameters. The Blank Flags button flags any runs that are
                                                   smaller than the user-defined ratio comparing the runs to the largest blank run, per compound.
                                                   The Signal-to-Noise Flags add flags to rows where the signal value is not significantly higher
                                                   than the background, according to the user-defined value.",
                                                   style = "font-family: 'times'; font-size:18px"),
                                                 # Download Data -----------------------------------------------------------------
                                                 h2("Download Data"),
                                                 hr(),
                                                 p("Finally, under the Download Data tab, the user can download the modified csv,
                                                   as well as the parameter table to record the QC parameters that were used.",
                                                   style = "font-family: 'times'; font-size:18px")),
                                        # Tidy tabPanel -----------------------------------------------------------------
                                        tabPanel("Tidy Data",
                                                 fluidRow(
                                                   column(6, helpText("Click the 'Tidy Data' button to transform original
                                                                      character column classes to numeric values and
                                                                      drop/rename unnecessary columns."),
                                                          actionButton("columnclasses", "Tidy Data"),
                                                          br(),
                                                          br(),
                                                          strong("Dataset Columns and Classes"),
                                                          textOutput("classes_status"),
                                                          verbatimTextOutput("classes"),
                                                          tags$head(tags$style("#classes_status{color: #26337a; font-size: 17px;}"))
                                                   ),
                                                   column(6, helpText("Click the 'Update Compound Names' button to ensure the compounds
                                                                      reflect the latest Ingalls Standards name."),
                                                          actionButton("replacenames", "Update Compoud Names"),
                                                          br(),
                                                          br(),
                                                          strong("Unique compound names"),
                                                          textOutput("names_status"),
                                                          verbatimTextOutput("names"),
                                                          tags$head(tags$style("#names_status{color: #26337a; font-size: 17px;}"))
                                                   ),
                                                 )
                                        ),
                                        # Browse tabPanel -----------------------------------------------------------------
                                        tabPanel("Browse Data",
                                                 fluidRow(
                                                   column(6, helpText("Upload data to see your tables and visualizations here!."),
                                                          absolutePanel(
                                                            h3("Skyline file"),
                                                            dataTableOutput("skyline1")
                                                          )
                                                   )
                                                 )
                                        ),
                                        # QC tabPanel -----------------------------------------------------------------
                                        tabPanel("QC Data",
                                                 fluidRow(
                                                   h3(id = "analysis-start", "Create QC Reference Tables Here!"),
                                                   tags$style(HTML("#analysis-start{color: #26337a;}")),
                                                   br(),
                                                   column(7, helpText("Create a table of acceptable Retention Time Ranges"),
                                                          actionButton("RT.Table", "Create Retention Time References"),
                                                          br(),
                                                          br(),
                                                          wellPanel(strong("Retention Time Reference Table"),
                                                                    dataTableOutput("Retention.Time.References")),
                                                          br()
                                                   ),
                                                   column(4,
                                                          wellPanel(h3("Parameter Table"),
                                                                    dataTableOutput("parameterTable")),
                                                          br(),
                                                   )
                                                 ),
                                                 fluidRow(
                                                   column(7,
                                                          br(),
                                                          helpText("Create a table of blank references for creation of blank flags."),
                                                          actionButton("Blk", "Create Blank Area References"),
                                                          br(),
                                                          br(),
                                                          wellPanel(strong("Blanks Reference Table"),
                                                                    dataTableOutput("Blank.Ratio.References"))
                                                   )
                                                 )
                                        ),
                                        # Flags tabPanel -----------------------------------------------------------------
                                        tabPanel("Flag Data", helpText("Flag any rows with values that are smaller than the minimum area value."),
                                              actionButton("Area.flags", "Area Flags"),
                                              textOutput("Area_flags_status"),
                                              br(),
                                              br(),
                                              helpText("Flag any rows with values that fall outside of the given retention time range."),
                                              actionButton("RT.flags", "Retention Time Flags"),
                                              textOutput("RT_flags_status"),
                                              br(),
                                              br(),
                                              helpText("Flag any rows with values that are larger than the maximum blank value."),
                                              actionButton("blk.flags", "Blank Flags"),
                                              textOutput("Blank_flags_status"),
                                              br(),
                                              br(),
                                              helpText("Flag any rows with values that fall outside the given Signal-to-Noise ratio."),
                                              actionButton("SN.flags", "Signal-to-Noise Flags"),
                                              textOutput("SN_flags_status"),
                                              br(),
                                              br()
                                        ),
                                        # Download tabPanel -----------------------------------------------------------------
                                        tabPanel("Download Data", helpText("Download your completed data here!"),
                                                 column(10, helpText("The new, QC'd file will be downloaded with the modifiers 'QEQC'
                                            and system date attached to the original filename."),
                                            downloadButton("QC_file", "Download your QC file here"),
                                            downloadButton("Parameters", "Download your parameter file here"),
                                            br(),
                                            br())
                                        )
                            ))))

# Server function -----------------------------------------------------------------
server <- function(input, output, session) {

  # Paramter entry, transformation, runtype ID and initial flags -----------------------------------------------------------------
  output$minimum   <- renderText({paste("You have selected", input$area.min, "as area")})
  output$retention <- renderText({paste("You have selected", input$RT.flex, "as retention time flexibility")})
  output$blank     <- renderText({paste("You have selected", input$blank.ratio.max, "as the blank ratio maximum")})
  output$signal    <- renderText({paste("You have selected", input$SN.min, "as signal to noise flexibility")})

  output$classes_status <- renderText({paste("Before transformation:")})
  output$classes        <- renderText({paste(colnames(skyline.file()), sapply(skyline.file(), class), " \n")})
  output$names_status   <- renderText({paste("Original names:")})
  output$names          <- renderText({paste(unique(skyline.file()$Precursor.Ion.Name), " \n")})
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

  # First transform event: columns -----------------------------------------------------------------
  observeEvent(input$columnclasses, {
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

  # Second transform event: names -----------------------------------------------------------------
  observeEvent(input$replacenames, {
    skyline.names.changed <<- reactive({skyline.transformed() %>%
          rename(Compound_Name_Original = Mass.Feature) %>%
          left_join(supporting.file() %>% select(Compound_Name, Compound_Name_Original)) %>%
          rename(Compound.Name_new = Compound_Name,
                 Mass.Feature = Compound_Name_Original) %>%
          mutate(Compound.Name_new = if_else(is.na(Compound.Name_new), Mass.Feature, Compound.Name_new)) %>%
          select(-Mass.Feature) %>%
          rename(Mass.Feature = Compound.Name_new) %>%
          select(Replicate.Name, Mass.Feature, everything())
    })

    output$skyline1 <- renderDataTable({
      skyline.names.changed()
    }, options = list(pageLength = 10))

    output$names_status <- renderText({paste("Updated names:")})
    output$names <- renderText({paste(unique(skyline.names.changed()$Mass.Feature), " \n")}) ## Should be pasted with old
  })

  # Retention Time Table event -----------------------------------------------------------------
  observeEvent(input$RT.Table, {
    Stds.test <- grepl("_Std_", skyline.names.changed()$Replicate.Name)
    if (any(Stds.test == TRUE)) {
      Retention.Time.References <<- reactive({skyline.names.changed() %>%
          select(Replicate.Name, Mass.Feature, Retention.Time) %>%
          mutate(Run.Type = (tolower(str_extract(skyline.names.changed()$Replicate.Name, "(?<=_)[^_]+(?=_)")))) %>%
          group_by(Mass.Feature) %>%
          filter(Run.Type == "std") %>%
          mutate(RT.min = min(Retention.Time, na.rm = TRUE)) %>%
          mutate(RT.max = max(Retention.Time, na.rm = TRUE)) %>%
          select(Mass.Feature, RT.min, RT.max) %>%
          unique()
      })
    } else {
      Retention.Time.References <<- reactive({skyline.names.changed() %>%
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
    }, options = list(pageLength = 5))
  })

  # Blank Reference Table event -----------------------------------------------------------------
  observeEvent(input$Blk, {
    Blank.Ratio.References <<- reactive({skyline.names.changed() %>%
          filter(str_detect(Replicate.Name, regex("Blk", ignore_case = TRUE))) %>%
          select(Mass.Feature, Area) %>%
          group_by(Mass.Feature) %>%
          mutate(Blank.min = min(Area, na.rm = TRUE)) %>%
          mutate(Blank.max = max(Area, na.rm = TRUE)) %>%
          select(-Area) %>%
          unique()
    })

    output$Blank.Ratio.References <- renderDataTable({
      Blank.Ratio.References()
    }, options = list(pageLength = 5))
  })


  # Area flags event -----------------------------------------------------------------
  observeEvent(input$Area.flags, {
    skyline.area.flagged <<- reactive({skyline.names.changed() %>%
        mutate(Area.Flag = ifelse((Area < input$area.min), "Area.Flag", NA))
    })
    output$skyline1 <- renderDataTable({
      skyline.area.flagged()
    })
    output$Area_flags_status <- renderText({
      paste("Area flags added to Skyline table!")
    })

  })

  # RT flags event -----------------------------------------------------------------
  observeEvent(input$RT.flags, {
    skyline.RT.flagged <<- reactive({skyline.area.flagged() %>%
        left_join(Retention.Time.References(), by = "Mass.Feature") %>%
        group_by(Mass.Feature) %>%
        mutate(RT.Flag = ifelse((Retention.Time >= (RT.max + input$RT.flex) | Retention.Time <= (RT.min - input$RT.flex)), "RT.Flag", NA)) %>%
        select(-RT.min, -RT.max)
    })
    output$skyline1 <- renderDataTable({
      skyline.RT.flagged()
    })
    output$RT_flags_status <- renderText({
      paste("Retention time flags added to Skyline table!")
    })

  })

  # Blank flags event -----------------------------------------------------------------
  observeEvent(input$blk.flags, {

    BlanktoJoin <<- reactive({skyline.names.changed() %>%
        filter(str_detect(Replicate.Name, regex("Blk", ignore_case = TRUE))) %>%
        select(Mass.Feature, Area) %>%
        rename(Blank.Area = Area) %>%
        group_by(Mass.Feature) %>%
        mutate(Blank.max = max(Blank.Area, na.rm = TRUE)) %>%
        unique()

    })
    skyline.blk.flagged <<- reactive({skyline.RT.flagged() %>%
        left_join(BlanktoJoin(), by = c("Mass.Feature")) %>%
        group_by(Mass.Feature) %>%
        mutate(Blank.Flag = suppressWarnings(ifelse((as.numeric(Area) / as.numeric(Blank.max)) < input$blank.ratio.max, "blank.Flag", NA))) %>%
        select(-Blank.max, -Blank.Area)
    })
    output$skyline1 <- renderDataTable({
      skyline.blk.flagged()
    })
    output$Blank_flags_status <- renderText({
      paste("Blank flags added to Skyline table!")
    })
  })

  # SN flags event -----------------------------------------------------------------
  observeEvent(input$SN.flags, {
    skyline.SN.flagged <<- reactive({skyline.blk.flagged() %>%
        mutate(SN.Flag = ifelse(((Area / Background) < input$SN.min), "SN.Flag", NA))
    })
    output$skyline1 <- renderDataTable({
      skyline.SN.flagged()
    })
    output$SN_flags_status <- renderText({
      paste("Signal-to-Noise flags added to Skyline table!")
    })

  })

  # Parameter table-----------------------------------------------------------------
  parametersReactive <- reactive({
    data.frame(Parameter= c("Area minimum", "Retention time flexibility", "Blank Ratio Maximum", "Signal to Noise Minimum"),
               Values = c(input$area.min, input$RT.flex, input$blank.ratio.max, input$SN.min))
  })

  output$parameterTable <- renderDataTable({
    parametersReactive()
  }, options = list(dom = 't'))


  # Download both files-----------------------------------------------------------------
  output$QC_file <- downloadHandler(
    filename = function() {
      paste("QEQC_", Sys.Date(), skyline.filename(), sep = "")
    },
    content = function(file) {
      write.csv(skyline.SN.flagged(), file)
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
