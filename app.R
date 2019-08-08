library(plyr)
library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)

# -----------------------------------------------------------------
ui <- fluidPage(useShinyjs(),
  theme = shinytheme("sandstone"),
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  
  # Title Panel -----------------------------------------------------------------
  titlePanel("Targeted Quality Control for the Ingalls Laboratory"),

    # Sidebar Panel -----------------------------------------------------------------
    sidebarLayout(
      sidebarPanel(width = 2,
        wellPanel(id = "tPanel", style = "overflow-y:scroll; max-height: 500",
          helpText("More info here about file input and parameter selection."),

          helpText("If you are analyzing files produced by the QE, upload a blank matcher csv here.
                    If you are analying files produced by the TQS, upload a master list of compounds here. 
                    See Information tab for more details on files."),
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
      
    # Main Panel -----------------------------------------------------------------
    mainPanel(width = 10,
      tabsetPanel(type = "tabs",
        tabPanel("Information", h3("How can YOU use the Ingalls Lab Quality Control?", align = "center"), 
          div(p(HTML(paste0('This code, written in R, performs a user-defined quality-control check on output from the open-source mass spectrometer software ', 
          a(href = 'https://skyline.ms/project/home/software/Skyline/begin.view', "Skyline.")))),
          style = "font-family: 'times'; font-sil6pt"),
          p("The application is split into two sections: targeted and untargeted metabolomic analysis, which can be accessed via the tabs at the top of the page (eventually). 
             Within each section, choose between code for Thermo Q Exactive HF (Orbitrap) and a Waters Xevo TQ-S (triple quadrupole) mass spectrometers. The code will clean up your peaks.
             In fact, beneath this paragraph is a lovely visualization of that process.", style = "font-family: 'times'; font-sil6pt"),
          img(src = "QC.png", height = 200, width = 200),
          br(),
          h4("LCMS Setup"),
          div(p(HTML(paste0("Samples should be run in the following manner for the quality control code and ", 
          a(href = "https://github.com/IngallsLabUW/B-MIS-normalization", "B-MIS Normalization"), "- a process used for matching internal standards."))),
          style = "font-family: 'times'; font-sil6pt"),
          br(),
          p("Please label all samples in the following manner:", style = "font-family: 'times'; font-sil6pt", 
          span(strong("Date_RunType_AdditionalID (e.g. 161018_Std_FirstStandardinH20)."), 
          ("RunType refers to whether the sample is a standard (Std), sample (Smp), pooled (poo), or blank (blk)."), style = "font-family: 'times'; font-sil6pt"),
          p("- Standards run (all mixed) at least once at the beginning and end of the run", style = "font-family: 'times'; font-sil6pt"),
          p("- Standards run (in representative matrix, all mixed) at least once the beginning and end of the run. Example label: 161019_Std_FirstStandardinMatrix", style = "font-family: 'times'; font-sil6pt"),
          p("- Blanks run (preferably method/filter blanks) at least once. Example label: 161018_Blk_FirstBlank", style = "font-family: 'times'; font-sil6pt"),
          p("- A pooled sample run at least three times throughout the run. Example label:161018_Poo_PooledSample_1", style = "font-family: 'times'; font-sil6pt"),
          p("- Samples. Example label: Date_Smp_AdditionalID_Rep", style = "font-family: 'times'; font-sil6pt"))),
    
    # QE tabPanel -----------------------------------------------------------------
    tabPanel("QExactive",
      fluidRow(
        column(4, helpText("This is some helpful text"), 
               actionButton("transform", "Change variable classes"),
               actionButton("Stds", "Re-add standards"),
               br(),
               br(),
               wellPanel(strong("Your run types are:"), textOutput("runtypes"), hr(), textOutput("std.status")),
               br(),
               wellPanel(strong("Retention Time Reference Table"), dataTableOutput("Retention.Time.References"))
        ),
        column(3, wellPanel(strong("Your Quality Control Parameters are:"),
          textOutput("machine"),
          textOutput("tags"),
          textOutput("minimum"),
          textOutput("retention"),
          textOutput("blank"),
          textOutput("signal"),
          textOutput("ppm"),
          tags$head(tags$style())),
          br()
        ),
        column(4, wellPanel(strong("Dataset Classes"),
          textOutput("classes_status"),
          textOutput("classes")),
          wellPanel(strong("Blanks Reference Table"), dataTableOutput("Blank.Ratio.References"))
        )
      ),
      hr(),
      fluidRow(
        column(10,
          absolutePanel(
            dataTableOutput("data1"),
            dataTableOutput("data2")
          )
        ),
        column(2,
          actionButton("SN", "Signal to Noise flags"),
          br(),
          br(),
          actionButton("RT", "Retention Time flags"),
          br(),
          br(),
          actionButton("Blk", "Blank flags"),
          actionButton("Blk2", "Blank Flags Part II")
        )    
      )
    ),

    # TQS tabPanel -----------------------------------------------------------------
    tabPanel("Triple-Quadropole",
      helpText("Stay tuned for future developments!"))
    )
  )
))




# Server function -----------------------------------------------------------------
server = function(input, output, session) {
  output$machine   <- renderText({paste("Your machine type is", input$machine.type)})
  output$tags      <- renderText({paste("Your tags for sample matching are (QE only): ", input$std.tags)})
  output$minimum   <- renderText({paste("You have selected", input$area.min, "as area")})
  output$retention <- renderText({paste("You have selected", input$RT.flex, "as retention time flexibility")})
  output$blank     <- renderText({paste("You have selected", input$blank.ratio.max, "as the blank ratio maximum")})
  output$signal    <- renderText({paste("You have selected", input$SN.min, "as signal to noise flexibility")})
  output$ppm       <- renderText({paste("You have selected", input$ppm.flex, "as parts per million time flexibility")})
  
  output$classes_status <- renderText({paste("Before transformation:")})
  output$classes <- renderText({paste(colnames(skyline.file()), ":", lapply(skyline.file(), class))})
  
  output$runtypes   <- renderText({paste(unique(tolower(str_extract(skyline.file()$Replicate.Name, "(?<=_)[^_]+(?=_)"))))})
  output$SN         <- renderText({"Add those flags"})


  skyline.file <- callModule(csvFile, "skyline.file", stringsAsFactors = FALSE)
  output$data1 <- renderDataTable({
    skyline.file()
  })

  supporting.file <- callModule(csvFile, "supporting.file", stringsAsFactors = FALSE)
  output$data2 <- renderDataTable({
    supporting.file()
  })

  # First transform event -----------------------------------------------------------------
  skyline.transformed <- NULL
  observeEvent(input$transform, {
    skyline.transformed <<- reactive({skyline.file() %>% 
      select(-Protein.Name, -Protein) %>%
      mutate(Retention.Time = suppressWarnings(as.numeric(as.character(Retention.Time)))) %>%
      mutate(Area           = suppressWarnings(as.numeric(as.character(Area)))) %>%
      mutate(Background     = suppressWarnings(as.numeric(as.character(Background)))) %>%
      mutate(Mass.Error.PPM = suppressWarnings(as.numeric(as.character(Mass.Error.PPM)))) %>%
      rename(Mass.Feature   = Precursor.Ion.Name)
    })
    output$data1 <- renderDataTable({
      skyline.transformed()
    })
    output$classes_status <- renderText({paste("After transformation:")})
    output$classes <- renderText({paste(colnames(skyline.transformed()), ":", lapply(skyline.transformed(), class))})
    # TODO (rlionheart): include filter(Replicate.Name %in% std.tags), check for correct RT table.
    output$Retention.Time.References <- renderDataTable(skyline.transformed() %>%
      select(Mass.Feature, Retention.Time) %>%
      group_by(Mass.Feature) %>%
      summarise(RT.References = mean((Retention.Time), na.rm = TRUE)))
  })

 
  
  
  # First flags event -----------------------------------------------------------------
  skyline.first.flagged <- NULL
  observeEvent(input$SN, {
    skyline.first.flagged <<- reactive({skyline.transformed() %>% 
      filter(Replicate.Name %in% supporting.file()$Replicate.Name) %>%
      mutate(SN.Flag       = ifelse(((Area / Background) < input$SN.min), "SN.Flag", NA)) %>%
      mutate(ppm.Flag      = ifelse(abs(Mass.Error.PPM) > input$ppm.flex, "ppm.Flag", NA)) %>%
      mutate(area.min.Flag = ifelse((Area < input$area.min), "area.min.Flag", NA))
    })
    output$data1 <- renderDataTable({
      skyline.first.flagged()
    })
  })
  
  # RT flags event -----------------------------------------------------------------
  skyline.RT.flagged <- NULL
  observeEvent(input$RT, {
    skyline.RT.flagged <<- reactive({skyline.first.flagged() %>%
      # TODO (rlionheart): This is repetitive- figure out a solution for not repeating the code. This is a temp fix.
      group_by(Mass.Feature) %>%
      mutate(RT.Reference = mean((Retention.Time), na.rm = TRUE)) %>%
      mutate(RT.Flag = ifelse((abs((Retention.Time) - RT.Reference) > input$RT.flex), "RT.Flag", NA))
    })
    output$data1 <- renderDataTable({
      skyline.RT.flagged()
    })
  })
  
  # Blank flags event -----------------------------------------------------------------
  Blank.Ratio.References <- NULL
  observeEvent(input$Blk, {
    Blank.Ratio.References <<- reactive({skyline.file() %>%
      #TODO (rlionheart): see if this repetitive code can be dropped, and the same transformation function can be applied to multiple files.
      #TODO (rlionheart): also double check this table itself- is it correct?
      filter(Replicate.Name %in% supporting.file()$Blank.Name) %>%
      select(-Protein.Name, -Protein) %>%
      rename(Mass.Feature = Precursor.Ion.Name) %>%
      rename(Blank.Name = Replicate.Name,
             Blank.Area = Area) %>%
      select(Blank.Name, Mass.Feature, Blank.Area) %>%
      left_join(supporting.file(), by = "Blank.Name") %>%
      arrange(desc(Blank.Area)) %>%
      group_by(Mass.Feature, Replicate.Name) %>%
      filter(row_number() == 1)
    })
    output$Blank.Ratio.References <- renderDataTable({
      Blank.Ratio.References()
    })
  })
  
  skyline.blk.flagged <- NULL
  observeEvent(input$Blk2, {
    skyline.blk.flagged <<- reactive({skyline.RT.flagged() %>%
      # TODO (rlionheart): This is repetitive- figure out a solution for not repeating the code. This is a temp fix.
      # TODO (rlionheart): also double check this table itself- is it correct?
      left_join(Blank.Ratio.References()) %>%
      mutate(Blank.Flag = ifelse((as.numeric(Area) / as.numeric(Blank.Area)) < input$blank.ratio.max, "Blank.Flag", NA))
    })
    output$data1 <- renderDataTable({
      skyline.blk.flagged()
    })
  })
  
  # Re-adding stds event -----------------------------------------------------------------
  final.skyline <- NULL
  observeEvent(input$Stds, {
    Stds.test <- grepl("_Std_", skyline.file()$Replicate.Name)
    if (any(Stds.test == TRUE)) {
      output$std.status <- renderText({"Standards in set. Joining them to the bottom of the dataset!"})
      standards <- skyline.transformed()[grep("Std", skyline.transformed()$Replicate.Name), ]
    } else {
      output$std.status <- renderText({"Nothing to see here."})
    }

    final.skyline <<- reactive({rbind.fill((skyline.blk.flagged()), standards)
    })
  
    output$data1 <- renderDataTable({
      final.skyline()
    })
  })
}

# Stds.test <- grepl("_Std_", skyline.output$Replicate.Name)
# 
# if (any(Stds.test == TRUE)) {
#   print("There are standards in this run. Joining standard samples to the bottom of the dataset!", quote = FALSE)
#   standards <- skyline.classes.transformed[grep("Std", skyline.classes.transformed$Replicate.Name), ]
#   last.join <- rbind.fill(last.join, standards)
# } else {
#   print("No standards exist in this set.")
# }

# -----------------------------------------------------------------
shinyApp(ui, server)


