library(plyr)
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

options(scipen = 999)
options(shiny.error = browser)

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
        sidebarPanel(width = 2,
          wellPanel(id = "tPanel", style = "overflow-y:scroll; max-height: 500",
            
            helpText("Please confirm the settings on this sidebar panel before beginning the main analysis. 
                      These selections will affect the level of conservatism in your quality control and can be changed throughout analysis."),
                               
            helpText("If you are analyzing files produced by the QE, upload a blank matcher csv here.
                      If you are analying files produced by the TQS, upload a master list of compounds here for ion ratio identification. 
                      See Information tab for more details on both files."),
            
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

