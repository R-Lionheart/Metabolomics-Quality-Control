library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  
  titlePanel("Quality Control for the Ingalls Laboratory"),
  
  sidebarLayout(
    sidebarPanel(
      wellPanel(id = "tPanel", style = "overflow-y:scroll; max-height: 1000px",
      helpText("More info here about file input and parameter selection."),
      
      radioButtons("machine.type", h4("Which machine is the output file from?"),
                   choices = list("Xevo TQS" = 1, "QExactive (QE)" = 2), selected = 1),
      hr(),
      fileInput("skylinefile", h4("Skyline file input")),
      hr(),
      helpText("Pick the minimum height to be counted as a 'real' peak (QE suggestion: HILIC - 1000, Cyano - 5000)"),
      sliderInput("area.min", h4("Area Minimum"), 
                  min = 1000, step = 1000, max = 5000, value = 1000),
      hr(),
      helpText("Pick retention time (RT) flexibility (QE suggestion: +/- 0.4 min for HILIC, +/- 0.2 min for Cyano)"),
      sliderInput("RT.flex", h4("Retention Time Flexibility"),
                  min = 0.0, step = 0.1, max = 1.0, value = 0.2),
      hr(),
      helpText("Pick signal size comparison between sample and blank to merit inclusion (QE suggestion: +/- 0.2)"),
      sliderInput("blank.ratio.max", h4("Blank Ratio Maximum"),
                  min = 0.0, step = 0.1, max = 0.5, value = 0.3),
      hr(),
      helpText("Pick acceptable signal to noise ratio value. Note: broader peaks create more background noise(QE suggestion: 5 for Cyano, 4 for HILIC)"),
      sliderInput("SN.min", h4("Signal to Noise Ratio"),
                  min = 1, step = 1, max = 5, value = 3),
      hr(),
      helpText("Pick an absolute value for a cutoff for parts per million (ppm) (QE suggestion: 7)"),
      sliderInput("ppm.flex", h4("Parts per Million"),
                  min = 1, step = 1, max = 10, value = 5)
      )),
    
    mainPanel(
      h5("Eventually put in tabs here, one for Targeted and one for Untargeted"),
      h3("How can YOU use the Ingalls Lab Quality Control?", align = "center"),
      div(p(HTML(paste0('This code, written in R, performs a user-defined quality-control check on output from the open-source mass spectrometer software ', a(href = 'https://skyline.ms/project/home/software/Skyline/begin.view', "Skyline.")))),
          style = "font-family: 'times'; font-sil6pt"),
      p("The application is split into two sections: targeted and untargeted metabolomic analysis, which can be accessed via the tabs at the top of the page (eventually).
        Within each section, choose between code for Thermo Q Exactive HF (Orbitrap) and a Waters Xevo TQ-S (triple quadrupole) mass spectrometers. The code will clean up your peaks.
        In fact, beneath this paragraph is a lovely visualization of that process.", 
        style = "font-family: 'times'; font-sil6pt"),
      img(src = "QC.png", height = 200, width = 200),
      br(),
      h4("LCMS Setup"),
      div(p(HTML(paste0("Samples should be run in the following manner for the quality control code and ", a(href = "https://github.com/IngallsLabUW/B-MIS-normalization", "B-MIS Normalization"), "- a process used for matching internal standards."))),
          style = "font-family: 'times'; font-sil6pt"),
      br(),
      p("Please label all samples in the following manner:", span(strong("Date_RunType_AdditionalID (e.g. 161018_Std_FirstStandardinH20)."), ("RunType refers to whether the sample is a standard (Std), sample (Smp), pooled (poo), or blank (blk).")),
      p("- Standards run (all mixed) at least once at the beginning and end of the run"),
      p("- Standards run (in representative matrix, all mixed) at least once the beginning and end of the run. Example label: 161019_Std_FirstStandardinMatrix"),
      p("- Blanks run (preferably method/filter blanks) at least once. Example label: 161018_Blk_FirstBlank"),
      p("- A pooled sample run at least three times throughout the run. Example label:161018_Poo_PooledSample_1"),
      p("- Samples. Example label: Date_Smp_AdditionalID_Rep")
      )
    )
  )
)


server <- function(input, output) {
  
}

shinyApp(ui, server)

