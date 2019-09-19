# Server function -----------------------------------------------------------------
server = function(input, output, session) {
  
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
    
    output$skyline1 <- renderDataTable({
      skyline.transformed()
    }, options = list(pageLength = 10))
    
    output$classes_status <- renderText({paste("After transformation:")})
    output$classes <- renderText({paste(colnames(skyline.transformed()), ":", sapply(skyline.transformed(), class), " \n")})
  })
  
  # Retention Time Table event -----------------------------------------------------------------
  Retention.Time.References <- NULL
  observeEvent(input$RT.Table, {
    Retention.Time.References <<- reactive({skyline.transformed() %>%
      # TODO (rlionheart): include filter(Replicate.Name %in% std.tags).
      # TODO (rlionheart): What about when there are no standards?
      # TODO (rlionheart): Can't reference this table during Flags event.
      select(Replicate.Name, Mass.Feature, Retention.Time) %>%
      mutate(Run.Type = (tolower(str_extract(skyline.transformed()$Replicate.Name, "(?<=_)[^_]+(?=_)")))) %>%
      group_by(Mass.Feature) %>%
      filter(Run.Type == "std") %>%
      mutate(RT.min = min(Retention.Time, na.rm = TRUE)) %>%
      mutate(RT.max = max(Retention.Time, na.rm = TRUE)) %>%
      select(Mass.Feature, RT.min, RT.max) %>%
      unique()
      })
      
    output$Retention.Time.References <- renderDataTable({
      Retention.Time.References()
    }, options = list(pageLength = 10))
  })
  
  # Blank Reference Table event -----------------------------------------------------------------
  Blank.Ratio.References <- NULL
  observeEvent(input$Blk, {
    Blank.Ratio.References <<- reactive({skyline.file() %>%
        filter(Replicate.Name %in% supporting.file()$Blank.Name) %>%
        rename(Blank.Name = Replicate.Name,
               Blank.Area = Area) %>%
        select(-Protein.Name, -Protein) %>%
        rename(Mass.Feature = Precursor.Ion.Name) %>%
        select(Blank.Name, Mass.Feature, Blank.Area) %>%
        left_join(supporting.file, by = "Blank.Name") %>% 
        select(-Blank.Name) %>%
        arrange(desc(Blank.Area)) %>%
        group_by(Mass.Feature, Replicate.Name) %>% 
        filter(row_number() == 1)
        

        #select(Blank.Name, Mass.Feature, Blank.Area) %>%
        unique()
    })
    
    ###


    ####
    
    
    output$Blank.Ratio.References <- renderDataTable({
      Blank.Ratio.References()
    }, options = list(pageLength = 10))
  })

  # First flags event -----------------------------------------------------------------
  skyline.first.flagged <- NULL
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
  skyline.RT.flagged <- NULL
  observeEvent(input$RT.flags, {
    skyline.RT.flagged <<- reactive({skyline.first.flagged() %>%
      group_by(Mass.Feature) 
      # TODO (rlionheart): Figure how wtf is happening here. Can't reference another table?
      
      #mutate(RT.max = ifelse((Retention.Time.References()$RT.max > 5), "yay", "nay"))
      #mutate(RT.Reference = mean((Retention.Time), na.rm = TRUE)) %>%
      #mutate(RT.Flag = ifelse((Retention.Time >= (Retention.Time.References()$RT.max + input$RT.flex) | Retention.Time <= (Retention.Time.References()$RT.min - input$RT.flex)), "RT.Flag", NA))
      #select(-RT.Reference)
    })
    output$skyline1 <- renderDataTable({
      skyline.RT.flagged()
    })
  })
  
  # Blank flags event -----------------------------------------------------------------
  skyline.blk.flagged <- NULL
  observeEvent(input$blk.flags, {
    skyline.blk.flagged <<- reactive({skyline.RT.flagged() %>%
        # TODO (rlionheart): Same issue as Retention time. How to reference another table?
        # TODO (rlionheart): also double check this table itself- is it correct?
        left_join(Blank.Ratio.References(), by = c("Replicate.Name", "Mass.Feature")) %>%
        mutate(Blank.Flag = suppressWarnings(ifelse((as.numeric(Area) / as.numeric(Blank.Area)) < input$blank.ratio.max, "Blank.Flag", NA))) %>%
        select(-Blank.Name, -Blank.Area)
    })
    output$skyline1 <- renderDataTable({
      skyline.blk.flagged()
    })
  })
  
  # Re-adding stds event -----------------------------------------------------------------
  skyline.stds.added <- NULL
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
  
  #####
   final.skyline <- NULL
   observeEvent(input$addrows, {
     final.skyline <<- reactive({final.skyline() %>%
       bind_rows(parametersReactive())
     })
     output$skyline1 <- renderDataTable({
       final.skyline()
     })
   })
   



  #####
  
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