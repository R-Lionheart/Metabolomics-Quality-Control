# Server function -----------------------------------------------------------------
server = function(input, output, session) {
  
  # Initial layout and data upload -----------------------------------------------------------------
  output$tags      <- renderText({paste("Your tags for sample matching are (QE only): ", input$std.tags)})
  output$minimum   <- renderText({paste("You have selected", input$area.min, "as area")})
  output$retention <- renderText({paste("You have selected", input$RT.flex, "as retention time flexibility")})
  output$blank     <- renderText({paste("You have selected", input$blank.ratio.max, "as the blank ratio maximum")})
  output$signal    <- renderText({paste("You have selected", input$SN.min, "as signal to noise flexibility")})
  output$ppm       <- renderText({paste("You have selected", input$ppm.flex, "as parts per million time flexibility")})
  
  output$classes_status <- renderText({paste("Before transformation:")})
  output$classes       <- renderText({paste(colnames(skyline.file()), sapply(skyline.file(), class), " \n")})
  output$runtypes      <- renderText({paste(unique(tolower(str_extract(skyline.file()$Replicate.Name, "(?<=_)[^_]+(?=_)"))))})
  output$SN            <- renderText({"Add those flags"})
  
  
  skyline.filename <- callModule(promptForFile, "skyline.file")
  skyline.file <- callModule(csvFile, "skyline.file", stringsAsFactors = FALSE)
  
  output$data1 <- renderDataTable({
    skyline.file()
  }, options = list(pageLength = 10))
  
  supporting.file <- callModule(csvFile, "supporting.file", stringsAsFactors = FALSE)
  output$data2 <- renderDataTable({
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
    output$data1 <- renderDataTable({
      skyline.transformed()
    }, options = list(pageLength = 10))
    output$classes_status <- renderText({paste("After transformation:")})
    output$classes <- renderText({paste(colnames(skyline.transformed()), sapply(skyline.transformed(), class), " \n")})
  })
  
  # Retention Time Table event -----------------------------------------------------------------
  Retention.Time.References <- NULL
  observeEvent(input$RT.Table, {
    Retention.Time.References <<- reactive({skyline.transformed() %>%
      # TODO (rlionheart): include filter(Replicate.Name %in% std.tags), check for correct RT table.
      # TODO (rlionheart): make the Rt a range, not a number!
      select(Mass.Feature, Retention.Time) %>%
      group_by(Mass.Feature) %>%
      summarise(RT.References = mean((Retention.Time), na.rm = TRUE))
      })
    output$Retention.Time.References <- renderDataTable({
      Retention.Time.References()
    }, options = list(pageLength = 10))
  })
  
  # Blank Reference Table event -----------------------------------------------------------------
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
    output$data1 <- renderDataTable({
      skyline.first.flagged()
    }, options = list(pageLength = 10))
  })
  
  # RT flags event -----------------------------------------------------------------
  skyline.RT.flagged <- NULL
  observeEvent(input$RT.flags, {
    skyline.RT.flagged <<- reactive({skyline.first.flagged() %>%
        # TODO (rlionheart): This is repetitive- figure out a solution for not repeating the code. This is a temp fix.
        group_by(Mass.Feature) %>%
        mutate(RT.Reference = mean((Retention.Time), na.rm = TRUE)) %>%
        mutate(RT.Flag = ifelse((abs((Retention.Time) - RT.Reference) > input$RT.flex), "RT.Flag", NA)) %>%
        select(-RT.Reference)
    })
    output$data1 <- renderDataTable({
      skyline.RT.flagged()
    })
  })
  
  # Blank flags event -----------------------------------------------------------------
  skyline.blk.flagged <- NULL
  observeEvent(input$blk.flags, {
    skyline.blk.flagged <<- reactive({skyline.RT.flagged() %>%
        # TODO (rlionheart): This is repetitive- figure out a solution for not repeating the code. This is a temp fix.
        # TODO (rlionheart): also double check this table itself- is it correct?
        left_join(Blank.Ratio.References(), by = c("Replicate.Name", "Mass.Feature")) %>%
        mutate(Blank.Flag = suppressWarnings(ifelse((as.numeric(Area) / as.numeric(Blank.Area)) < input$blank.ratio.max, "Blank.Flag", NA)))
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
      final.skyline <<- reactive({rbind.fill((skyline.blk.flagged()), standards)})
    } else {
    # TODO (rlionheart): Should blanks be added as well?
      output$std.status <- renderText({"No standards exist in this set. Table remains as is."})
      final.skyline <<- reactive(skyline.blk.flagged())
    }
    
    output$data1 <- renderDataTable({
      final.skyline()
    })
  })
  
  
  
  # Download -----------------------------------------------------------------
  
  output$Download <- downloadHandler(
    
    output$Download <- downloadHandler(
      filename = function() {
        paste("TQSQC_", Sys.Date(), skyline.filename(), sep = "")
      },
      content = function(file) {
        write.csv(final.skyline(), file)
      }
    )
  )
  
}


# -----------------------------------------------------------------
#shinyApp(ui, server)

# con <- file(paste("TQSQC_", basename(input_file), sep = ""), open = "wt")
# writeLines(paste("Hello! Welcome to the world of TQS Quality Control! ",
#                  "Minimum height for a real peak: ", min.height, ". ",
#                  "Minimum area for a real peak: ", area.min, ". ",
#                  "RT flexibility: ", RT.flex, ". ",
#                  "Ion ratio (IR) flexibility: ", IR.flex, ". ",
#                  "Blank can be this fraction of a sample: ", blk.thresh, ". ",
#                  "S/N ratio: " , SN.min, ". ",
#                  "Processed on: ", Sys.time(), ". ",
#                  sep = ""), con)
# write.csv(final.table, con, row.names = FALSE)
# close(con)
