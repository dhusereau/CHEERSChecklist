shinyServer(function(input, output, session) {
  

# NAVIGATION --------------------------------------------------------------

  # Move to PRISMA-A tab from Item 2 of the Main checklist
  observeEvent(input$gotoAb, {
    sectionId <- sapply(sectionsList, function(section) section$Value)
    currSection <- which(sectionId == input$sections)
    nextSection <- currSection + 1
    updateTabsetPanel(session, "sections", selected = sectionId[[nextSection]])
    shinyjs::runjs("document.getElementById('scrollAnchor').scrollIntoView({behavior: 'smooth'});")
  })

  # Carry across responses to the "Title" question in the Main checklist to the
  # PRISMA-A checklist
  observeEvent(input$ind_m_1, {
    updateRadioButtons(session, "ind_a_1", selected = input$ind_m_1)
  })
  
  observeEvent(input$ind_m_1_text, {
    updateRadioButtons(session, "ind_a_1_text", selected = input$ind_m_1_text)
  })

  
# TESTING -----------------------------------------------------------------

  # For testing - delete when done
  observeEvent(input$fill, {
    updateRadioButtons(session = session, "ind_m_1", selected = "Reported")
    updateTextInput(session = session, "ind_m_1", value = "Title, Page 2")
    updateTextInput(session = session, "ind_m_2", value = "Abstract, Page 2")
    updateTextInput(session = session, "ind_m_3", value = "Introduction, Line 7")
    updateTextInput(session = session, "ind_m_4", value = "Methods, Line 5")
    updateTextInput(session = session, "ind_m_5", value = "Methods, Line 10")
    updateTextInput(session = session, "ind_m_6", value = "Methods, Third Paragraph")
    updateTextInput(session = session, "ind_m_7", value = "Methods, Fourth Paragraph")
    updateTextInput(session = session, "ind_m_8", value = "Methods, Fifth Paragraph")
    updateTextInput(session = session, "ind_m_9", value = "Methods, Fifth Paragraph")
    updateTextInput(session = session, "ind_m_10", value = "Methods, Fifth Paragraph")
    updateTextInput(session = session, "ind_m_11", value = "Methods, Sixth Paragraph")
    updateTextInput(session = session, "ind_m_12", value = "Methods, Sixth Paragraph")
    updateTextInput(session = session, "ind_m_13", value = "Methods, Sixth Paragraph")
    updateTextInput(session = session, "ind_m_14", value = "Not reported")
    updateTextInput(session = session, "ind_m_15", value = "Methods, Seventh Paragraph")
    updateTextInput(session = session, "ind_m_16", value = "Methods, Eighth Paragraph")
    updateTextInput(session = session, "ind_m_17", value = "Methods , Last paragraph and Appendix")
    updateTextInput(session = session, "ind_m_18", value = "Methods , Last paragraph")
    updateTextInput(session = session, "ind_m_19", value = "Methods , Last paragraph")
    updateTextInput(session = session, "ind_m_20", value = "Not applicable")
    updateTextInput(session = session, "ind_m_21", value = "Appendix")
    updateTextInput(session = session, "ind_m_22", value = "Results, first paragraph and Table")
    updateTextInput(session = session, "ind_m_23", value = "Results, second paragraph")
    updateTextInput(session = session, "ind_m_24", value = "Results, third paragraph")
    updateTextInput(session = session, "ind_m_25", value = "Not reported")
    updateTextInput(session = session, "ind_m_26", value = "Discussion")
    updateTextInput(session = session, "ind_m_27", value = "End of manuscript")
    updateTextInput(session = session, "ind_m_28", value = "End of manuscript")
    })
  

# VALIDATION --------------------------------------------------------------

  # Checks which sections are complete, and enables download when they are
  # Three different set-ups: one for both, and one for each (Main ("_main") /
  # Abstract (_abs))
  
  # observe({
  #   shinyjs::disable("report")
  # })
  
  # Validation for both checklists
    whichComplete <- reactive({
      isComplete(answers = answers(), sectionsList = sectionsList, headList = headList)
    })
  
    isDownloadable <- reactive({
      all(whichComplete())
    })

  # Validation for Abstract checklist only
    whichComplete_abs <- reactive({
      isComplete(answers = answers(),
                 sectionsList = sectionsList[2],
                 headList = headList)
    })
    
    isDownloadable_abs <- reactive({
      all(whichComplete_abs())
    })
    

  # Validation for Main checklist only
    whichComplete_main <- reactive({
      isComplete(answers = answers(),
                 sectionsList = sectionsList[1],
                 headList = headList)
    })
    
    isDownloadable_main <- reactive({
      all(whichComplete_main())
    })

    # Enable download only for those that are complete
    
    observe({
      shinyjs::disable("report")
      if(isDownloadable() & input$report_type == "Main + Abstract"){
        shinyjs::enable("report")
      }
      if(isDownloadable_abs() & input$report_type == "_abs"){
        shinyjs::enable("report")
      }
      if(isDownloadable_main() & input$report_type == "_main"){
        shinyjs::enable("report")
      }
    })
  

# DYNAMIC FEEDBACK --------------------------------------------------------

  # Show exclamation beside items that are not complete
  output$trigger <- renderUI({
    if(isDownloadable()){
      tags$script("$('#report').tooltip('hide');")
    } else{
      tags$script("$('#report').tooltip('show');")
    }

  })
  
  # Change icon to tick when a question is answered
  observe({
    items <- getItemList(sectionsList, all = FALSE) # loop only on items
    
    for (item in items) {
      session$sendCustomMessage(type = "toggleChecker",
                                message = list(
                                  id = paste0(item, "Checker"),
                                  val = input[[item]],
                                  divId = paste0("div", item, "Checker")
                                ))
    }
    
  })
  
  # Change icons in Section headings
  observe({
    sectionValues <- sapply(sectionsList, function(sec)
      sec$Value)
    for (i in seq_along(sectionValues)) {
      session$sendCustomMessage(type = "toggleSectionIcon",
                                message = list(
                                  id = paste0(".icon", sectionValues[[i]]),
                                  val = ifelse(
                                    input$generatereport == 0 && !whichComplete()[[i]],
                                    "init",
                                    whichComplete()[[i]]
                                  )
                                ))
    }
  })
  

# CLEAN AND FORMAT ANSWERS ------------------------------------------------

  # Convert answers to list
  answers <- reactive({
    reactiveValuesToList(input)
  })
  
  # Create reactive values containing the dataframes produced from JSON in
  # global.R
  rv <- reactiveValues(df_m = df_m)
  
  # Once "Generate report" is clicked, create clean datasets
  observeEvent(input$generatereport,{
    
    # Create clean Main checklist 
    # Extract answers and text to dataframes
    ll <- answers()[grepl("ind_m_.*\\b", names(answers()))]
    df <- data.frame(ID = rep(names(ll), sapply(ll, length)),
                      response = unlist(ll))

    # Get item ID
    df$ID <- gsub("ind_m_", "",df$ID)

    # Merge to create dataframe containing ID, answer, answer text
    colnames(df)[2] <- "Location where item is reported"
    
    # Merge with dataframe containing question text
    df_m <- merge(df_m, df, by.x = "No", by.y = "ID", all.x = TRUE, sort = FALSE)
    
    # Order by seq and select relevant columns
    df_m <- df_m[order(df_m$seq),] %>%
      select(Domain,No,Label,"Location where item is reported")
    
    colnames(df_m)[1] <- "Topic"
    colnames(df_m)[2] <- "No."
    colnames(df_m)[3] <- "Item"
    
    # Assign to reactive value
    rv$df_m <- df_m
    
    
    # Create clean abstract checklist dataframe (with answers)
    ll <- answers()[grepl("ind_a_.*\\b", names(answers()))]
    df <- data.frame(ID = rep(names(ll), sapply(ll, length)),
                      response = unlist(ll))

    # Get item ID
    df$ID <- gsub("ind_a_", "",df$ID)
    df$response <- gsub("Reported","Yes",df$response)
    df$response <- gsub("Not reported","No",df$response)
    
    # Merge to create dataframe containing ID, answer, answer text
    colnames(df)[2] <- "Reported?"

  
  })
  
  

# DOWNLOADS ---------------------------------------------------------------

  # Download report
  output$report <- downloadHandler(
    filename = function() {
      format <- ifelse(input$format == "word", "docx", "pdf")
      paste0("CHEERS Checklist.", format)
    },
    
    content = function(file) {
      shiny::withProgress(message = paste0("Downloading checklist"),
                          value = 0.8,
                          {
                            tempReport <- file.path(tempdir(), "report.Rmd")
                            message(tempReport)
                            tempfile <- file.path(tempdir(), "reference.docx")
                            report_type <- ifelse(input$report_type == "Main + Abstract","",input$report_type)
                            
                            if (input$format == "PDF") {
                              
                              file.copy(paste0("www/doc/report_pdf",report_type,"_",input$orient,".Rmd"), tempReport, overwrite = TRUE)
                            } else {
                              file.copy(paste0("www/doc/report_word",report_type,".Rmd"), tempReport, overwrite = TRUE)
                              file.copy(paste0("www/doc/word-styles-reference-",input$orient,".docx"),
                                        tempfile,
                                        overwrite = TRUE)
                            }

                            
                            # Render the report
                            rmarkdown::render(
                              tempReport,
                              output_file = file,
                              params = list(df_m = rv$df_m ), 
                              envir = new.env(parent = globalenv())
                            )
                            
                          })
    }
  )
  

## Download citations
  
  output$downloadbib <- downloadHandler(
    filename = function() {
      paste("citation", ".bib", sep = "")
    },
    content = function(file) {
      file.copy("www/prismacitation.bib", file)
    }
  )
  
  output$downloadris <- downloadHandler(
    filename = function() {
      paste("citation", ".ris", sep = "")
    },
    content = function(file) {
      file.copy("www/prismacitation.ris", file)
    }
  )
  
  
})








