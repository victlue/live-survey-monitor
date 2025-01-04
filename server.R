server <- function(input, output, session) {
  
  # Add error handling
  # options(shiny.error = function() {
  #   # Get the last error
  #   err <- geterrmessage()
  #   # Log it to stderr (this will show up in the Docker logs)
  #   cat(file=stderr(), "App Error:", err, "\n")
  #   # You can also show it to the user if you want
  #   showModal(modalDialog(
  #     title = "Error",
  #     "An error occurred. Please check your inputs and try again.",
  #     easyClose = TRUE
  #   ))
  # })
  #
  # # Log when server starts
  # cat(file=stderr(), "Server starting...\n")
  #
  # print("Server starting...")
  # print("Working directory:", getwd())
  # print("Files in directory:", list.files())
  
  # Reactive values for managing state
  rv <- reactiveValues(
    current_survey = NULL,
    show_dashboard = FALSE,
    surveys = load_surveys(),
    display_tables = FALSE,
    current_weights_csv = NULL
  )
  
  # Render list of active surveys
  output$surveyList <- renderUI({
    surveys <- rv$surveys
    
    if (length(surveys) == 0) {
      return(h4("No active survey monitors"))
    }
    
    survey_ids <- names(surveys)
    lapply(survey_ids, function(id) {
      survey <- surveys[[id]]
      div(
        class = "survey-card",
        h4(survey$name),
        p(survey$description),
        div(class = "last-updated",
            "Last updated: ", format(as.POSIXct(survey$last_updated), "%Y-%m-%d %H:%M:%S")),
        div(
          class = "action-buttons",
          actionButton(paste0("view_", id), "View/Refresh",
                       class = "btn-primary",
                       onclick = sprintf("Shiny.setInputValue('last_btn', '%s', {priority: 'event'})", paste0("view_", id))),
          actionButton(paste0("delete_", id), "Remove",
                       class = "btn-danger",
                       onclick = sprintf("Shiny.setInputValue('last_btn', '%s', {priority: 'event'})", paste0("delete_", id)))
        )
      )
    })
  })
  
  # Handle create new survey
  observeEvent(input$createNew, {
    showModal(modalDialog(
      title = "Create New Survey Monitor",
      
      textInput("surveyName", "Survey Name"),
      textAreaInput("surveyDesc", "Description"),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("createSurveyConfirm", "Create")
      )
    ))
  })
  
  # Handle survey creation confirmation
  observeEvent(input$createSurveyConfirm, {
    req(input$surveyName)
    
    # Generate unique ID for new survey
    survey_id <- paste0("survey_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    
    # Create new survey entry
    rv$surveys[[survey_id]] <- list(
      name = input$surveyName,
      description = input$surveyDesc,
      created_at = format(Sys.time()),
      last_updated = format(Sys.time()),
      alchemer_link = "",
      survey_id = "",
      files = list()
    )
    
    # Save to file
    save_surveys(rv$surveys)
    
    # Switch to dashboard
    rv$current_survey <- survey_id
    rv$show_dashboard <- TRUE
    print("hi 55")
    rv$display_tables <- FALSE
    updateTabsetPanel(session, "mainNav", selected = "dashboard")
    removeModal()
  })
  
  observeEvent(input$last_btn, {
    btn_id <- input$last_btn
    
    if (startsWith(btn_id, "view_")) {
      id <- sub("view_", "", btn_id)
      
      # Handle View/Refresh button click
      rv$current_survey <- id
      rv$show_dashboard <- TRUE
      rv$display_tables <- FALSE  # Hide tables
      updateTabsetPanel(session, "mainNav", selected = "dashboard")
    }
    
    else if (startsWith(btn_id, "delete_")) {
      id <- sub("delete_", "", btn_id)
      
      # Handle Delete button click
      showModal(modalDialog(
        title = "Confirm Removal",
        "Are you sure you want to remove this survey monitor?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            inputId = paste0("confirmDelete_", id),
            label = "Remove",
            class = "btn-danger",
            onclick = sprintf("Shiny.setInputValue('last_btn', '%s', {priority: 'event'})", paste0("confirmDelete_", id))
          )
        )
      ))
    }
    
    else if (startsWith(btn_id, "confirmDelete_")) {
      id <- sub("confirmDelete_", "", btn_id)
      
      # Handle Confirm Delete button click
      rv$surveys[[id]] <- NULL
      save_surveys(rv$surveys)
      removeModal()
    }
  })
  
  # Render dashboard UI
  output$dashboardUI <- renderUI({
    req(rv$show_dashboard)
    survey <- rv$surveys[[rv$current_survey]]
    
    fluidPage(
      # Pre-filled dashboard UI
      div(
        class = "input-container",
        div(
          class = "data-inputs-column",
          textInput("alchemer_link", "Realtime Alchemer Export Link",
                    value = survey$alchemer_link),
          fileInput("alchemer_file", "Alchemer Datafile",
                    accept = c(".csv", ".sav", ".dta")),
          fileInput("kgs_file", "KGS Datafile",
                    accept = c(".csv", ".sav", ".dta")),
          fileInput("weighted_file", "Weighted file",
                    accept = c(".csv", ".sav", ".dta"))
        ),
        div(
          class = "other-inputs-column",
          if (!is.null(survey$weights_file)) {
            div(
              "Current weights file loaded: ",
              tags$b(survey$weights_file$name),
              tags$br(),
              tags$br()
            )
          },
          fileInput("weights_csv", "Weights CSV",
                    accept = c(".csv")),
          textInput("survey_id", "Alchemer Survey ID Number",
                    value = survey$survey_id),
          actionButton("submit_btn", "Refresh Data", class = "btn-primary submit-btn"),
          uiOutput("download_button_ui")
        )
      ),
      
      # Your existing table layouts
      fluidRow(
        column(3,
               div(class = "table-wrapper",
                   h4("Alchemer Quotas"),
                   DTOutput("table1")
               ),
               style = "min-width: 0;"
        ),
        column(5,
               div(class = "table-wrapper",
                   h4("Demographic Weighting Targets"),
                   DTOutput("table2")
               ),
               style = "min-width: 0;"
        ),
        column(4,
               div(class = "table-wrapper",
                   h4("Topline and Crosstabs"),
                   uiOutput("topline_stats"),
                   uiOutput("dropdown_container"),
                   uiOutput("category_submit_button"),
                   DTOutput("table3")
               ),
               style = "min-width: 0;"
        )
      )
    )
  })
  
  # Update survey data when submitted
  observeEvent(input$submit_btn, {
    values$submitted <- FALSE
    print(paste("rv$current_survey is:", rv$current_survey))
    req(rv$current_survey)
    rv$display_tables <- TRUE
    
    # Save uploaded files
    if (!is.null(input$alchemer_file)) {
      file_path <- save_upload(input$alchemer_file, rv$current_survey, "alchemer")
      rv$surveys[[rv$current_survey]]$files$alchemer <- file_path
    }
    if (!is.null(input$kgs_file)) {
      file_path <- save_upload(input$kgs_file, rv$current_survey, "kgs")
      rv$surveys[[rv$current_survey]]$files$kgs <- file_path
    }
    if (!is.null(input$weighted_file)) {
      file_path <- save_upload(input$weighted_file, rv$current_survey, "weighted")
      rv$surveys[[rv$current_survey]]$files$weighted <- file_path
    }
    if (!is.null(input$weights_csv)) {
      file_path <- save_upload(input$weights_csv, rv$current_survey, "weights")
      rv$surveys[[rv$current_survey]]$files$weights <- file_path
      rv$surveys[[rv$current_survey]]$weights_file <- list(
        name = input$weights_csv$name,
        path = file_path
      )
    }
    
    # Update survey record
    rv$surveys[[rv$current_survey]]$alchemer_link <- input$alchemer_link
    rv$surveys[[rv$current_survey]]$survey_id <- input$survey_id
    rv$surveys[[rv$current_survey]]$last_updated <- format(Sys.time())
    
    # Save changes
    save_surveys(rv$surveys)
    
    
    # Continue with your existing submit_btn logic for processing the survey data...
    # [Insert your existing survey processing code here]
    has_data_source <- (!is.null(input$alchemer_link) && input$alchemer_link != "") ||
      (!is.null(input$alchemer_file)) ||
      (!is.null(input$weighted_file)) ||
      (!is.null(input$kgs_file))
    
    # Check if weights file is provided
    has_weights <- !is.null(input$weights_csv) ||
      (!is.null(rv$surveys[[rv$current_survey]]$weights_file$path) &&
         file.exists(rv$surveys[[rv$current_survey]]$weights_file$path))
    
    if (!has_data_source) {
      showModal(modalDialog(
        title = "Missing Input",
        "Please provide one of the following: Alchemer Export Link, Alchemer Datafile, or KGS Datafile.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    if (!is.null(input$weights_csv)) {
      weights <- read.csv(input$weights_csv$datapath)
      rv$current_weights_csv <- weights
    } else if (!is.null(rv$surveys[[rv$current_survey]]$weights_file$path)) {
      weights <- read.csv(rv$surveys[[rv$current_survey]]$weights_file$path)
      rv$current_weights_csv <- weights
    } else {
      showModal(modalDialog(
        title = "Missing Input",
        "Please provide a Weights CSV file.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    # Set submitted flag to TRUE
    values$submitted <- TRUE
    
    if (!is.null(input$survey_id) && input$survey_id != ""){
      values$alchemer_quotas_submitted <- TRUE
    }
    
    # Disable the inputs
    # disable("alchemer_link")
    # disable("alchemer_file")
    # disable("kgs_file")
    # disable("survey_id")
    # disable("weights_csv")
    # disable("submit_btn")
  })
  
  # [Insert rest of your existing server logic here, including the table renders
  # and topline calculations from your original app]
  
  processed_data <- eventReactive(values$submitted, {
    
    api_token <- "[REDACTED]"
    api_secret <- "[REDACTED]"
    
    
    withProgress(message = 'Processing data', value = 0, {
      gist_url <- "https://gist.githubusercontent.com/victlue/161ee8289efbcd4f1e59f998eb9e60dd/raw/processed-data.R"
      
      
      # Replace with your actual Gist ID
      tryCatch({
        response <- GET(gist_url)
        if (status_code(response) == 200) {
          code_text <- rawToChar(response$content)
          parsed_code <- try(parse(text = code_text))
          eval(parsed_code)
        } else {
          warning("Failed to fetch code from Gist. Status code: ", status_code(response))
        }
      }, error = function(e) {
        warning("Error: ", e$message)
      })
      

    })
  }, ignoreNULL = TRUE)
  
  # Render the data tables when data is processed
  table_opts <- list(
    pageLength = -1,
    searching = FALSE,
    info = FALSE,
    ordering = FALSE,
    scrollX = FALSE,
    dom = 't',
    columnDefs = list(
      list(
        targets = '_all',
        render = JS("
          function(data, type, row, meta) {
            if (type === 'display' && meta.row !== undefined) {
              return '<div title=\"' + data + '\" style=\"width:100%; white-space:nowrap; overflow:hidden; text-overflow:ellipsis;\">' + data + '</div>';
            }
            return data;
          }
        ")
      )
    )
  )
  
  
  output$download_button_ui <- renderUI({
    req(rv$display_tables)
    req(values$submitted)
    downloadButton("download_weighted", "Download Weighted SAV File",
                   class = "btn-success",
                   style = "width: 100%;")
  })
  
  output$download_weighted <- downloadHandler(
    filename = function() {
      # Create a filename with current timestamp
      "weighted-file.sav"
    },
    content = function(file) {
      req(processed_data()$df4)
      # Write the processed data to SPSS format
      haven::write_sav(
        setNames(processed_data()$df4, gsub("\\s", "_", colnames(processed_data()$df4))),
        file
      )
    }
  )
  
  
  
  # Render table 1
  output$table1 <- DT::renderDT(server=FALSE, {
    print(paste("Checking rv$display_tables:", rv$display_tables))
    req(rv$display_tables)
    print(paste("Checking values$alchemer_quotas_submitted:", values$alchemer_quotas_submitted))
    req(values$submitted && values$alchemer_quotas_submitted)
    df1 <- processed_data()$df1
    
    datatable(
      df1[, c("Attribute", "Completes", "Max Cap", "Quota Filled")],
      options = list(
        pageLength = -1,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE,
        scrollX = TRUE,
        autoWidth = TRUE,
        dom = 't',
        columnDefs = list(
          # # Set specific widths for each column
          # list(targets = 0, width = "80px"),  # name column
          # list(targets = 1, width = "70px"),  # responses column
          # list(targets = 2, width = "70px"),  # limit column
          list(
            targets = 3,  # quota_filled column
            # width = "100px",
            render = JS("
              function(data, type, row) {
                if (type === 'display') {
                  var value = parseFloat(data);
                  var color = value >= 100 ? '#28a745' :
                             value >= 75 ? '#17a2b8' : '#ffc107';
                  return '<div style=\"background: ' + color + '; width: ' +
                         Math.min(value, 100) + '%; height: 100%; color: white; text-align: right; padding-right: 4px;\">' +
                         data + '</div>';
                }
                return data;
              }
            ")
          )
        )
      ),
      class = 'compact stripe',
      rownames = FALSE
    ) %>%
      formatStyle(
        columns = c("Attribute", "Completes", "Max Cap", "Quota Filled"),
        fontSize = '10px'
      )
  })
  
  # Render table 2
  output$table2 <- DT::renderDT(server=FALSE, {
    req(rv$display_tables)
    req(values$submitted)
    df2 <- processed_data()$df2
    
    datatable(
      df2,
      options = list(
        pageLength = -1,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE,
        scrollX = TRUE,
        autoWidth = TRUE,
        dom = 't',
        columnDefs = list(
          list(
            targets = '_all',
            render = JS("
              function(data, type, row, meta) {
                if (type === 'display' && meta.row !== undefined) {
                  return '<div title=\"' + data + '\" style=\"width:100%; white-space:nowrap; overflow:hidden; text-overflow:ellipsis;\">' + data + '</div>';
                }
                return data;
              }
            ")
          )
        )
      ),
      class = 'compact stripe',
      rownames = FALSE
    ) %>%
      formatStyle(
        columns = c('Category', 'Attribute', 'Weighting Benchmark', 'Current Unweighted %'),
        fontSize = '10px',
        width = '100px'
      ) %>%
      formatStyle(
        'Ratio',
        fontSize = '10px',
        width = '70px',
        backgroundColor = styleInterval(
          cuts = c(0.5, 0.65, .85, 1.15, 1.5, 2),  # one less cut than values
          values = c(
            '#d73027', # dark red for < 0.5
            '#f46d43', # orange-red for 0.5-0.75
            '#fdae61', # light orange for 0.75-1
            '#ffffff', # white for 1-1.25
            '#e5f4e3',
            '#66bd63', # green for 1.25-1.5
            '#1a9850'  # dark green for > 1.5
          )
        ),
        color = styleInterval(
          cuts = c(0.75, 1.25),
          values = c('white', 'black', 'white')
        )
      ) %>%
      formatRound('Ratio', digits = 3) %>%
      formatStyle(
        'Difference',
        fontSize = '10px',
        width = '70px',
        backgroundColor = styleInterval(
          cuts = c(-0.1, -0.075, -0.045, 0.045, 0.075, 0.1),
          values = c(
            '#d73027', # dark red for < -0.1
            '#f46d43', # orange-red
            '#fdae61', # light orange
            '#ffffff', # white for ~0
            '#e5f4e3', # very light green
            '#66bd63', # green
            '#1a9850'  # dark green for > 0.1
          )
        ),
        color = styleInterval(
          cuts = c(-0.05, 0.05),
          values = c('white', 'black', 'white')
        )
      ) %>%
      formatRound('Difference', digits = 3)
  })
  
  output$topline_stats <- renderUI({
    req(values$submitted)
    req(rv$display_tables)
    
    # Calculate your statistics here
    # Example statistic - replace with your actual calculation:
    survey <- processed_data()$df4
    total_completes <- nrow(survey)
    
    calculate_design_effect <- function(weights) {
      n <- length(weights)
      sum_weights_squared <- sum(weights^2)
      mean_weight_squared <- (sum(weights) / n)^2
      deff <- sum_weights_squared / (n * mean_weight_squared)
      return(deff)
    }
    calculate_moe <- function(sample_size, confidence_level = 0.95, design_effect = 1) {
      z_score <- qnorm(1 - (1 - confidence_level) / 2)
      p <- 0.5
      se <- sqrt((p * (1 - p)) / sample_size)
      moe <- z_score * se * sqrt(design_effect)
      return(moe)
    }
    calculate_moe_standard <- function(sample_size, confidence_level = 0.95, design_effect = 1) {
      z_score <- qnorm(1 - (1 - confidence_level) / 2)
      p <- 0.5
      se <- sqrt((p * (1 - p)) / sample_size)
      moe <- z_score * se
      return(moe)
    }
    design_effect <- calculate_design_effect(survey$weights)
    moe <- round(calculate_moe(sample_size = nrow(survey), design_effect = design_effect)*100, 1)
    
    # You can format multiple lines of stats with HTML
    HTML(paste0(
      "<div style='margin: 15px 0; font-size: 16px; font-weight: 500;'>",  # Increased font size and added weight
      "<strong>Survey Name:</strong> ", rv$surveys[[rv$current_survey]][["name"]],
      "<br><strong>Completes Fielded So Far:</strong> ", total_completes,
      "<br><strong>Margin of Error (Weighted): ±", moe, " percentage points</strong>",
      "</div>"
    ))
  })
  
  
  values <- reactiveValues(
    submitted = FALSE,
    alchemer_quotas_submitted = FALSE,
    categories_submitted = FALSE
  )
  
  output$dropdown_container <- renderUI({
    req(values$submitted)
    req(rv$display_tables)
    
    # Get the categories from your data
    categories <- c(colnames(processed_data()$df4))
    
    tagList(
      selectInput(
        "isWeighted",
        "Select Analysis Type",
        choices = c("Weighted", "Unweighted"),
        selected = NULL
      ),
      
      selectizeInput(
        "selected_categories",
        "Select Questions for Analysis",
        choices = categories,
        multiple = TRUE,
        options = list(
          placeholder = 'Select questions to generate weighted results...',
          plugins = list('remove_button'),
          onInitialize = I('function() { this.setValue(""); }')
        )
      ),
      
      selectizeInput(
        "crosstab_columns",
        "Crosstab Columns",
        choices = c("None", colnames(processed_data()$df4)),
        multiple = FALSE,
        options = list(
          placeholder = 'Select column to crosstab...'
        )
      )
    )
  })
  
  output$category_submit_button <- renderUI({
    req(values$submitted)
    req(rv$display_tables)
    
    actionButton(
      "category_submit",
      "Generate Results",
      class = "btn-primary",
      style = "margin-top: 10px; margin-bottom: 10px;"
    )
  })
  
  observeEvent(input$category_submit, {
    if (length(input$selected_categories) == 0) {
      showModal(modalDialog(
        title = "Missing Selection",
        "Please select at least one question for analysis.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    if (is.null(input$isWeighted)) {
      showModal(modalDialog(
        title = "Missing Selection",
        "Please select whether you want Weighted or Unweighted analysis.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    # if (is.null(input$crosstab_columns)) {
    #   showModal(modalDialog(
    #     title = "Missing Selection",
    #     "Please select a column for crosstab analysis.",
    #     easyClose = TRUE,
    #     footer = NULL
    #   ))
    #   return()
    # }
    
    values$categories_submitted <- TRUE
  })
  
  table3_data <- reactive({
    req(values$submitted)
    req(input$selected_categories)
    req(input$isWeighted)
    
    # Filter the data based on selected categories
    survey <- processed_data()$df4
    
    survey$noweights <- 1
    topline_questions <- input$selected_categories
    crosstab_col <- input$crosstab_columns
    if (input$isWeighted == "Unweighted"){
      weighted <- FALSE
    }
    else{
      weighted <- TRUE
    }
    
    all_results <- list()
    for (varname in topline_questions){
      if ("haven_labelled" %in% class(survey[[varname]])){
        survey[["currentvar"]] <- sjlabelled::as_character(survey[[varname]])
        origlabels <- data.frame(Answer=sjlabelled::get_labels(survey[[varname]]))
      }
      else{
        survey[["currentvar"]] <- survey[[varname]]
        origlabels <- data.frame(Answer=unique(survey[[varname]]))
      }
      
      if (weighted){
        results <- prop.table(tapply(survey[["weights"]], survey[["currentvar"]], sum))
      }
      else{
        results <- prop.table(tapply(survey[["noweights"]], survey[["currentvar"]], sum))
      }
      results <- data.frame(Answer=names(results), Topline_Results=unname(as.numeric(results)))
      convertToPercent <- function(x){
        if (x >= .005){
          return(paste(round(x*100, 1),"%",sep=""))
        }
        if (x > 0){
          return("*%")
        }
        "-"
      }
      results$Topline_Results <- sapply(results$Topline_Results, function(x) convertToPercent(x))
      results <- dplyr::left_join(origlabels, results, by=c("Answer"="Answer"))
      results[["Topline_Results"]] <- ifelse(is.na(results[["Topline_Results"]]), "-", results[["Topline_Results"]])
      results <- data.frame(Question=varname, Answer=results[["Answer"]], Topline=results[["Topline_Results"]])
      topline_results <- results
      # results <- rbind(results, data.frame(Question=" ", Answer=" ", Topline_Results=" "))
      
      # Results for each column-----
      if (crosstab_col != "None"){
        if ("haven_labelled" %in% class(survey[[crosstab_col]])){
          survey[["currentcol"]] <- sjlabelled::as_character(survey[[crosstab_col]])
          allColOptions <- sjlabelled::get_labels(survey[[crosstab_col]])
          allColOptions <- allColOptions[allColOptions %in% survey[["currentcol"]]]
        }
        else{
          survey[["currentcol"]] <- survey[[crosstab_col]]
          allColOptions <- unique(survey[[crosstab_col]])
        }
        allColOptions <- head(allColOptions, 20)
        
        for (answerOption in allColOptions){
          currentSurvey <- survey[survey[["currentcol"]]==answerOption, ]
          if ("haven_labelled" %in% class(currentSurvey[[varname]])){
            currentSurvey[["currentvar"]] <- sjlabelled::as_character(currentSurvey[[varname]])
            origlabels <- data.frame(Answer=sjlabelled::get_labels(currentSurvey[[varname]]))
          }
          else{
            currentSurvey[["currentvar"]] <- currentSurvey[[varname]]
            origlabels <- data.frame(Answer=unique(currentSurvey[[varname]]))
          }
          
          if (weighted){
            results <- prop.table(tapply(currentSurvey[["weights"]], currentSurvey[["currentvar"]], sum))
          }
          else{
            results <- prop.table(tapply(currentSurvey[["noweights"]], currentSurvey[["currentvar"]], sum))
          }
          results <- data.frame(Answer=names(results), Topline_Results=unname(as.numeric(results)))
          convertToPercent <- function(x){
            if (x >= .005){
              return(paste(round(x*100, 1),"%",sep=""))
            }
            if (x > 0){
              return("*%")
            }
            "-"
          }
          results$Topline_Results <- sapply(results$Topline_Results, function(x) convertToPercent(x))
          results <- dplyr::left_join(origlabels, results, by=c("Answer"="Answer"))
          results[["Topline_Results"]] <- ifelse(is.na(results[["Topline_Results"]]), "-", results[["Topline_Results"]])
          results <- data.frame(Question=varname, Answer=results[["Answer"]], Topline_Results=results[["Topline_Results"]])
          results <- dplyr::select(results, any_of(c("Topline_Results")))
          colnames(results) <- answerOption
          topline_results <- cbind(topline_results, results)
        }
        
      }
      
      all_results <- c(all_results, list(topline_results))
    }
    all_results <- do.call(rbind, all_results)
    
    all_results
  })
  
  output$table3 <- DT::renderDT({
    req(rv$display_tables)
    req(values$categories_submitted)
    req(table3_data())
    
    datatable(
      table3_data(),
      options = list(
        pageLength = -1,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE,
        scrollX = TRUE,
        autoWidth = TRUE,
        dom = 't',
        columnDefs = list(
          list(
            targets = '_all',
            render = JS("
              function(data, type, row, meta) {
                if (type === 'display' && meta.row !== undefined) {
                  return '<div title=\"' + data + '\" style=\"width:100%; white-space:nowrap; overflow:hidden; text-overflow:ellipsis;\">' + data + '</div>';
                }
                return data;
              }
            ")
          )
        )
      ),
      class = 'compact stripe',
      rownames = FALSE
    ) %>%
      formatStyle(
        columns = names(table3_data()),
        fontSize = '10px',
        width = '100px'
      )
  }, server=FALSE)
  
}