
library(shiny)
library(dplyr)
library(readr)
library(lubridate)
library(DT)
library(shinydashboard)

# UI 
ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(title = "WHOOP Data Exporter"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Data",      tabName = "upload",   icon = icon("upload")),
      menuItem("Recovery",         tabName = "recovery", icon = icon("heart")),
      menuItem("Workouts",         tabName = "workouts", icon = icon("dumbbell")),
      menuItem("Sleep",            tabName = "sleep",    icon = icon("moon")),
      menuItem("HRV & Resting HR", tabName = "hrv",      icon = icon("heartbeat"))
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .box { border-top-color: #00f19f !important; }
      .skin-black .main-header .logo { background-color: #1a1a1a; }
      .skin-black .main-header .navbar { background-color: #1a1a1a; }
      .skin-black .main-sidebar { background-color: #1a1a1a; }
      .download-btn { background-color: #00f19f !important;
                      border-color: #00f19f !important;
                      color: #000 !important;
                      font-weight: bold; }
    "))),
    
    tabItems(
      
      # Upload Tab
      tabItem(tabName = "upload",
              fluidRow(
                box(width = 12, title = "How to export your WHOOP data",
                    status = "info", solidHeader = TRUE,
                    tags$ol(
                      tags$li("Open the WHOOP app on your phone."),
                      tags$li("Go to ", tags$b("Profile → Settings → Privacy & Security → Request My Data.")),
                      tags$li("WHOOP will email you a ZIP file — download and unzip it."),
                      tags$li("Upload the individual CSV files below.")
                    )
                )
              ),
              fluidRow(
                box(width = 6, title = "Upload WHOOP CSV Files",
                    status = "primary", solidHeader = TRUE,
                    fileInput("recovery_file", "Recovery CSV",   accept = ".csv"),
                    fileInput("workout_file",  "Workouts CSV",   accept = ".csv"),
                    fileInput("sleep_file",    "Sleep CSV",      accept = ".csv"),
                    fileInput("cycle_file",
                              "Physiological Cycles CSV (contains HRV & Resting HR)",
                              accept = ".csv"),
                    br(),
                    actionButton("process_btn", "Process Files", icon = icon("cogs"),
                                 style = "background-color:#00f19f; color:#000;
                       font-weight:bold; border:none; padding:8px 20px;")
                ),
                box(width = 6, title = "Processing Status",
                    status = "primary", solidHeader = TRUE,
                    verbatimTextOutput("status_output")
                )
              )
      ),
      
      # Recovery Tab
      tabItem(tabName = "recovery",
              fluidRow(
                box(width = 12, title = "Recovery Data",
                    status = "primary", solidHeader = TRUE,
                    DTOutput("recovery_table"),
                    br(),
                    downloadButton("download_recovery", "Download Recovery CSV",
                                   class = "download-btn")
                )
              )
      ),
      
      # Workouts Tab
      tabItem(tabName = "workouts",
              fluidRow(
                box(width = 12, title = "Workout Data",
                    status = "primary", solidHeader = TRUE,
                    DTOutput("workout_table"),
                    br(),
                    downloadButton("download_workouts", "Download Workouts CSV",
                                   class = "download-btn")
                )
              )
      ),
      
      # Sleep Tab
      tabItem(tabName = "sleep",
              fluidRow(
                box(width = 12, title = "Sleep Data",
                    status = "primary", solidHeader = TRUE,
                    DTOutput("sleep_table"),
                    br(),
                    downloadButton("download_sleep", "Download Sleep CSV",
                                   class = "download-btn")
                )
              )
      ),
      
      # HRV / Resting HR Tab
      tabItem(tabName = "hrv",
              fluidRow(
                box(width = 12, title = "HRV & Resting Heart Rate",
                    status = "primary", solidHeader = TRUE,
                    DTOutput("hrv_table"),
                    br(),
                    downloadButton("download_hrv", "Download HRV & HR CSV",
                                   class = "download-btn")
                )
              )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    recovery = NULL,
    workouts = NULL,
    sleep    = NULL,
    hrv      = NULL,
    status   = "No files processed yet. Upload files and click 'Process Files'."
  )
  
  # Safe CSV reader
  safe_read <- function(file_input) {
    tryCatch(
      read_csv(file_input$datapath, show_col_types = FALSE),
      error = function(e) NULL
    )
  }
  
  # Standardise column names
  clean_names <- function(df) {
    names(df) <- names(df) |>
      tolower() |>
      trimws() |>
      gsub(pattern = "[^a-z0-9_]", replacement = "_") |>
      gsub(pattern = "_+",          replacement = "_") |>
      gsub(pattern = "^_|_$",       replacement = "")
    df
  }
  
  # Parse date/datetime columns where possible
  parse_dates <- function(df) {
    date_cols <- grep("date|time|start|end|created",
                      names(df), value = TRUE, ignore.case = TRUE)
    for (col in date_cols) {
      parsed <- suppressWarnings(
        parse_date_time(df[[col]],
                        orders = c("ymd HMS", "ymd HM", "ymd",
                                   "mdy HMS", "mdy")))
      if (sum(!is.na(parsed)) > 0.3 * nrow(df)) df[[col]] <- parsed
    }
    df
  }
  
  # Process button
  observeEvent(input$process_btn, {
    msgs <- character(0)
    
    # Recovery
    if (!is.null(input$recovery_file)) {
      df <- safe_read(input$recovery_file)
      if (!is.null(df)) {
        rv$recovery <- df |> clean_names() |> parse_dates()
        msgs <- c(msgs, sprintf("Recovery: %d rows, %d columns loaded.",
                                nrow(rv$recovery), ncol(rv$recovery)))
      } else {
        msgs <- c(msgs, "Recovery: Could not read file.")
      }
    }
    
    # Workouts
    if (!is.null(input$workout_file)) {
      df <- safe_read(input$workout_file)
      if (!is.null(df)) {
        rv$workouts <- df |> clean_names() |> parse_dates()
        msgs <- c(msgs, sprintf("Workouts: %d rows, %d columns loaded.",
                                nrow(rv$workouts), ncol(rv$workouts)))
      } else {
        msgs <- c(msgs, " Workouts: Could not read file.")
      }
    }
    
    # Sleep
    if (!is.null(input$sleep_file)) {
      df <- safe_read(input$sleep_file)
      if (!is.null(df)) {
        rv$sleep <- df |> clean_names() |> parse_dates()
        msgs <- c(msgs, sprintf("Sleep: %d rows, %d columns loaded.",
                                nrow(rv$sleep), ncol(rv$sleep)))
      } else {
        msgs <- c(msgs, " Sleep: Could not read file.")
      }
    }
    
    # HRV / Resting HR from Physiological Cycles
    if (!is.null(input$cycle_file)) {
      df <- safe_read(input$cycle_file)
      if (!is.null(df)) {
        df <- df |> clean_names() |> parse_dates()
        
        hrv_cols  <- grep("hrv|heart_rate_variability",
                          names(df), value = TRUE, ignore.case = TRUE)
        hr_cols   <- grep("resting_heart_rate|resting_hr",
                          names(df), value = TRUE, ignore.case = TRUE)
        date_col  <- grep("date|time|start|cycle",
                          names(df), value = TRUE, ignore.case = TRUE)[1]
        
        keep <- unique(c(date_col, hrv_cols, hr_cols))
        keep <- keep[!is.na(keep) & keep %in% names(df)]
        
        rv$hrv <- if (length(keep) > 1) select(df, all_of(keep)) else df
        msgs <- c(msgs, sprintf(" HRV/HR: %d rows loaded from Cycles file.",
                                nrow(rv$hrv)))
      } else {
        msgs <- c(msgs, " Cycles file: Could not read file.")
      }
    }
    
    if (length(msgs) == 0) msgs <- " No files were uploaded."
    rv$status <- paste(msgs, collapse = "\n")
  })
  
  # Status
  output$status_output <- renderText({ rv$status })
  
  # Tables helper
  make_table <- function(data_fn) {
    renderDT({
      df <- data_fn()
      req(df)
      datatable(df,
                options  = list(scrollX = TRUE, pageLength = 15),
                rownames = FALSE)
    })
  }
  
  output$recovery_table <- make_table(reactive(rv$recovery))
  output$workout_table  <- make_table(reactive(rv$workouts))
  output$sleep_table    <- make_table(reactive(rv$sleep))
  output$hrv_table      <- make_table(reactive(rv$hrv))
  
  # ── Download helper ─────────────────────────────────────────────────────────
  make_download <- function(data_fn, stem) {
    downloadHandler(
      filename = function() paste0(stem, "_", Sys.Date(), ".csv"),
      content  = function(file) { req(data_fn()); write_csv(data_fn(), file) }
    )
  }
  
  output$download_recovery <- make_download(reactive(rv$recovery), "whoop_recovery")
  output$download_workouts <- make_download(reactive(rv$workouts), "whoop_workouts")
  output$download_sleep    <- make_download(reactive(rv$sleep),    "whoop_sleep")
  output$download_hrv      <- make_download(reactive(rv$hrv),      "whoop_hrv_hr")
}

shinyApp(ui, server)

