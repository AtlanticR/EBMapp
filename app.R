library(shiny)
library(DT)
library(dplyr)
library(shinydashboard)
library(shinyWidgets)
library(openxlsx)
library(officer)
library(flextable)

# Read the data
ebm_data <- read.csv("data/EBM Framework Spreadsheet 3-Nov-2025.csv", stringsAsFactors = FALSE) |>
  mutate(Main_Objective = trimws(Main_Objective),
         Pillar = trimws(Pillar))

ui <- dashboardPage(
  dashboardHeader(title = "EBM App"),

  dashboardSidebar(width=350,
                   sidebarMenu(id = "sidebar",
                               menuItem("Explore the EBM",
                                        tabName = "home",
                                        icon = icon("magnifying-glass")),
                               menuItem("Ways to Use the EBM Framework",
                                        startExpanded=TRUE,
                                        icon = icon("tools"),
                                        menuSubItem("Using EBM the framework",
                                                    tabName = "useEBM",
                                                    icon = icon("leanpub")),
                                        menuSubItem("Checklist of objectives",
                                                    tabName = "checklist",
                                                    icon = icon("square-check")),
                                        menuSubItem("Evaluating policies and management approaches",
                                                    tabName = "policies",
                                                    icon = icon("magnifying-glass")),
                                        menuSubItem("Scenario comparison",
                                                    tabName = "scenario",
                                                    icon = icon("code-compare")),
                                        menuSubItem("Management report card",
                                                    tabName = "report",
                                                    icon = icon("list")),
                                        menuSubItem("Cumulative effects, risk assessment and tradeoffs",
                                                    tabName = "cumulative",
                                                    icon = icon("arrows-up-to-line"))
                               )
                   )
  ),

  dashboardBody(
    tags$head(
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jQuery-rwdImageMaps/1.6/jquery.rwdImageMaps.min.js"),
      tags$style(HTML("
        .centered-image {
          display: block;
          margin-left: auto;
          margin-right: auto;
          width: 70%;
          max-width: 1280px;
          height: auto;
        }
        .back-button {
          margin: 20px 0;
        }
        .level-1 { margin-left: 20px; }
        .level-2 { margin-left: 40px; }
        .level-3 { margin-left: 60px; }
        .level-4 { margin-left: 80px; }
        .pillar-section {
          margin-bottom: 20px;
          padding: 10px;
          background-color: #f8f9fa;
          border-radius: 5px;
        }
        .main-objective-section {
          margin-bottom: 15px;
          padding: 8px;
          background-color: #ffffff;
          border-left: 3px solid #3c8dbc;
        }
        .checkbox-label {
          font-weight: normal;
          margin-bottom: 5px;
        }
        .pillar-header {
          font-size: 18px;
          font-weight: bold;
          color: #3c8dbc;
        }
        .main-objective-header {
          font-size: 16px;
          font-weight: bold;
          color: #605ca8;
        }
      "))
    ),

    tabItems(
      # Home Tab with Image Map
      tabItem(tabName = "home",
              fluidRow(
                box(width = 12, title = "Welcome to the Ecosystem-Based Management Framework Exploration Tool", status = "primary", solidHeader = TRUE,
                    p("This interactive tool helps you explore and apply the Ecosystem-Based Management (EBM) framework."),
                    p("Fisheries and Oceans Canada (DFO) has a mandate to apply an ecosystem approach to fisheries and oceans management decisions. An ecosystem approach includes ecological, economic, social, and cultural and governance objectives, which are required in many of DFO's decision-making processes.",
                      br(),
                      "The EBM Framework will support DFO decision-making across sectors by providing a broad range of objectives and indicators within a consistent, structured framework to support transparent, evidence-based decision-making.
The purpose of the Maritimes EBM Framework is to support a more holistic approach to decision-making.",
                      br(),
                      " The Regional EBM framework uses a hierarchical structure which includes:",
                      br(),
                      "1.	Pillars (inner circle in figure)",
                      br(),
                      "2.	Main Objectives (outer circle in figure)",
                      br(),
                      "3.	Sub-Objectives",
                      br(),
                      "4.	Indicators (to measure objectives)
"),
                    p(strong("Click on any section of the framework below to view detailed information or explore the menu to the left for additional tools and resources."))
                )
              ),
              fluidRow(
                box(width = 12, title = "Interactive EBM Framework", status = "info", solidHeader = TRUE,
                    uiOutput("home_map_content")
                )
              )
      ),

      tabItem(tabName = "useEBM",
              fluidRow(
                box(width = 12, title = "Using EBM the framework", status = "primary", solidHeader = TRUE,
                    p("Five broad areas of use have been envisioned for the EBM Framework (below). These are premised on DFO's objective/outcomes based approach to management, and provide the candidate objectives to be used in different decision-making contexts. "),
                    img(src = "EBMuses.png",
                        width = "100%")

                )
              )
      ),
      tabItem(tabName = "checklist",
              fluidRow(
                box(width = 12, title = "Checklist of objectives", status = "primary", solidHeader = TRUE,
                    p("This is the first step in any use of the EBM Framework
Question: what objectives are relevant to my management plan/decision/project development etc?",
                      br(),
                      "1.	Review objectives in each of the 4 Pillars",
                      br(),
                      "2.	Suggestion to start with Level 1 Objectives and use lower level objectives to inform intent of the objectives",
                      br(),
                      "3.	In some case, Level Two or lower level objectives may be more useful",
                      br(),
                      "4.	Assess which objectives are relevant to your needs")
                )
              ),
              fluidRow(
                box(width = 4, title = "Filter Options", status = "info", solidHeader = TRUE,
                    h4("Level of Detail"),
                    radioButtons("detail_level",
                                 label = NULL,
                                 choices = list(
                                   "Pillars only" = "pillar",
                                   "Pillars + Main Objectives" = "main",
                                   "Pillars + Main Objectives + Main Objectives Text" = "main_text",
                                   "Add Level 1" = "level1",
                                   "Add Level 2" = "level2",
                                   "Add Level 3" = "level3",
                                   "Add Level 4 (all levels)" = "level4"
                                 ),
                                 selected = "main"),
                    hr(),
                    h4("Select Pillars"),
                    checkboxGroupInput("pillar_filter",
                                       label = NULL,
                                       choices = NULL,
                                       selected = NULL),
                    hr(),
                    h4("Select Main Objectives"),
                    uiOutput("main_objective_checkboxes"),
                    hr(),
                    actionButton("select_all", "Select All", class = "btn-sm"),
                    actionButton("deselect_all", "Deselect All", class = "btn-sm"),
                    hr(),
                    h4("Download Checklist"),
                    downloadButton("download_word", "Download as Word", class = "btn-sm btn-primary"),
                    br(), br(),
                    downloadButton("download_excel", "Download as Excel", class = "btn-sm btn-success"),
                    br(), br(),
                    downloadButton("download_csv", "Download as CSV", class = "btn-sm btn-info")
                ),
                box(width = 8, title = "Interactive Checklist", status = "primary", solidHeader = TRUE,
                    uiOutput("checklist_content")
                )
              )
      ),
      tabItem(tabName = "policies",
              fluidRow(
                box(width = 12, title = "Evaluating policies and management approaches", status = "primary", solidHeader = TRUE,
                    p("TBD")
                )
              )
      ),
      tabItem(tabName = "scenario",
              fluidRow(
                box(width = 12, title = "Scenario comparison", status = "primary", solidHeader = TRUE,
                    p("TBD")
                )
              )
      ),
      tabItem(tabName = "report",
              fluidRow(
                box(width = 12, title = "Management report card", status = "primary", solidHeader = TRUE,
                    p("TBD")
                )
              )
      ),
      tabItem(tabName = "cumulative",
              fluidRow(
                box(width = 12, title = "Cumulative effects, risk assessment and tradeoffs", status = "primary", solidHeader = TRUE,
                    p("TBD")
                )
              )
      )


    )
  )
)

server <- function(input, output, session) {

  # Reactive value to track current view in home page map
  home_view <- reactiveVal("map")
  home_filter <- reactiveVal(NULL)

  # Initialize pillar filter choices
  observe({
    pillars <- unique(ebm_data$Pillar[!is.na(ebm_data$Pillar) & ebm_data$Pillar != ""])
    updateCheckboxGroupInput(session, "pillar_filter",
                             choices = pillars,
                             selected = pillars)
  })

  # Create main objective checkboxes based on selected pillars
  output$main_objective_checkboxes <- renderUI({
    req(input$pillar_filter)

    main_objs <- ebm_data %>%
      filter(Pillar %in% input$pillar_filter,
             !is.na(Main_Objective) & Main_Objective != "",
             !(Main_Objective %in% input$pillar_filter)) %>%
      select(Main_Objective) %>%
      distinct() %>%
      pull(Main_Objective)

    if(length(main_objs) > 0) {
      checkboxGroupInput("main_objective_filter",
                         label = NULL,
                         choices = main_objs,
                         selected = main_objs)
    } else {
      p("No main objectives available for selected pillars.")
    }
  })

  # Select/Deselect all buttons
  observeEvent(input$select_all, {
    pillars <- unique(ebm_data$Pillar[!is.na(ebm_data$Pillar) & ebm_data$Pillar != ""])
    updateCheckboxGroupInput(session, "pillar_filter", selected = pillars)

    # Wait for main objectives to update
    observe({
      req(input$pillar_filter)
      main_objs <- ebm_data %>%
        filter(Pillar %in% input$pillar_filter,
               !is.na(Main_Objective) & Main_Objective != "",
               !(Main_Objective %in% input$pillar_filter)) %>%
        select(Main_Objective) %>%
        distinct() %>%
        pull(Main_Objective)

      updateCheckboxGroupInput(session, "main_objective_filter", selected = main_objs)
    })
  })

  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "pillar_filter", selected = character(0))
    updateCheckboxGroupInput(session, "main_objective_filter", selected = character(0))
  })

  # Generate checklist content
  output$checklist_content <- renderUI({
    req(input$pillar_filter, input$detail_level)

    if(input$detail_level == "pillar") {
      # Only show pillars with checkboxes
      pillar_items <- list()
      for(pillar in input$pillar_filter) {
        pillar_items <- c(pillar_items, list(
          div(class = "pillar-section",
              div(class = "checkbox-label",
                  checkboxInput(paste0("chk_pillar_", make.names(pillar)),
                                label = strong(pillar),
                                value = FALSE)
              )
          )
        ))
      }
      return(do.call(tagList, pillar_items))
    }

    req(input$main_objective_filter)

    filtered_data <- ebm_data %>%
      filter(Pillar %in% input$pillar_filter,
             Main_Objective %in% input$main_objective_filter)

    if(nrow(filtered_data) == 0) {
      return(p("No objectives selected. Please select at least one Pillar and Main Objective."))
    }

    # Build hierarchical checklist
    checklist_items <- list()

    for(pillar in input$pillar_filter) {
      pillar_data <- filtered_data %>% filter(Pillar == pillar)

      if(nrow(pillar_data) == 0) next

      pillar_content <- list(
        div(class = "pillar-section",
            div(class = "pillar-header", pillar)
        )
      )

      # Main objectives level
      for(main_obj in unique(pillar_data$Main_Objective)) {
        if(is.na(main_obj) || main_obj == "") next

        main_obj_data <- pillar_data %>% filter(Main_Objective == main_obj)
        main_obj_text <- unique(main_obj_data$Main_Objectives_text)[1]

        main_obj_content <- list(
          div(class = "main-objective-section",
              div(class = "checkbox-label",
                  checkboxInput(paste0("chk_main_", make.names(paste(pillar, main_obj))),
                                label = strong(main_obj),
                                value = FALSE)
              )
          )
        )

        # Show main objectives text if requested
        if(input$detail_level %in% c("main_text", "level1", "level2", "level3", "level4")) {
          if(!is.na(main_obj_text) && main_obj_text != "") {
            main_obj_content <- c(main_obj_content, list(
              p(style = "font-style: italic; margin-left: 10px;", main_obj_text)
            ))
          }
        }

        # Level 1 and beyond
        if(input$detail_level %in% c("level1", "level2", "level3", "level4")) {
          level1_items <- main_obj_data %>%
            filter(!is.na(Level_1) & Level_1 != "") %>%
            select(Level_1) %>%
            distinct()

          if(nrow(level1_items) > 0) {
            for(i in 1:nrow(level1_items)) {
              level1_val <- level1_items$Level_1[i]
              main_obj_content <- c(main_obj_content, list(
                div(class = "level-1 checkbox-label",
                    checkboxInput(paste0("chk_l1_", make.names(paste(pillar, main_obj, level1_val))),
                                  label = level1_val,
                                  value = FALSE)
                )
              ))

              # Level 2
              if(input$detail_level %in% c("level2", "level3", "level4")) {
                level2_items <- main_obj_data %>%
                  filter(Level_1 == level1_val, !is.na(Level_2) & Level_2 != "") %>%
                  select(Level_2) %>%
                  distinct()

                if(nrow(level2_items) > 0) {
                  for(j in 1:nrow(level2_items)) {
                    level2_val <- level2_items$Level_2[j]
                    main_obj_content <- c(main_obj_content, list(
                      div(class = "level-2 checkbox-label",
                          checkboxInput(paste0("chk_l2_", make.names(paste(pillar, main_obj, level1_val, level2_val))),
                                        label = level2_val,
                                        value = FALSE)
                      )
                    ))

                    # Level 3
                    if(input$detail_level %in% c("level3", "level4")) {
                      level3_items <- main_obj_data %>%
                        filter(Level_1 == level1_val, Level_2 == level2_val,
                               !is.na(Level_3) & Level_3 != "") %>%
                        select(Level_3) %>%
                        distinct()

                      if(nrow(level3_items) > 0) {
                        for(k in 1:nrow(level3_items)) {
                          level3_val <- level3_items$Level_3[k]
                          main_obj_content <- c(main_obj_content, list(
                            div(class = "level-3 checkbox-label",
                                checkboxInput(paste0("chk_l3_", make.names(paste(pillar, main_obj, level1_val, level2_val, level3_val))),
                                              label = level3_val,
                                              value = FALSE)
                            )
                          ))

                          # Level 4
                          if(input$detail_level == "level4") {
                            level4_items <- main_obj_data %>%
                              filter(Level_1 == level1_val, Level_2 == level2_val,
                                     Level_3 == level3_val, !is.na(Level_4) & Level_4 != "") %>%
                              select(Level_4) %>%
                              distinct()

                            if(nrow(level4_items) > 0) {
                              for(l in 1:nrow(level4_items)) {
                                main_obj_content <- c(main_obj_content, list(
                                  div(class = "level-4 checkbox-label",
                                      checkboxInput(paste0("chk_l4_", make.names(paste(pillar, main_obj, level1_val, level2_val, level3_val, level4_items$Level_4[l]))),
                                                    label = level4_items$Level_4[l],
                                                    value = FALSE)
                                  )
                                ))
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }

        pillar_content <- c(pillar_content, main_obj_content)
      }

      checklist_items <- c(checklist_items, pillar_content)
    }

    do.call(tagList, checklist_items)
  })

  # Function to get full checklist with checkbox status
  get_full_checklist <- reactive({
    req(input$pillar_filter, input$detail_level)

    checklist_data <- data.frame(
      Checked = character(),
      Pillar = character(),
      Main_Objective = character(),
      Main_Objectives_text = character(),
      Level_1 = character(),
      Level_2 = character(),
      Level_3 = character(),
      Level_4 = character(),
      stringsAsFactors = FALSE
    )

    if(input$detail_level == "pillar") {
      # Just pillars
      for(pillar in input$pillar_filter) {
        checkbox_id <- paste0("chk_pillar_", make.names(pillar))
        checked <- if(!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) "☑" else "☐"

        checklist_data <- rbind(checklist_data, data.frame(
          Checked = checked,
          Pillar = pillar,
          Main_Objective = "",
          Main_Objectives_text = "",
          Level_1 = "",
          Level_2 = "",
          Level_3 = "",
          Level_4 = "",
          stringsAsFactors = FALSE
        ))
      }
      return(checklist_data)
    }

    req(input$main_objective_filter)

    filtered_data <- ebm_data %>%
      filter(Pillar %in% input$pillar_filter,
             Main_Objective %in% input$main_objective_filter)

    for(pillar in input$pillar_filter) {
      pillar_data <- filtered_data %>% filter(Pillar == pillar)

      if(nrow(pillar_data) == 0) next

      for(main_obj in unique(pillar_data$Main_Objective)) {
        if(is.na(main_obj) || main_obj == "") next

        main_obj_data <- pillar_data %>% filter(Main_Objective == main_obj)
        main_obj_text <- unique(main_obj_data$Main_Objectives_text)[1]
        if(is.na(main_obj_text)) main_obj_text <- ""

        checkbox_id <- paste0("chk_main_", make.names(paste(pillar, main_obj)))
        checked <- if(!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) "☑" else "☐"

        # Add main objective row
        checklist_data <- rbind(checklist_data, data.frame(
          Checked = checked,
          Pillar = pillar,
          Main_Objective = main_obj,
          Main_Objectives_text = if(input$detail_level %in% c("main_text", "level1", "level2", "level3", "level4")) main_obj_text else "",
          Level_1 = "",
          Level_2 = "",
          Level_3 = "",
          Level_4 = "",
          stringsAsFactors = FALSE
        ))

        if(input$detail_level %in% c("level1", "level2", "level3", "level4")) {
          level1_items <- main_obj_data %>%
            filter(!is.na(Level_1) & Level_1 != "") %>%
            select(Level_1) %>%
            distinct()

          for(i in 1:nrow(level1_items)) {
            level1_val <- level1_items$Level_1[i]
            checkbox_id <- paste0("chk_l1_", make.names(paste(pillar, main_obj, level1_val)))
            checked <- if(!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) "☑" else "☐"

            checklist_data <- rbind(checklist_data, data.frame(
              Checked = checked,
              Pillar = "",
              Main_Objective = "",
              Main_Objectives_text = "",
              Level_1 = level1_val,
              Level_2 = "",
              Level_3 = "",
              Level_4 = "",
              stringsAsFactors = FALSE
            ))

            if(input$detail_level %in% c("level2", "level3", "level4")) {
              level2_items <- main_obj_data %>%
                filter(Level_1 == level1_val, !is.na(Level_2) & Level_2 != "") %>%
                select(Level_2) %>%
                distinct()

              for(j in 1:nrow(level2_items)) {
                level2_val <- level2_items$Level_2[j]
                checkbox_id <- paste0("chk_l2_", make.names(paste(pillar, main_obj, level1_val, level2_val)))
                checked <- if(!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) "☑" else "☐"

                checklist_data <- rbind(checklist_data, data.frame(
                  Checked = checked,
                  Pillar = "",
                  Main_Objective = "",
                  Main_Objectives_text = "",
                  Level_1 = "",
                  Level_2 = level2_val,
                  Level_3 = "",
                  Level_4 = "",
                  stringsAsFactors = FALSE
                ))

                if(input$detail_level %in% c("level3", "level4")) {
                  level3_items <- main_obj_data %>%
                    filter(Level_1 == level1_val, Level_2 == level2_val,
                           !is.na(Level_3) & Level_3 != "") %>%
                    select(Level_3) %>%
                    distinct()

                  for(k in 1:nrow(level3_items)) {
                    level3_val <- level3_items$Level_3[k]
                    checkbox_id <- paste0("chk_l3_", make.names(paste(pillar, main_obj, level1_val, level2_val, level3_val)))
                    checked <- if(!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) "☑" else "☐"

                    checklist_data <- rbind(checklist_data, data.frame(
                      Checked = checked,
                      Pillar = "",
                      Main_Objective = "",
                      Main_Objectives_text = "",
                      Level_1 = "",
                      Level_2 = "",
                      Level_3 = level3_val,
                      Level_4 = "",
                      stringsAsFactors = FALSE
                    ))

                    if(input$detail_level == "level4") {
                      level4_items <- main_obj_data %>%
                        filter(Level_1 == level1_val, Level_2 == level2_val,
                               Level_3 == level3_val, !is.na(Level_4) & Level_4 != "") %>%
                        select(Level_4) %>%
                        distinct()

                      for(l in 1:nrow(level4_items)) {
                        level4_val <- level4_items$Level_4[l]
                        checkbox_id <- paste0("chk_l4_", make.names(paste(pillar, main_obj, level1_val, level2_val, level3_val, level4_val)))
                        checked <- if(!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) "☑" else "☐"

                        checklist_data <- rbind(checklist_data, data.frame(
                          Checked = checked,
                          Pillar = "",
                          Main_Objective = "",
                          Main_Objectives_text = "",
                          Level_1 = "",
                          Level_2 = "",
                          Level_3 = "",
                          Level_4 = level4_val,
                          stringsAsFactors = FALSE
                        ))
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }

    # Remove empty columns based on detail level
    if(input$detail_level == "pillar") {
      checklist_data <- checklist_data[, c("Checked", "Pillar")]
    } else if(input$detail_level == "main") {
      checklist_data <- checklist_data[, c("Checked", "Pillar", "Main_Objective")]
    } else if(input$detail_level == "main_text") {
      checklist_data <- checklist_data[, c("Checked", "Pillar", "Main_Objective", "Main_Objectives_text")]
    } else if(input$detail_level == "level1") {
      checklist_data <- checklist_data[, c("Checked", "Pillar", "Main_Objective", "Main_Objectives_text", "Level_1")]
    } else if(input$detail_level == "level2") {
      checklist_data <- checklist_data[, c("Checked", "Pillar", "Main_Objective", "Main_Objectives_text", "Level_1", "Level_2")]
    } else if(input$detail_level == "level3") {
      checklist_data <- checklist_data[, c("Checked", "Pillar", "Main_Objective", "Main_Objectives_text", "Level_1", "Level_2", "Level_3")]
    }

    return(checklist_data)
  })

  # Download handlers
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("EBM_Checklist_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- get_full_checklist()
      write.csv(data, file, row.names = FALSE)
    }
  )

  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("EBM_Checklist_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      data <- get_full_checklist()

      wb <- createWorkbook()
      addWorksheet(wb, "Checklist")
      writeData(wb, "Checklist", data)

      # Add formatting
      headerStyle <- createStyle(fontColour = "#ffffff", fgFill = "#4F81BD",
                                 halign = "center", valign = "center",
                                 textDecoration = "bold", border = "TopBottomLeftRight")
      addStyle(wb, "Checklist", headerStyle, rows = 1, cols = 1:ncol(data), gridExpand = TRUE)

      # Add borders to all cells
      bodyStyle <- createStyle(border = "TopBottomLeftRight")
      addStyle(wb, "Checklist", bodyStyle, rows = 2:(nrow(data) + 1), cols = 1:ncol(data),
               gridExpand = TRUE, stack = TRUE)

      setColWidths(wb, "Checklist", cols = 1:ncol(data), widths = "auto")

      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  output$download_word <- downloadHandler(
    filename = function() {
      paste0("EBM_Checklist_", Sys.Date(), ".docx")
    },
    content = function(file) {
      data <- get_full_checklist()

      doc <- read_docx()
      doc <- body_add_par(doc, "EBM Framework Checklist", style = "heading 1")
      doc <- body_add_par(doc, paste("Generated:", Sys.Date()))

      detail_labels <- c(
        "pillar" = "Pillars only",
        "main" = "Pillars + Main Objectives",
        "main_text" = "Pillars + Main Objectives + Text",
        "level1" = "Through Level 1",
        "level2" = "Through Level 2",
        "level3" = "Through Level 3",
        "level4" = "All levels (through Level 4)"
      )

      doc <- body_add_par(doc, paste("Detail Level:", detail_labels[input$detail_level]))
      doc <- body_add_par(doc, "")

      # Create formatted table
      ft <- flextable(data)
      ft <- theme_booktabs(ft)

      # Set column widths more intelligently
      if("Main_Objectives_text" %in% names(data)) {
        # Calculate proportional widths (in inches, for letter size page ~6.5" usable width)
        col_widths <- c(0.4)  # Checked column

        if("Pillar" %in% names(data)) col_widths <- c(col_widths, 1.2)
        if("Main_Objective" %in% names(data)) col_widths <- c(col_widths, 1.5)
        if("Main_Objectives_text" %in% names(data)) col_widths <- c(col_widths, 2.5)
        if("Level_1" %in% names(data)) col_widths <- c(col_widths, 1.5)
        if("Level_2" %in% names(data)) col_widths <- c(col_widths, 1.5)
        if("Level_3" %in% names(data)) col_widths <- c(col_widths, 1.5)
        if("Level_4" %in% names(data)) col_widths <- c(col_widths, 1.5)

        ft <- width(ft, width = col_widths)
      } else {
        ft <- autofit(ft)
      }

      # Enable text wrapping for all columns
      ft <- valign(ft, valign = "top", part = "all")

      # Make text smaller and wrap for Main_Objectives_text column
      if("Main_Objectives_text" %in% names(data)) {
        text_col_idx <- which(names(data) == "Main_Objectives_text")
        ft <- fontsize(ft, j = text_col_idx, size = 9, part = "body")
        ft <- padding(ft, j = text_col_idx, padding = 2, part = "all")
      }

      # Make checkbox column centered and slightly larger
      ft <- align(ft, j = 1, align = "center", part = "all")
      ft <- fontsize(ft, j = 1, size = 12, part = "body")

      # Left align all other columns
      if(ncol(data) > 1) {
        ft <- align(ft, j = 2:ncol(data), align = "left", part = "all")
      }

      # Make header bold and slightly larger
      ft <- bold(ft, part = "header")
      ft <- fontsize(ft, size = 10, part = "header")

      # Add alternating row colors for better readability
      ft <- bg(ft, bg = "#F0F0F0", part = "body",
               i = seq(2, nrow(data), 2))

      doc <- body_add_flextable(doc, ft)

      print(doc, target = file)
    }
  )

  # Home page map content
  output$home_map_content <- renderUI({
    if (home_view() == "map") {
      tagList(
        tags$img(
          src = "EBM.png",
          usemap = "#puzzle-map",
          width = "1280",
          class = "centered-image"
        ),
        tags$map(
          name = "puzzle-map",
          tags$area(
            shape = "circle",
            coords = "633,627,243",
            href = "#",
            alt = "EBM Framework",
            title = "EBM Framework - Click for details",
            onclick = "Shiny.setInputValue('home_area_clicked', 'EBM Framework', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "224,624,370,622,382,550,406,498,454,438,526,394,606,368,636,370,632,220,504,242,382,302,292,408,242,516,228,572",
            href = "#",
            alt = "Ecological",
            title = "Ecological - Click for details",
            onclick = "Shiny.setInputValue('home_area_clicked', 'Ecological', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "62,618,137,338,259,411,212,619",
            href = "#",
            alt = "Habitat",
            title = "Habitat - Click for details",
            onclick = "Shiny.setInputValue('home_area_clicked', 'Habitat', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "147,329,345,128,424,256,272,406",
            href = "#",
            alt = "Biodiversity",
            title = "Biodiversity - Click for details",
            onclick = "Shiny.setInputValue('home_area_clicked', 'Biodiversity', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "360,120,434,247,638,204,632,46",
            href = "#",
            alt = "Productivity",
            title = "Productivity - Click for details",
            onclick = "Shiny.setInputValue('home_area_clicked', 'Productivity', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "648,224,648,372,724,384,776,412,828,458,868,528,886,604,886,624,1036,622,1018,508,966,402,906,332,788,252",
            href = "#",
            alt = "Economic",
            title = "Economic - Click for details",
            onclick = "Shiny.setInputValue('home_area_clicked', 'Economic', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "647,47,648,207,846,263,927,118",
            href = "#",
            alt = "Economic Efficiency",
            title = "Economic Efficiency - Click for details",
            onclick = "Shiny.setInputValue('home_area_clicked', 'Economic Efficiency', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "938,125,859,271,993,413,1139,332",
            href = "#",
            alt = "Economic Equity",
            title = "Economic Equity - Click for details",
            onclick = "Shiny.setInputValue('home_area_clicked', 'Economic Equity', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "1141,346,1000,427,1049,620,1219,620",
            href = "#",
            alt = "Economic Sustainability",
            title = "Economic Sustainability - Click for details",
            onclick = "Shiny.setInputValue('home_area_clicked', 'Economic Sustainability', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "888,636,1030,634,1018,742,966,850,868,952,746,1010,638,1028,644,886,730,864,794,822,836,778,866,732,882,676",
            href = "#",
            alt = "Governance",
            title = "Governance - Click for details",
            onclick = "Shiny.setInputValue('home_area_clicked', 'Governance', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "650,1048,649,1206,924,1128,844,991",
            href = "#",
            alt = "Legal Obligations & Other Commitments",
            title = "Legal Obligations & Other Commitments - Click for details",
            onclick = "Shiny.setInputValue('home_area_clicked', 'Legal Obligations and other commitments', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "858,976,936,1124,1135,924,989,836",
            href = "#",
            alt = "Governance Structure & Processes",
            title = "Governance Structure & Processes - Click for details",
            onclick = "Shiny.setInputValue('home_area_clicked', 'Governance Structures and Processes', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "1005,824,1143,910,1216,639,1056,637",
            href = "#",
            alt = "Governance Outcomes",
            title = "Governance Outcomes - Click for details",
            onclick = "Shiny.setInputValue('home_area_clicked', 'Governance Outcomes', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "227,632,372,630,394,727,485,835,559,870,636,880,637,1031,500,1008,376,940,283,825,237,720",
            href = "#",
            alt = "Social & Cultural",
            title = "Social & Cultural - Click for details",
            onclick = "Shiny.setInputValue('home_area_clicked', 'Social & Cultural', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "212,633,244,780,101,844,62,631",
            href = "#",
            alt = "Sustainable Communities",
            title = "Sustainable Communities - Click for details",
            onclick = "Shiny.setInputValue('home_area_clicked', 'Sustainable Communities', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "250,793,339,920,229,1039,105,850",
            href = "#",
            alt = "Health & Well-being",
            title = "Health & Well-being - Click for details",
            onclick = "Shiny.setInputValue('home_area_clicked', 'Health and Well-being', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "349,932,483,1014,424,1160,241,1043",
            href = "#",
            alt = "Ethical & Just Activities",
            title = "Ethical & Just Activities - Click for details",
            onclick = "Shiny.setInputValue('home_area_clicked', 'Ethical and Just Activities', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "497,1016,434,1168,633,1203,639,1041",
            href = "#",
            alt = "culture",
            title = "Culture - Click for details",
            onclick = "Shiny.setInputValue('home_area_clicked', 'culture', {priority: 'event'}); return false;"
          )
        ),
        tags$script(HTML("
          $(document).ready(function() {
            $('img[usemap]').rwdImageMaps();
          });
        "))
      )
    } else {
      tagList(
        actionButton("home_back_button", "Back to Infographic", class = "btn-primary back-button"),
        box(width = 12, title = paste("Viewing:", home_filter()), status = "info", solidHeader = TRUE,
            DTOutput("home_data_table")
        )
      )
    }
  })

  # Handle home page area clicks
  observeEvent(input$home_area_clicked, {
    home_filter(input$home_area_clicked)
    home_view("table")
  })

  # Handle home page back button
  observeEvent(input$home_back_button, {
    home_view("map")
  })

  # Render filtered data table on home page
  output$home_data_table <- renderDT({
    req(home_filter())

    filter_value <- home_filter()

    pillar_search <- ""
    objective_search <- ""

    if (filter_value %in% c("Ecological", "Economic", "Governance", "Social & Cultural")) {
      pillar_search <- filter_value
    } else if (filter_value != "EBM Framework") {
      objective_search <- filter_value
    }

    # Create display version with blanked repeats
    cols_to_merge <- c("Pillar", "Main_Objective", "Main_Objectives_text",
                       "Level_1", "Level_2", "Level_3")

    datatable(
      ebm_data,
      colnames = gsub("_", " ", names(ebm_data)),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        autoWidth = TRUE,
        searchCols = list(
          list(search = pillar_search),
          list(search = objective_search),
          NULL, NULL, NULL, NULL
        ),
        ordering = FALSE,
        columnDefs = list(
          list(
            targets = which(names(ebm_data) %in% cols_to_merge) - 1,
            render = JS(
              "function(data, type, row, meta) {",
              "  if (type === 'display') {",
              "    var api = new $.fn.dataTable.Api(meta.settings);",
              "    if (meta.row > 0) {",
              "      var prevData = api.cell(meta.row - 1, meta.col).data();",
              "      if (prevData === data) return '';",
              "    }",
              "  }",
              "  return data;",
              "}"
            )
          )
        )
      ),
      filter = "top",
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)
