library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)
library(officer)
library(flextable)

# Null coalesce helper
`%||%` <- function(a,b) if (is.null(a)) b else a

# Load data
ebm_data <- read.csv("data/EBM Framework Spreadsheet 3-Nov-2025.csv",
                     stringsAsFactors = FALSE) |>
  mutate(across(where(is.character), ~trimws(.)))

# Build a full hierarchical checklist for selected pillars + MOs
build_hierarchy_ui <- function(data, pillars, main_objs, detail) {
  items <- list()

  # Only pillars
  if (detail == "pillar") {
    for (p in pillars) {
      items <- c(items, list(
        div(class = "pillar-section",
            div(class = "checkbox-label",
                checkboxInput(paste0("chk_pillar_", make.names(p)), strong(p), FALSE)))
      ))
    }
    return(tagList(items))
  }

  # Pillars + main objectives
  sel <- data |> filter(Pillar %in% pillars)
  if (detail %in% c("main", "main_text")) {
    for (p in pillars) {
      pdat <- sel |> filter(Pillar == p)
      mos <- pdat |> filter(!is.na(Main_Objective), Main_Objective != "") |> distinct(Main_Objective) |> pull()
      if (length(main_objs)) mos <- intersect(mos, main_objs)
      if (!length(mos)) next

      items <- c(items, list(
        div(class = "pillar-section", div(class = "pillar-header", p))
      ))

      for (mo in mos) {
        motext <- pdat |> filter(Main_Objective == mo) |> pull(Main_Objectives_text) |> unique()
        motext <- motext[!is.na(motext)][1] %||% ""

        sect <- list(
          div(class = "main-objective-section",
              div(class = "checkbox-label",
                  checkboxInput(paste0("chk_main_", make.names(paste(p, mo))), strong(mo), FALSE)))
        )
        if (detail == "main_text" && nchar(motext)) {
          sect <- c(sect, list(p(style = "font-style: italic; margin-left: 10px;", motext)))
        }
        items <- c(items, sect)
      }
    }
    return(tagList(items))
  }

  # Full hierarchy (Level 1–4)
  for (p in pillars) {
    pdat <- sel |> filter(Pillar == p,
                          Main_Objective %in% main_objs)
    if (nrow(pdat) == 0) next

    items <- c(items, list(
      div(class = "pillar-section", div(class = "pillar-header", p))
    ))

    mos <- pdat |> filter(!is.na(Main_Objective), Main_Objective != "") |> distinct(Main_Objective) |> pull()
    for (mo in mos) {
      mdat <- pdat |> filter(Main_Objective == mo)
      motext <- mdat |> pull(Main_Objectives_text) |> unique()
      motext <- motext[!is.na(motext)][1] %||% ""

      mo_block <- list(
        div(class = "main-objective-section",
            div(class = "checkbox-label",
                checkboxInput(paste0("chk_main_", make.names(paste(p, mo))), strong(mo), FALSE)))
      )
      if (nchar(motext)) {
        mo_block <- c(mo_block, list(p(style = "font-style: italic; margin-left: 10px;", motext)))
      }

      l1s <- mdat |> filter(!is.na(Level_1), Level_1 != "") |> distinct(Level_1) |> pull()
      for (l1 in l1s) {
        mo_block <- c(mo_block, list(
          div(class = "level-1 checkbox-label",
              checkboxInput(paste0("chk_l1_", make.names(paste(p, mo, l1))), l1, FALSE))
        ))
        l2s <- mdat |> filter(Level_1 == l1, !is.na(Level_2), Level_2 != "") |> distinct(Level_2) |> pull()
        for (l2 in l2s) {
          mo_block <- c(mo_block, list(
            div(class = "level-2 checkbox-label",
                checkboxInput(paste0("chk_l2_", make.names(paste(p, mo, l1, l2))), l2, FALSE))
          ))
          l3s <- mdat |> filter(Level_1 == l1, Level_2 == l2, !is.na(Level_3), Level_3 != "") |> distinct(Level_3) |> pull()
          for (l3 in l3s) {
            mo_block <- c(mo_block, list(
              div(class = "level-3 checkbox-label",
                  checkboxInput(paste0("chk_l3_", make.names(paste(p, mo, l1, l2, l3))), l3, FALSE))
            ))
            l4s <- mdat |> filter(Level_1 == l1, Level_2 == l2, Level_3 == l3,
                                  !is.na(Level_4), Level_4 != "") |> distinct(Level_4) |> pull()
            for (l4 in l4s) {
              mo_block <- c(mo_block, list(
                div(class = "lefvel-4 checkbox-label",
                    checkboxInput(paste0("chk_l4_", make.names(paste(p, mo, l1, l2, l3, l4))), l4, FALSE))
              ))
            }
          }
        }
      }

      items <- c(items, mo_block)
    }
  }
  tagList(items)
}

# Template constructors used by Step 2
make_objective_table <- function(so) {
  # Ensure stable base columns
  base <- so |>
    select(Pillar, Main_Objective, Level_1, Level_2, Level_3, Level_4) |>
    mutate(
      Objective_Label = apply(select(., Main_Objective, Level_1, Level_2, Level_3, Level_4), 1, function(r) {
        paste(na.omit(r), collapse = " / ")
      })
    )
  base
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "EBM App"),
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      id = "sidebar",
      menuItem("Explore the EBM", tabName = "home", icon = icon("globe")),
      menuItem("Using the EBM Framework", tabName = "useEBM", icon = icon("leanpub")),
      hr(),
      h4("EBM Framework Workflow", style = "padding-left: 15px; color: #fff;"),
      menuItem("STEP 1: Select Objectives",
               tabName = "select_objectives",
               icon = icon("square-check"),
               badgeLabel = "Start Here",
               badgeColor = "green"),
      menuItem("STEP 2: Assessment & Comparison",
               tabName = "assessment",
               icon = icon("clipboard-list"),
               badgeLabel = "Optional",
               badgeColor = "blue")
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jQuery-rwdImageMaps/1.6/jquery.rwdImageMaps.min.js"),
      tags$style(HTML("
        .centered-image {
          display: block; margin-left: auto; margin-right: auto;
          width: 70%; max-width: 1280px; height: auto;
        }
        .pillar-section { margin-bottom: 20px; padding: 10px; background-color: #f8f9fa; border-radius: 5px; }
        .main-objective-section { margin-bottom: 15px; padding: 8px; background-color: #fff; border-left: 3px solid #3c8dbc; }
        .checkbox-label { font-weight: normal; margin-bottom: 5px; }
        .pillar-header { font-size: 18px; font-weight: bold; color: #3c8dbc; }
        .level-1 { margin-left: 20px; } .level-2 { margin-left: 40px; }
        .level-3 { margin-left: 60px; } .level-4 { margin-left: 80px; }
        .assessment-type-box { border: 2px solid #ddd; border-radius: 5px; padding: 15px; margin: 10px 0; cursor: pointer; transition: all 0.3s; }
        .assessment-type-box:hover { border-color: #3c8dbc; background-color: #f0f8ff; }
        .assessment-type-box.selected { border-color: #3c8dbc; background-color: #e3f2fd; border-width: 3px; }
      "))
    ),
    tabItems(
      # Home: image map
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
                      "4.	Indicators (to measure objectives)"
                      ),
                    p(strong("Click on any section of the framework below to view detailed information or explore the menu to the left for additional tools and resources."))
                )),
              fluidRow(
                box(width = 12, title = "Interactive EBM Framework", status = "info", solidHeader = TRUE,
                    uiOutput("home_map_content"))
              )
      ),

      # Using EBM
      tabItem(tabName = "useEBM",
              fluidRow(
                box(width = 12, title = "Using this app", status = "primary", solidHeader = TRUE,
                    p("This tool supports a simple two-step workflow:"),
                    p(strong("Step 1: Select objectives")),
                    p("Creates a checklist of objectives (i.e. use 1 below) at the desired level of detail and scope which is used to assess which objectives are relevant to my management plan, decision, project development, etc. This checklist is also used to determine the level of detail and scope of Step 2"),
                    actionButton("goto_step1", "Go to Step 1", class = "btn-primary"),
                    br(),br(),
                    p(strong("Step 2: Assessment & comparison.")),
                    p("Guides the user through the assessment of policies, management approaches (i.e. use 2 below), scenarios (i.e. use 3 below), management performance (i.e. use 4 below), and activities (i.e. use 5 below). Also allows for the comparison of multiple scenarios or activies"),
                    actionButton("goto_step2", "Go to Step 2", class = "btn-default")
                    )
                ),
                fluidRow(
                  box(width = 12, title = "Using the EBM Framework", status = "primary", solidHeader = TRUE,
                      p("Five broad areas of use have been envisioned for the EBM Framework (below). These are premised on DFO's objective/outcomes based approach to management, and provide the candidate objectives to be used in different decision-making contexts. "),
                                        img(src = "EBMuses.png",
                                            width = "100%"))
                  )

      ),

      # Step 1: full hierarchy checklist
      tabItem(tabName = "select_objectives",
              fluidRow(
                box(width = 12, title = "STEP 1: Select Relevant Objectives", status = "success", solidHeader = TRUE,
                    "Use filters and the checklist below.")
              ),
              fluidRow(
                column(4,
                       box(width = NULL, title = "Filter Options", status = "info", solidHeader = TRUE,
                           radioButtons(
                             "detail_level",
                             label = "Level of Detail",
                             choices = list(
                               "Pillars only" = "pillar",
                               "Pillars + Main Objectives" = "main",
                               "Pillars + Main Objectives + Main Objectives Text" = "main_text",
                               "Add Level 1" = "level1",
                               "Add Level 2" = "level2",
                               "Add Level 3" = "level3",
                               "Add Level 4 (all levels)" = "level4"
                             ),
                             selected = "main"
                           ),
                           h4("Select Pillars"),
                           checkboxGroupInput("pillar_filter", label = NULL, choices = NULL, selected = NULL),
                           h4("Select Main Objectives"),
                           uiOutput("main_objective_checkboxes"),
                           actionButton("select_all", "Select All", class = "btn-sm"),
                           actionButton("deselect_all", "Deselect All", class = "btn-sm")
                       )
                ),
                column(8,
                       box(width = NULL, title = "Interactive Checklist", status = "primary", solidHeader = TRUE,
                           uiOutput("checklist_content")
                       ),
                       box(width = NULL, title = "Export Checklist", status = "warning", solidHeader = TRUE,
                           fluidRow(
                             column(4, downloadButton("download_checklist_word", "Download as Word", class = "btn-primary btn-block")),
                             column(4, downloadButton("download_checklist_excel", "Download as Excel", class = "btn-success btn-block")),
                             column(4, downloadButton("download_checklist_csv", "Download as CSV", class = "btn-info btn-block"))
                           )
                       ),
                       div(style = "text-align:center; background-color:#d4edda; padding:15px; border-radius:5px; margin-top:10px;",
                           h5(icon("arrow-right"), "Ready for Step 2?"),
                           actionButton("proceed_to_step2", "Go to Step 2: Assessment", class = "btn-success btn-lg"))
                )
              )
      ),

      # Step 2: assessment
      tabItem(tabName = "assessment",
              fluidRow(
                box(width = 12, title = "STEP 2: Assessment & Comparison", status = "primary", solidHeader = TRUE,
                    "Use the selected objectives from Step 1 to assess.")
              ),
              conditionalPanel(
                condition = "output.objectives_selected == false",
                fluidRow(
                  box(width = 12, status = "warning", solidHeader = TRUE,
                      div(style = "background-color:#fff3cd; padding:15px; border-radius:5px;",
                          h4(icon("exclamation-triangle"), "No Objectives Selected"),
                          p("Please complete Step 1 first."),
                          actionButton("goto_step1_from_step2", "Go to Step 1", class = "btn-warning")
                      )
                  )
                )
              ),
              conditionalPanel(
                condition = "output.objectives_selected == true",
                fluidRow(
                  box(width = 12, title = "Choose Assessment Type", status = "info", solidHeader = TRUE,
                      fluidRow(
                        column(4, div(class = "assessment-type-box", id = "assessment_type_policy",
                                      onclick = "Shiny.setInputValue('assessment_type', 'policy', {priority: 'event'});",
                                      h4(icon("file-text"), "Policy / Scenario"), p("Qualitative or Likert (0–4). Multiple scenarios supported.")
                        )),
                        column(4, div(class = "assessment-type-box", id = "assessment_type_performance",
                                      onclick = "Shiny.setInputValue('assessment_type', 'performance', {priority: 'event'});",
                                      h4(icon("chart-line"), "Management Performance"), p("0=Not met; 1=Met; 2=Exceeded.")
                        )),
                        column(4, div(class = "assessment-type-box", id = "assessment_type_cumulative",
                                      onclick = "Shiny.setInputValue('assessment_type', 'cumulative', {priority: 'event'});",
                                      h4(icon("layer-group"), "Cumulative Effects"), p("0–3 impact per activity; sum = cumulative.")
                        ))
                      )
                  )
                ),
                uiOutput("assessment_interface")
              )
      )
    )
  )
)

server <- function(input, output, session) {

  # Navigation
  observeEvent(input$goto_step1, updateTabItems(session, "sidebar", "select_objectives"))
  observeEvent(input$goto_step2, updateTabItems(session, "sidebar", "assessment"))
  observeEvent(input$proceed_to_step2, updateTabItems(session, "sidebar", "assessment"))
  observeEvent(input$goto_step1_from_step2, updateTabItems(session, "sidebar", "select_objectives"))

  # Pillar choices
  observe({
    pillars <- ebm_data |> filter(!is.na(Pillar), Pillar != "") |> distinct(Pillar) |> arrange() |> pull()
    updateCheckboxGroupInput(session, "pillar_filter", choices = pillars, selected = pillars)
  })

  # Main objective choices
  output$main_objective_checkboxes <- renderUI({
    req(input$pillar_filter)
    main_objs <- ebm_data |>
      filter(Pillar %in% input$pillar_filter,
             !is.na(Main_Objective), Main_Objective != "",
             !(Main_Objective %in% input$pillar_filter)) |>
      distinct(Main_Objective) |>
      arrange() |>
      pull(Main_Objective)
    if (!length(main_objs)) return(p("No main objectives for selected pillars."))
    checkboxGroupInput("main_objective_filter", label = NULL, choices = main_objs, selected = main_objs)
  })

  observeEvent(input$select_all, {
    pillars <- ebm_data |> filter(!is.na(Pillar), Pillar != "") |> distinct(Pillar) |> arrange() |> pull()
    updateCheckboxGroupInput(session, "pillar_filter", selected = pillars)
    # Update main objectives after pillars change
    main_objs <- ebm_data |>
      filter(Pillar %in% pillars, !is.na(Main_Objective), Main_Objective != "") |>
      distinct(Main_Objective) |>
      arrange() |>
      pull(Main_Objective)
    updateCheckboxGroupInput(session, "main_objective_filter", selected = main_objs)
  })

  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "pillar_filter", selected = character(0))
    updateCheckboxGroupInput(session, "main_objective_filter", selected = character(0))
  })

  # Checklist UI
  output$checklist_content <- renderUI({
    req(input$pillar_filter, input$detail_level)

    if (input$detail_level == "pillar") {
      pillar_items <- list()
      for (pillar in input$pillar_filter) {
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

    if (nrow(filtered_data) == 0) {
      return(p("No objectives selected. Please select at least one Pillar and Main Objective."))
    }

    checklist_items <- list()

    for (pillar in input$pillar_filter) {
      pillar_data <- filtered_data %>% filter(Pillar == pillar)
      if (nrow(pillar_data) == 0) next

      pillar_content <- list(
        div(class = "pillar-section",
            div(class = "pillar-header", pillar)
        )
      )

      for (main_obj in unique(pillar_data$Main_Objective)) {
        if (is.na(main_obj) || main_obj == "") next

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

        if (input$detail_level %in% c("main_text", "level1", "level2", "level3", "level4")) {
          if (!is.na(main_obj_text) && main_obj_text != "") {
            main_obj_content <- c(main_obj_content, list(
              p(style = "font-style: italic; margin-left: 10px;", main_obj_text)
            ))
          }
        }

        if (input$detail_level %in% c("level1", "level2", "level3", "level4")) {
          level1_items <- main_obj_data %>%
            filter(!is.na(Level_1) & Level_1 != "") %>%
            distinct(Level_1)

          if (nrow(level1_items) > 0) {
            for (i in seq_len(nrow(level1_items))) {
              level1_val <- level1_items$Level_1[i]
              main_obj_content <- c(main_obj_content, list(
                div(class = "level-1 checkbox-label",
                    checkboxInput(paste0("chk_l1_", make.names(paste(pillar, main_obj, level1_val))),
                                  label = level1_val,
                                  value = FALSE)
                )
              ))

              if (input$detail_level %in% c("level2", "level3", "level4")) {
                level2_items <- main_obj_data %>%
                  filter(Level_1 == level1_val, !is.na(Level_2) & Level_2 != "") %>%
                  distinct(Level_2)

                if (nrow(level2_items) > 0) {
                  for (j in seq_len(nrow(level2_items))) {
                    level2_val <- level2_items$Level_2[j]
                    main_obj_content <- c(main_obj_content, list(
                      div(class = "level-2 checkbox-label",
                          checkboxInput(paste0("chk_l2_", make.names(paste(pillar, main_obj, level1_val, level2_val))),
                                        label = level2_val,
                                        value = FALSE)
                      )
                    ))

                    if (input$detail_level %in% c("level3", "level4")) {
                      level3_items <- main_obj_data %>%
                        filter(Level_1 == level1_val, Level_2 == level2_val, !is.na(Level_3) & Level_3 != "") %>%
                        distinct(Level_3)

                      if (nrow(level3_items) > 0) {
                        for (k in seq_len(nrow(level3_items))) {
                          level3_val <- level3_items$Level_3[k]
                          main_obj_content <- c(main_obj_content, list(
                            div(class = "level-3 checkbox-label",
                                checkboxInput(paste0("chk_l3_", make.names(paste(pillar, main_obj, level1_val, level2_val, level3_val))),
                                              label = level3_val,
                                              value = FALSE)
                            )
                          ))

                          if (input$detail_level == "level4") {
                            level4_items <- main_obj_data %>%
                              filter(Level_1 == level1_val, Level_2 == level2_val, Level_3 == level3_val,
                                     !is.na(Level_4) & Level_4 != "") %>%
                              distinct(Level_4)

                            if (nrow(level4_items) > 0) {
                              for (l in seq_len(nrow(level4_items))) {
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

  # Get full checklist for Step 1

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

    if (input$detail_level == "pillar") {
      for (pillar in input$pillar_filter) {
        checkbox_id <- paste0("chk_pillar_", make.names(pillar))
        checked <- if (!is.null(input[[checkbox_id]]) && isTRUE(input[[checkbox_id]])) "☑" else "☐"
        checklist_data <- rbind(checklist_data, data.frame(
          Checked = checked, Pillar = pillar, Main_Objective = "", Main_Objectives_text = "",
          Level_1 = "", Level_2 = "", Level_3 = "", Level_4 = "", stringsAsFactors = FALSE
        ))
      }
      return(checklist_data[, c("Checked", "Pillar")])
    }

    req(input$main_objective_filter)

    filtered_data <- ebm_data %>%
      filter(Pillar %in% input$pillar_filter, Main_Objective %in% input$main_objective_filter)

    for (pillar in input$pillar_filter) {
      pillar_data <- filtered_data %>% filter(Pillar == pillar)
      if (nrow(pillar_data) == 0) next

      for (main_obj in unique(pillar_data$Main_Objective)) {
        if (is.na(main_obj) || main_obj == "") next

        main_obj_data <- pillar_data %>% filter(Main_Objective == main_obj)
        main_obj_text <- unique(main_obj_data$Main_Objectives_text)[1]
        if (is.na(main_obj_text)) main_obj_text <- ""

        checkbox_id <- paste0("chk_main_", make.names(paste(pillar, main_obj)))
        checked_main <- if (!is.null(input[[checkbox_id]]) && isTRUE(input[[checkbox_id]])) "☑" else "☐"

        row <- data.frame(
          Checked = checked_main,
          Pillar = pillar,
          Main_Objective = main_obj,
          Main_Objectives_text = if (input$detail_level %in% c("main_text", "level1", "level2", "level3", "level4")) main_obj_text else "",
          Level_1 = "", Level_2 = "", Level_3 = "", Level_4 = "", stringsAsFactors = FALSE
        )
        checklist_data <- rbind(checklist_data, row)

        if (input$detail_level %in% c("level1", "level2", "level3", "level4")) {
          l1s <- main_obj_data %>% filter(!is.na(Level_1), Level_1 != "") %>% distinct(Level_1) %>% pull()
          for (l1 in l1s) {
            id_l1 <- paste0("chk_l1_", make.names(paste(pillar, main_obj, l1)))
            checked_l1 <- if (!is.null(input[[id_l1]]) && isTRUE(input[[id_l1]])) "☑" else "☐"
            checklist_data <- rbind(checklist_data, data.frame(
              Checked = checked_l1, Pillar = "", Main_Objective = "", Main_Objectives_text = "",
              Level_1 = l1, Level_2 = "", Level_3 = "", Level_4 = "", stringsAsFactors = FALSE
            ))

            if (input$detail_level %in% c("level2", "level3", "level4")) {
              l2s <- main_obj_data %>% filter(Level_1 == l1, !is.na(Level_2), Level_2 != "") %>% distinct(Level_2) %>% pull()
              for (l2 in l2s) {
                id_l2 <- paste0("chk_l2_", make.names(paste(pillar, main_obj, l1, l2)))
                checked_l2 <- if (!is.null(input[[id_l2]]) && isTRUE(input[[id_l2]])) "☑" else "☐"
                checklist_data <- rbind(checklist_data, data.frame(
                  Checked = checked_l2, Pillar = "", Main_Objective = "", Main_Objectives_text = "",
                  Level_1 = "", Level_2 = l2, Level_3 = "", Level_4 = "", stringsAsFactors = FALSE
                ))

                if (input$detail_level %in% c("level3", "level4")) {
                  l3s <- main_obj_data %>% filter(Level_1 == l1, Level_2 == l2, !is.na(Level_3), Level_3 != "") %>% distinct(Level_3) %>% pull()
                  for (l3 in l3s) {
                    id_l3 <- paste0("chk_l3_", make.names(paste(pillar, main_obj, l1, l2, l3)))
                    checked_l3 <- if (!is.null(input[[id_l3]]) && isTRUE(input[[id_l3]])) "☑" else "☐"
                    checklist_data <- rbind(checklist_data, data.frame(
                      Checked = checked_l3, Pillar = "", Main_Objective = "", Main_Objectives_text = "",
                      Level_1 = "", Level_2 = "", Level_3 = l3, Level_4 = "", stringsAsFactors = FALSE
                    ))

                    if (input$detail_level == "level4") {
                      l4s <- main_obj_data %>% filter(Level_1 == l1, Level_2 == l2, Level_3 == l3,
                                                      !is.na(Level_4), Level_4 != "") %>% distinct(Level_4) %>% pull()
                      for (l4 in l4s) {
                        id_l4 <- paste0("chk_l4_", make.names(paste(pillar, main_obj, l1, l2, l3, l4)))
                        checked_l4 <- if (!is.null(input[[id_l4]]) && isTRUE(input[[id_l4]])) "☑" else "☐"
                        checklist_data <- rbind(checklist_data, data.frame(
                          Checked = checked_l4, Pillar = "", Main_Objective = "", Main_Objectives_text = "",
                          Level_1 = "", Level_2 = "", Level_3 = "", Level_4 = l4, stringsAsFactors = FALSE
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

    if (input$detail_level == "main") {
      checklist_data <- checklist_data[, c("Checked", "Pillar", "Main_Objective")]
    } else if (input$detail_level == "main_text") {
      checklist_data <- checklist_data[, c("Checked", "Pillar", "Main_Objective", "Main_Objectives_text")]
    } else if (input$detail_level == "level1") {
      checklist_data <- checklist_data[, c("Checked", "Pillar", "Main_Objective", "Main_Objectives_text", "Level_1")]
    } else if (input$detail_level == "level2") {
      checklist_data <- checklist_data[, c("Checked", "Pillar", "Main_Objective", "Main_Objectives_text", "Level_1", "Level_2")]
    } else if (input$detail_level == "level3") {
      checklist_data <- checklist_data[, c("Checked", "Pillar", "Main_Objective", "Main_Objectives_text", "Level_1", "Level_2", "Level_3")]
    } else if (input$detail_level == "level4") {
      checklist_data <- checklist_data[, c("Checked", "Pillar", "Main_Objective", "Main_Objectives_text", "Level_1", "Level_2", "Level_3", "Level_4")]
    }

    checklist_data
  })

  # Selected objectives table for Step 2 and exports
  selected_objectives <- reactive({
    req(input$pillar_filter)
    mos <- ebm_data |>
      filter(Pillar %in% input$pillar_filter, !is.na(Main_Objective), Main_Objective != "") |>
      distinct(Main_Objective) |> pull()
    mos <- if (!is.null(input$main_objective_filter) && length(input$main_objective_filter) > 0) {
      intersect(mos, input$main_objective_filter)
    } else mos
    out <- ebm_data |>
      filter(Pillar %in% input$pillar_filter,
             Main_Objective %in% mos) |>
      select(Pillar, Main_Objective, Level_1, Level_2, Level_3, Level_4) |>
      distinct()
    if (nrow(out) == 0) return(NULL)
    out
  })

  output$objectives_selected <- reactive({
    !is.null(selected_objectives()) && nrow(selected_objectives()) > 0
  })
  outputOptions(output, "objectives_selected", suspendWhenHidden = FALSE)

  # Checklist downloads
  output$download_checklist_csv <- downloadHandler(
    filename = function() paste0("EBM_Checklist_", Sys.Date(), ".csv"),
    content = function(file) {
      dat <- get_full_checklist(); req(dat)
      write.csv(dat, file, row.names = FALSE)
    }
  )

  output$download_checklist_excel <- downloadHandler(
    filename = function() paste0("EBM_Checklist_", Sys.Date(), ".xlsx"),
    content = function(file) {
      dat <- get_full_checklist(); req(dat)
      wb <- createWorkbook()
      addWorksheet(wb, "Checklist")
      writeData(wb, "Checklist", dat)

      headerStyle <- createStyle(fontColour = "#ffffff", fgFill = "#4F81BD",
                                 halign = "center", valign = "center",
                                 textDecoration = "bold", border = "TopBottomLeftRight")
      addStyle(wb, "Checklist", headerStyle, rows = 1, cols = 1:ncol(dat), gridExpand = TRUE)

      bodyStyle <- createStyle(border = "TopBottomLeftRight")
      addStyle(wb, "Checklist", bodyStyle, rows = 2:(nrow(dat) + 1), cols = 1:ncol(dat),
               gridExpand = TRUE, stack = TRUE)
      setColWidths(wb, "Checklist", cols = 1:ncol(dat), widths = "auto")
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  output$download_checklist_word <- downloadHandler(
    filename = function() paste0("EBM_Checklist_", Sys.Date(), ".docx"),
    content = function(file) {
      dat <- get_full_checklist(); req(dat)
      doc <- read_docx()
      doc <- body_add_par(doc, "EBM Framework Checklist", style = "heading 1")
      doc <- body_add_par(doc, paste("Generated:", Sys.Date()))
      doc <- body_add_par(doc, "")

      ft <- flextable(dat)
      ft <- theme_booktabs(ft)

      if ("Main_Objectives_text" %in% names(dat)) {
        col_widths <- c(0.4)
        if ("Pillar" %in% names(dat)) col_widths <- c(col_widths, 1.2)
        if ("Main_Objective" %in% names(dat)) col_widths <- c(col_widths, 1.5)
        if ("Main_Objectives_text" %in% names(dat)) col_widths <- c(col_widths, 2.5)
        if ("Level_1" %in% names(dat)) col_widths <- c(col_widths, 1.5)
        if ("Level_2" %in% names(dat)) col_widths <- c(col_widths, 1.5)
        if ("Level_3" %in% names(dat)) col_widths <- c(col_widths, 1.5)
        if ("Level_4" %in% names(dat)) col_widths <- c(col_widths, 1.5)
        ft <- width(ft, width = col_widths)
      } else {
        ft <- autofit(ft)
      }

      ft <- align(ft, j = 1, align = "center", part = "all")
      ft <- fontsize(ft, j = 1, size = 12, part = "body")
      if (ncol(dat) > 1) {
        ft <- align(ft, j = 2:ncol(dat), align = "left", part = "all")
      }
      ft <- bold(ft, part = "header")
      ft <- fontsize(ft, size = 10, part = "header")
      ft <- bg(ft, bg = "#F0F0F0", part = "body", i = seq(2, nrow(dat), 2))

      doc <- body_add_flextable(doc, ft)
      print(doc, target = file)
    }
  )

  # Assessment type highlighting
  observeEvent(input$assessment_type, {
    shinyjs::removeClass(selector = ".assessment-type-box", class = "selected")
    shinyjs::addClass(id = paste0("assessment_type_", input$assessment_type), class = "selected")
  })

  # Step 2: interfaces
  output$assessment_interface <- renderUI({
    req(input$assessment_type)
    switch(input$assessment_type,
           policy = policy_ui(),
           performance = performance_ui(),
           cumulative = cumulative_ui())
  })

  # ========== POLICY/SCENARIO ASSESSMENT ==========
  policy_tbl <- reactiveVal(NULL)

  policy_ui <- function() {
    so <- selected_objectives(); req(so)
    tagList(
      fluidRow(
        box(width = 12, title = "Policy / Scenario Assessment", status = "success", solidHeader = TRUE,
            fluidRow(
              column(4, numericInput("num_scenarios", "Number of policies/scenarios:", value = 2, min = 1, max = 10, step = 1)),
              column(4, radioButtons("policy_method", "Scoring method:",
                                     choices = c("Qualitative (Content & Context / Content / Implicit / No mention / NA)" = "qual",
                                                 "Likert 0–4 (data/information adequacy)" = "likert"),
                                     selected = "qual")),
              column(4, actionButton("policy_make_template", "Create/Reset Template", class = "btn-primary"))
            ),
            hr(),
            DTOutput("policy_editor"),
            br(),
            fluidRow(
              column(3, downloadButton("policy_download_excel", "Download Template/Results (Excel)", class = "btn-success btn-block")),
              column(3, downloadButton("policy_download_csv", "Download (CSV)", class = "btn-info btn-block")),
              column(6, fileInput("policy_upload", "Upload completed (CSV/XLSX)", accept = c(".csv",".xlsx")))
            )
        )
      ),
      fluidRow(
        box(width = 12, title = "Summary & Plots", status = "info", solidHeader = TRUE,
            uiOutput("policy_summary_ui"),
            plotOutput("policy_bar_per_scenario", height = "300px"),
            plotOutput("policy_heat_per_objective", height = "380px")
        )
      )
    )
  }

  observeEvent(input$policy_make_template, {
    so <- selected_objectives(); req(so)
    base <- make_objective_table(so)
    n <- input$num_scenarios %||% 1
    method <- input$policy_method %||% "qual"
    if (method == "qual") {
      for (i in seq_len(n)) {
        base[[paste0("S", i, "_align")]] <- ""
      }
    } else {
      for (i in seq_len(n)) {
        base[[paste0("S", i, "_score")]] <- NA_real_
      }
    }
    policy_tbl(base)
    showNotification("Template created. You can edit the table.", type = "message", duration = 3)
  })

  output$policy_editor <- renderDT({
    df <- policy_tbl()
    if (is.null(df)) return(datatable(data.frame(Note = "Click 'Create/Reset Template' to begin."), rownames = FALSE))
    lock_cols <- which(names(df) %in% c("Pillar","Main_Objective","Level_1","Level_2","Level_3","Level_4","Objective_Label")) - 1
    dt <- datatable(df, editable = list(target = "cell", disable = list(columns = lock_cols)),
                    options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    dt
  })

  observeEvent(input$policy_editor_cell_edit, {
    info <- input$policy_editor_cell_edit
    df <- policy_tbl(); req(df)
    i <- info$row; j <- info$col; v <- info$value
    df[i, j] <- v
    policy_tbl(df)
  })

  observeEvent(input$policy_upload, {
    ext <- tools::file_ext(input$policy_upload$name)
    df <- tryCatch({
      if (ext == "csv") read.csv(input$policy_upload$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      else read.xlsx(input$policy_upload$datapath, sheet = 1, check.names = FALSE)
    }, error = function(e) NULL)
    if (is.null(df)) {
      showNotification("Could not read file. Check columns.", type = "warning")
    } else {
      policy_tbl(df)
      showNotification("Uploaded.", type = "message")
    }
  })

  output$policy_download_excel <- downloadHandler(
    filename = function() paste0("Policy_Assessment_", Sys.Date(), ".xlsx"),
    content = function(file) {
      df <- policy_tbl(); req(df)
      wb <- createWorkbook(); addWorksheet(wb, "Policy"); writeData(wb, "Policy", df)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  output$policy_download_csv <- downloadHandler(
    filename = function() paste0("Policy_Assessment_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- policy_tbl(); req(df); write.csv(df, file, row.names = FALSE)
    }
  )

  output$policy_summary_ui <- renderUI({
    df <- policy_tbl(); if (is.null(df)) return(NULL)
    score_cols <- grep("^S\\d+_score$", names(df), value = TRUE)
    align_cols <- grep("^S\\d+_align$", names(df), value = TRUE)
    if (length(score_cols)) {
      scores <- unlist(lapply(score_cols, function(cn) suppressWarnings(as.numeric(df[[cn]]))))
      scores <- scores[!is.na(scores)]
      if (!length(scores)) return(p("No numeric scores yet."))
      tagList(
        h4("Likert Summary"),
        p("Objective–scenario cells scored: ", length(scores)),
        p("Average score: ", round(mean(scores), 2))
      )
    } else if (length(align_cols)) {
      aligns <- unlist(lapply(align_cols, function(cn) df[[cn]]))
      aligns <- aligns[!is.na(aligns) & aligns != ""]
      tagList(
        h4("Qualitative Summary"),
        p("Total filled (non-empty): ", length(aligns))
      )
    } else {
      p("No scenario columns. Create/Reset a template.")
    }
  })

  output$policy_bar_per_scenario <- renderPlot({
    df <- policy_tbl(); if (is.null(df)) return(NULL)
    score_cols <- grep("^S\\d+_score$", names(df), value = TRUE)
    align_cols <- grep("^S\\d+_align$", names(df), value = TRUE)

    if (length(score_cols)) {
      tall <- df |>
        pivot_longer(all_of(score_cols), names_to = "Scenario", values_to = "Score") |>
        mutate(Score = suppressWarnings(as.numeric(Score))) |>
        filter(!is.na(Score))
      if (nrow(tall) == 0) return(NULL)
      ggplot(tall, aes(x = Scenario, y = Score, fill = Scenario)) +
        stat_summary(fun = mean, geom = "bar", width = 0.7) +
        labs(title = "Average score per scenario (0–4)", x = "Scenario", y = "Avg score") +
        theme_minimal() + theme(legend.position = "none")
    } else if (length(align_cols)) {
      tall <- df |>
        pivot_longer(all_of(align_cols), names_to = "Scenario", values_to = "Align") |>
        filter(!is.na(Align), Align != "")
      if (nrow(tall) == 0) return(NULL)
      ggplot(tall, aes(x = Scenario, fill = Align)) +
        geom_bar(position = "stack") +
        labs(title = "Qualitative alignment counts by scenario", x = "Scenario", y = "Count") +
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })

  output$policy_heat_per_objective <- renderPlot({
    df <- policy_tbl(); if (is.null(df)) return(NULL)
    score_cols <- grep("^S\\d+_score$", names(df), value = TRUE)
    align_cols <- grep("^S\\d+_align$", names(df), value = TRUE)

    if (length(score_cols)) {
      tall <- df |>
        mutate(Objective = Objective_Label) |>
        pivot_longer(all_of(score_cols), names_to = "Scenario", values_to = "Score") |>
        mutate(Score = suppressWarnings(as.numeric(Score))) |>
        filter(!is.na(Score))
      if (nrow(tall) == 0) return(NULL)
      ggplot(tall, aes(x = Scenario, y = Objective, fill = Score)) +
        geom_tile(color = "#ddd") +
        scale_fill_gradient(low = "#f0f9ff", high = "#084081", na.value = "#eee") +
        labs(title = "Objective × Scenario heatmap (Likert score)", x = "Scenario", y = "Objective") +
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (length(align_cols)) {
      map_val <- function(x) {
        dplyr::case_when(
          x == "Content & Context" ~ 3,
          x == "Content" ~ 2,
          x == "Implicit" ~ 1,
          x == "No mention" ~ 0,
          TRUE ~ NA_real_
        )
      }
      tall <- df |>
        mutate(Objective = Objective_Label) |>
        pivot_longer(all_of(align_cols), names_to = "Scenario", values_to = "Align") |>
        mutate(Score = map_val(Align)) |>
        filter(!is.na(Score))
      if (nrow(tall) == 0) return(NULL)
      ggplot(tall, aes(x = Scenario, y = Objective, fill = Score)) +
        geom_tile(color = "#ddd") +
        scale_fill_gradient(low = "#f0fff0", high = "#006d2c", na.value = "#eee") +
        labs(title = "Objective × Scenario heatmap (qualitative mapped to numeric)", x = "Scenario", y = "Objective") +
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })

  # ========== PERFORMANCE ==========
  perf_tbl <- reactiveVal(NULL)

  performance_ui <- function() {
    so <- selected_objectives(); req(so)
    tagList(
      fluidRow(
        box(width = 12, title = "Management Performance (Report Card)", status = "success", solidHeader = TRUE,
            actionButton("perf_make_template", "Create/Reset Template", class = "btn-primary"),
            hr(),
            DTOutput("perf_editor"),
            br(),
            fluidRow(
              column(3, downloadButton("perf_download_excel", "Download (Excel)", class = "btn-success btn-block")),
              column(3, downloadButton("perf_download_csv", "Download (CSV)", class = "btn-info btn-block")),
              column(6, fileInput("perf_upload", "Upload completed (CSV/XLSX)", accept = c(".csv",".xlsx")))
            )
        )
      ),
      fluidRow(
        box(width = 12, title = "Summary & Plot", status = "info", solidHeader = TRUE,
            uiOutput("perf_summary_ui"),
            plotOutput("perf_bar_objective", height = "320px"),
            plotOutput("perf_bar_pillar", height = "300px")
        )
      )
    )
  }

  observeEvent(input$perf_make_template, {
    so <- selected_objectives(); req(so)
    base <- make_objective_table(so)
    base[["Score"]] <- NA_real_  # 0/1/2
    perf_tbl(base)
    showNotification("Template created.", type = "message")
  })

  output$perf_editor <- renderDT({
    df <- perf_tbl()
    if (is.null(df)) return(datatable(data.frame(Note = "Click 'Create/Reset Template' to begin."), rownames = FALSE))
    lock_cols <- which(names(df) %in% c("Pillar","Main_Objective","Level_1","Level_2","Level_3","Level_4","Objective_Label")) - 1
    datatable(df, editable = list(target = "cell", disable = list(columns = lock_cols)),
              options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })

  observeEvent(input$perf_editor_cell_edit, {
    info <- input$perf_editor_cell_edit
    df <- perf_tbl(); req(df)
    df[info$row, info$col] <- info$value
    perf_tbl(df)
  })

  observeEvent(input$perf_upload, {
    ext <- tools::file_ext(input$perf_upload$name)
    df <- tryCatch({
      if (ext == "csv") read.csv(input$perf_upload$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      else read.xlsx(input$perf_upload$datapath, 1, check.names = FALSE)
    }, error = function(e) NULL)
    if (is.null(df)) showNotification("Could not read file.", type = "warning") else { perf_tbl(df); showNotification("Uploaded.", type = "message") }
  })

  output$perf_download_excel <- downloadHandler(
    filename = function() paste0("Performance_", Sys.Date(), ".xlsx"),
    content = function(file) {
      df <- perf_tbl(); req(df)
      wb <- createWorkbook(); addWorksheet(wb, "Performance"); writeData(wb, "Performance", df)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  output$perf_download_csv <- downloadHandler(
    filename = function() paste0("Performance_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- perf_tbl(); req(df); write.csv(df, file, row.names = FALSE)
    }
  )

  output$perf_summary_ui <- renderUI({
    df <- perf_tbl(); if (is.null(df) || !"Score" %in% names(df)) return(NULL)
    sc <- suppressWarnings(as.numeric(df$Score)); sc <- sc[!is.na(sc)]
    if (!length(sc)) return(p("No scores yet."))
    tagList(
      h4("Summary"),
      p("Objectives scored: ", length(sc)),
      p("Exceeded (2): ", sum(sc == 2)),
      p("Met (1): ", sum(sc == 1)),
      p("Not met (0): ", sum(sc == 0)),
      p("Average score: ", round(mean(sc), 2))
    )
  })
  output$perf_bar_objective <- renderPlot({
    df <- perf_tbl(); if (is.null(df) || !"Score" %in% names(df)) return(NULL)
    df$Score <- suppressWarnings(as.numeric(df$Score))
    ggplot(df |> filter(!is.na(Score)), aes(x = Objective_Label, y = Score, fill = factor(Score))) +
      geom_bar(stat = "identity") + coord_flip() +
      scale_fill_manual(values = c("0" = "#e74c3c", "1" = "#f9ca24", "2" = "#27ae60"),
                        name = "Score", labels = c("0 Not met", "1 Met", "2 Exceeded")) +
      labs(title = "Performance per objective", x = "Objective", y = "Score") +
      theme_minimal() + theme(legend.position = "bottom")
  })
  output$perf_bar_pillar <- renderPlot({
    df <- perf_tbl(); if (is.null(df) || !"Score" %in% names(df)) return(NULL)
    df$Score <- suppressWarnings(as.numeric(df$Score))
    pdat <- df |> filter(!is.na(Score)) |> group_by(Pillar) |> summarise(Avg = mean(Score), .groups = "drop")
    ggplot(pdat, aes(x = Pillar, y = Avg, fill = Pillar)) + geom_bar(stat = "identity") +
      labs(title = "Average performance by pillar", x = "Pillar", y = "Average score (0–2)") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  })

  # ========== CUMULATIVE ==========
  cumu_tbl <- reactiveVal(NULL)

  cumulative_ui <- function() {
    so <- selected_objectives(); req(so)
    tagList(
      fluidRow(
        box(width = 12, title = "Cumulative Effects", status = "success", solidHeader = TRUE,
            numericInput("num_activities", "Number of activities:", value = 3, min = 2, max = 10, step = 1),
            actionButton("cumu_make_template", "Create/Reset Template", class = "btn-primary"),
            hr(),
            DTOutput("cumu_editor"),
            br(),
            fluidRow(
              column(3, downloadButton("cumu_download_excel", "Download (Excel)", class = "btn-success btn-block")),
              column(3, downloadButton("cumu_download_csv", "Download (CSV)", class = "btn-info btn-block")),
              column(6, fileInput("cumu_upload", "Upload completed (CSV/XLSX)", accept = c(".csv",".xlsx")))
            )
        )
      ),
      fluidRow(
        box(width = 12, title = "Summary & Plots", status = "info", solidHeader = TRUE,
            uiOutput("cumu_summary_ui"),
            plotOutput("cumu_bar_per_activity", height = "300px"),
            plotOutput("cumu_heat_objective_activity", height = "380px")
        )
      )
    )
  }

  observeEvent(input$cumu_make_template, {
    so <- selected_objectives(); req(so)
    base <- make_objective_table(so)
    n <- input$num_activities %||% 3
    for (i in seq_len(n)) base[[paste0("A", i, "_impact")]] <- NA_real_
    cumu_tbl(base)
    showNotification("Template created.", type = "message")
  })

  output$cumu_editor <- renderDT({
    df <- cumu_tbl()
    if (is.null(df)) return(datatable(data.frame(Note = "Click 'Create/Reset Template' to begin."), rownames = FALSE))
    lock_cols <- which(names(df) %in% c("Pillar","Main_Objective","Level_1","Level_2","Level_3","Level_4","Objective_Label")) - 1
    datatable(df, editable = list(target = "cell", disable = list(columns = lock_cols)),
              options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })

  observeEvent(input$cumu_editor_cell_edit, {
    info <- input$cumu_editor_cell_edit
    df <- cumu_tbl(); req(df)
    df[info$row, info$col] <- info$value
    cumu_tbl(df)
  })

  observeEvent(input$cumu_upload, {
    ext <- tools::file_ext(input$cumu_upload$name)
    df <- tryCatch({
      if (ext == "csv") read.csv(input$cumu_upload$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      else read.xlsx(input$cumu_upload$datapath, 1, check.names = FALSE)
    }, error = function(e) NULL)
    if (is.null(df)) showNotification("Could not read file.", type = "warning") else { cumu_tbl(df); showNotification("Uploaded.", type = "message") }
  })

  output$cumu_download_excel <- downloadHandler(
    filename = function() paste0("Cumulative_", Sys.Date(), ".xlsx"),
    content = function(file) {
      df <- cumu_tbl(); req(df)
      wb <- createWorkbook(); addWorksheet(wb, "Cumulative"); writeData(wb, "Cumulative", df)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  output$cumu_download_csv <- downloadHandler(
    filename = function() paste0("Cumulative_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- cumu_tbl(); req(df); write.csv(df, file, row.names = FALSE)
    }
  )

  output$cumu_summary_ui <- renderUI({
    df <- cumu_tbl(); if (is.null(df)) return(NULL)
    imp_cols <- grep("^A\\d+_impact$", names(df), value = TRUE)
    if (!length(imp_cols)) return(NULL)
    vals <- unlist(lapply(imp_cols, function(cn) suppressWarnings(as.numeric(df[[cn]]))))
    vals <- vals[!is.na(vals)]
    tagList(
      h4("Summary"),
      p("Objective–activity cells scored: ", length(vals)),
      p("Average impact: ", round(mean(vals), 2))
    )
  })

  output$cumu_bar_per_activity <- renderPlot({
    df <- cumu_tbl(); if (is.null(df)) return(NULL)
    imp_cols <- grep("^A\\d+_impact$", names(df), value = TRUE)
    if (!length(imp_cols)) return(NULL)
    tall <- df |>
      pivot_longer(all_of(imp_cols), names_to = "Activity", values_to = "Impact") |>
      mutate(Impact = suppressWarnings(as.numeric(Impact))) |>
      filter(!is.na(Impact))
    if (nrow(tall) == 0) return(NULL)
    ggplot(tall, aes(x = Activity, y = Impact, fill = Activity)) +
      stat_summary(fun = mean, geom = "bar", width = 0.7) +
      labs(title = "Average impact per activity (0–3)", x = "Activity", y = "Avg impact") +
      theme_minimal() + theme(legend.position = "none")
  })

  output$cumu_heat_objective_activity <- renderPlot({
    df <- cumu_tbl(); if (is.null(df)) return(NULL)
    imp_cols <- grep("^A\\d+_impact$", names(df), value = TRUE)
    if (!length(imp_cols)) return(NULL)
    tall <- df |>
      mutate(Objective = Objective_Label) |>
      pivot_longer(all_of(imp_cols), names_to = "Activity", values_to = "Impact") |>
      mutate(Impact = suppressWarnings(as.numeric(Impact)))
    ggplot(tall, aes(x = Activity, y = Objective, fill = Impact)) +
      geom_tile(color = "#ddd") +
      scale_fill_gradient(low = "#fff5f0", high = "#a50f15", na.value = "#eee") +
      labs(title = "Objective × Activity heatmap (impact 0–3)", x = "Activity", y = "Objective") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # -------- HOME IMAGE MAP (restored) --------
  home_view <- reactiveVal("map")
  home_filter <- reactiveVal(NULL)

  output$home_map_content <- renderUI({
    if (home_view() == "map") {
      tagList(
        tags$img(src = "EBM.png", usemap = "#puzzle-map", width = "1280", class = "centered-image"),
        tags$map(
          name = "puzzle-map",
          tags$area(shape = "circle", coords = "633,627,243", href = "#", alt = "EBM Framework",
                    title = "EBM Framework - Click for details",
                    onclick = "Shiny.setInputValue('home_area_clicked', 'EBM Framework', {priority: 'event'}); return false;"),
          tags$area(shape = "poly",
                    coords = "224,624,370,622,382,550,406,498,454,438,526,394,606,368,636,370,632,220,504,242,382,302,292,408,242,516,228,572",
                    href = "#", alt = "Ecological",
                    title = "Ecological - Click for details",
                    onclick = "Shiny.setInputValue('home_area_clicked', 'Ecological', {priority: 'event'}); return false;"),
          tags$area(shape = "poly", coords = "62,618,137,338,259,411,212,619", href = "#",
                    alt = "Habitat", title = "Habitat - Click for details",
                    onclick = "Shiny.setInputValue('home_area_clicked', 'Habitat', {priority: 'event'}); return false;"),
          tags$area(shape = "poly", coords = "147,329,345,128,424,256,272,406", href = "#",
                    alt = "Biodiversity", title = "Biodiversity - Click for details",
                    onclick = "Shiny.setInputValue('home_area_clicked', 'Biodiversity', {priority: 'event'}); return false;"),
          tags$area(shape = "poly", coords = "360,120,434,247,638,204,632,46", href = "#",
                    alt = "Productivity", title = "Productivity - Click for details",
                    onclick = "Shiny.setInputValue('home_area_clicked', 'Productivity', {priority: 'event'}); return false;"),
          tags$area(shape = "poly",
                    coords = "648,224,648,372,724,384,776,412,828,458,868,528,886,604,886,624,1036,622,1018,508,966,402,906,332,788,252",
                    href = "#", alt = "Economic",
                    title = "Economic - Click for details",
                    onclick = "Shiny.setInputValue('home_area_clicked', 'Economic', {priority: 'event'}); return false;"),
          tags$area(shape = "poly", coords = "647,47,648,207,846,263,927,118", href = "#",
                    alt = "Economic Efficiency", title = "Economic Efficiency - Click for details",
                    onclick = "Shiny.setInputValue('home_area_clicked', 'Economic Efficiency', {priority: 'event'}); return false;"),
          tags$area(shape = "poly", coords = "938,125,859,271,993,413,1139,332", href = "#",
                    alt = "Economic Equity", title = "Economic Equity - Click for details",
                    onclick = "Shiny.setInputValue('home_area_clicked', 'Economic Equity', {priority: 'event'}); return false;"),
          tags$area(shape = "poly", coords = "1141,346,1000,427,1049,620,1219,620", href = "#",
                    alt = "Economic Sustainability", title = "Economic Sustainability - Click for details",
                    onclick = "Shiny.setInputValue('home_area_clicked', 'Economic Sustainability', {priority: 'event'}); return false;"),
          tags$area(shape = "poly",
                    coords = "888,636,1030,634,1018,742,966,850,868,952,746,1010,638,1028,644,886,730,864,794,822,836,778,866,732,882,676",
                    href = "#", alt = "Governance",
                    title = "Governance - Click for details",
                    onclick = "Shiny.setInputValue('home_area_clicked', 'Governance', {priority: 'event'}); return false;"),
          tags$area(shape = "poly", coords = "650,1048,649,1206,924,1128,844,991", href = "#",
                    alt = "Legal Obligations & Other Commitments",
                    title = "Legal Obligations & Other Commitments - Click for details",
                    onclick = "Shiny.setInputValue('home_area_clicked', 'Legal Obligations and other commitments', {priority: 'event'}); return false;"),
          tags$area(shape = "poly", coords = "858,976,936,1124,1135,924,989,836", href = "#",
                    alt = "Governance Structure & Processes",
                    title = "Governance Structure & Processes - Click for details",
                    onclick = "Shiny.setInputValue('home_area_clicked', 'Governance Structures and Processes', {priority: 'event'}); return false;"),
          tags$area(shape = "poly", coords = "1005,824,1143,910,1216,639,1056,637", href = "#",
                    alt = "Governance Outcomes", title = "Governance Outcomes - Click for details",
                    onclick = "Shiny.setInputValue('home_area_clicked', 'Governance Outcomes', {priority: 'event'}); return false;"),
          tags$area(shape = "poly",
                    coords = "227,632,372,630,394,727,485,835,559,870,636,880,637,1031,500,1008,376,940,283,825,237,720",
                    href = "#", alt = "Social & Cultural",
                    title = "Social & Cultural - Click for details",
                    onclick = "Shiny.setInputValue('home_area_clicked', 'Social & Cultural', {priority: 'event'}); return false;"),
          tags$area(shape = "poly", coords = "212,633,244,780,101,844,62,631", href = "#",
                    alt = "Sustainable Communities", title = "Sustainable Communities - Click for details",
                    onclick = "Shiny.setInputValue('home_area_clicked', 'Sustainable Communities', {priority: 'event'}); return false;"),
          tags$area(shape = "poly", coords = "250,793,339,920,229,1039,105,850", href = "#",
                    alt = "Health & Well-being", title = "Health & Well-being - Click for details",
                    onclick = "Shiny.setInputValue('home_area_clicked', 'Health and Well-being', {priority: 'event'}); return false;"),
          tags$area(shape = "poly", coords = "349,932,483,1014,424,1160,241,1043", href = "#",
                    alt = "Ethical & Just Activities", title = "Ethical & Just Activities - Click for details",
                    onclick = "Shiny.setInputValue('home_area_clicked', 'Ethical and Just Activities', {priority: 'event'}); return false;"),
          tags$area(shape = "poly", coords = "497,1016,434,1168,633,1203,639,1041", href = "#",
                    alt = "culture", title = "Culture - Click for details",
                    onclick = "Shiny.setInputValue('home_area_clicked', 'culture', {priority: 'event'}); return false;")
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
            DTOutput("home_data_table"))
      )
    }
  })

  observeEvent(input$home_area_clicked, {
    home_filter(input$home_area_clicked)
    home_view("table")
  })
  observeEvent(input$home_back_button, home_view("map"))

  output$home_data_table <- renderDT({
    req(home_filter())
    filter_value <- home_filter()
    pillar_search <- ""; objective_search <- ""
    if (filter_value %in% c("Ecological", "Economic", "Governance", "Social & Cultural")) {
      pillar_search <- filter_value
    } else if (filter_value != "EBM Framework") {
      objective_search <- filter_value
    }
    datatable(ebm_data,
              filter = "top",
              options = list(pageLength = 25, scrollX = TRUE,
                             searchCols = list(list(search = pillar_search), list(search = objective_search))),
              rownames = FALSE)
  })
}

shinyApp(ui, server)
