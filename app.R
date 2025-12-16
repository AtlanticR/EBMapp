library(shiny)
library(DT)
library(dplyr)

# Read the data
ebm_data <- read.csv("data/EBM Framework Spreadsheet 3-Nov-2025.csv", stringsAsFactors = FALSE) |>
  mutate(Main_Objective = trimws(Main_Objective),
         Pillar = trimws(Pillar))

ui <- fluidPage(
  tags$head(
    # Load the responsive image maps library
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jQuery-rwdImageMaps/1.6/jquery.rwdImageMaps.min.js"),
    tags$style(HTML("
      .centered-image {
        display: block;
        margin-left: auto;
        margin-right: auto;
        width: 40%;
        max-width: 1280px;
        height: auto;
      }
      .back-button {
        margin: 20px auto;
        display: block;
        width: 200px;
      }
    "))
  ),

  # Show/hide image based on state
  uiOutput("main_content")
)

server <- function(input, output, session) {

  # Reactive value to track current view
  current_view <- reactiveVal("map")
  current_filter <- reactiveVal(NULL)

  # Main content output
  output$main_content <- renderUI({
    if (current_view() == "map") {
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
            onclick = "Shiny.setInputValue('area_clicked', 'EBM Framework', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "224,624,370,622,382,550,406,498,454,438,526,394,606,368,636,370,632,220,504,242,382,302,292,408,242,516,228,572",
            href = "#",
            alt = "Ecological",
            title = "Ecological - Click for details",
            onclick = "Shiny.setInputValue('area_clicked', 'Ecological', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "62,618,137,338,259,411,212,619",
            href = "#",
            alt = "Habitat",
            title = "Habitat - Click for details",
            onclick = "Shiny.setInputValue('area_clicked', 'Habitat', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "147,329,345,128,424,256,272,406",
            href = "#",
            alt = "Biodiversity",
            title = "Biodiversity - Click for details",
            onclick = "Shiny.setInputValue('area_clicked', 'Biodiversity', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "360,120,434,247,638,204,632,46",
            href = "#",
            alt = "Productivity",
            title = "Productivity - Click for details",
            onclick = "Shiny.setInputValue('area_clicked', 'Productivity', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "648,224,648,372,724,384,776,412,828,458,868,528,886,604,886,624,1036,622,1018,508,966,402,906,332,788,252",
            href = "#",
            alt = "Economic",
            title = "Economic - Click for details",
            onclick = "Shiny.setInputValue('area_clicked', 'Economic', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "647,47,648,207,846,263,927,118",
            href = "#",
            alt = "Economic Efficiency",
            title = "Economic Efficiency - Click for details",
            onclick = "Shiny.setInputValue('area_clicked', 'Economic Efficiency', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "938,125,859,271,993,413,1139,332",
            href = "#",
            alt = "Economic Equity",
            title = "Economic Equity - Click for details",
            onclick = "Shiny.setInputValue('area_clicked', 'Economic Equity', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "1141,346,1000,427,1049,620,1219,620",
            href = "#",
            alt = "Economic Sustainability",
            title = "Economic Sustainability - Click for details",
            onclick = "Shiny.setInputValue('area_clicked', 'Economic Sustainability', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "888,636,1030,634,1018,742,966,850,868,952,746,1010,638,1028,644,886,730,864,794,822,836,778,866,732,882,676",
            href = "#",
            alt = "Governance",
            title = "Governance - Click for details",
            onclick = "Shiny.setInputValue('area_clicked', 'Governance', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "650,1048,649,1206,924,1128,844,991",
            href = "#",
            alt = "Legal Obligations & Other Commitments",
            title = "Legal Obligations & Other Commitments - Click for details",
            onclick = "Shiny.setInputValue('area_clicked', 'Legal Obligations and other commitments', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "858,976,936,1124,1135,924,989,836",
            href = "#",
            alt = "Governance Structure & Processes",
            title = "Governance Structure & Processes - Click for details",
            onclick = "Shiny.setInputValue('area_clicked', 'Governance Structures and Processes', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "1005,824,1143,910,1216,639,1056,637",
            href = "#",
            alt = "Governance Outcomes",
            title = "Governance Outcomes - Click for details",
            onclick = "Shiny.setInputValue('area_clicked', 'Governance Outcomes', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "227,632,372,630,394,727,485,835,559,870,636,880,637,1031,500,1008,376,940,283,825,237,720",
            href = "#",
            alt = "Social & Cultural",
            title = "Social & Cultural - Click for details",
            onclick = "Shiny.setInputValue('area_clicked', 'Social & Cultural', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "212,633,244,780,101,844,62,631",
            href = "#",
            alt = "Sustainable Communities",
            title = "Sustainable Communities - Click for details",
            onclick = "Shiny.setInputValue('area_clicked', 'Sustainable Communities', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "250,793,339,920,229,1039,105,850",
            href = "#",
            alt = "Health & Well-being",
            title = "Health & Well-being - Click for details",
            onclick = "Shiny.setInputValue('area_clicked', 'Health and Well-being', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "349,932,483,1014,424,1160,241,1043",
            href = "#",
            alt = "Ethical & Just Activities",
            title = "Ethical & Just Activities - Click for details",
            onclick = "Shiny.setInputValue('area_clicked', 'Ethical and Just Activities', {priority: 'event'}); return false;"
          ),
          tags$area(
            shape = "poly",
            coords = "497,1016,434,1168,633,1203,639,1041",
            href = "#",
            alt = "culture",
            title = "Culture - Click for details",
            onclick = "Shiny.setInputValue('area_clicked', 'culture', {priority: 'event'}); return false;"
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
        h3(paste("Viewing:", current_filter())),
        actionButton("back_button", "Back to Home", class = "back-button btn-primary"),
        br(),
        DTOutput("data_table")
      )
    }
  })

  # Handle area clicks
  observeEvent(input$area_clicked, {
    current_filter(input$area_clicked)
    current_view("table")
  })

  # Handle back button
  observeEvent(input$back_button, {
    current_view("map")
  })

  # Render filtered data table
  output$data_table <- renderDT({
    req(current_filter())

    filter_value <- current_filter()

    pillar_search <- ""
    objective_search <- ""

    if (filter_value %in% c("Ecological", "Economic", "Governance", "Social & Cultural")) {
      pillar_search <- filter_value
    } else if (filter_value != "EBM Framework") {
      objective_search <- filter_value
    }

    # Create display version with blanked repeats
    data_display <- ebm_data
    cols_to_merge <- c("Pillar", "Main_Objective", "Main_Objectives_text",
                       "Level_1", "Level_2", "Level_3")

    for (col in cols_to_merge) {
      for (i in 2:nrow(data_display)) {
        if (!is.na(data_display[i, col]) &&
            !is.na(data_display[i-1, col]) &&
            data_display[i, col] == data_display[i-1, col]) {
          data_display[i, col] <- ""
        }
      }
    }

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
          NULL,
          NULL,
          NULL,
          NULL
        ),
        ordering = FALSE,
        columnDefs = list(
          list(
            targets = which(names(ebm_data) %in% cols_to_merge) - 1,  # 0-based for JS
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
