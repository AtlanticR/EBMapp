library(shiny)

ui <- fluidPage(
  tags$head(
    # Load the responsive image maps library
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jQuery-rwdImageMaps/1.6/jquery.rwdImageMaps.min.js"),
    tags$script(HTML("
      $(document).ready(function() {
        $('img[usemap]').rwdImageMaps();
      });
    ")),
    tags$style(HTML("
      .centered-image {
        display: block;
        margin-left: auto;
        margin-right: auto;
        width: 40%;  /* Adjust this percentage to make it smaller/larger */
        max-width: 1280px;  /* Original size as maximum */
        height: auto;
      }
    "))
  ),

  tags$img(
    src = "EBM.png",
    usemap = "#puzzle-map",
    width = "1280",#"1626",  # Keep original width for coordinate reference
    class = "centered-image"
  ),



  tags$map(
    name = "puzzle-map",
    tags$area(
      shape = "circle",
      coords = "633,627,243",
      href = "./EBM",
      target = "_blank",
      alt = "EBM Framework"
    ),


    tags$area(
      shape = "poly",
      coords = "224,624,370,622,382,550,406,498,454,438,526,394,606,368,636,370,632,220,504,242,382,302,292,408,242,516,228,572",
      href = "./ecological",
      target = "_blank",
      alt = "Ecological"
    ),

    tags$area(
      shape = "poly",
      coords = "62,618,137,338,259,411,212,619",
      href = "./ecological/habitat",
      target = "_blank",
      alt = "Habitat"
    ),

    tags$area(
      shape = "poly",
      coords = "147,329,345,128,424,256,272,406",
      href = "./ecological/biodiversity",
      target = "_blank",
      alt = "Biodiversity"
    ),

    tags$area(
      shape = "poly",
      coords = "360,120,434,247,638,204,632,46",
      href = "./ecological/productivity",
      target = "_blank",
      alt = "Productivity"
    ),


    tags$area(
      shape = "poly",
      coords = "648,224,648,372,724,384,776,412,828,458,868,528,886,604,886,624,1036,622,1018,508,966,402,906,332,788,252",
      href = "./economic",
      target = "_blank",
      alt = "Economic"
    ),

    tags$area(
      shape = "poly",
      coords = "647,47,648,207,846,263,927,118",
      href = "./economic/efficiency",
      target = "_blank",
      alt = "Economic Efficiency"
    ),

    tags$area(
      shape = "poly",
      coords = "938,125,859,271,993,413,1139,332",
      href = "./economic/equity",
      target = "_blank",
      alt = "Economic Equity"
    ),

    tags$area(
      shape = "poly",
      coords = "1141,346,1000,427,1049,620,1219,620",
      href = "./economic/sustainability",
      target = "_blank",
      alt = "Economic Sustainability"
    ),


    tags$area(
      shape = "poly",
      coords = "888,636,1030,634,1018,742,966,850,868,952,746,1010,638,1028,644,886,730,864,794,822,836,778,866,732,882,676",
      href = "./governance",
      target = "_blank",
      alt = "Governance"
    ),

    tags$area(
      shape = "poly",
      coords = "650,1048,649,1206,924,1128,844,991",
      href = "./governance/legal",
      target = "_blank",
      alt = "Legal Obligations & Other Commitments"
    ),

    tags$area(
      shape = "poly",
      coords = "858,976,936,1124,1135,924,989,836",
      href = "./governance/structure",
      target = "_blank",
      alt = "Governance Structure & Processes"
    ),

    tags$area(
      shape = "poly",
      coords = "1005,824,1143,910,1216,639,1056,637",
      href = "./governance/outcomes",
      target = "_blank",
      alt = "Governance Outcomes"
    ),


    tags$area(
      shape = "poly",
      coords = "227,632,372,630,394,727,485,835,559,870,636,880,637,1031,500,1008,376,940,283,825,237,720",
      href = "./social",
      target = "_blank",
      alt = "Social & Cultural"
    ),

    tags$area(
      shape = "poly",
      coords = "212,633,244,780,101,844,62,631",
      href = "./social/communities",
      target = "_blank",
      alt = "Sustainable Communities"
    ),
    tags$area(
      shape = "poly",
      coords = "250,793,339,920,229,1039,105,850",
      href = "./social/health",
      target = "_blank",
      alt = "Health & Well-being"
    ),
    tags$area(
      shape = "poly",
      coords = "349,932,483,1014,424,1160,241,1043",
      href = "./social/activities",
      target = "_blank",
      alt = "Ethical & Just Activities"
    ),
    tags$area(
      shape = "poly",
      coords = "497,1016,434,1168,633,1203,639,1041",
      href = "./social/culture",
      target = "_blank",
      alt = "culture"
    ),


  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
