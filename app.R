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

use_explanations <- list()
use_explanations[[1]] <- function() {
  tagList(
    tags$p("Question: what objectives are relevant to my management plan/decision/project development etc?"),

    tags$ul(
      tags$li("Review objectives in each of the 4 Pillars (Ecological, Economic, Social and Cultural, and Governance)"),
      tags$li("Suggestion to start with Level 1 Objectives and use lower level objectives to inform intent of the objectives"),
      tags$li("In some cases, Level Two or lower level objectives may be more useful"),
      tags$li("Assess which objectives are relevant to your needs")
    )
  )
}

use_explanations[[2]] <- function() {
  tagList(

    tags$p(
      "Do current DFO policies and management approaches, some of which date back to the 1990s,
       embrace the full holistic spectrum of current EBM thinking, as outlined in the EBM Framework.
       Therefore, the EBM Framework can be used to evaluate how current policies and management
       approaches align with EBM objectives, and develop recommendations for updates where appropriate."
    ),

    tags$hr(),
    tags$p(
      "For each row of the table below, paste the line from your policy or management approach that aligns
       with the associated objective in the Policy Alignment column. Assess alignment using one of the two scoring systems below and enter the score in the Score column. Lastly, Enter a rationale in the Rationale column."
    ),

    tags$h4("Policy Alignment Table"),

    # Scrollable wide table
    div(
      style = "overflow-x: auto; width: 100%;",
      tags$table(
        style = "min-width: 1500px; border-collapse: collapse; border: 1px solid #ccc;",
        tags$thead(
          tags$tr(
            tags$th("Pillar", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Main_Objective", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Level_1", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Level_2", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Level_3", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Level_4", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Objective_Label", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Score_1", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: center;"),
            tags$th("Policy_1_alignment", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Rationale", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("Ecological", style = "border: 1px solid #ccc; padding: 6px; vertical-align: top;"),
            tags$td("Productivity", style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("1. Pressures on ecosystem productivity are managed", style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("a. Fishing pressures on ecosystem productivity are managed", style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("i. Fishing mortality is limited to levels necessary to promote sustainability", style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("", style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("Productivity / 1. Pressures on ecosystem productivity are managed / a. Fishing pressures on ecosystem productivity are managed / i. Fishing mortality is limited to levels necessary to promote sustainability /", style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("NA", style = "border: 1px solid #ccc; padding: 6px; text-align: center;"),
            tags$td(NA, style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("", style = "border: 1px solid #ccc; padding: 6px;")
          )
        )
      )
    )
    ,

    tags$h4("Qualitative Scoring"),

    tags$table(
      style = "width: 100%; border-collapse: collapse; border: 1px solid #ccc;",
      tags$thead(
        tags$tr(
          tags$th("Value", style = "border: 1px solid #ccc; padding: 4px;"),
          tags$th("Description", style = "border: 1px solid #ccc; padding: 4px;")
        )
      ),
      tags$tbody(
        tags$tr(tags$td("X", style = "border: 1px solid #ccc; padding: 4px;"),
                tags$td("Not relevant / applicable - dependent on context, case study, purview, department, jurisdiction, etc.", style = "border: 1px solid #ccc; padding: 4px;")),
        tags$tr(tags$td("0", style = "border: 1px solid #ccc; padding: 4px;"),
                tags$td("No mention - the objective of the EBM framework is not present within this unit of analysis", style = "border: 1px solid #ccc; padding: 4px;")),
        tags$tr(tags$td("1", style = "border: 1px solid #ccc; padding: 4px;"),
                tags$td("Implicit Content mention - the phrase/terms used in the unit of analysis generally or vaguely aligns with, or alludes to, the intended meaning in the EBM Framework (e.g., high-level concepts)", style = "border: 1px solid #ccc; padding: 4px;")),
        tags$tr(tags$td("2", style = "border: 1px solid #ccc; padding: 4px;"),
                tags$td("Explicit Content mention - the phrase/terms used in the unit of analysis partially aligns with, or alludes to, the meaning intended in the EBM framework, but may appear slightly different.", style = "border: 1px solid #ccc; padding: 4px;")),
        tags$tr(tags$td("3", style = "border: 1px solid #ccc; padding: 4px;"),
                tags$td("Content & Context alignment - the phrase/terms used in the unit of analysis explicitly matches the intended content as well as the context of the EBM framework objectives", style = "border: 1px solid #ccc; padding: 4px;"))
      )
    ),
    tags$h4("Quantitative Scoring"),
    tags$table(
      style = "width: 100%; border-collapse: collapse; border: 1px solid #ccc;",
      tags$thead(
        tags$tr(
          tags$th("Value", style = "border: 1px solid #ccc; padding: 4px;"),
          tags$th("Description", style = "border: 1px solid #ccc; padding: 4px;")
        )
      ),
      tags$table(
        style = "width: 100%; border-collapse: collapse; border: 1px solid #ccc;",
        tags$thead(
          tags$tr(
            tags$th("Value", style = "border: 1px solid #ccc; padding: 4px;"),
            tags$th("Description", style = "border: 1px solid #ccc; padding: 4px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("X", style = "border: 1px solid #ccc; padding: 4px;"),
            tags$td("Not relevant / applicable – Objective is not relevant to policy/management approach or plan.", style = "border: 1px solid #ccc; padding: 4px;")
          ),
          tags$tr(
            tags$td("0", style = "border: 1px solid #ccc; padding: 4px;"),
            tags$td("ZERO – No data or information to address this objective of the EBM framework", style = "border: 1px solid #ccc; padding: 4px;")
          ),
          tags$tr(
            tags$td("1", style = "border: 1px solid #ccc; padding: 4px;"),
            tags$td("Weak – Some data or information to address this objective, but it is inconsistent temporally and/or spatially", style = "border: 1px solid #ccc; padding: 4px;")
          ),
          tags$tr(
            tags$td("2", style = "border: 1px solid #ccc; padding: 4px;"),
            tags$td("Moderate – Some data or information with reasonable temporal and spatial coverage to address this objective", style = "border: 1px solid #ccc; padding: 4px;")
          ),
          tags$tr(
            tags$td("3", style = "border: 1px solid #ccc; padding: 4px;"),
            tags$td("Strong – Good information and long term data with temporal and/or spatial coverage to address this objective", style = "border: 1px solid #ccc; padding: 4px;")
          )
        )
      )
    )


  )
}




use_explanations[[3]] <- function() {
  tagList(

    tags$p(
      "In the complex world of decision making, clear objectives are required to evaluate different potential management options or scenarios.
      The objectives of the Governance, Ecological, Economic and Social and Cultural Pillars of the EBM Framework provide a holistic basis for comparison across scenarios.
      For example, the scenarios could be different options for opening a new fishery, decisions on whether to approve introductions and transfers of fish, different placements of a protected area, different locations of wind energy, or aquaculture sites or stocking options for Atlantic salmon.
      The simplest scenario comparison would be to decide whether or not to move ahead with a project. This decision would be evaluated against the EBM Framework objectives.
      A more complex decision would involve two or more scenarios and two or more activities (eg. Offshore wind and fisheries)."
    ),

    tags$hr(),
    tags$p(
      "For each row of the table below, paste the line from your policy or management approaches that align
       with the associated objective in the Policy Alignment columns. Assess alignment using one of the two
      tables below and enter the score in the Score column. Lastly, enter your rationale in the Rationale column"
    ),

    # Scrollable wide table
    div(
      style = "overflow-x: auto; width: 100%;",
      tags$table(
        style = "min-width: 1500px; border-collapse: collapse; border: 1px solid #ccc;",
        tags$thead(
          tags$tr(
            tags$th("Pillar", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Main_Objective", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Level_1", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Level_2", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Level_3", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Level_4", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Objective_Label", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Score_1", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: center;"),
            tags$th("Score_2", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: center;"),
            tags$th("Policy_1_alignment", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Policy_2_alignment", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Rationale", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("Ecological", style = "border: 1px solid #ccc; padding: 6px; vertical-align: top;"),
            tags$td("Productivity", style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("1. Pressures on ecosystem productivity are managed", style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("a. Fishing pressures on ecosystem productivity are managed", style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("i. Fishing mortality is limited to levels necessary to promote sustainability", style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("", style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("Productivity / 1. Pressures on ecosystem productivity are managed / a. Fishing pressures on ecosystem productivity are managed / i. Fishing mortality is limited to levels necessary to promote sustainability /", style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("NA", style = "border: 1px solid #ccc; padding: 6px; text-align: center;"),
            tags$td(NA, style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("", style = "border: 1px solid #ccc; padding: 6px;")
          )
        )
      )
    )
    ,

    tags$h4("Qualitative Scoring"),

    tags$table(
      style = "width: 100%; border-collapse: collapse; border: 1px solid #ccc;",
      tags$thead(
        tags$tr(
          tags$th("Value", style = "border: 1px solid #ccc; padding: 4px;"),
          tags$th("Description", style = "border: 1px solid #ccc; padding: 4px;")
        )
      ),
      tags$tbody(
        tags$tr(tags$td("X", style = "border: 1px solid #ccc; padding: 4px;"),
                tags$td("Not relevant / applicable - dependent on context, case study, purview, department, jurisdiction, etc.", style = "border: 1px solid #ccc; padding: 4px;")),
        tags$tr(tags$td("0", style = "border: 1px solid #ccc; padding: 4px;"),
                tags$td("No mention - the objective of the EBM framework is not present within this unit of analysis", style = "border: 1px solid #ccc; padding: 4px;")),
        tags$tr(tags$td("1", style = "border: 1px solid #ccc; padding: 4px;"),
                tags$td("Implicit Content mention - the phrase/terms used in the unit of analysis generally or vaguely aligns with, or alludes to, the intended meaning in the EBM Framework (e.g., high-level concepts)", style = "border: 1px solid #ccc; padding: 4px;")),
        tags$tr(tags$td("2", style = "border: 1px solid #ccc; padding: 4px;"),
                tags$td("Explicit Content mention - the phrase/terms used in the unit of analysis partially aligns with, or alludes to, the meaning intended in the EBM framework, but may appear slightly different.", style = "border: 1px solid #ccc; padding: 4px;")),
        tags$tr(tags$td("3", style = "border: 1px solid #ccc; padding: 4px;"),
                tags$td("Content & Context alignment - the phrase/terms used in the unit of analysis explicitly matches the intended content as well as the context of the EBM framework objectives", style = "border: 1px solid #ccc; padding: 4px;"))
      )
    ),
    tags$h4("Quantitative Scoring"),
    tags$table(
      style = "width: 100%; border-collapse: collapse; border: 1px solid #ccc;",
      tags$thead(
        tags$tr(
          tags$th("Value", style = "border: 1px solid #ccc; padding: 4px;"),
          tags$th("Description", style = "border: 1px solid #ccc; padding: 4px;")
        )
      ),
      tags$table(
        style = "width: 100%; border-collapse: collapse; border: 1px solid #ccc;",
        tags$thead(
          tags$tr(
            tags$th("Value", style = "border: 1px solid #ccc; padding: 4px;"),
            tags$th("Description", style = "border: 1px solid #ccc; padding: 4px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("X", style = "border: 1px solid #ccc; padding: 4px;"),
            tags$td("Not relevant / applicable – Objective is not relevant to policy/management approach or plan.", style = "border: 1px solid #ccc; padding: 4px;")
          ),
          tags$tr(
            tags$td("0", style = "border: 1px solid #ccc; padding: 4px;"),
            tags$td("ZERO – No data or information to address this objective of the EBM framework", style = "border: 1px solid #ccc; padding: 4px;")
          ),
          tags$tr(
            tags$td("1", style = "border: 1px solid #ccc; padding: 4px;"),
            tags$td("Weak – Some data or information to address this objective, but it is inconsistent temporally and/or spatially", style = "border: 1px solid #ccc; padding: 4px;")
          ),
          tags$tr(
            tags$td("2", style = "border: 1px solid #ccc; padding: 4px;"),
            tags$td("Moderate – Some data or information with reasonable temporal and spatial coverage to address this objective", style = "border: 1px solid #ccc; padding: 4px;")
          ),
          tags$tr(
            tags$td("3", style = "border: 1px solid #ccc; padding: 4px;"),
            tags$td("Strong – Good information and long term data with temporal and/or spatial coverage to address this objective", style = "border: 1px solid #ccc; padding: 4px;")
          )
        )
      )
    )

  )
}



use_explanations[[4]] <- function() {
  tagList(

    tags$p(
      "The EBM Framework can be used as the basis for assessing management success against the relevant objectives, thus forming a management report card.
      Ideally management plans will have been developed or evaluated and updated using the EBM Framework (see USE 1 above).
      In this case, indicators would be used to assess whether the objectives have been met, identify objectives that may need more focus in the future and possibly, identify the need to adjust the management plan to better meet all objectives.
      If the management plan has not been developed or reviewed with respect to the EBM Framework, the first step for the report card would be as described above, i.e., to explore how the management approaches align with EBM objectives.
      The next step would be to assess whether these objectives are being met, then to develop recommendations for addition of further EBM objectives (e.g., see Paul and Stephenson (2020) for an evaluation of IFMPs).
      Question: are we achieving our management objectives?"
    ),

    tags$hr(),
    tags$p(
      "For each row of the table below, paste the associated indicator with each objective. For each indicator, write the associated target. Score the indicator based on the scoring table below and write your rationale in the Rationale column"
    ),

    # Scrollable wide table
    div(
      style = "overflow-x: auto; width: 100%;",
      tags$table(
        style = "min-width: 1500px; border-collapse: collapse; border: 1px solid #ccc;",
        tags$thead(
          tags$tr(
            tags$th("Pillar", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Main_Objective", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Level_1", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Level_2", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Level_3", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Level_4", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Objective_Label", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Indicator", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: center;"),
            tags$th("Target", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: center;"),
            tags$th("Score", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Rationale", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("Ecological", style = "border: 1px solid #ccc; padding: 6px; vertical-align: top;"),
            tags$td("Productivity", style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("1. Pressures on ecosystem productivity are managed", style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("a. Fishing pressures on ecosystem productivity are managed", style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("i. Fishing mortality is limited to levels necessary to promote sustainability", style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("", style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("Productivity / 1. Pressures on ecosystem productivity are managed / a. Fishing pressures on ecosystem productivity are managed / i. Fishing mortality is limited to levels necessary to promote sustainability /", style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("NA", style = "border: 1px solid #ccc; padding: 6px; text-align: center;"),
            tags$td(NA, style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("", style = "border: 1px solid #ccc; padding: 6px;")
          )
        )
      )
    )
    ,


    tags$table(
      style = "width: 100%; border-collapse: collapse; border: 1px solid #ccc;",
      tags$thead(
        tags$tr(
          tags$th("Value", style = "border: 1px solid #ccc; padding: 4px;"),
          tags$th("Description", style = "border: 1px solid #ccc; padding: 4px;")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td("0", style = "border: 1px solid #ccc; padding: 4px;"),
          tags$td("Not met target", style = "border: 1px solid #ccc; padding: 4px;")
        ),
        tags$tr(
          tags$td("1", style = "border: 1px solid #ccc; padding: 4px;"),
          tags$td("Met target", style = "border: 1px solid #ccc; padding: 4px;")
        ),
        tags$tr(
          tags$td("2", style = "border: 1px solid #ccc; padding: 4px;"),
          tags$td("Exceeded target", style = "border: 1px solid #ccc; padding: 4px;")
        )
      )
    )

  )
}

use_explanations[[5]] <- function() {
  tagList(

    tags$p(
      "Assessing the cumulative effects of activities, the risks associated with them and the potential trade-offs among them is the basis for EBM, Integrated Management, Marine Spatial Planning and the Blue Economy, i.e., the holistic management of aquatic resource use.
      This use requires common objectives across the activities being assessed. These may need to be established, since management plans will already exist for each activity and they will not share all the same objectives."
    ),
    tags$p("Question: what are cumulative effects of activities 1, 2, 3 etc., in area X?"),

    tags$hr(),
    tags$p(
      "For each row of the table, paste the associated indicators (or activites) with each objective. For each indicator, write the associated target. Score the indicator based on the scoring table below and write your rationale in the Rationale column"
    ),

    # Scrollable wide table
    div(
      style = "overflow-x: auto; width: 100%;",
      tags$table(
        style = "min-width: 1500px; border-collapse: collapse; border: 1px solid #ccc;",
        tags$thead(
          tags$tr(
            tags$th("Pillar", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Main_Objective", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Level_1", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Level_2", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Level_3", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Level_4", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Objective_Label", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Indicator_1", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: center;"),
            tags$th("Target_1", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: center;"),
            tags$th("Impact_1", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Indicator_2", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: center;"),
            tags$th("Target_2", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: center;"),
            tags$th("Impact_2", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;"),
            tags$th("Rationale", style = "border: 1px solid #ccc; padding: 6px; white-space: nowrap; text-align: left;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("Ecological", style = "border: 1px solid #ccc; padding: 6px; vertical-align: top;"),
            tags$td("Productivity", style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("1. Pressures on ecosystem productivity are managed", style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("a. Fishing pressures on ecosystem productivity are managed", style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("i. Fishing mortality is limited to levels necessary to promote sustainability", style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("", style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("Productivity / 1. Pressures on ecosystem productivity are managed / a. Fishing pressures on ecosystem productivity are managed / i. Fishing mortality is limited to levels necessary to promote sustainability /", style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("", style = "border: 1px solid #ccc; padding: 6px; text-align: center;"),
            tags$td("", style = "border: 1px solid #ccc; padding: 6px;"),
            tags$td("", style = "border: 1px solid #ccc; padding: 6px;")
          )
        )
      )
    )
    ,

    tags$table(
      style = "width: 100%; border-collapse: collapse; border: 1px solid #ccc;",
      tags$thead(
        tags$tr(
          tags$th("Value", style = "border: 1px solid #ccc; padding: 4px;"),
          tags$th("Description", style = "border: 1px solid #ccc; padding: 4px;")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td("0", style = "border: 1px solid #ccc; padding: 4px;"),
          tags$td("Not met target", style = "border: 1px solid #ccc; padding: 4px;")
        ),
        tags$tr(
          tags$td("1", style = "border: 1px solid #ccc; padding: 4px;"),
          tags$td("Met target", style = "border: 1px solid #ccc; padding: 4px;")
        ),
        tags$tr(
          tags$td("2", style = "border: 1px solid #ccc; padding: 4px;"),
          tags$td("Exceeded target", style = "border: 1px solid #ccc; padding: 4px;")
        )
      )
    )

  )
}

names(use_explanations) <- c("Checklist of objectives", "Evaluating Policies and Management Appropaches/ Plans",
                             "Scenario Comparison", "Management Report Card", "Cumulative Effects and Tradeoffs")



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

# ---- Global scoring helpers for Step 2 ----

# USE 2, Q1: qualitative alignment scale (Table 2)
policy_alignment_levels <- c(
  "X Not relevant / not applicable",
  "0 No mention",
  "1 Implicit content mention",
  "2 Explicit content mention",
  "3 Content & context alignment"
)

# USE 2, Q2: data/information adequacy scale (Table 4)
info_adequacy_levels <- c(
  "X Not relevant / not applicable",
  "0 ZERO – No data or information",
  "1 Weak – some, inconsistent in time/space",
  "2 Moderate – some, reasonable time/space coverage",
  "3 Strong – good long‑term info/time/space coverage"
)

# USE 4 & 5: management performance / target‑based 0–2 scale
perf_levels <- c(
  "0 Not met",
  "1 Met",
  "2 Exceeded"
)

# USE 5 impact Likert
cumu_impact_levels <- c("X Not applicable", "0 No impact", "1 Low", "2 Moderate", "3 High")


# Template constructors used by Step 2
make_objective_table <- function(so) {
  # Ensure stable base columns
  base <- so |>
    dplyr::select(Pillar, Main_Objective, Level_1, Level_2, Level_3, Level_4)

  # Build label from non‑NA levels for each row
  label_mat <- base |>
    dplyr::select(Main_Objective, Level_1, Level_2, Level_3, Level_4) |>
    as.data.frame()

  base$Objective_Label <- apply(
    label_mat,
    1,
    function(r) paste(stats::na.omit(r), collapse = " / ")
  )

  base
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "EBM App"),
  dashboardSidebar(
    width = 350,
    tags$head(   # 🔴 add here
      tags$style(HTML("
      li a[data-value='EBM_in_action'] {
        border: 4px solid green !important;
        border-radius: 4px;
      }
    "))
    ),
    sidebarMenu(
      id = "sidebar",
      menuItem("Exploring the EBM Framework", tabName = "home", icon = icon("globe")),
      menuItem("Exploring the Uses of the EBM Framework", tabName = "useEBM", icon = icon("leanpub")),
      menuItem("Use the EBM Framework", tabName= 'EBM_in_action', icon=icon("cubes")),
      menuItem("References", tabName='references', icon=icon('book')),
      conditionalPanel(
        condition = "input.sidebar == 'EBM_in_action' || input.sidebar == 'select_objectives' || input.sidebar == 'assessment'",
        #condition = "input.sidebar == 'EBM_in_action'",
      hr(),
      #h4("EBM Framework Workflow", style = "padding-left: 15px; color: #fff;"),
      menuItem("STEP 1: Select Objectives",
              tabName = "select_objectives",
              icon = icon("square-check"),
              badgeLabel = "Start Here",
              badgeColor = "green")),
      conditionalPanel(
        condition = "input.sidebar == 'select_objectives' || input.sidebar == 'assessment'",
      menuItem("STEP 2: Assessment & Comparison",
              tabName = "assessment",
              icon = icon("clipboard-list"),
              badgeLabel = "Optional",
              badgeColor = "blue")
    )

    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$script(HTML("
      document.addEventListener('keydown', function(e) {
        if (e.key === 's' &&
            !['INPUT','TEXTAREA'].includes(e.target.tagName)) {
          Shiny.setInputValue('key_s', Date.now());
        }
      });
    ")),
    tags$head(
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jQuery-rwdImageMaps/1.6/jquery.rwdImageMaps.min.js"),
      tags$script(src = "dataTables.cellEdit.js"),
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
                    p(
                      style = "background-color: #fff3cd; padding: 4px 6px;",
                      strong(
                        "Click on any section of the framework below to view detailed information or explore the menu to the left for additional tools and resources."
                      )
                    )
                )),
              fluidRow(
                box(width = 12, title = "Interactive EBM Framework", status = "info", solidHeader = TRUE,
                    uiOutput("home_map_content"))
              )
      ),

      # Using EBM
      tabItem(tabName='useEBM',

              box(width = 12, title = "Understanding Uses of EBM Framework", status = "primary", solidHeader = TRUE,
                  p("Five broad areas of use have been envisioned for the EBM Framework (below). These are premised on DFO's objective/outcomes based approach to management, and provide the candidate objectives to be used in different decision-making contexts. "),
                  p(
                    style = "background-color: #fff3cd; padding: 4px 6px;",
                    strong(
                      "Click on each of the use cases to learn more about how to apply the EBM Framework"
                    )
                  ),

                  tags$img(
                    src = "EBMuses.png",
                    width = "1000px",
                    id = "my_image"
                  ),

                  tags$script(HTML("
    const img = document.getElementById('my_image');

    // define 5 rectangular zones using top-left and bottom-right
    const zones600 = [
      {id:1, x1:39,  y1:22,  x2:292, y2:143},
      {id:2, x1:42,  y1:160, x2:276, y2:314},
      {id:3, x1:326, y1:20,  x2:540, y2:124},
      {id:4, x1:327, y1:139, x2:541, y2:195},
      {id:5, x1:327, y1:209, x2:552, y2:325}
    ];
    const scale = 1000 / 600;  // new_width / old_width

    const zones = zones600.map(z => ({
    id: z.id,
    x1: z.x1 * scale,
    y1: z.y1 * scale,
    x2: z.x2 * scale,
    y2: z.y2 * scale
    }));

    img.addEventListener('click', function(e) {
      const rect = img.getBoundingClientRect();
      const xClick = e.clientX - rect.left;
      const yClick = e.clientY - rect.top;

      for (const z of zones) {
        if (xClick >= z.x1 && xClick <= z.x2 && yClick >= z.y1 && yClick <= z.y2) {
                  Shiny.setInputValue('zone_click', z.id, {priority:'event'});
          break;
        }
      }
    });
  ")))
      ),

      # ABOVE:
      # Shiny.setInputValue(
      #   'zone_click',
      #   { id: z.id, x: xClick, y: yClick },
      #   { priority: 'event' }
      # );

      tabItem(tabName = "EBM_in_action",
              fluidRow(
                box(
                  width = 12, title = "Using this app", status = "primary", solidHeader = TRUE,
                  p(strong("Check list of Objectives")),
                  p("Determine what objectives are relevant to your management plan/decision/project development etc."),
                  p("Responses from Use 1 are applied to all other uses."),
                  tagList(
                    tags$style(HTML("
    .btn-step1 {
      background-color: #fff3cd !important;
      color: #856404 !important;
      border: 1px solid #ffeeba !important;
    }
  ")),

                    actionButton(
                      "goto_step1",
                      "Go to Step 1",
                      class = "btn btn-step1"
                    )
                  ),
                  #actionButton("goto_step1", "Go to Step 1", class = "btn-primary"),
                  br(), br(),

                  # 🔴 Cropped image for Use 1
                  {
                    scale <- 1.2 #JAIM

                    # Scaled coordinates for Use 1
                    x1 <- 39 * (1000 / 600)
                    y1 <- 22 * (1000 / 600)
                    x2 <- 292 * (1000 / 600)
                    y2 <- 143 * (1000 / 600)


                    cx <- (x1 + x2) / 2
                    cy <- (y1 + y2) / 2
                    hw <- (x2 - x1) / 2 * scale
                    hh <- (y2 - y1) / 2 * scale

                    x1 <- cx - hw  # 🔴 scaled
                    x2 <- cx + hw  # 🔴 scaled
                    y1 <- cy - hh  # 🔴 scaled
                    y2 <- cy + hh  # 🔴 scaled

                    crop_width <- x2 - x1
                    crop_height <- y2 - y1

                    div(
                      style = sprintf("
            width: %fpx;
            height: %fpx;
            overflow: hidden;
            position: relative;
            border: 2px solid #3c8dbc;  /* optional border */
            margin-top: 15px;
          ", crop_width, crop_height),

                      tags$img(
                        src = "EBMuses.png",
                        style = sprintf("
              position: absolute;
              top: -%fpx;
              left: -%fpx;
              width: 1000px;
            ", y1, x1)
                      )
                    )
                  }
                )
              )
      ),

      tabItem(
        tabName = "references",
        fluidRow(
          box(
            width = 12,
            title = "References",
            status = "primary",
            solidHeader = TRUE,

            p("See relevant Ecosystem Based Management (EBM) references below:"),

            tags$ul(
              tags$li(
                "Eger, S. L., & Bundy, A. (2024). ",
                tags$em(
                  "Application of the Maritimes Ecosystem Based Management (EBM) Framework to an Oceans Act Marine Protected Area Case Study: St. Anns Bank Marine Protected Area."
                ),
                " Fisheries and Oceans Canada, Bedford Institute of Oceanography."
              ),

              tags$li(
                "Silver, K. (2024). ",
                tags$em(
                  "Charting the Progress on Ecosystem-Based Management of Fisheries in NAFO Division 4X: Integrated Fisheries Management Plans through the Lens of the Maritimes Region EBM Framework."
                )
              ),

              tags$li(
                "Tam, J. C., Parlee, C. E., Campbell-Miller, J., Bellanger, M., Bentley, J., Pourfaraj, V., et al. (2024). ",
                tags$em(
                  "Expanding the scope and roles of social sciences and humanities to support integrated ecosystem assessments and ecosystem-based management."
                ),
                " ICES Journal of Marine Science, 81(1), 22–42."
              ),

              tags$li(
                "Stephenson, R. L., Wiber, M., Paul, S., Angel, E., Benson, A., Charles, A., et al. (2019). ",
                tags$em(
                  "Integrating diverse objectives for sustainable fisheries in Canada."
                ),
                " Canadian Journal of Fisheries and Aquatic Sciences, 76(3), 480–496."
              )
            )
          )
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
                           actionButton("proceed_to_step2", "Go to Step 2: Assessment", class = "btn-success btn-lg",  style = "background-color:#fff3cd; color:#000; border-color:#ffe69c;"))
                )
              )
      ),

      # Step 2: assessment
      tabItem(tabName = "assessment",
              fluidRow(
                box(width = 12, title = "Choose Assessment Type (EBM USE 2–5)", status = "info", solidHeader = TRUE,
                    p(
                      tags$span(
                        strong("Click on the assessment type that meets your needs"),
                        style = "background-color: #fff3cd; padding: 4px 6px; border-radius: 4px;"
                      )
                    ),                    fluidRow(
                      column(
                        3,
                        div(
                          class = "assessment-type-box", id = "assessment_type_policy",
                          onclick = "Shiny.setInputValue('assessment_type', 'policy', {priority: 'event'});",
                          h4(icon("file-text"), "Evaluating policies and management approaches (USE 2)"),
                          tags$ul(
                            tags$li("Evaluate alignment of policy / management approach with EBM objectives."),
                            tags$li("Evaluate whether advice provision has sufficient data/information."),
                            p(" ")
                          ),
                          {

                            # Scaled coordinates for Use 1
                            x1 <- 42 * (600 / 600)   # = 42
                            y1 <- 160 * (600 / 600)  # = 160
                            x2 <- 285 * (600 / 600)  # = 276
                            y2 <- 347 * (600 / 600)  # = 314

                            crop_width <- x2 - x1
                            crop_height <- y2 - y1

                            div(
                              style = sprintf("
            width: %fpx;
            height: %fpx;
            overflow: hidden;
            position: relative;
            border: 2px solid #3c8dbc;  /* optional border */
            margin-top: 15px;
          ", crop_width, crop_height),

                              tags$img(
                                src = "EBMuses.png",
                                style = sprintf("
              position: absolute;
              top: -%fpx;
              left: -%fpx;
              width: 600px;
            ", y1, x1)
                              )
                            )
                          },
                          p(" "),
                          # p(tags$b("Scoring options:")),
                          # tags$ul(
                          #   tags$li("Table 2 style – qualitative content/context alignment (X / 0–3)."),
                          #   tags$li("Table 4 style – data/information adequacy (X / 0–3).")
                          # )
                        )
                      ),
                      column(
                        3,
                        div(
                          class = "assessment-type-box", id = "assessment_type_scenarios",
                          onclick = "Shiny.setInputValue('assessment_type', 'scenarios', {priority: 'event'});",
                          h4(icon("copy"), "Scenario Comparison (USE 3)"),
                          tags$ul(
                            tags$li("Compare scenarios or options against EBM objectives.")
                          ),
                          {
                            # Scaled coordinates for Use 1
                            x1 <- 326 * (600 / 600)   # = 326
                            y1 <- 20  * (600 / 600)   # = 20
                            x2 <- 550 * (600 / 600)   # = 540
                            y2 <- 140 * (600 / 600)   # = 124


                            crop_width <- x2 - x1
                            crop_height <- y2 - y1

                            div(
                              style = sprintf("
            width: %fpx;
            height: %fpx;
            overflow: hidden;
            position: relative;
            border: 2px solid #3c8dbc;  /* optional border */
            margin-top: 15px;
          ", crop_width, crop_height),

                              tags$img(
                                src = "EBMuses.png",
                                style = sprintf("
              position: absolute;
              top: -%fpx;
              left: -%fpx;
              width: 600px;
            ", y1, x1)
                              )
                            )
                          },
                          p(" "),


                          # p(tags$b("Scoring options:")),
                          # tags$ul(
                          #   tags$li("Table 2 style – qualitative content/context alignment (X / 0–3)."),
                          #   tags$li("Table 4 style – data/information adequacy (X / 0–3).")
                          # )
                        )
                      ),
                      column(
                        3,
                        div(
                          class = "assessment-type-box", id = "assessment_type_performance",
                          onclick = "Shiny.setInputValue('assessment_type', 'performance', {priority: 'event'});",
                          h4(icon("chart-line"), "Management Report Card (USE 4)"),
                          tags$ul(
                            tags$li("Are we achieving our management objectives?"),
                            p(" "),

                          ),
                          {

                            # Scaled coordinates for Use 1
                            x1 <- 327 * (600 / 600)   # = 327
                            y1 <- 135 * (600 / 600)   # = 139
                            x2 <- 550 * (600 / 600)   # = 541
                            y2 <- 205 * (600 / 600)   # = 195



                            crop_width <- x2 - x1
                            crop_height <- y2 - y1

                            div(
                              style = sprintf("
            width: %fpx;
            height: %fpx;
            overflow: hidden;
            position: relative;
            border: 2px solid #3c8dbc;  /* optional border */
            margin-top: 15px;
          ", crop_width, crop_height),

                              tags$img(
                                src = "EBMuses.png",
                                style = sprintf("
              position: absolute;
              top: -%fpx;
              left: -%fpx;
              width: 600px;
            ", y1, x1)
                              )
                            )
                          },
                          p(" "),
                          #p(strong("Score using operational indicators per objective."))

                        )
                      ),
                      column(
                        3,
                        div(
                          class = "assessment-type-box", id = "assessment_type_cumulative",
                          onclick = "Shiny.setInputValue('assessment_type', 'cumulative', {priority: 'event'});",
                          h4(icon("layer-group"), "Cumulative Effects / Trade‑offs (USE 5)"),
                          tags$ul(
                            tags$li("Assess cumulative effects / risks / trade‑offs across activities.")                          ),

                            {
                              # Scaled coordinates for Use 1
                              x1 <- 327 * (600 / 600)   # = 327
                              y1 <- 209 * (600 / 600)   # = 209
                              x2 <- 560 * (600 / 600)   # = 552
                              y2 <- 330 * (600 / 600)   # = 325

                              crop_width <- x2 - x1
                              crop_height <- y2 - y1

                              div(
                                style = sprintf("
            width: %fpx;
            height: %fpx;
            overflow: hidden;
            position: relative;
            border: 2px solid #3c8dbc;  /* optional border */
            margin-top: 15px;
          ", crop_width, crop_height),

                                tags$img(
                                  src = "EBMuses.png",
                                  style = sprintf("
              position: absolute;
              top: -%fpx;
              left: -%fpx;
              width: 600px;
            ", y1, x1)
                                )
                              )
                            },
                          p(" "),

                          #p(strong("Impacts scored per activity (e.g., 0–3; NA / X where appropriate)."))
                        )
                      )
                    ),
                    #actionButton("goto_step1", "Back to Step 1", class = "btn-primary"),

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


  # observeEvent(input$zone_click, {
  #   cat(
  #     'x =', input$zone_click$x,
  #     'y =', input$zone_click$y, '\n'
  #   )
  # })

  observeEvent(input$zone_click, {
    showModal(
      modalDialog(
        title = paste("Use", input$zone_click, ":",
                      names(use_explanations)[input$zone_click]),
        use_explanations[[input$zone_click]](),
        size = "xl",
        easyClose = TRUE,
        footer = NULL
      )
    )
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
           scenarios = policy_ui(),   # USE 3 (same UI)
           performance = performance_ui(),
           cumulative = cumulative_ui())
  })

  # ========== POLICY/SCENARIO ASSESSMENT ==========
  policy_tbl <- reactiveVal(NULL)

  policy_ui <- function() {
    so <- selected_objectives(); req(so)

    tagList(
      fluidRow(
        box(
          width = 12,
          title = ifelse(input$assessment_type == 'policy', "Evaluating policies and management approaches (USE 2)", "Scenario Comparison (USE 3)"),
          status = "success",
          solidHeader = TRUE,
          fluidRow(
            column(
              4,
              radioButtons(
                "policy_method",
                "Step A. Select Scoring method:",
                choices = c(
                  "Policy language alignment (X / 0–3)" = "qual",
                  "Data/information adequacy (X / 0–3)" = "likert"
                ),
                selected = 'qual'
              )
            ),
            conditionalPanel(
              condition = "input.assessment_type == 'policy'",
            column(
              4,
                numericInput(
                  "num_scenarios",
                  "Step B. Select Number of policies / advice sources / scenarios:",
                  value = 1, min = 1, max = 1, step = 1
                )
            )

            ),
            conditionalPanel(
              condition = "input.assessment_type == 'scenarios'",
              column(
                4,
                numericInput(
                  "num_scenarios",
                  "Step B. Select Number of policies / advice sources / scenarios:",
                  value = 1, min = 1, max = 10, step = 1
                )
              )

            ),
            column(
              4,
              actionButton(
                "policy_make_template",
                "Step C: Create / Reset Template to begin",
                class = "btn-primary",
                style = "background-color:#fff3cd; color:#000; border-color:#ffe69c;"
              )
            )
          ),
          conditionalPanel(
            condition = "input.policy_make_template > 0",
          p(
            style = "background-color: #fff3cd; padding: 4px 6px;",
            strong(
              "You can click 's' on your keyboard at anytime to for a reminder of how to (s)core"
            )
          )),
          DTOutput("policy_editor"),
          br(),
          uiOutput("policy_template_ui")
        )
      )
    )
  }

  output$policy_template_ui <- renderUI({
    req(input$policy_make_template)  # only show if TRUE
    fluidRow(
      column(
        3,
        downloadButton(
          "policy_download_excel",
          "Download Template / Results (Excel)",
          class = "btn-success btn-block"
        )
      ),
      column(
        3,
        downloadButton(
          "policy_download_csv",
          "Download (CSV)",
          class = "btn-info btn-block"
        )
      ),
      # column(
      #   6,
      #   fileInput(
      #     "policy_upload",
      #     "Upload completed (CSV / XLSX)",
      #     accept = c(".csv", ".xlsx")
      #   )
      # )
    )
  })


  observeEvent(list(input$policy_make_template, input$key_s, input$perf_make_template, input$cumu_make_template), {
    req(input$policy_make_template > 0 || input$perf_make_template > 0 || input$cumu_make_template > 0)

    #browser()

    # Conditional scoring table
    scoring_ui <-
      if (
        !(input$assessment_type %in% c("policy", "scenarios"))
      ) {
      # Performance scoring table ONLY
      tagList(
        tags$h4("Performance Scoring"),
        tags$table(
          style = "width:100%; border-collapse: collapse; border:1px solid #ccc;",
          tags$thead(
            tags$tr(
              tags$th("Value", style = "border:1px solid #ccc; padding:4px;"),
              tags$th("Description", style = "border:1px solid #ccc; padding:4px;")
            )
          ),
          tags$tbody(
            tags$tr(tags$td("0", style = "border:1px solid #ccc; padding:4px;"), tags$td("Not met target", style = "border:1px solid #ccc; padding:4px;")),
            tags$tr(tags$td("1", style = "border:1px solid #ccc; padding:4px;"), tags$td("Met target", style = "border:1px solid #ccc; padding:4px;")),
            tags$tr(tags$td("2", style = "border:1px solid #ccc; padding:4px;"), tags$td("Exceeded target", style = "border:1px solid #ccc; padding:4px;"))
          )
        )
      )
    } else {
      # Existing qualitative + quantitative tables
      tagList(
        tags$h4("Qualitative Scoring"),
        # paste your qualitative table code here
        tags$table(
          style = "width: 100%; border-collapse: collapse; border: 1px solid #ccc;",
          tags$thead(
            tags$tr(
              tags$th("Value", style = "border: 1px solid #ccc; padding: 4px;"),
              tags$th("Description", style = "border: 1px solid #ccc; padding: 4px;")
            )
          ),
          tags$tbody(
            tags$tr(tags$td("X", style = "border: 1px solid #ccc; padding: 4px;"), tags$td("Not relevant/ applicable - dependent on context, case study, purview, department, jurisdiction, etc.", style = "border: 1px solid #ccc; padding: 4px;")),
            tags$tr(tags$td("0", style = "border: 1px solid #ccc; padding: 4px;"), tags$td("No mention - the objective of the EBM framework is not present within this unit of analysis", style = "border: 1px solid #ccc; padding: 4px;")),
            tags$tr(tags$td("1", style = "border: 1px solid #ccc; padding: 4px;"), tags$td("Implicit Content mention - the phrase/terms used in the unit of analysis generally or vaguely aligns with, or alludes to, the intended meaning in the EBM Framework (e.g., high-level concepts)", style = "border: 1px solid #ccc; padding: 4px;")),
            tags$tr(tags$td("2", style = "border: 1px solid #ccc; padding: 4px;"), tags$td("Explicit Content mention - the phrase/terms used in the unit of analysis partially aligns with, or alludes to, the meaning intended in the EBM framework, but may appear slightly different.", style = "border: 1px solid #ccc; padding: 4px;")),
            tags$tr(tags$td("3", style = "border: 1px solid #ccc; padding: 4px;"), tags$td("Content & Context alignment - the phrase/terms used in the unit of analysis explicitly matches the intended content as well as the context of the EBM framework objectives", style = "border: 1px solid #ccc; padding: 4px;"))
          )
        ),
        tags$br(),
        tags$h4("Quantitative/Data Scoring"),
        # paste your quantitative table code here
        tags$table(
          style = "width: 100%; border-collapse: collapse; border: 1px solid #ccc;",
          tags$thead(
            tags$tr(
              tags$th("Value", style = "border: 1px solid #ccc; padding: 4px;"),
              tags$th("Description", style = "border: 1px solid #ccc; padding: 4px;")
            )
          ),
          tags$tbody(
            tags$tr(tags$td("X", style = "border: 1px solid #ccc; padding: 4px;"), tags$td("Not relevant/ applicable – Objective is not relevant to policy/management approach or plan.", style = "border: 1px solid #ccc; padding: 4px;")),
            tags$tr(tags$td("0", style = "border: 1px solid #ccc; padding: 4px;"), tags$td("ZERO – No data or information to address this objective of the EBM framework", style = "border: 1px solid #ccc; padding: 4px;")),
            tags$tr(tags$td("1", style = "border: 1px solid #ccc; padding: 4px;"), tags$td("Weak - Some data or information to address this objective, but it is inconsistent temporally and/or spatially", style = "border: 1px solid #ccc; padding: 4px;")),
            tags$tr(tags$td("2", style = "border: 1px solid #ccc; padding: 4px;"), tags$td("Moderate - Some data or information with reasonable temporal and spatial coverage to address this objective", style = "border: 1px solid #ccc; padding: 4px;")),
            tags$tr(tags$td("3", style = "border: 1px solid #ccc; padding: 4px;"), tags$td("Strong - Good information and long term data with temporal and/or spatial coverage to address this objective", style = "border: 1px solid #ccc; padding: 4px;"))
          )
        )
      )
    }

    # Show modal
    showModal(modalDialog(
      title = "Scoring Criteria",
      tagList(
        tags$br(), tags$br(),
        scoring_ui
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))

  })




  observeEvent(input$policy_make_template, {
    # Creates policy_tbl, which is a filtered Pillar from step 1.
    so <- selected_objectives(); req(so) # Filtering the relevant Pillars
    base <- make_objective_table(so)

    # Pink column – statement / evidence text (policy / advice / scenario description)
    #base[["Statement_or_Evidence"]] <- ""

    n <- input$num_scenarios %||% 1
    method <- input$policy_method %||% "qual"

    for (i in seq_len(n)) {
      col_name <- paste0("Score_", i)
      base[[col_name]] <- NA
    }

    if (method == "qual") {
      # Table 2 style – policy language alignment with objectives
      for (i in seq_len(n)) {
        col_name <- paste0("Policy_", i, "_alignment")
        base[[col_name]] <- factor(
          NA_character_,
          levels = policy_alignment_levels
        )
      }

      # Add Scoring
      #base[["Score"]] <- ""
    } else {
      # Table 4 style – data/information adequacy for advice provision
      for (i in seq_len(n)) {
        col_name <- paste0("Source_", i, "_data_adequacy")
        base[[col_name]] <- factor(
          NA_character_,
          levels = info_adequacy_levels
        )
      }
    }
    #base[["Score"]] <- ""

    # White column – rationale for scores
    base[["Rationale"]] <- ""
    policy_tbl(base)
    # showNotification(
    #   "Template created. Fill in Statement/Evidence, then assign scores and rationale.",
    #   type = "message", duration = 5
    # )
  })

# KYLO
  output$policy_editor <- renderDT({
    df <- policy_tbl()
    if (is.null(df)) {
      return(
        datatable(
          data.frame(Note = "Complete Steps A, B, and C to begin."),
          rownames = FALSE
        )
      )
    }

    # 0-based indices of non-editable columns
    lock_cols <- which(names(df) %in%
                         c("Pillar", "Main_Objective", "Level_1",
                           "Level_2", "Level_3", "Level_4", "Objective_Label")) - 1

    # 0-based indices of scoring columns
    align_cols    <- grep("alignment$",    names(df)) - 1

    score_cols <- grep("Score", names(df)) - 1

    adequacy_cols <- grep("data_adequacy$", names(df)) - 1

    dt <- datatable(
      df,
      rownames = FALSE,
      editable = list(
        target = "cell",
        disable = list(columns = lock_cols)
      ),
      options = list(
        scrollX = TRUE,
        pageLength = 10
      )
    ) %>%
      # Light gray columns
      formatStyle(
        columns = c("Pillar", "Main_Objective", "Level_1", "Level_2",
                    "Level_3", "Level_4", "Objective_Label"),
        backgroundColor = '#d3d3d3'
      ) %>%
      # Pink columns (alignment columns)
      formatStyle(
        columns = align_cols + 1,  # DT uses 1-based indexing
        backgroundColor = '#ffc0cb'
      ) %>%
      # Blue columns
      formatStyle(
        columns = score_cols + 1,
        backgroundColor = '#add8e6'
      ) %>%
      # Rationale column white
      formatStyle(
        columns = c("Rationale"),
        backgroundColor = 'white'
      )


    # ASK REMI

    dt
  })

  observeEvent(input$policy_editor_cell_edit, {
    info <- input$policy_editor_cell_edit
    df <- policy_tbl(); req(df)

    #browser()

    i <- info$row
    j <- info$col
    v <- info$value

    if (is.factor(df[[j+1]])) {
      df[[j+1]] <- as.character(df[[j+1]])
    }


    #df[i, j] <- v
    df[[names(df)[j+1]]][i] <- v
    policy_tbl(df)
  })

  # observeEvent(input$policy_upload, {
  #   ext <- tools::file_ext(input$policy_upload$name)
  #   df <- tryCatch({
  #     if (ext == "csv") read.csv(input$policy_upload$datapath, stringsAsFactors = FALSE, check.names = FALSE)
  #     else read.xlsx(input$policy_upload$datapath, sheet = 1, check.names = FALSE)
  #   }, error = function(e) NULL)
  #   if (is.null(df)) {
  #     showNotification("Could not read file. Check columns.", type = "warning")
  #   } else {
  #     policy_tbl(df)
  #     showNotification("Uploaded.", type = "message")
  #   }
  # })

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
    df <- policy_tbl()
    if (is.null(df)) return(NULL)

    score_cols  <- grep("alignment$|data_adequacy$", names(df), value = TRUE)
    if (!length(score_cols)) {
      return(p("No scoring columns yet. Click 'Create / Reset Template' to begin."))
    }

    # Map factor labels to numeric 0–3 where possible
    to_num <- function(x) {
      x <- as.character(x)
      vals <- suppressWarnings(as.numeric(substr(x, 1, 1))) # first char '0','1','2','3' or 'X'
      vals[substr(x, 1, 1) == "X"] <- NA_real_
      vals
    }

    scores <- unlist(lapply(score_cols, function(cn) to_num(df[[cn]])))
    scores <- scores[!is.na(scores)]

    if (!length(scores)) {
      return(p("No numeric scores yet (all X / NA)."))
    }

    tagList(
      h4("Summary"),
      p("Objective × column cells with numeric scores (0–3): ", length(scores)),
      p("Average score: ", round(mean(scores), 2)),
      p("Min / Max: ", paste0(min(scores), " / ", max(scores)))
    )
  })

  output$policy_bar_per_scenario <- renderPlot({
    df <- policy_tbl(); if (is.null(df)) return(NULL)

    score_cols <- grep("alignment$|data_adequacy$", names(df), value = TRUE)
    if (!length(score_cols)) return(NULL)

    to_num <- function(x) {
      x <- as.character(x)
      vals <- suppressWarnings(as.numeric(substr(x, 1, 1)))
      vals[substr(x, 1, 1) == "X"] <- NA_real_
      vals
    }

    tall <- df |>
      pivot_longer(all_of(score_cols), names_to = "Scenario", values_to = "ScoreLabel") |>
      mutate(Score = to_num(ScoreLabel)) |>
      filter(!is.na(Score))

    if (nrow(tall) == 0) return(NULL)

    ggplot(tall, aes(x = Scenario, y = Score, fill = Scenario)) +
      stat_summary(fun = mean, geom = "bar", width = 0.7) +
      labs(
        title = "Average score per column (0–3)",
        x = "Policy / Advice source / Scenario column",
        y = "Average score"
      ) +
      theme_minimal() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$policy_heat_per_objective <- renderPlot({
    df <- policy_tbl(); if (is.null(df)) return(NULL)

    score_cols <- grep("alignment$|data_adequacy$", names(df), value = TRUE)
    if (!length(score_cols)) return(NULL)

    to_num <- function(x) {
      x <- as.character(x)
      vals <- suppressWarnings(as.numeric(substr(x, 1, 1)))
      vals[substr(x, 1, 1) == "X"] <- NA_real_
      vals
    }

    tall <- df |>
      mutate(Objective = Objective_Label) |>
      pivot_longer(all_of(score_cols), names_to = "Scenario", values_to = "ScoreLabel") |>
      mutate(Score = to_num(ScoreLabel))

    if (all(is.na(tall$Score))) return(NULL)

    ggplot(tall, aes(x = Scenario, y = Objective, fill = Score)) +
      geom_tile(color = "#ddd") +
      scale_fill_gradient(
        low = "#f0f9ff", high = "#084081", na.value = "#eee",
        name = "Score (0–3)"
      ) +
      labs(
        title = "Objective × column heatmap (0–3; X treated as NA)",
        x = "Policy / Advice source / Scenario column",
        y = "Objective"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # ========== PERFORMANCE ==========
  perf_tbl <- reactiveVal(NULL)

  performance_ui <- function() {
    so <- selected_objectives(); req(so)

    tagList(
      fluidRow(
        box(
          width = 12,
          title = "Management Performance Report Card (EBM USE 4)",
          status = "success",
          solidHeader = TRUE,
          actionButton("perf_make_template", "Create / Reset Template to begin", class = "btn-primary",  style = "background-color:#fff3cd; color:#000; border-color:#ffe69c;"),
          br(),
          br(),
          conditionalPanel(
            condition = "input.perf_make_template > 0",
            p(
              style = "background-color: #fff3cd; padding: 4px 6px;",
              strong(
                "You can click 's' on your keyboard at anytime to for a reminder of how to (s)core"
              )
            )),
          tags$hr(),
          DTOutput("perf_editor"),
          br(),
          uiOutput("perf_download_ui")
        )
      )
      # fluidRow(
      #   box(
      #     width = 12,
      #     title = "Summary & Plots",
      #     status = "info",
      #     solidHeader = TRUE,
      #     uiOutput("perf_summary_ui"),
      #     tags$hr(),
      #     tags$strong("Performance per objective"),
      #     plotOutput("perf_bar_objective", height = "320px"),
      #     tags$hr(),
      #     tags$strong("Average performance by pillar"),
      #     plotOutput("perf_bar_pillar", height = "300px")
      #   )
      # )
    )
  }

  show_dl <- reactiveVal(FALSE)

  observeEvent(input$perf_make_template, {
    show_dl(TRUE)
  })

  output$perf_download_ui <- renderUI({
    req(show_dl())
    fluidRow(
      column(3, downloadButton("perf_download_excel", "Download (Excel)", class = "btn-success btn-block")),
      column(3, downloadButton("perf_download_csv", "Download (CSV)", class = "btn-info btn-block"))
      #column(6, fileInput("perf_upload", "Upload completed (CSV / XLSX)", accept = c(".csv", ".xlsx")))
    )
  })



  observeEvent(input$perf_make_template, {
    so <- selected_objectives(); req(so)
    base <- make_objective_table(so)
    base[["Indicator"]] <- ""
    base[["Target"]]    <- ""
    base[["Score"]]     <- NA_real_  # 0, 1, 2
    base[["Rationale"]] <- ""

    perf_tbl(base)
    # showNotification(
    #   "Template created. Define indicators, targets, scores (0–2) and rationale.",
    #   type = "message"
    # )
  })

  output$perf_editor <- renderDT({
    df <- perf_tbl()
    if (is.null(df)) {
      return(
        datatable(
          data.frame(Note = "Click 'Create / Reset Template' to begin."),
          rownames = FALSE
        )
      )
    }

    lock_cols <- which(names(df) %in%
                         c("Pillar", "Main_Objective", "Level_1",
                           "Level_2", "Level_3", "Level_4",
                           "Objective_Label")) - 1

    score_col <- which(names(df) == "Score") - 1  # 0-based

    #browser()

    dt <- datatable(
      df,
      rownames = FALSE,
      editable = list(
        target = "cell",
        disable = list(columns = lock_cols)
      ),
      options = list(
        scrollX = TRUE,
        pageLength = 10
      )
    ) %>%
      # Light gray columns
      formatStyle(
        columns = c("Pillar", "Main_Objective", "Level_1", "Level_2",
                    "Level_3", "Level_4", "Objective_Label"),
        backgroundColor = '#d3d3d3'
      ) %>%
      # Pink columns (alignment columns)
      formatStyle(
        columns = "Indicator",  # DT uses 1-based indexing
        backgroundColor = '#ffc0cb'
      ) %>%
      # Blue columns
      formatStyle(
        columns = c("Target", "Score"),
        backgroundColor = '#add8e6'
      ) %>%
      # Rationale column white
      formatStyle(
        columns = c("Rationale"),
        backgroundColor = 'white'
      )



    # REMI
    # dt <- datatable(
    #   df,
    #   rownames = FALSE,
    #   editable = "cell",
    #   options = list(scrollX = TRUE, pageLength = 10),
    #   callback = JS(sprintf("
    #   table.MakeCellsEditable({
    #     'inputCss': 'form-control input-sm',
    #     'columns': %s,
    #     'getEditor': function(oldValue, cell, colIdx) {
    #       var scoreCol = %s;
    #       var levelsScore = %s;
    #
    #       if (colIdx === scoreCol) {
    #         var sel = $('<select></select>');
    #         for (var i = 0; i < levelsScore.length; i++) {
    #           $('<option>').val(levelsScore[i]).text(levelsScore[i]).appendTo(sel);
    #         }
    #         sel.val(oldValue);
    #         return sel;
    #       }
    #
    #       var input = $('<input type=\"text\"/>').val(oldValue);
    #       return input;
    #     },
    #     'onUpdate': function(cell, rowIdx, colIdx, oldValue, newValue) {
    #       Shiny.setInputValue(
    #         'perf_editor_cell_edit',
    #         {
    #           row:  rowIdx + 1,
    #           col:  colIdx + 1,
    #           value: newValue
    #         },
    #         {priority: 'event'}
    #       );
    #     }
    #   });
    # ",
    #                         jsonlite::toJSON(setdiff(seq_len(ncol(df)) - 1, lock_cols), auto_unbox = TRUE),
    #                         score_col,
    #                         jsonlite::toJSON(perf_levels, auto_unbox = TRUE)
    #   ))
    # )

    dt
  })

  observeEvent(input$perf_editor_cell_edit, {
    info <- input$perf_editor_cell_edit
    df <- perf_tbl(); req(df)

    i <- info$row
    j <- info$col
    v <- info$value

    if (is.factor(df[[j+1]])) {
      df[[j+1]] <- as.character(df[[j+1]])
    }

    df[[names(df)[j+1]]][i] <- v
    policy_tbl(df)
  })

  # observeEvent(input$perf_upload, {
  #   ext <- tools::file_ext(input$perf_upload$name)
  #   df <- tryCatch({
  #     if (ext == "csv") read.csv(input$perf_upload$datapath, stringsAsFactors = FALSE, check.names = FALSE)
  #     else read.xlsx(input$perf_upload$datapath, 1, check.names = FALSE)
  #   }, error = function(e) NULL)
  #   if (is.null(df)) showNotification("Could not read file.", type = "warning") else { perf_tbl(df); showNotification("Uploaded.", type = "message") }
  # })

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

  show_dl_cum <- reactiveVal(FALSE)

  observeEvent(input$cumu_make_template, {
    show_dl_cum(TRUE)
  })

  output$cum_download_ui <- renderUI({
    req(show_dl_cum())
    fluidRow(
      column(3, downloadButton("cumu_download_excel", "Download (Excel)", class = "btn-success btn-block")),
      column(3, downloadButton("cumu_download_csv", "Download (CSV)", class = "btn-info btn-block"))
      #column(6, fileInput("cumu_upload", "Upload completed (CSV / XLSX)", accept = c(".csv", ".xlsx")))
    )

  })


  cumulative_ui <- function() {
    so <- selected_objectives(); req(so)

    tagList(
      fluidRow(
        box(
          width = 12,
          title = "Cumulative Effects / Risk & Trade‑offs (EBM USE 5)",
          status = "success",
          solidHeader = TRUE,
          fluidRow(
            column(6,
          numericInput(
            "num_activities",
            "Step A: Select Number of activities:",
            value = 3, min = 2, max = 10, step = 1
          )),
          column(6,actionButton("cumu_make_template", "Step B: Create / Reset Template", class = "btn-primary",  style = "background-color:#fff3cd; color:#000; border-color:#ffe69c;"))),
          conditionalPanel(
            condition = "input.cumu_make_template > 0",
            p(
              style = "background-color: #fff3cd; padding: 4px 6px;",
              strong(
                "You can click 's' on your keyboard at anytime to for a reminder of how to (s)core"
              )
            )),
          tags$hr(),
          DTOutput("cumu_editor"),
          br(),
          uiOutput("cum_download_ui")
        )
      )
      # fluidRow(
      #   box(
      #     width = 12,
      #     title = "Summary & Plots",
      #     status = "info",
      #     solidHeader = TRUE,
      #     uiOutput("cumu_summary_ui"),
      #     tags$hr(),
      #     tags$strong("Average impact per activity"),
      #     plotOutput("cumu_bar_per_activity", height = "300px"),
      #     tags$hr(),
      #     tags$strong("Objective × activity heatmap"),
      #     plotOutput("cumu_heat_objective_activity", height = "380px")
      #   )
      # )
    )
  }

  observeEvent(input$cumu_make_template, {
    so <- selected_objectives(); req(so)
    base <- make_objective_table(so)

    #base[["Indicator"]] <- ""
    #base[["Target"]]    <- ""

    n <- input$num_activities %||% 3
    for (i in seq_len(n)) {
      base[[paste0("Indicator_",i)]] <- NA_real_  # e.g., 0–3
      base[[paste0("Target_",i)]] <- NA_real_  # e.g., 0–3
      base[[paste0("A", i, "_impact")]] <- NA_real_  # e.g., 0–3
    }
    base[["Tally"]] <- ""


    cumu_tbl(base)
    # showNotification(
    #   "Template created. Add indicators, targets and impacts (0–3 or your chosen scale) per activity.",
    #   type = "message"
    # )
  })

  output$cumu_editor <- renderDT({
    df <- cumu_tbl()
    if (is.null(df)) {
      return(
        datatable(
          data.frame(Note = "Complete step A and B to begin."),
          rownames = FALSE
        )
      )
    }

    #browser()

    lock_cols <- which(names(df) %in%
                         c("Pillar", "Main_Objective", "Level_1",
                           "Level_2", "Level_3", "Level_4",
                           "Objective_Label")) - 1

    impact_cols <- grep("^A\\d+_impact$", names(df)) - 1  # 0-based # SCORE
    #impact_cols <- c(impact_cols,which(names(df) == "Tally"))

    target_cols <- grep("Target", names(df)) - 1 # PINK
    indicator_cols <- grep("Indicator", names(df)) - 1 # PINK
    indicator_cols <- c(indicator_cols, target_cols)
#browser()

df[ , indicator_cols + 1] <- lapply(df[ , indicator_cols + 1], as.character)


    dt <- datatable(
      df,
      rownames = FALSE,
      editable = list(
        target = "cell",
        disable = list(columns = lock_cols)
      ),
      options = list(
        scrollX = TRUE,
        pageLength = 10
      )
    ) %>%
      # Light gray columns
      formatStyle(
        columns = c("Pillar", "Main_Objective", "Level_1", "Level_2",
                    "Level_3", "Level_4", "Objective_Label"),
        backgroundColor = '#d3d3d3'
      ) %>%
      # Pink columns (alignment columns)
      formatStyle(
        columns = indicator_cols + 1,  # DT uses 1-based indexing
        backgroundColor = '#ffc0cb'
      ) %>%
      # Blue columns
      formatStyle(
        columns = impact_cols+1,
        backgroundColor = '#add8e6'
      ) %>%
      formatStyle(columns="Tally",
                  backgroundColor = '#add8e6')

    dt
  })

  observeEvent(input$cumu_editor_cell_edit, {
    info <- input$cumu_editor_cell_edit
    df <- cumu_tbl(); req(df)

    i <- info$row
    j <- info$col
    v <- info$value

    if (is.factor(df[[j+1]])) {
      df[[j+1]] <- as.character(df[[j+1]])
    }

    df[[names(df)[j+1]]][i] <- v
    #policy_tbl(df)
    cumu_tbl(df)
  })

  # observeEvent(input$cumu_upload, {
  #   ext <- tools::file_ext(input$cumu_upload$name)
  #   df <- tryCatch({
  #     if (ext == "csv") read.csv(input$cumu_upload$datapath, stringsAsFactors = FALSE, check.names = FALSE)
  #     else read.xlsx(input$cumu_upload$datapath, 1, check.names = FALSE)
  #   }, error = function(e) NULL)
  #   if (is.null(df)) showNotification("Could not read file.", type = "warning") else { cumu_tbl(df); showNotification("Uploaded.", type = "message") }
  # })

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
        tags$style(HTML("
    .back-button {
      background-color: #fff3cd !important;
      color: #856404 !important;
      border: 1px solid #ffeeba !important;
    }
    .back-button-container {
      text-align: center;
      margin-bottom: 15px;
    }
  ")),

        div(
          class = "back-button-container",
          actionButton(
            "home_back_button",
            "Back to Framework",
            class = "btn back-button"
          )
        ),

        box(
          width = 12,
          title = paste("Viewing:", home_filter()),
          status = "info",
          solidHeader = TRUE,
          DTOutput("home_data_table")
        )
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
