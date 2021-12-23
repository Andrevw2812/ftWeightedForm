
fluidPage(
  theme = bs_theme(version = 5, bootswatch = "materia"),
  fluidRow(
    column(12,
           h2("Weighted Form Calculator"), 
           align = "center"),
    column(8, offset = 2, align = "center", hr())
  ),
  fluidRow(
    tabsetPanel(type = "pills",
                tabPanel("Eng Prem",
                         hr(),
                         fluidRow(column(
                           h3("England Premier League"),
                           tags$img(src="england.png", height = 48, width = 64),
                           #h5(strong("Data last updated: ", engUpdated)),
                           br(),
                           h5("Select Teams for Analysis"),
                           width = 12, align = "center"
                         )),
                         fluidRow(
                           column(2),
                           column(
                             selectizeInput("wf_engprem_h", "Home Team", choices = NULL, width = "70%"),
                             width = 4, align = "center"),
                           column(
                             selectizeInput("wf_engprem_a", "Away Team", choices = NULL, width = "70%"),
                             width = 4, align = "center"),
                           column(2)
                         ),
                         fluidRow(
                           column(6, offset = 3, align = "center",
                           actionButton("update_engprem", "Fetch Data", width = "50%"), 
                           hr()
                           )
                         ), 
                         fluidRow(
                           column(4, align = "center",
                                  gt_output("eng_form_tbl")
                                  ),
                           column(2),
                           column(6, align = "center",
                                  gt_output("eng_prob_odds_tbl")
                           )
                         )
                         ),
                tabPanel("Panel 2"
                         )
                )
  )
)