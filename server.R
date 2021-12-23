library(shiny)
engpremTeams <- readRDS("data/engpremTeams.rds")
server <- function(input, output, session) {
  
  teams <- reactiveValues()
  
  updateSelectizeInput(session, "wf_engprem_h",
                       choices = c("", unique(engpremTeams$engpremTeam)))
  observeEvent(input$wf_engprem_h, {
    engpremH <<- input$wf_engprem_h
  })
  
  updateSelectizeInput(session, "wf_engprem_a",
                       choices = c("", engpremTeams$engpremTeam))
  observeEvent(input$wf_engprem_a, {
    engpremA <<- input$wf_engprem_a
  })
  
  output$eng_form_tbl <- render_gt({
    print(input$update_engprem)
    
    if(input$update_engprem == 0){
      return()
    }
    source("leagueFiles/engpremTeamData.R")
    
    eng_form_tbl <- engpremFormTblGT
  })
  output$eng_prob_odds_tbl <- render_gt({
    print(input$update_engprem)
    
    if(input$update_engprem == 0){
      return()
    }
    # source("leagueFiles/engpremTeamData.R")
    
    eng_prob_odds_tbl <- engpremProbOddsTblGT
  })
  

  
  
  
    
}

