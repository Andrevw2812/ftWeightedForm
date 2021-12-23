library(tidyverse)
library(rvest)
library(gt)

# England Premiership Data
engPg <- read_html("http://football-data.co.uk/englandm.php")
engUpdated <- engPg %>% html_element("td:nth-child(3) > p:nth-child(1)") %>% html_element('i') %>% html_text2()

allengpremData <- read_csv("http://football-data.co.uk/mmz4281/2122/E0.csv", col_types = cols(Date = col_date(format = "%d/%m/%Y")))

engprem <- allengpremData %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)

engpremTeams <- unique(c(engprem$HomeTeam, engprem$AwayTeam))
engpremTeams <-  engpremTeams %>% as.data.frame()
names(engpremTeams) <- "engpremTeam"
engpremTeams <<- engpremTeams %>% arrange(engpremTeam)
saveRDS(engpremTeams, "data/engpremTeams.rds")

engpremH.data <- function(engpremH){
  engpremH <- engpremH
  engpremHData <- engprem %>% filter(HomeTeam==engpremH|AwayTeam==engpremH) %>% slice_tail(n=10)
  #Home Games
  engpremHHGTen <- engpremHData %>% filter(HomeTeam==engpremH)
  engpremHHGames <- engpremHHGTen %>% nrow()
  engpremHHGTen <- engpremHHGTen %>% mutate(gMarg = FTHG-FTAG)
  engpremHHGTenTwoPlus <- engpremHHGTen %>% filter(gMarg>=2) %>% nrow()
  engpremHHGTenOne <- engpremHHGTen %>% filter(gMarg == 1) %>% nrow()
  engpremHHGTenDraw <- engpremHHGTen %>% filter(FTR == "D") %>% nrow()
  engpremHHGTenLoss <- engpremHHGTen %>% filter(FTR == "A") %>% nrow()
  #Away Games
  engpremHAGTen <- engpremHData %>% filter(AwayTeam==engpremH)
  engpremHAGames <- engpremHAGTen %>% nrow()
  engpremHAGTen <- engpremHAGTen %>% mutate(gMarg = FTAG-FTHG)
  engpremHAGTenTwoPlus <- engpremHAGTen %>% filter(gMarg>=2) %>% nrow()
  engpremHAGTenOne <- engpremHAGTen %>% filter(gMarg == 1) %>% nrow()
  engpremHAGTenDraw <- engpremHAGTen %>% filter(FTR == "D") %>% nrow()
  engpremHAGTenLoss <- engpremHAGTen %>% filter(FTR == "H") %>% nrow()
  
  engpremHTenTotal <- ((engpremHHGTenTwoPlus+engpremHAGTenTwoPlus)*3)+((engpremHHGTenOne+engpremHAGTenOne)*2)+(engpremHHGTenDraw+engpremHAGTenDraw)+(engpremHHGTenLoss+engpremHAGTenLoss)
  
  engpremHDF <- data.frame(engpremH, (engpremHHGTenTwoPlus+engpremHAGTenTwoPlus),(engpremHHGTenOne+engpremHAGTenOne), (engpremHHGTenDraw+engpremHAGTenDraw), (engpremHHGTenLoss+engpremHAGTenLoss),engpremHTenTotal, stringsAsFactors = F)
  names(engpremHDF) <- c("Team", "2+GoalsWin", "1GoalWin", "Draw", "Loss", "Total")
  engpremHDF
}
engpremHDF <- engpremH.data(engpremH)

engpremA.data <- function(engpremA){
  engpremA <- engpremA
  engpremAData <- engprem %>% filter(HomeTeam==engpremA|AwayTeam==engpremA) %>% slice_tail(n=10)
  #Home Games
  engpremAHGTen <- engpremAData %>% filter(HomeTeam==engpremA)
  engpremAHGames <- engpremAHGTen %>% nrow()
  engpremAHGTen <- engpremAHGTen %>% mutate(gMarg = FTHG-FTAG)
  engpremAHGTenTwoPlus <- engpremAHGTen %>% filter(gMarg>=2) %>% nrow()
  engpremAHGTenOne <- engpremAHGTen %>% filter(gMarg == 1) %>% nrow()
  engpremAHGTenDraw <- engpremAHGTen %>% filter(FTR == "D") %>% nrow()
  engpremAHGTenLoss <- engpremAHGTen %>% filter(FTR == "A") %>% nrow()
  #Away Games
  engpremAAGTen <- engpremAData %>% filter(AwayTeam==engpremA)
  engpremAAGames <- engpremAAGTen %>% nrow()
  engpremAAGTen <- engpremAAGTen %>% mutate(gMarg = FTAG-FTHG)
  engpremAAGTenTwoPlus <- engpremAAGTen %>% filter(gMarg>=2) %>% nrow()
  engpremAAGTenOne <- engpremAAGTen %>% filter(gMarg == 1) %>% nrow()
  engpremAAGTenDraw <- engpremAAGTen %>% filter(FTR == "D") %>% nrow()
  engpremAAGTenLoss <- engpremAAGTen %>% filter(FTR == "H") %>% nrow()
  
  engpremATenTotal <- ((engpremAHGTenTwoPlus+engpremAAGTenTwoPlus)*3)+((engpremAHGTenOne+engpremAAGTenOne)*2)+(engpremAHGTenDraw+engpremAAGTenDraw)+(engpremAHGTenLoss+engpremAAGTenLoss)
  
  engpremADF <- data.frame(engpremA, (engpremAHGTenTwoPlus+engpremAAGTenTwoPlus),(engpremAHGTenOne+engpremAAGTenOne), (engpremAHGTenDraw+engpremAAGTenDraw), (engpremAHGTenLoss+engpremAAGTenLoss),engpremATenTotal, stringsAsFactors = F)
  names(engpremADF) <- c("Team", "2+GoalsWin", "1GoalWin", "Draw", "Loss", "Total")
  engpremADF
}
engpremADF <- engpremA.data(engpremA)

engpremFormTbl <- as_tibble(bind_rows(engpremHDF, engpremADF))
engpremFormTblGT <- engpremFormTbl %>% gt() %>% tab_header("Form Table for last 10 H & A Games") %>% tab_options(table.width = "70%") %>% cols_align(align = "center", columns = c("2+GoalsWin", "1GoalWin", "Draw", "Loss", "Total"))

engpremHPts <- engpremFormTbl[1,2]+engpremFormTbl[1,3]+engpremFormTbl[2,4]
engpremAPts <- engpremFormTbl[2,2]+engpremFormTbl[2,3]+engpremFormTbl[1,4]

engpremProbOddsTbl <- data.frame(matrix(data=NA, ncol = 4, nrow = 3), stringsAsFactors = F)
names(engpremProbOddsTbl) <- c("Winner", "Pts", "Prob%", "Odds")
engpremProbOddsTbl$Winner <- c(as.character(engpremFormTbl[1,1]), as.character(engpremFormTbl[2,1]), "Draw")
engpremProbOddsTbl$Pts <- c(engpremHPts, engpremAPts, sum(engpremFormTbl[,4]))
engpremProbOddsTbl$`Prob%` <- round((as.numeric(engpremProbOddsTbl$Pts)/sum(engpremFormTbl$Total)*100), 2)
engpremProbOddsTbl$Odds <- round(100/engpremProbOddsTbl$Prob, 2)


engpremProbOddsTblGT <- engpremProbOddsTbl %>% gt() %>% tab_header("Calculated Probablity and Odds", subtitle = "Based on Previous 10 Games, and weighted by Victory margin") %>% tab_options(table.width = "80%") %>% cols_align(align = "center", columns = everything())
