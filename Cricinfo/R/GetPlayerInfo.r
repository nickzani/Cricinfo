#' getPlayerInfo
#'
#' Scrapes player info from cricinfo
#' @param view Do you want to show in a table? Defaults to TRUE.
#' @param PlayerNumber		Cricinfo player number
#' @param BattingBowling	Batting or bowling figures. Default is batting
#' @param ViewType			Data to be returned. Default is average
#' @param Clean				Do you want the data to be cleaned? Default is yes
#' @export
#' @examples
#' playerInfo <- getPlayerInfo(PlayerNumber="11728", BattingBowling = "Batting", ViewType = "Inn", Clean="Y")

getPlayerInfo <- function(PlayerNumber, BattingBowling="Batting", ViewType="Ave", Clean="Y"){
  
  theurl <- paste0("http://stats.espncricinfo.com/ci/engine/player/",PlayerNumber,".html?class=1;template=results;type=",tolower(BattingBowling),";view=innings")
  tables <- readHTMLTable(theurl)
  
  if (ViewType=="Ave"){
    
    keptTable <- as.data.frame(tables[3], stringsAsFactors = FALSE)
      
  }
  
  else {
    
    keptTable <- as.data.frame(tables[4], stringsAsFactors = FALSE)
    
  }
  
  if (Clean != "Y"){
    
    return(as.data.frame(keptTable, stringsAsFactors = FALSE))
    
  }
  
  else if (ViewType !="Ave" & BattingBowling=="Batting"){
    
    names(keptTable)
    
    names(keptTable) <- c("Runs","Mins","BallsFaced","No4s","No6s","SR","Pos","Dismissal","Inns","Blank","Opposition","Ground","StartDate","Test")
    
    keptTable$Blank <- NULL
    keptTable$Test <- NULL
    
    keptTable$NOFlag <- ifelse(grepl("\\*",keptTable$Runs),1,0)
    
    keptTable$Runs <- str_replace_all(keptTable$Runs, "\\*","")
    
    keptTable$Runs <- as.numeric(as.character(keptTable$Runs))
    keptTable$Mins <- as.numeric(as.character(keptTable$Mins))
    keptTable$BallsFaced <- as.numeric(as.character(keptTable$BallsFaced))
    keptTable$No4s <- as.numeric(as.character(keptTable$No4s))
    keptTable$No6s <- as.numeric(as.character(keptTable$No6s))
    keptTable$SR <- as.numeric(as.character(keptTable$SR))
    keptTable$Pos <- as.numeric(as.character(keptTable$Pos))
    
    keptTable$Opposition <- str_replace_all(keptTable$Opposition, "v ","")
    
    keptTable <- subset(keptTable, !is.na(keptTable$Runs))
    
    ## add an innings number and an innings inc not out here
    
    keptTable$InningsNo <- 1:nrow(keptTable)
    keptTable$InningsNoIncNO <- 1:nrow(keptTable)
    
    rownum <- 1
    
    for (i in 1:nrow(keptTable)){
      
      rownum <- ifelse(keptTable[i,"NOFlag"] == 1, rownum-1,rownum)
      keptTable[i,"InningsNoIncNO"] <- rownum
      
      rownum <- rownum + 1
      
      
    }
    
    keptTable$CareerAverage <- cumsum(keptTable$Runs)/keptTable$InningsNoIncNO
    
    keptTable$CumRuns <- cumsum(keptTable$Runs)
    keptTable$CumMins <- cumsum(keptTable$Mins)
    
    keptTable$RunsPerMin <- keptTable$CumRuns/keptTable$CumMins
    
  }
  
  keptTable
  
}


