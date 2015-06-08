#' getPlayerInfo
#'
#' Scrapes player info from cricinfo
#' @param PlayerSearch 		Name of the player to search Cricinfo for
#' @param PlayerCountry 	Country the player plays for
#' @param PlayerNumber		Cricinfo player number
#' @param BattingBowling	Batting or bowling figures. Default is batting
#' @param ViewType			Data to be returned. Default is average
#' @param Clean				Do you want the data to be cleaned? Default is yes
#' @export
#' @examples
#' playerInfo <- getPlayerInfo(PlayerNumber="11728", BattingBowling = "Batting", 
#'					ViewType = "Inn", Clean="Y")

getPlayerInfo <- function(PlayerSearch, PlayerNumber="", PlayerCountry = "England", BattingBowling="Batting", ViewType="Ave", Clean="Y"){

	if (!(PlayerCountry %in% c("England","Australia","New Zealand","Bangladesh","Pakistan","India","Sri Lanka","West Indies","South Africa","Zimbabwe")))
		{ return(cat("ERROR: Country not in list. Must be one of England, Australia, New Zealand, Bangladesh \n Pakistan, India, Sri Lanka, West Indies, South Africa, Zimbabwe"))}
  
	if (PlayerNumber != ""){
  
		theurl <- paste0("http://stats.espncricinfo.com/ci/engine/player/",PlayerNumber,".html?class=1;template=results;type=",tolower(BattingBowling),";view=innings")
		tables <- readHTMLTable(theurl)
	  
	}
	
	else {
	
		number_play <- getPlayerNumber(PlayerSearch, CountrySearch = PlayerCountry)
	
		if (length(number_play) == 0) { return(cat("ERROR: Player name spelt incorrectly"))}
		
		theurl <- paste0("http://stats.espncricinfo.com/ci/engine/player/",number_play,".html?class=1;template=results;type=",tolower(BattingBowling),";view=innings")
		tables <- readHTMLTable(theurl)
	
	}
  
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
		
		## add an innings number here
		
		keptTable$InningsNo <- 1:nrow(keptTable)
		
	  }
	  
	  else if (ViewType !="Ave" & BattingBowling=="Bowling"){
		
		names(keptTable)
		
		names(keptTable) <- c("Overs","Maidens","Runs","Wickets","Econ","Pos","Inns","Blank","Opposition","Ground","StartDate","Test")
		
		keptTable$Blank <- NULL
		keptTable$Test <- NULL
		
		keptTable$Overs <- as.numeric(as.character(keptTable$Overs))
		keptTable$Maidens <- as.numeric(as.character(keptTable$Maidens))
		keptTable$Runs <- as.numeric(as.character(keptTable$Runs))
		keptTable$Wickets <- as.numeric(as.character(keptTable$Wickets))
		keptTable$Econ <- as.numeric(as.character(keptTable$Econ))
		keptTable$Inns <- as.numeric(as.character(keptTable$Inns))
		keptTable$Pos <- as.numeric(as.character(keptTable$Pos))
		
		keptTable$Opposition <- str_replace_all(keptTable$Opposition, "v ","")
		
		keptTable <- subset(keptTable, !is.na(keptTable$Overs))
		
		## add an innings number here
		
		keptTable$InningsNo <- 1:nrow(keptTable)
		
	  }
	  
	  else if (ViewType =="Ave" & BattingBowling=="Batting"){
		
		names(keptTable) <- c("Overall","CareerSpan","Matches","Innings","NOs","Runs","HS","Ave","BF","SR","100s","50s","0s","4s","6s","Profile")
		
		keptTable$Overall <- NULL
		keptTable$Profile <- NULL
	  
	  }
	  
	  keptTable
  
}


