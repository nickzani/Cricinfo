#' compareTwoPlayerAves
#'
#' Uses the getPlayerInfo function to return a dataframe with the two player averages
#' @param Player1 Name of the first player
#' @param Player2 Name of the second player
#' @param Player1Country Country of the first player
#' @param Player2Country Country of the second player
#' @export
#' @examples
#' compareTwoPlayerAves("Stuart Broad", "England", "Alastair Cook", "England")

compareTwoPlayerAves <- function(Player1Name,Player1Country,Player2Name,Player2Country){
  
	df1 <- getPlayerInfo(PlayerSearch = Player1Name, PlayerCountry = Player1Country, PlayerNumber = "", BattingBowling = "Batting", ViewType = "Ave", Clean = "Y")
	df2 <- getPlayerInfo(PlayerSearch = Player2Name, PlayerCountry = Player2Country, PlayerNumber = "", BattingBowling = "Batting", ViewType = "Ave", Clean = "Y")
	colBind <- data.frame(PlayerName = c(Player1Name, Player2Name), stringsAsFactors = FALSE)
  
    if (length(names(df1)) == length(names(df2)))
		{df3 <- cbind(colBind, rbind(df1,df2))}
		
    else if (length(names(df1)) == 10)
	{	
		df1$BF <- NA
		df1$SR <- NA
		df1$No4s <- NA
		df1$No6s <- NA
		
	df3 <- cbind(colBind, rbind(df1,df2))
	
	}
	
	else if (length(names(df2)) == 10)
		{	
		df2$BF <- NA
		df2$SR <- NA
		df2$No4s <- NA
		df2$No6s <- NA
		
	df3 <- cbind(colBind, rbind(df1,df2))
	
	}
  
  df3
  
}

