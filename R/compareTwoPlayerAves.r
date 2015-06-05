#' compareTwoPlayerAves
#'
#' Uses the getPlayerInfo function to return a dataframe with the two player averages
#' @param Player1 Name of the first player
#' @param Player2 Name of the second player
#' @export
#' @examples
#' compareTwoPlayerAves("Stuart Broad", "Alistair Cook")

compareTwoPlayerAves <- function(Player1,Player2){
  
  df1 <- getPlayerInfo(PlayerSearch = Player1, PlayerNumber = "", BattingBowling = "Batting", ViewType = "Ave", Clean = "Y")
  df2 <- getPlayerInfo(PlayerSearch = Player2, PlayerNumber = "", BattingBowling = "Batting", ViewType = "Ave", Clean = "Y")
  colBind <- data.frame(PlayerName = c(Player1,Player2), stringsAsFactors = FALSE)
  
  df3 <- cbind(colBind, rbind(df1,df2))
  
  df3
  
}


