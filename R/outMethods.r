#' outMethods
#'
#' Creates a dataframe giving player outs
#' @param PlayerName			Player name to be searched for in the cricinfo database
#' @param PlayerCountry			Country for associated player
#' @export
#' @examples
#' Aggregates <- outMethods(PlayerName="Alastair Cook", PlayerCountry = "England")


outMethods <- function(PlayerName, PlayerCountry){

	rawdata <- getPlayerInfo(PlayerSearch = PlayerName, PlayerCountry = PlayerCountry, PlayerNumber="", BattingBowling="Batting", ViewType="Inns", Clean="Y")
	
	aggregates <- sqldf("select Dismissal, count(*) as Total from rawdata group by Dismissal")
	
	aggregates[ order(-aggregates[,"Total"]), ]
	
} 