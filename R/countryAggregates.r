#' countryAggregates
#'
#' Creates a dataframe giving player averages against countries played against
#' @param PlayerName			Player name to be searched for in the cricinfo database
#' @param PlayerCountry			Country for associated player
#' @export
#' @examples
#' Aggregates <- countryAggregates(PlayerName="Alastair Cook", PlayerCountry = "England")


countryAggregates <- function(PlayerName, PlayerCountry){

	rawdata <- getPlayerInfo(PlayerSearch = PlayerName, PlayerCountry = PlayerCountry, PlayerNumber="", BattingBowling="Batting", ViewType="Inns", Clean="Y")
	
	aggregates <- sqldf("select Opposition, count(*) as Total_Inns, sum(NOFlag) as NOs, sum(Runs) as Total_Runs,  sum(Runs) / ( count(*) - sum(NOFlag)) as Ave from rawdata where Opposition != 'ICC World XI' group by Opposition")
	
	aggregates[ order(-aggregates[,"Ave"]), ]
	
}  
		