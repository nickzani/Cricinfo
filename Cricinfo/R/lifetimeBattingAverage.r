#' lifetimeBattingAverage
#'
#' Creates a dataframe giving a career batting average
#' @param NOFlag			Flag showing if the innings was not out. By default 1 is not out
#' @param BattingBowling	Batting or bowling figures. Default is batting
#' @param runs				Runs scored in innings
#' @export
#' @examples
#' playerInfo <- getPlayerInfo(PlayerNumber="11728", BattingBowling = "Batting", ViewType = "Inn", Clean="Y")
#' playerInfo$Average <- lifetimeBattingAverage(NOFlag=playerInfo$NOFlag, runs = playerInfo$Runs)


lifetimeBattingAverage <- function(NOFlag, runs){

	IncNO <- as.vector(1)
  
	for (i in 1:length(NOFlag)){
    
		if (i == 1){IncNO[1] <- ifelse(NOFlag[1] == 1,0,1)}
    
		if (i > 1) {IncNO[i] <- ifelse(NOFlag[i] == 1, IncNO[i-1],IncNO[i-1]+1)}
    
	}
  
	toreturn <- cumsum(runs)/IncNO
  
	return(ifelse(is.infinite(toreturn),NA,toreturn))
}  
		