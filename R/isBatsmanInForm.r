#' isBatsmanInForm
#'
#' Compares the lastest of a batsmans innings against their career average
#' @param Runs 				Vector containing the runs for the player
#' @param PValue		 	P Value to compare against
#' @param Popsize		 	Number of innings to consider
#' @export
#' @examples
#' playerInfo <- getPlayerInfo(PlayerNumber="11728", BattingBowling = "Batting", ViewType = "Inn", Clean="Y")
#' isBatsmanInForm(playerInfo$Runs, 0.05)

isBatsmanInForm <- function(Runs, Popsize=10, PValue=0.05){

	len <- length(Runs)
	
	# keep the first n - 5 rows
	
	kept_runs <- Runs[1:(len-Popsize)]
    
	# Mean of this kept sample
	mean_runs  <- round(mean(kept_runs),2)
	
	# The sample of the latest innings
	sample_runs <- Runs[(len-(Popsize-1)):len]
	
	# Mean and standard deviation of sample
	sample_mean <- round(mean(sample_runs),2)
    sd_sample <- round(sd(sample_runs),2)
	
	# Calculate the t statistic
	t_stat <- (sample_mean - mean_runs)/(sd_sample/sqrt(Popsize))
	
	# The null hypothesis here is that the batsman scores at the mean or above the mean in their last Popsize innings
	Value <- round(pt(t_stat, Popsize, lower.tail = TRUE),6)
	
	if(Value > PValue){
        paste_string <- paste0("Batsmans is in-Form because the p value: ", Value,"is greater than the comparison value of", PValue)
    } else {
        paste_string <- paste0("Batsmans is NOT in-Form because the p value: ", Value,"is less than the comparison value of", PValue)
    }
    cat("*******************************************************************************************\n\n")
    cat("Population size:",len - Popsize," Mean of population:",mean_runs,"\n")
    cat("Sample size:",Popsize," Mean of sample:",sample_mean, "SD of sample:", sd_sample,"\n\n")
    print(paste_string)
    cat("*******************************************************************************************\n\n")
	
  
}


