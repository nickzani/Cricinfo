#' Return Cricinfo player number
#'
#' Returns the cricinfo player number
#' @param Name				Player name
#' @param CountrySearch		Player country
#' @param FuzzyMatch		If set to N will return an exact match
#' @export
#' @examples
#' getPlayerNumber(Name="Stuart Broad")


getPlayerNumber <- function(Name, CountrySearch, FuzzyMatch = "N"){

	# get the country number
	
	if (CountrySearch == "England")
		{CountryNumber <- 1}
	else if (CountrySearch == "Australia")	
		{CountryNumber <- 2}
	else if (CountrySearch == "Bangladesh")	
		{CountryNumber <- 25}
	else if (CountrySearch == "India")	
		{CountryNumber <- 6}	
	else if (CountrySearch == "New Zealand")	
		{CountryNumber <- 5}	
	else if (CountrySearch == "Pakistan")	
		{CountryNumber <- 7}	
	else if (CountrySearch == "South Africa")	
		{CountryNumber <- 3}		
	else if (CountrySearch == "Sri Lanka")	
		{CountryNumber <- 8}
	else if (CountrySearch == "West Indies")	
		{CountryNumber <- 4}		
	else if (CountrySearch == "Zimbabwe")	
		{CountryNumber <- 9}			
 
	# get the final initial to search for
	finalword <- toupper(tail(strsplit(Name,split=" ")[[1]],1))
	initial_Use <- head(strsplit(finalword,"")[[1]],1)
	url_search <- paste0("http://www.espncricinfo.com/ci/content/player/country.html?country=",CountryNumber,";alpha=",initial_Use)
	table_search <- as.data.frame(toupper(as.character(readLines(url_search))), stringsAsFactors = FALSE)
	names(table_search) <- c("Col1")
	
	if (FuzzyMatch == "N")
		{pattern_match <- paste0("\\(",Name,",")}
	else 
		{pattern_match <- Name}
	
	name_row  <-   which(grepl(toupper(pattern_match),table_search$Col1))
	player_url <- table_search[name_row-1,1]
	player_url_clean <- as.numeric(gsub(".HTML\"CLASS=\"COLUMNISTSMRY\"STYLE=\"VERTICAL-ALIGN:MIDDLE;\">","",gsub("<AHREF=\"/CI/CONTENT/PLAYER/","",gsub(" ","",player_url))))
	player_url_clean
  
}   