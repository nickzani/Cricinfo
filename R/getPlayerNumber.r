#' Return Cricinfo player number
#'
#' Returns the cricinfo player number
#' @param Name			Player name
#' @export
#' @examples
#' getPlayerNumber(Name="Stuart Broad")


getPlayerNumber <- function(Name){
  
  # get the final initial to search for
  finalword <- toupper(tail(strsplit(Name,split=" ")[[1]],1))
  initial_Use <- head(strsplit(finalword,"")[[1]],1)
  url_search <- paste0("http://www.espncricinfo.com/ci/content/player/country.html?country=1;alpha=",initial_Use)
  table_search <- as.data.frame(toupper(as.character(readLines(url_search))), stringsAsFactors = FALSE)
  names(table_search) <- c("Col1")
  name_row  <-   which(grepl(toupper(Name),table_search$Col1))
  player_url <- table_search[name_row-1,1]
  player_url_clean <- as.numeric(gsub(".HTML\"CLASS=\"COLUMNISTSMRY\"STYLE=\"VERTICAL-ALIGN:MIDDLE;\">","",gsub("<AHREF=\"/CI/CONTENT/PLAYER/","",gsub(" ","",player_url))))
  player_url_clean
  
}   