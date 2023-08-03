#' function for search SENAMHI stations information by station ID.
#'
#' function for search SENAMHI stations information by station name inside SENAMHI data web.
#' @param x character or number; optional character vector to filter results by station ID.
#' @param stations stations information of SENAMHI web scraping
#'
#' @export
#'
#' @examples
#'
#' x <- 114
#' stations <- stations()
#'
#' searchId(x, stations)
#'
#' @author Geomar Perales Apaico
#'
#' @name searchId

searchId <- function(x, stations) {
  for (i in 1:length(x)) {
    cod_stn <- list()
    cod_stn[[i]] <- x[i]
    idx.cod <- grep(as.character(cod_stn[[i]]), stations$cod)
    df_idx_stn <- data.frame(stations[idx.cod,], stringsAsFactors = FALSE)
  }
  return(df_idx_stn)
}
