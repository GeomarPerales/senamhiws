#' function for search SENAMHI stations information by station name.
#'
#' function for search SENAMHI stations information by station name inside SENAMHI data web.
#' @param x character; optional character vector to filter results by station name.
#' @param stations stations information of SENAMHI web scraping
#'
#' @export
#'
#' @examples
#'
#' x <- "CORD"
#' stations <- stations()
#'
#' searchname(x, stations)
#'
#' @author Geomar Perales Apaico
#'
#' @name searchname

searchname <- function(x, stations) {
  if(is.null(x)){
    return(print("nombre no definido"))

  } else if(is.numeric(x)){
    return(print("nombre no definido"))

  } else if(is.character(x)){
    x <- x

  }

  for (i in 1:length(x)) {
    name_stn <- list()
    name_stn[[i]] <- x[i]
    idx.name <- grep(as.character(name_stn[[i]]), stations$estacion)
    df_name_stn <- data.frame(stations[idx.name,], stringsAsFactors = FALSE)
  }
  return(df_name_stn)
  print("desarrollado por Hydroprime")
}
