#' function for get stations information of SENAMHI.
#'
#' function for get information (coordinates, station code and altitude) about stations.
#' @param x a vector from stations code
#' @param stations data of SENAMHI web scraping
#'
#' @import rvest
#'
#' @export
#'
#' @author Geomar Perales Apaico
#'
#' @name senamhinfo

senamhinfo <- function(x, stations) {
  cod_stn <- list()
  read_snm <- list()
  df_idx_stn <- list()
  scraping_web_senamhi <- list()
  data_stn_senamhi <- list()

  for (i in 1:length(x)) {
    cod_stn[[i]] <- x[i]
    idx.cod <- grep(as.character(cod_stn[[i]]), stations$cod)
    df_idx_stn[[i]] <- data.frame(stations[idx.cod,], stringsAsFactors = FALSE)

    if(is.na(df_idx_stn[[i]]$cod)){
      link <- paste0("https://www.senamhi.gob.pe//mapas/mapa-estaciones-2/_dato_esta_tipo02.php?estaciones=",
                     as.character(df_idx_stn[[i]]$cod), #cod estacion
                     "&CBOFiltro=",
                     "202401", #fecha año y mes
                     "&t_e=",
                     as.character(df_idx_stn[[i]]$ico), #ico
                     "&estado=",
                     as.character(df_idx_stn[[i]]$estado),
                     "&cod_old=", as.character(df_idx_stn[[i]]$cod_old)) #estado

    } else {
      link <- paste0("https://www.senamhi.gob.pe//mapas/mapa-estaciones-2/_dato_esta_tipo02.php?estaciones=",
                     as.character(df_idx_stn[[i]]$cod), #cod estacion
                     "&CBOFiltro=",
                     "202401", #fecha año y mes
                     "&t_e=",
                     as.character(df_idx_stn[[i]]$ico), #ico
                     "&estado=",
                     as.character(df_idx_stn[[i]]$estado)) #estado
    }
    data_stn_senamhi <- read_html(link)
    scraping_web_senamhi[[i]] <- html_table(data_stn_senamhi)
    data_stn_senamhi[[i]] <- scraping_web_senamhi[[i]][[1]]
    df_dss <- data.frame(data_stn_senamhi[[i]])
    df_idx_stn[[i]]$alt <- strsplit(df_dss$X6[3], " ")[[1]][1]
  }
  data_stn_senamhi <- do.call("rbind", df_idx_stn)
  return(data_stn_senamhi)
}
