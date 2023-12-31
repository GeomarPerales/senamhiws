#' function for get records of SENAMHI stations
#'
#' function for get record of SENAMHI stations from 01-01-2016 to 31-12-2022 and
#' save data files and save dataset files in work directory.
#'
#' @param x a vector from SENAMHI stations code
#' @param stations stations information of SENAMHI web scraping
#' @param from a vector of date begin
#' @param to a vector of date end
#'
#' @import rvest
#' @import openxlsx
#'
#' @export
#'
#' @examples
#'
#' x <- c("114123", "114030")
#' stations <- stations()
#' senamhiws(x, stations)
#'
#' @author Geomar Perales Apaico
#'
#' @name senamhiws


senamhiws <- function(x, stations, from = NULL, to = NULL) {
  if(is.null(x)){
    return(print("codigo no definido"))

  } else if(is.numeric(x)){
    x <- as.character(x)

  } else if(is.character(x)){
    x <- x

  } else if(is.na(x)){
    return(print("codigo no definido"))

  }

  cod_stn <- list()
  read_snm <- list()
  scraping_web_senamhi <- list()
  data_history_senamhi <- list()
  data_df_history_senamhi <- list()
  data_stn_senamhi <- list()
  df_history_senamhi <- list()

  if(is.null(from) & is.null(to)){
    from <- c(2016, 01, 01)
    to <- c(2023, 12, 31)

  } else if(is.null(from) & !is.null(to)){
    from <- c(2016, 01, 01)
    to <- to

  } else if(!is.null(from) & is.null(to)){
    from <- from
    to <- c(2023, 12, 31)
  } else if(!is.null(from) & !is.null(to)){
    from <- from
    to <- to
  }

  for (i in 1:length(x)) {
    cod_stn[[i]] <- x[i]
    idx.cod <- grep(as.character(cod_stn[[i]]), stations$cod)
    df_idx_stn <- data.frame(stations[idx.cod,], stringsAsFactors = FALSE)
    ts_date <- seq.Date(as.Date(paste0(from[1], "-",from[2], "-", from[3])),
                        as.Date(paste0(to[1], "-", to[2], "-", to[3])), "month")
    tsw_date <- gsub("-","",substr(ts_date, 1, 7))

    for (j in 1:length(ts_date)) {
      if(is.na(df_idx_stn$cod[j])){
        link <- paste0("https://www.senamhi.gob.pe//mapas/mapa-estaciones-2/_dato_esta_tipo02.php?estaciones=",
                       as.character(df_idx_stn$cod), #cod estacion
                       "&CBOFiltro=",
                       tsw_date[j], #fecha año y mes
                       "&t_e=",
                       as.character(df_idx_stn$ico), #ico
                       "&estado=",
                       as.character(df_idx_stn$estado),
                       "&cod_old=", as.character(df_idx_stn$cod_old)) #estado

      } else {
        link <- paste0("https://www.senamhi.gob.pe//mapas/mapa-estaciones-2/_dato_esta_tipo02.php?estaciones=",
                       as.character(df_idx_stn$cod), #cod estacion
                       "&CBOFiltro=",
                       tsw_date[j], #fecha año y mes
                       "&t_e=",
                       as.character(df_idx_stn$ico), #ico
                       "&estado=",
                       as.character(df_idx_stn$estado)) #estado
      }
      data_stn_senamhi <- read_html(link)
      scraping_web_senamhi[[j]] <- html_table(data_stn_senamhi)
      data_stn_senamhi[[j]] <- scraping_web_senamhi[[j]][[1]]
      data_history_senamhi[[j]] <- scraping_web_senamhi[[j]][[2]]
      data_df_history_senamhi[[j]] <- data.frame(data_history_senamhi[[j]])
      colnames(data_df_history_senamhi[[j]]) <- data_df_history_senamhi[[j]][1,]
      data_df_history_senamhi[[j]] <- data_df_history_senamhi[[j]][-c(1),]

    }
    df_history_senamhi[[i]] <- do.call("rbind", data_df_history_senamhi)
    write.xlsx(df_history_senamhi[[i]], paste0(cod_stn[[i]], "_", as.character(df_idx_stn$estacion),".xlsx"))
  }
  return(df_history_senamhi)
  print("desarrollado por Hydroprime")
}

