#' Generic wrapper function to get JSON data from URL address
#'
#' @param URL a URL address
#' @param query API query
#'
#' @return A list containing the received data
#' @import httr
#' @import jsonlite
#'
# @examples
# get_data("http://api.kolada.se/v2/municipality", query="title=Linköping")
get_data <- function(URL,query){
  stopifnot(is.character(URL), is.character(query))
  stopifnot(!http_error(URL))

  http_data <- GET(URL,query=query)
  list_data <- fromJSON(rawToChar(http_data$content))
  return(list_data)
}

#' function to get municipality ID
#'
#' @param muni_name name of municipality
#'
#' @return the municipality ID
#'
# @examples
# get_muni_id("Linköping")
get_muni_id <- function(muni_name){
  stopifnot(is.character(muni_name))
  muni_list <- get_data("http://api.kolada.se/v2/municipality", query=paste("title=",muni_name,sep=""))
  muni_id <- muni_list$values$id
  return(muni_id)
}

#' function to get emission data for given municipality
#'
#' @param muni_name name of municipality
#'
#' @return data frame containing different emission data by year
#' @export
#'
#' @examples
#' get_figures("Linköping")
get_figures <- function(muni_name){
  stopifnot(is.character(muni_name))
  names_list <- list("N00401","N85078","N85072","N85075","N85077","N85073","N85076")
  names_str <- paste(names_list,collapse=",")
  muni_id <- get_muni_id(muni_name)
  figures <- get_data(paste("http://api.kolada.se/v2/data/kpi/",names_str,"/municipality/",muni_id[1],sep=""),"")

  figures_df <- data.frame(years=unique(figures$values$period))
  for(name in names_list){
    figures_df[name] <- c(NA)
  }

  for(i in 1:figures$count){
    figures_df[figures_df$years==figures$values$period[i],figures$values$kpi[i]] <- figures$values$values[[i]]$value
  }
  #print(figures_df)
  return(figures_df)
}

#' function to plot emission data of given municipality
#'
#' @param muni_name name of municipality
#'
#' @return a list of ggplots
#' @export
#' @import ggplot2
#' @import stats
#'
#' @examples
#' plot_figures("Linköping")
plot_figures <- function(muni_name){
  stopifnot(is.character(muni_name))
  plot_titles <- list(
    "Total greenhouse gas emissions to air, tonnes of CO2 equivalents per capita",
    "Greenhouse gas emissions to air, agriculture, tonnes of CO2 equivalents per capita",
    "Greenhouse gas emissions to air, heating, tonnes of CO2 equivalents per capita",
    "Greenhouse gas emissions to air, machinery, tonnes of CO2 equivalents per capita",
    "Greenhouse gas emissions to air, industry, tonnes of CO2 equivalents per capita",
    "Greenhouse gas emissions to air, transport, tonnes of CO2 equivalents per capita",
    "Greenhouse gas emissions to air, electricity and district heating,\n tonnes of CO2 equivalents per capita"
  )
  df1 <- get_figures(muni_name)
  plotlist <- list()
  for(i in 1:length(names(df1[-1]))){
    temp_df <- na.omit(df1[1:(i+1)])

    plotlist[[i]] <- ggplot() +
      geom_col(data=temp_df, aes_string(x="years",y=names(df1[-1])[i]), fill = '#1070AA') +
      ylab("tonnes of CO2 equivalents per capita") +
      ggtitle(plot_titles[i])
  }

  return(plotlist)
}

