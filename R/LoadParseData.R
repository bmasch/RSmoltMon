


#' Convert SAS datetime string to R posix Datetime
#'
#' @param String
#'
#' @return Posix Datetime
#' @export
#'
#' @examples
#' parseSASDatetime("09DEC2012:01:29:11")
parseSASDatetime <- function(sdate){
  #require(lubridate)
  months <- list()
  months["JAN"] <- "01"
  months["FEB"] <- "02"
  months["MAR"] <- "03"
  months["APR"] <- "04"
  months["MAY"] <- "05"
  months["JUN"] <- "06"
  months["JUL"] <- "07"
  months["AUG"] <- "08"
  months["SEP"] <- "09"
  months["OCT"] <- "10"
  months["NOV"] <- "11"
  months["DEC"] <- "12"
  day <- substr(sdate,1,2)
  month <- months[substr(sdate,3,5)]
  year <- substr(sdate,6,9)
  hour <- substr(sdate,11,12)
  minute <- substr(sdate,14,15)
  second <- substr(sdate,17,18)
  date <- paste(year,month,day,sep="-")
  time <- paste(hour,minute,second,sep=":")
  return(lubridate::ymd_hms(paste(date,time)))
}

#' Parse CSV output from PTAGIS query
#'
#' @param data.frame
#'
#' @return data.frame with simplified variable names and parsed dates
#' @export
#'
#' @examples
#' parseDetections(df)
parseDetections <- function(df){
  data(PTAGIS.species)
  data(GRJAntennaGroups)
  names(df) <- c("Sortid1","tag","mfile","release","smp.group","rel.site","SRRCode","SRRName","MonitorMod","Remove","Fraction","nDetLT1day","mon1","ant1","date1","smp.date1","mon2","ant2","date2","smp.date2","mon3","ant3","date3","smp.date3","mon4","ant4","date4","smp.date4","nDetGT1day","MarkYear","ObsYear","Mnum","YearDiff","DOY","Sortid2")
  df$date1 <- parseSASDatetime(df$date1)
  df$date2 <- parseSASDatetime(df$date2)
  df$smp.date1 <- lubridate::mdy(df$smp.date1)
  df$smp.date2<- lubridate::mdy(df$smp.date2)
  #put last detected sites in order for visualizations
  df$mon2 <- factor(df$mon2, levels=rev(GRJAntennaGroups$antenna_group))
  df <- merge(df,PTAGIS.species)
  return (df)
}

#' Extract subyearling Fall Chinook from full detection dataset
#'
#' @param parsed data.frame
#'
#' @return data.frame
#' @export
#'
#' @examples
#' subs <- getSubyearlings(dataset)
getSubyearlings <- function(df){
  return (dplyr::filter(df,MarkYear==lubridate::year(date1),release != "", release != "YEARLING"))
}

#' Extract Yearling Fall Chinook from full detection dataset
#'
#' @param data.frame
#'
#' @return data.frame
#' @export
#'
#' @examples
#' yls <- getYearlings(dataset)
getYearlings <- function(df){
  return (dplyr::filter(df,release == "YEARLING"))
}



