#' Returns a set of tags, one from each mark file.
#'
#'
#'
#' @param smp
#'
#' @return A dataframe with one column, one tag from each mark file.
#' @export
#'
#' @examples
#' mfile.tags <- getMarkFileFish(data)
getMarkFileFish <- function(smp){
  fish <- dplyr::summarise(dplyr::group_by(smp,mfile),date1=min(date1))
  fish <- merge(fish,smp)
  return(dplyr::select(fish,tag))
}

#' Get surrogates (i.e. GATE1 etc.).
#'
#'
#'
#' @param data.frame
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' surrogates <- surrogates(data)
surrogates <- function(df,keep=TRUE){
  if(keep)return(dplyr::filter(df,smp.group=="Surrogate"))
  else return(dplyr::filter(df,smp.group!="Surrogate"))
}

#' Add release date to detections.
#'
#' @param data.frame
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' detections.plus.release <- addReleaseDate(detections,releases)
#' detections.plus.release2 <- addReleaseDate(detections,releases,PTAGIS=TRUE)
addReleaseDate <- function(df,release.data,PTAGIS=FALSE){
  #PTAGIS==TRUE
  #merge detections with dataframe of mark file name and release date
  if(PTAGIS){
    releases <- data.frame(mfile=release.data$Mark.File.Name,rel.date=mdy(release.data$Release.Date.MMDDYYYY))
  }
  else{
    releases <- data.frame(release=release.data$name,release.date=lubridate::mdy(release.data$Start))
  }
  return(merge(df,releases))
}


#' Get sample data
#'
#' @param detections
#' @param smp
#'
#' @return data.frame
#' @export
#'
#' @examples
#' srates <- getSampleData(detections,smp)
getSampleData <- function(detections,smp){
  detections <- dplyr::filter(detections,mon2 != "ADULT FISH RETURN")
  total <- dplyr::summarise(dplyr::group_by(detections,smp.date1),total=n())
  yes.total <- dplyr::summarise(dplyr::group_by(dplyr::filter(detections,MonitorMod=="Y"),smp.date1),yes.total=n())
  no.total <- dplyr::summarise(dplyr::group_by(dplyr::filter(detections,MonitorMod=="N"),smp.date1),no.total=n())
  yes.sample <- dplyr::summarise(dplyr::group_by(dplyr::filter(detections,mon2=="SAMPLE TANK" | mon2 == "SUBSAMPLE", MonitorMod=="Y"),smp.date1),yes.sample=n())
  no.sample <- dplyr::summarise(dplyr::group_by(dplyr::filter(detections,mon2=="SAMPLE TANK" | mon2 == "SUBSAMPLE", MonitorMod=="N"),smp.date1),no.sample=n())
  rates <- merge(yes.sample,no.sample,all.x=TRUE,all.y=TRUE)
  rates <- merge(rates,yes.total,all.x=TRUE,all.y=TRUE)
  rates <- merge(rates,no.total,all.x=TRUE,all.y=TRUE)
  rates[is.na(rates)] <- 0
  rates$yes.rate <- rates$yes.sample/rates$yes.total
  rates$no.rate <- rates$no.sample/rates$no.total
  rates$total.rate <- (rates$yes.sample+rates$no.sample)/(rates$yes.total+rates$no.total)
  names(rates)[1] <- "smp.date"
  names(smp)[1] <- "smp.date"
  smp$smp.date <- lubridate::mdy(smp$smp.date)
  sample <- merge(rates,smp,all.x=TRUE)
  sample[is.na(sample)] <- 0
  return(sample)
}

#' Parse smp file
#'
#' @param smp
#'
#' @return data.frame
#' @export
#'
#' @examples
#' smp <- parseSMP(smp)
parseSMP <- function(smp){
  names(smp)[1] <- "smp.date"
  smp$smp.date <- lubridate::mdy(smp$smp.date)
  return(smp)
}

#' Get month and day from a date
#'
#' @param date
#'
#' @return string
#' @export
#'
#' @examples
#' getSmallDate(lubridate::ymd("2012-05-01"))
getSmallDate <- function(date){
  return(paste(lubridate::month(date),lubridate::day(date),sep="/"))
}






