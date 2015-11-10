#' Create jitter plot of first detected date vs. last detected location
#'
#' @param data.frame
#' @param color
#'
#' @return data.frame
#' @export
#'
#' @examples
#' plotDetections(dataset)
plotDetections <- function(df,color="MonitorMod"){
  if(color=="release")ggplot2::ggplot(df,ggplot2::aes(date1,mon2,colour=release)) + ggplot2::geom_jitter() + ggplot2::xlab("First Detected Date") + ggplot2::ylab("Last Detected Location")
  else ggplot2::ggplot(df,ggplot2::aes(date1,mon2,colour=MonitorMod)) + ggplot2::geom_jitter() + ggplot2::xlab("First Detected Date") + ggplot2::ylab("Last Detected Location")
}

#' Create plot of arrivals at GRJ, aggregated by day
#'
#' @param data.frame
#' @param colour.scheme
#'
#' @return data.frame
#' @export
#'
#' @examples
#' plotArrivals(dataset)
plotArrivals <- function(df,group.surrogates=TRUE,type="area",scale.free="TRUE"){
  if(group.surrogates)df$release <- ifelse(df$smp.group=="Surrogate","GATE",as.character(df$release))
  sites <- dplyr::arrange(dplyr::summarise(dplyr::group_by(df,release),min.date=min(date1)),min.date)
  df <- dplyr::summarise(dplyr::group_by(df,smp.date1,release,rel.site),count=n())
  df$release <- factor(df$release,levels=sites$release)
    p <- ggplot2::ggplot(df,aes(smp.date1,count)) + xlab("GRJ Arrival Date") + ylab("Count")
    if(type=="area") p <- p + ggplot2::geom_area(aes(fill=rel.site))
    else p <- p + ggplot2::geom_line(aes(group=release))
    if(scale.free) p <- p + facet_grid(release ~ .,scale="free")
    else p <- p + facet_grid(release ~ .)
    p
}

#' Plot listed release date vs. PTAGIS release date
#'
#' @param data.frame
#' @param color
#'
#' @return data.frame
#' @export
#'
#' @examples
#' compareReleaseDates(dataset)
compareReleaseDates <- function(df,color="release"){
  if(color=="release")g <- ggplot2::ggplot(df,ggplot2::aes(release.date,rel.date,colour=release))
  else g <- ggplot2::ggplot(df,ggplot2::aes(release.date,rel.date,colour=rel.site))
  g + ggplot2::geom_point() + ggplot2::xlab("Listed Release Date") + ggplot2::ylab("PTAGIS Release Date")
}


#' Check Release Dates
#'
#' @param data.frame
#'
#' @return data.frame
#' @export
#'
#' @examples
#' checkReleaseDates(dataset)
checkReleaseDates <- function(df){
  df1 <- unique(dplyr::select(df,release,release.date))
  df1 <- dplyr::arrange(df1,release.date)
  df2 <- df1
  df1$release <- paste(getSmallDate(df1$release.date),df1$release)
  df$release <- paste(getSmallDate(df$release.date),df$release)
  df$release <- factor(df$release,levels=df1$release)
  ggplot2::ggplot(df,ggplot2::aes(release.date,date1,colour=release)) + ggplot2::geom_jitter() +  ggplot2::geom_point(data=df1,ggplot2::aes(release.date,release.date),shape=3,size=5,colour="black") + ggplot2::xlab("Release Date") + ggplot2::ylab("First Detected Date")
}

#' Plot sample rates
#'
#' @param data.frame
#' @param data.frame
#'
#' @return data.frame
#' @export
#'
#' @examples
#' plotSampleRates(dataset)
plotSampleRates <- function(sample.data,facet=FALSE){
  sdlong <- reshape2::melt(sample.data,id.vars=1,measure.vars=c(6,7,8,9),variable.name="rate.type")
  if(facet)ggplot(sdlong,aes(smp.date,value,colour=rate.type)) + geom_point() + facet_grid(rate.type ~ .)
  else ggplot(sdlong,aes(smp.date,value,colour=rate.type)) + geom_point()
}

#' Plot bar chart to check for potential matches for untagged releases
#'
#' @param releases
#' @param mode
#' @return NULL
#' @export
#'
#' @examples
#' plotMarkingTagging(releases)
plotMarkingTagging <- function(releases,mode="marking"){
    releases$tagged <- ifelse(releases$PIT.tagged!=0,"PIT-tagged","Not PIT-tagged")
    releases$Date <- lubridate::mdy(releases$Start)
    releases$release <- releases$name
    releases$Unmarked <- releases$released - releases$AD.only - releases$AD.CWT - releases$CWT.only
    if(mode=="marking"){
    rel.long <- melt(releases,id.vars=c("Date","tagged"),measure.vars=c("Unmarked", "AD.only","AD.CWT","CWT.only"),variable.name="marking",value.name="count")
    ggplot2::ggplot(rel.long,aes(Date,count,fill=marking)) + geom_bar(stat="identity") + facet_grid(tagged ~ .)
    }
  else{
    ggplot2::ggplot(releases,aes(Date,release,colour=tagged,size=released)) + geom_point()
  }
}

#' Plot Operational Dates
#'
#' @param ops
#' @param size
#'
#' @return NULL
#' @export
#'
#' @examples
#' ops <- read.csv("operations.csv")
#' plotOperations(ops)
plotOperations <- function(ops,size=5){
  ops$id=seq(1:nrow(ops))
  ops$start <- lubridate::ymd_hms(ops$start)
  ops$stop <- lubridate::ymd_hms(ops$stop)
  ops.long <- reshape2::melt(ops,id.vars=c("id","operation","message","reference"),measure.vars=c("start","stop"),variable.name="mode", value.name="date")
  ggplot2::ggplot(ops.long,aes(date,operation,group=id,colour=reference)) + geom_line(size=size)
}


