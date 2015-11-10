#roxygen2::roxygenise()
#devtools::use_data(PTAGIS.IntSites,PTAGIS.MRRSites,overwrite=TRUE)

#' PTAGIS Interrogation Sites
#'
#' A dataframe with the PIT-tag Interrogation Sites
#'
#' @format A data frame with 336 rows and 18 variables:
#' \describe{
#'   \item{price}{price, in US dollars}
#'   \item{carat}{weight of the diamond, in carats}
#'   ...
#' }
#' @source \url{http://www.ptagis.org/}
"PTAGIS.IntSites"

#extract and rename PTAGIS.IntSites from PTAGIS
renameIntSites <- function(df){
  df <- select(df,Int.Site.Code,Int.Site.Description,Site.Type.Name,Interrogation.Site.RKM.Value,Interrogation.Site.RKM.Total,Latitude.Value,Longitude.Value,Interrogation.Site.Subbasin.Code)
  names(df) <- c("obs.site","obs.site.desc","obs.site.type","obs.site.rkm","obs.site.rkmtotal","obs.site.latitude","obs.site.longitude","obs.site.subbasin")
  return(df)
}

#' PTAGIS MRR Sites
#'
#' A dataframe with the PIT-tag MRR Sites
#'
#' @format A data frame with 336 rows and 18 variables:
#' \describe{
#'   \item{price}{price, in US dollars}
#'   \item{carat}{weight of the diamond, in carats}
#'   ...
#' }
#' @source \url{http://www.ptagis.org/}
"PTAGIS.MRRSites"

#extract and rename PTAGIS.MRRSites from PTAGIS
renameMRRSites <- function(df){
  df <- df[1:8,]
  names(df) <- c("rel.site","rel.site.desc","rel.site.rkm","rel.site.type","rel.site.subbasin","subbasin.name","rel.site.latitude","rel.site.longitude")
  return(df)
}
