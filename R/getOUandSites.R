
##  Get hierarchy and sites location by OU
##  G. Sarfaty & A. Chafetz
##  Date: 2018.05.03
##  Purpose: pull hierarchy for an org unit + site location


##  Dependencies
library(devtools)
install_github("jason-p-pickering/datim-validation")
library(datimvalidation)
library(tidyverse)

#initialize loadSecrets & login to API; enter username & password; then URL when prompted
loadSecrets(secrets = NA)

##use pickering's code to pull hierarchy, referencing the OU of interest by its UID; Ex is SA
df <- getOrganisationUnitMap("cDGPF739ZZr")

#create function to pull site location; 
getSites <-function(organisationUnit=NA,level=NA) {
  if ( is.na(organisationUnit) ) { organisationUnit<-getOption("organisationUnit") }
  url<-URLencode(paste0(getOption("baseurl"),"api/organisationUnits.json?&filter=path:like:",organisationUnit,"&fields=id,code,name,coordinates&paging=false&filter=level:eq:",level))
  sig<-digest::digest(url,algo='md5', serialize = FALSE)
  sites<-getCachedObject(sig)
  if (is.null(sites)){
    r<-httr::GET(url,httr::timeout(600))
    if (r$status == 200L ){
      r<- httr::content(r, "text")
      sites<-jsonlite::fromJSON(r,flatten=TRUE)[[1]]
      saveCachedObject(sites,sig)
    } else {
      print(paste("Could not retreive site listing",httr::content(r,"text")))
      stop()
    }
  }
  
  return( sites )
}



##Call new GetSites function to get site locations as df; must reference OU of interest by UID and site level by #
df2<-getSites("nBo9Y4yZubB",7)
