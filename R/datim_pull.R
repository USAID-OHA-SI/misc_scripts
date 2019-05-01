library(keyringr)
library(tidyverse)
library(httr)
library(jsonlite)


# Login -------------------------------------------------------------------

  myuser<-"blah"
  
  mypwd <- function() { 
    credential_label <- "blah"
    credential_path <- paste0(Sys.getenv("USERPROFILE"), 
                              '\\DPAPI\\passwords\\', Sys.info()["nodename"], 
                              '\\', credential_label, '.txt')
    pass <- decrypt_dpapi_pw(credential_path)
    return(pass)
  }
  
  baseurl<-"https://datim.org/"

# Data Elements -----------------------------------------------------------

  getDataElements<-function(url,username,password) {
    df <- URLencode(url) %>%
      GET(.,timeout(60),authenticate(username,password)) %>% 
      content(.,"text") %>% 
      fromJSON(.,flatten=TRUE) %>%
      pluck("dataElements") %>% 
      as_tibble()
    
    return(df)
  }

  #program areas of interest
    ind <- c("HTS_TST", "TX_NEW", "TX_CURR", "TX_PVLS", 
             "PrEP_NEW", "OVC_SERV", "AGYW_PREV", "TX_ML", 
             "TB_PREV")
  
  #list of data element ids
    urls<-paste0(baseurl,"api/dataElements?filter=name:like:",ind,"&paging=false")
    de_ids <- urls %>% 
      map_dfr(~ getDataElements(.x,myuser,mypwd())) %>% 
      filter(
        #TX_NEW, TX_CURR
        str_detect(displayName, "TX_(CURR|NEW).*(DSD|TA), (Age|Age Aggregated)/Sex/HIVStatus\\):") |
        #HTS_TST
        str_detect(displayName, "HTS_TST.*(DSD|TA), .*/(Age|Age Aggregated)/Sex/.*\\):") |
        #PrEP_NEW
        str_detect(displayName, "PrEP_NEW.*(DSD|TA), Age/Sex\\):") |
        #TX_ML
        str_detect(displayName, "TX_ML.*Age/Sex/ARTCauseofDeath") |
        #TX_TB (D/N)
        str_detect(displayName, "TB_PREV.*Age Aggregated/Sex/HIVStatus\\):")
      ) %>% 
      pull(id)

# Org Units ---------------------------------------------------------------

  #list of OU ids
    url<-paste0(baseurl,"api/organisationUnits?filter=level:eq:3")
    ou3_ids <- url %>% 
      #URLencode() %>%
      GET(.,timeout(60),authenticate(myuser,mypwd())) %>% 
      content(.,"text") %>% 
      fromJSON(.,flatten=TRUE) %>% 
      pluck("organisationUnits") %>% 
      pull(id)


# Analytics Pull Function -------------------------------------------------

  d2_analyticsResponse2 <- function(ou_id,data_elem_id, username,password,remapCols=TRUE) {
    url<-paste0(baseurl,"api/26/analytics.json?dimension=dx:", data_elem_id, "&dimension=pe:2018Q4;2019Q1&filter=ou:", ou_id,"&displayProperty=SHORTNAME&skipMeta=false")
    d <- url %>% 
      GET(authenticate(username,password)) %>%
      content("text") %>% 
      fromJSON()

    if ( NROW(d$rows) > 0 ) {
      metadata <- do.call(rbind,
                          lapply(d$metaData$items,
                                 data.frame, stringsAsFactors = FALSE)) %>% mutate(., from = row.names(.))
      remapMeta <-
        function(x) {
          plyr::mapvalues(x, metadata$from, metadata$name, warn_missing = FALSE)
        }
      
      d<-as_tibble(d$rows) %>% `names<-`(., d$headers$column)
      
      if(remapCols == TRUE) {
        d<-plyr::colwise(remapMeta)(d)
      }
      return(d) } else {
        return(NULL)
      }
  }

    
# Analytics Pull ----------------------------------------------------------   
    
  #create a full set of data element for each OU
    full_set <- 
      tibble(ou_id = rep(ou3_ids, length(de_ids)) %>% sort(),
             de_id = rep(de_ids, length(ou3_ids)))
      
  #test: pull TX_NEW (DSD) for TZA
    i <- 1546
    d2_analyticsResponse2(full_set$ou_id[i], full_set$de_id[i], myuser, mypwd())
    
  #full pull
    # df <- map2_dfr(full_set$ou_id, full_set$de_id,
    #                ~ d2_analyticsResponse2(.x, .y, myuser, mypwd()))
