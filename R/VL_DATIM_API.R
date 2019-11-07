##  Project:  VL Coverage and Suppression
##  Author:   Aaron Chafetz, USAID
##  Purpose:  API DATIM call to pull VL data 
##  Date:     2019-09-09

library(magrittr)

## DATIM API CALL FOR TARGETS
## Mechanism x Site

#https://www.datim.org/api/dimensions?filter=dimension:like:IeMmjHyBUpi


# Check for Packges that are in Suggests ----------------------------------

#' Check if package exists
#'
#' @param pkg package name
#'
#' @export

package_check <- function(pkg){
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste("Package", pkg, "needed for this function to work. Please install it."),
         call. = FALSE)
  }
}

# Login -------------------------------------------------------------------

#' Proper credentials from secure file
#'
#' @param username DATIM username
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  #see information about keyringr about setting up storing secure credentials
#'   myuser <- "UserX"
#'   mypwd(myuser) }

mypwd <- function(username) {
  
  package_check("keyringr")
  
  credential_label <- username
  credential_path <- paste0(Sys.getenv("USERPROFILE"),
                            '\\DPAPI\\passwords\\', Sys.info()["nodename"],
                            '\\', credential_label, '.txt')
  pass <- keyringr::decrypt_dpapi_pw(credential_path)
  return(pass)
}



# OU UIDs -----------------------------------------------------------------

#' Pull OU UIDS
#'
#' @param baseurl base url for the API, default = https://final.datim.org/
#' @param username DATIM Username
#' @param password DATIM password, recommend using `mypwd()`
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  ous <- identify_ouuids() }

identify_ouuids <- function(username, password, baseurl = "https://final.datim.org/"){
  
  package_check("httr")
  package_check("jsonlite")
  
  baseurl %>%
    paste0("api/organisationUnits?filter=level:eq:3") %>%
    httr::GET(httr::authenticate(username,password)) %>%
    httr::content("text") %>%
    jsonlite::fromJSON(flatten=TRUE) %>%
    purrr::pluck("organisationUnits")
  
}


# Identify Levels ---------------------------------------------------------

#' Identify Facility/Community levels in org hierarchy
#'
#' @param ou operating unit name
#' @param type extraction type, eg facility, community
#' @param username DATIM username
#' @param password DATIM password, recommend using `mypwd()`
#' @param baseurl base API url, default = https://final.datim.org/
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  #table for all OUs
#'   myuser <- "UserX"
#'   identify_levels(username = myuser, password = mypwd())
#'  #table for just Kenya
#'    identify_levels("Kenya", username = myuser, password = mypwd())
#'  #facility level of Kenya
#'    identify_levels("Kenya", "facility", username = myuser, password = mypwd()) }

identify_levels <- function(ou = NULL, type = NULL, username, password, baseurl = "https://final.datim.org/"){
  
  package_check("httr")
  package_check("jsonlite")
  
  df_levels <- baseurl %>%
    paste0(.,"api/dataStore/dataSetAssignments/ous") %>%
    httr::GET(httr::authenticate(username,password)) %>%
    httr::content("text") %>%
    jsonlite::fromJSON(flatten=TRUE) %>%
    purrr::map_dfr(dplyr::bind_rows) %>%
    dplyr::mutate_if(is.character, ~ dplyr::na_if(., ""))
  
  #adjust for regional missions
  df_levels <- df_levels %>%
    dplyr::mutate(country_name = ifelse(is.na(name4), name3, name4))
  
  if(!is.null(ou))
    df_levels <- dplyr::filter(df_levels, country_name == ou)
  
  if(!is.null(ou) && !is.null(type))
    df_levels <- dplyr::pull(df_levels, type)
  
  return(df_levels)
}


# Pull Target Data from DATIM ---------------------------------------------

#' DATIM API Call for Targets
#'
#' @param url supply url forAPI call, recommend using`gen_url()`
#' @param username DATIM username
#' @param password DATIM password, recommend using `mypwd()`
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  myurl <- paste0(baseurl,
#'                  "api/29/analytics.json?dimension=LxhLO68FcXm:udCop657yzi&dimension=ou:LEVEL-4;HfVjCurKxh2&filter=pe:2018Oct&displayProperty=SHORTNAME&outputIdScheme=CODE")
#'  myuser <- "UserX"
#'  df_targets <- get_datim_targets(myurl, myuser, mypwd(myuser)) }

get_datim_targets <- function(url,username,password) {
  
  package_check("httr")
  package_check("jsonlite")
  
  json <- url %>%
    httr::GET(httr::authenticate(username,password)) %>%
    httr::content("text") %>%
    jsonlite::fromJSON()
  
  if ( NROW(json$rows) > 0 ) {
    metadata <- purrr::map_dfr(json$metaData$items, dplyr::bind_rows, .id = "from")
    
    df <- tibble::as_tibble(json$rows, .name_repair = ~ json$headers$column)
    
    orguids <- df$`Organisation unit`
    
    if(stringr::str_detect(url, "hierarchyMeta=true")){
      
      orgpath <- dplyr::bind_rows(json$metaData$ouHierarchy) %>%
        tidyr::gather()
      
      levels <- orgpath$value %>%
        stringr::str_count("/") %>%
        max() + 1
      
      headers <- paste0("orglvl_", seq(1:levels))
      
      df <- dplyr::left_join(df, orgpath, by = c("Organisation unit" = "key")) %>%
        tidyr::separate(value, headers, sep = "/")
    }
    
    
    df <- df %>%
      dplyr::mutate_all(~plyr::mapvalues(., metadata$from, metadata$name, warn_missing = FALSE)) %>%
      dplyr::mutate(Value = as.numeric(Value)) %>%
      dplyr::bind_cols(orgunituid = orguids)
    
    return(df)
    
  } else {
    
    return(NULL)
    
  }
}

# Compile URL -------------------------------------------------------------

#' Generate a API URL
#'
#' @param ou_uid UID for the country, recommend using `identify_ouuids()`
#' @param org_lvl org hierarchy level, eg facility is level 7 in country X, recommend using `identify_levels()`
#' @param curr_fy current fiscal year, default = 2019
#' @param baseurl API base url, default = https://final.datim.org/
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  #get OU UID
#'   ouuid <- identify_ouuids() %>% dplyr::filter(ou == "Ghana")
#'  #get facility level
#'   faclvl <- identify_levels("Ghana", "facility", username = myuser, password = mypwd())
#'  #gen url
#'   myurl <- gen_url(mechuid, ouuid, faclvl, org_type = facility, type_hts = NULL) }

gen_url <- function(ou_uid, org_lvl, curr_fy = 2018, baseurl = "https://final.datim.org/"){
  
  fy_pd <- paste0(curr_fy-1, "Oct")
  
  core_url <-
    paste0(baseurl,"api/29/analytics?",
           "dimension=pe:LAST_4_QUARTERS&", #period
           "dimension=ou:LEVEL-", org_lvl, ";", ou_uid, "&", #level and ou
           "dimension=bw8KHXzxd9i&", #Funding Agency
           "dimension=SH885jaRe0o&", #Funding Mechanism
           "dimension=BOyWrF33hiR&", #Implementing Partner
           "dimension=IeMmjHyBUpi:Jh0jDM5yQ2E&", #Targets / Results -> targets = W8imnja2Owd, results = Jh0jDM5yQ2E
           "dimension=LxhLO68FcXm:MvszPTQrUhy;bZOF8bon1dD&", #technical area -> TX_CURR, TX_PVLS
           "dimension=RUkVjD3BsS1&", #top level ie total numerator/denominator
           #"dimension=HWPJnUTMjEq:h0pvSVe1TYf;pxz2gGSIQhG;S0hz2Pfq6bw&", #Disaggregation Type -> Age Aggregated/Sex/HIVStatus, Age/Sex/HIVStatus, Service Delivery Area
           "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=true")
  
  return(core_url)
  
}


# Extract All Targets -----------------------------------------------------

#' Extract DATIM Results and Targets (DATIM API Call)
#'
#' @param ou_name Operating Unit name, if mechanism is not specified
#' @param username DATIM username
#' @param password DATIM password, recommend using `mypwd()`
#' @param baseurl API base url, default = https://final.datim.org/
#' @param folderpath_output folder path to store DATIM output, default = NULL
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  #ou targets
#'  mech_x_targets <- extract_datim(ou_name = "Namibia", username = myuser, password = mypwd(myuser))
#'  }

extract_datim <- function(ou_name = NULL,
                          username, password,
                          baseurl = "https://final.datim.org/",
                          folderpath_output = NULL){
  
  print(paste(ou_name, "API started at",  format(Sys.time(), "%T")))
  
  #identify reporting levels
  ou_info <- identify_levels(ou_name, username = username, password = password, baseurl = baseurl) %>%
    dplyr::left_join(identify_ouuids(username = username, password = password, baseurl = baseurl),
                     by = c("country_name" = "displayName"))
  ou_fac <- ou_info$facility
  ou_psnu <- ou_info$prioritization
  ou_uid <- ou_info$id
  
  #pull non-HTS data (vars only facility)
  df_pull <-
    gen_url(ou_uid, ou_fac, baseurl = baseurl) %>%
    get_datim_targets(username, password)
  
  #clean up orgunits, keeping just OU, PSNU, Community and Facility
  df_pull <- df_pull %>%
    dplyr::rename(facility = `Organisation unit`,
                  operatingunit = orglvl_3,
                  snu1 = orglvl_4)
  
  if(!!paste0("orglvl_", ou_psnu) %in% names(df_pull)){
    df_pull <- dplyr::rename(df_pull, psnu = !!paste0("orglvl_", ou_psnu))
  } else {
    df_pull <- dplyr::mutate(df_pull, psnu = snu1)
  }
  
  df_pull <- df_pull %>%
    dplyr::select(facility, orgunituid, dplyr::everything()) %>%
    dplyr::select(-dplyr::starts_with("orglvl_"))
  
  #clean variables and variable names
  df_pull <- df_pull %>%
    dplyr::mutate(qtr = dplyr::case_when(stringr::str_detect(Period, "Oct") ~  "Q1",
                                         stringr::str_detect(Period, "Jan") ~  "Q2",
                                         stringr::str_detect(Period, "Apr") ~  "Q3",
                                         stringr::str_detect(Period, "Jul") ~  "Q4"),
                  fy = dplyr::case_when(stringr::str_detect(Period, "Oct") ~ paste0("FY", as.numeric(stringr::str_sub(Period, -2))+1),
                                        TRUE ~ paste0("FY",stringr::str_sub(Period, -2)))) %>%
    tidyr::unite(pd, fy, qtr, sep = "") %>%
    dplyr::rename(fundingagency = `Funding Agency`, mech_name = `Funding Mechanism`,
                  primepartner = `Implementing Partner`, indicator = `Technical Area`,
                  standardizeddisaggregate =`Top Level`, type = `Targets / Results`) %>%
    tibble::add_column(mech_code = as.character(NA), .before = "mech_name") %>%
    dplyr::mutate(indicator = ifelse(standardizeddisaggregate == "Top Level Denominator", 
                                     paste0(indicator, "_D"), indicator),
                  mech_code = stringr::str_extract(mech_name, "^[:alnum:]{5,6}"),
                  mech_name = stringr::str_remove(mech_name, "^[:alnum:]{5,6} - "),
                  type = stringr::str_replace(type, " ", "_")) %>%
    dplyr::group_by_if(is.character) %>%
    dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(operatingunit, snu1, psnu, facility, orgunituid,
                  fundingagency, mech_code, mech_name, primepartner,
                  pd, type, indicator, Value) %>%
    tidyr::spread(indicator, Value) %>% 
    dplyr::arrange(psnu, orgunituid, pd)
  
  df_prod <- df_pull %>% 
    tibble::add_column(TX_CURR_2Qprior = as.double(NA), .after = "TX_CURR") %>%
    dplyr::group_by(orgunituid, mech_code) %>% 
    dplyr::mutate(TX_CURR_2Qprior = lag(TX_CURR, 2)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-TX_CURR) %>% 
    dplyr::mutate(coverage = TX_PVLS_D/TX_CURR_2Qprior
                  #coverage = ifelse(pd == "FY19Q3", TX_PVLS_D/TX_CURR_2Qprior, NA
                  )
  
  #export
  if(!is.null(folderpath_output)){
    
    filename <- paste0("DATIM_API_", ou_name, "_", format(Sys.Date(),"%Y%m%d"), ".txt")
    
    readr::write_tsv(df_prod, file.path(folderpath_output, filename), na = "")
  }
  
  invisible(df_prod)
  
}


# RUN ---------------------------------------------------------------------

myuser <- ""

path <- ""

ous <- c("Botswana", "Burundi", "Cameroon", "Cote d'Ivoire", 
         "Democratic Republic of the Congo", "Eswatini", "Ethiopia", 
         "Haiti", "Kenya", "Lesotho", "Malawi", "Mozambique", "Namibia", 
         "Nigeria", "South Africa", "South Sudan", "Tanzania", "Uganda", 
         "Zambia", "Zimbabwe")


df_api <- purrr::map_dfr(.x = ous, 
                         .f = ~ extract_datim(.x, myuser, mypwd(myuser))) 


df_api %>% 
  dplyr::select(-type) %>% 
  dplyr::filter(stringr::str_detect(pd, "FY19")) %>% 
  readr::write_csv(file.path(path, paste0("DATIM_API_GLOBAL_SITE_VL_", format(Sys.Date(),"%Y%m%d"), ".csv")), na = "")

