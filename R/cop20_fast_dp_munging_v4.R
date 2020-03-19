##  instructions for recreating ER 'dashboard dataset' using COP20 Datapack and FAST tool
##  This requires a few steps from the user

##Load required programs
install.packages("devtools")
devtools::install_github("USAID-OHA-SI/tameDP")
devtools::install_github("USAID-OHA-SI/fastR")

#step 1: filepath to the FAST tool
fast <- "C:/Users/Josh/Documents/data/fy20_q1_v1/COP20_tools/Cote_d'Ivoire_COP20_FAST_V4_feb19.xlsx"

#Step 2: filepath to the Datapack
datapack <- "C:/Users/Josh/Documents/data/fy20_q1_v1/COP20_tools/Data Pack_Cote d'Ivoire_20200214103356.xlsx"

#step 3: filpath to the dashboard dataset
dataset <- "C:/Users/Josh/Documents/data/fy20_q1_v1/COP20_tools/Budget.ER.MER Clean Post FY 19 1.18.20v2.xlsx"

#Step 4: folderpath where you want the .csv output to be saved
folderpath_output <- "C:/Users/Josh/Documents/data/fy20_q1_v1/COP20_tools"

#Step 5: add required library
library(magrittr)

#Step 6: Load the function
prep_data <- function(fast, datapack, dataset, folderpath_output) {

  ##start with FAST

  df_fast <- fastR::run_fastR_cop20(fast, "2 Intervention-E")

  df_fast <- df_fast %>%
    dplyr::select(-`int#`) %>%
    dplyr::rename(`Funding Agency` = fundingagency,
                  `Mechanism Name` = mechanismname,
                  `Partner Name` = primepartner,
                  `Partner Org Type` = orgtype,
                  `Mechanism ID` = mech_code,
                  `Mechanism Name` = mechanismname,
                  `Program Area` = program,
                  `Sub-Program Area` = programarea,
                  `Service Delivery` = servicedelivery,
                  `Sub-Beneficiary` = subbeneficiary,
                  Beneficiary = beneficiary,
                  `OU (COP 2019)` = ou,
                  Budget = amt) %>%
    dplyr::filter(cop == "COP20",
                  amt_type == "Budget")

  df_fast <- df_fast %>%
    dplyr::mutate(`Fiscal Year` = "FY 21",
                  `Data Stream` = "Budget/Expenditure",
                  `Local Partner Type` = "Unknown",
                  Country = `OU (COP 2019)`,
                  `OU (Pre-COP 2019)` = `OU (COP 2019)`) %>%
    dplyr::select(-cop, -amt_type)

  ## create ou
  operatingunit <- unique(df_fast$`OU (COP 2019)`)


  ## datapack starts here

  df_dp <- tameDP::tame_dp(datapack)

  df_dp <- df_dp %>%
    dplyr::filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_NEW", "VMMC_CIRC", "OVC_SERV"),
                  disaggregate != "KeyPop/Result") %>%
    dplyr::group_by(operatingunit, fundingagency, mech_code, primepartner, mech_name, indicator, fiscal_year) %>%
    dplyr::summarise(targets = sum(targets, na.rm = TRUE))
    dplyr::ungroup()

  df_dp <- df_dp %>%
    dplyr::rename(`OU (COP 2019)` = operatingunit,
                  `Funding Agency` = fundingagency,
                  `Mechanism ID` = mech_code,
                  `Partner Name` = primepartner,
                  `Mechanism Name` = mech_name,
                  `Fiscal Year` = fiscal_year,
                  Indicator = indicator,
                  Target = targets) %>%
    dplyr::mutate(`OU (Pre-COP 2019)` = `OU (COP 2019)`,
                  `Target Result` = "Target",
                  `Fiscal Year` = "FY 21",
                  `Data Stream` = "MER",
                  Country = `OU (COP 2019)`) %>%
    dplyr::mutate(`Program Area` = dplyr::case_when(Indicator == "HTS_TST_POS" ~ "HTS",
                                                    Indicator == "HTS_TST" ~ "HTS",
                                                    Indicator == "TX_CURR" ~ "C&T",
                                                    Indicator == "TX_NEW" ~ "C&T",
                                                    Indicator == "VMMC" ~ "PREV"))


  ## read in and clean up dashboard dataset
  data <- readxl::read_excel(dataset) %>%
    dplyr::filter(Country == operatingunit) %>%
    dplyr::mutate(`Mechanism ID` = as.character(`Mechanism ID`),
                Budget = as.numeric(Budget),
                Target = as.numeric(Target),
                Result = as.numeric(Result))

  ## now bind them together

  df <- dplyr::bind_rows(df_dp, df_fast, data)

  ##write file to output folder

  filename <- paste0("merged_er_dp_", operatingunit,"_", format(Sys.Date(), "%Y.%m.%d"), ".csv")

  readr::write_csv(df, file.path(folderpath_output, filename))

  return(df)

}

#Step 7: run the function
df <- prep_data(fast, datapack, dataset, folderpath_output)









