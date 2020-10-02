# Generate cleaned Public Use File for Data Catalog and use in feature
library(tidyverse)
library(readxl)
library(testit)
library(tigris)
library(stringr)
library(tidyverse)
library(httr)
library(here)

###### Notes on Changes Needed #########
# - done- add new vars
# - check to confirm var names unchanged
# - double check denominators where new options added (e.g. spend vars need to all add spndsrc8 >= 0 to denom)
# - done- remove class_cancelled and stimulus_expenses
# - done- update var names/definitions for rent_not_paid and mortgage_not_paid
# - done- update data dictionary

download_and_clean_puf_data <- function(week_num, output_filepath = "data/raw-data/public_use_files/") {
  # Function to download in Pulse Public Use File for a given week, and add:
  #   1) cleaned Non Hispanic Race variable, (hisp_rrace)
  #   2) MSA and state names (cbsa_title  and state_name)
  #   3) a week number variable (week_num)
  #   4) indicator variable for uninsured persons (uninsured)
  #   5) indicator variable for publicly insured persons (insured_public)
  #   6) indicator variable for if a person has experienced loss in employment income (inc_loss)
  #   7) indicator variable for if a person expects a loss in employment income (expect_inc_loss)
  #   8) indicator variable for if a person has low to no confidence in paying rent next month or has already deferred (rent_not_conf)
  #   9) indicator variable for if a person has low to no confidence in paying mortgage next month or has already deferred (mortgage_not_conf)
  #   10)joint indicator variable for if a person has either 8 or 9, (payment_not_conf)
  #   11)adjusted score columns for the mental health questions
  #   12)indicator variable for if a person displays signs of anxiety (anxiety_signs)
  #   13)indicator variable for if a person displays signs of depression (depression_signs)
  #   14) indicator variable for if a person did not pay rent last month or has already deferred (rent_not_paid)
  #   15) indicator variable for if a person did not pay mortgage last month or has already deferred (mortgage_not_paid)
  #   16) indicator variable for if a person that received stimulus payment used it on expenses (stimulus_expenses)
  #   17) indicator variable for if a person used credit cards or loans to meet spending needs in past 7 days (spend_credit)
  #   18) indicator variable for if a person used savings to meet spending needs in past 7 days (spend_savings)
  #   19) indicator variable for if a person used UI benefits to meet spending needs in past 7 days (spend_ui)
  #   20) indicator variable for if a person used stimulus payment to meet spending needs in past 7 days (spend_stimulus)


  # INPUT:
  #   week_num (num): week number of Pulse survey (ie 1, 2,... 12, etc). Can be a vector if
  #     you want to pull data for multiple weeks
  #   output_filepaths (chr): Output folder where puf file and data dictionary
  #     be written to
  # OUPUT:
  #   df_clean: cleaned pubilc use file
  #   This fxn will also write out the raw downlaoded public use files into the data/raw-data directory

  week_num_padded <- str_pad(week_num, width = 2, side = "left", pad = "0")


  puf_url <- str_glue("https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk{week_num}/HPS_Week{week_num_padded}_PUF_CSV.zip")

  # Create public_use_files directory if it doesn't exist
  dir.create("data/raw-data/public_use_files/", showWarnings = F)

  # Download zip file
  if(!file.exists(str_glue("data/raw-data/public_use_files/week_{week_num_padded}.zip"))){
    download.file(puf_url,
                  destfile = str_glue("data/raw-data/public_use_files/week_{week_num_padded}.zip"),
                  # By default uses winnet method which is for some reason very slow
                  method = "libcurl"
    )
    
  }
  
  # Unzip PUF, data dictionary files, and repweight file
  unzip(str_glue("data/raw-data/public_use_files/week_{week_num_padded}.zip"),
    exdir = "data/raw-data/public_use_files",
    # extract PUF file and data dictionary
    files = c(
      str_glue("pulse2020_puf_{week_num_padded}.csv"),
      str_glue("pulse2020_data.dictionary_CSV_{week_num_padded}.xlsx"),
      str_glue("pulse2020_repwgt_puf_{week_num_padded}.csv")
    )
  )

  # Get MSA FIPS Codes for appending later
  fips_msa_url <- "https://query.data.world/s/vn4chhniqhslgt5fpb7swxbkcsq3oj"
  GET(fips_msa_url, write_disk(tf <- tempfile(fileext = ".xls")))
  msa_fips_codes <- read_excel(tf, skip = 2) %>%
    select("CBSA Code", "CSA Title", "CBSA Title") %>%
    # There seem to be some duplicate entries, so we remove them
    distinct(.keep_all = TRUE)

  ### Read in PUF file and weights file
  puf_filepath <- str_glue("{output_filepath}pulse2020_puf_{week_num_padded}.csv")
  rep_wt_filepath <- str_glue("{output_filepath}pulse2020_repwgt_puf_{week_num_padded}.csv")
  df <- read_csv(puf_filepath)
  rep_wt <- read_csv(rep_wt_filepath) %>%
    janitor::clean_names()

  # add missing variables for weeks before 7
  if (week_num < 7) {
    df <- df %>%
      mutate(eip = NA_real_,
             spndsrc1 = NA_real_,
             spndsrc2 = NA_real_,
             spndsrc3 = NA_real_,
             spndsrc4 = NA_real_,
             spndsrc5 = NA_real_,
             spndsrc6 = NA_real_,
             spndsrc7 = NA_real_)
  }  


  df_clean <- df %>%
    janitor::clean_names() %>%
    ### Append Urban specific columns
    mutate(
      # Generate hispanic/non-hispanic race variables
      hisp_rrace = case_when(
        rrace == 1 ~ "White alone, not Hispanic",
        rrace == 2 ~ "Black alone, not Hispanic",
        rrace == 3 ~ "Asian alone, not Hispanic",
        rrace == 4 ~ "Two or more races + Other races, not Hispanic",
        TRUE ~ NA_character_
      ),
      # Note: hispanic ethnicity overrides all other race variables
      hisp_rrace = case_when(
        rhispanic == 1 ~ hisp_rrace,
        rhispanic == 2 ~ "Hispanic or Latino (may be of any race)",
        TRUE ~ hisp_rrace
      ),
      # Dummy var for uninsured respondents
      uninsured = case_when(
        # Note the order of these case_when statements matter
        # First if they have any of the first 6 healath insurance options, they are insured (0)
        hlthins1 == 1 |
          hlthins2 == 1 |
          hlthins3 == 1 |
          hlthins4 == 1 |
          hlthins5 == 1 |
          hlthins6 == 1 ~ 0,
        # If they answer no to all 6 health insurance quetions, they are uninsured (1)
        (hlthins1 == 2 &
          hlthins2 == 2 &
          hlthins3 == 2 &
          hlthins4 == 2 &
          hlthins5 == 2 &
          hlthins6 == 2) ~ 1,
        # Note Census includes those who have Indian Health Service coverage as uninsured (1)
        hlthins7 == 1 ~ 1,

        TRUE ~ NA_real_
      ),
      # Dummy var for respondents with public insurance
      insured_public = case_when(
        # if they have medicare, medicaid, or VA insurance = 1
        hlthins3 == 1 | hlthins4 == 1 | hlthins6 == 1 ~ 1,

        # if they didn't answer any of the helath insurance questions& assign them NA
        hlthins1 == -99 & hlthins2 == -99 & hlthins3 == -99 &
          hlthins4 == -99 & hlthins5 == -99 & hlthins6 == -99 &
          hlthins7 == -99 & hlthins8 == -99 ~ NA_real_,

        hlthins1 == -88 & hlthins2 == -88 & hlthins3 == -88 &
          hlthins4 == -88 & hlthins5 == -88 & hlthins6 == -88 &
          hlthins7 == -88 & hlthins8 == -88 ~ NA_real_,
        # If they answered at least one question but were not publicly insured, mark as 0
        TRUE ~ 0
      ),
      # Dummy var for lost income in past 4 weeks? (1 = lost income, 0 = didn't lose income)
      inc_loss = case_when(
        wrkloss == 1 ~ 1,
        wrkloss == 2 ~ 0,
        TRUE ~ NA_real_
      ),
      # Dummy var for future Employment income loss
      expect_inc_loss = case_when(
        expctloss == 1 ~ 1,
        expctloss == 2 ~ 0,
        TRUE ~ NA_real_
      ),
      # Dummy var for not confident in housing payment next month? (1 = yes, 0 = no)
      payment_not_conf = case_when(
        # slight or no confidnece or payment already deferred = 1
        mortconf %in% c(1, 2, 5) ~ 1,
        # moderate or high confidence = 0
        mortconf %in% c(3, 4) ~ 0,
        TRUE ~ NA_real_
      ),
      # Dummy var for not confident in rent payment next month (1 = yes, 0 = no)
      rent_not_conf = case_when(
        # slight or no confidnece or payment already deferred = 1
        mortconf %in% c(1, 2, 5) & tenure == 3 ~ 1,
        # moderate or high confidence = 0
        mortconf %in% c(3, 4) & tenure == 3 ~ 0,
        TRUE ~ NA_real_
      ),
      mortgage_not_conf = case_when(
        # slight or no confidnece or payment already deferred = 1
        mortconf %in% c(1, 2, 5) & tenure == 2 ~ 1,
        # moderate or high confidence = 0
        mortconf %in% c(3, 4) & tenure == 2 ~ 0,
        TRUE ~ NA_real_
      ),
      # Dummy var caught up on rent (1 = yes, 0 = no)
      rent_caughtup = case_when(
        # did not pay on time or payment deferred = 1
        rentcur == 1 & tenure == 3 ~ 1,
        # paid on time = 0
        rentcur == 2 & tenure == 3 ~ 0,
        TRUE ~ NA_real_
      ),
      # Dummy var for caught up on mortage (1 = yes, 0 = no)
      mortgage_caughtup = case_when(
        # slight or no confidnece or payment already deferred = 1
        mortcur == 1 & tenure == 2 ~ 1,
        # moderate or high confidence = 0
        mortcur == 2 & tenure == 2 ~ 0,
        TRUE ~ NA_real_
      ),
      # Dummy var for Food Insufficient households
      food_insufficient = case_when(
        curfoodsuf %in% c(3, 4) ~ 1,
        curfoodsuf %in% c(1, 2) ~ 0,
        TRUE ~ NA_real_
      ),
      spend_credit = as.numeric(case_when(
        #set 1 if respondent answered they use credit cards or loans
        spndsrc2 == 1 ~ 1,
        # Set 0 if respondent answered atleast one of the spending questions
        (spndsrc1 >= 0 | spndsrc2 >= 0 | spndsrc3 >= 0 | spndsrc4 >= 0 | 
           spndsrc5 >= 0 | spndsrc6 >= 0 | spndsrc7 >= 0| spndsrc8 >= 0) ~ 0,
        # Set NA otherwise
        TRUE ~ NA_real_
      )),
      spend_savings = as.numeric(case_when(
        #set 1 if respondent answered they use savings or selling assets
        spndsrc3 == 1 ~ 1,
        # Set 0 if respondent answered at least one of the spending questions
        (spndsrc1 >= 0 | spndsrc2 >= 0 | spndsrc3 >= 0 | spndsrc4 >= 0 | 
           spndsrc5 >= 0 | spndsrc6 >= 0 | spndsrc7 >= 0| spndsrc8 >= 0) ~ 0,
        # Set NA otherwise
        TRUE ~ NA_real_
      )),
      spend_ui = as.numeric(case_when(
        #set 1 if respondent answered they use creditcards or loans
        spndsrc5 == 1 ~ 1,
        # Set 0 if respondent answered at least one of the spending questions
        (spndsrc1 >= 0 | spndsrc2 >= 0 | spndsrc3 >= 0 | spndsrc4 >= 0 | 
           spndsrc5 >= 0 | spndsrc6 >= 0 | spndsrc7 >= 0| spndsrc8 >= 0) ~ 0,
        # Set NA otherwise
        TRUE ~ NA_real_
      )),
      spend_stimulus = as.numeric(case_when(
        #set 1 if respondent answered they use creditcards or loans
        spndsrc6 == 1 ~ 1,
        # Set 0 if respondent answered atleast one of the child education questions
        (spndsrc1 >= 0 | spndsrc2 >= 0 | spndsrc3 >= 0 | spndsrc4 >= 0 | 
           spndsrc5 >= 0 | spndsrc6 >= 0 | spndsrc7 >= 0| spndsrc8 >= 0) ~ 0,
        # Set NA otherwise
        TRUE ~ NA_real_
      )),
      # Score variables for mental health qs. Note we use scoring scheme laid out here:
      # https://www.cdc.gov/nchs/covid19/pulse/mental-health.htm which requires recoding
      # the 4 mental health questions to thier specific scores. If sum of sets of 2 qs
      # are greater than 3, that means the respondent has a sign of anxiety or depression
      anxious_score = case_when(
        anxious == 1 ~ 0,
        anxious == 2 ~ 1,
        anxious == 3 ~ 2,
        anxious == 4 ~ 3,
        TRUE ~ NA_real_
      ),
      worry_score = case_when(
        worry == 1 ~ 0,
        worry == 2 ~ 1,
        worry == 3 ~ 2,
        worry == 4 ~ 3,
        TRUE ~ NA_real_
      ),
      interest_score = case_when(
        interest == 1 ~ 0,
        interest == 2 ~ 1,
        interest == 3 ~ 2,
        interest == 4 ~ 3,
        TRUE ~ NA_real_
      ),
      down_score = case_when(
        down == 1 ~ 0,
        down == 2 ~ 1,
        down == 3 ~ 2,
        down == 4 ~ 3,
        TRUE ~ NA_real_
      ),
      #YS PULSE 2 update- adding new vars
      #difficulty paying household expenses in past 7 days
      expense_dif= case_when(
      expns_dif >= 3 ~ 1,
      expns_dif %in% c(1, 2) ~ 0,
      TRUE ~ NA_real_
      ),
      #dummy for telework
      telework= case_when(tw_start == 1 ~ 1,
                          tw_start %in% c(2, 3) ~ 0,
                          TRUE ~ NA_real_
      ),
      #dummy for unmet need for mental health services in last 4 weeks
      mentalhealth_unmet= case_when(mh_notget == 1 ~ 1,
                                    mh_notget == 2 ~ 0,
                                    TRUE ~ NA_real_
      ),
      #dummy for eviction risk
      eviction_risk = case_when(evict %in% c(1, 2) ~ 1,
                                evict %in% c(3, 4) ~ 0,
                                TRUE ~ NA_real_
      ),
      #dummy for foreclosure risk
      foreclosure_risk = case_when(forclose %in% c(1, 2) ~ 1,
                                   forclose %in% c(3, 4) ~ 0,
                                   TRUE ~ NA_real_
      ),
      #dummy for Proportion of adults with children in school who spend fewer 
      #hours on learning activities in the past 7 days relative to before the pandemic
      learning_fewer= case_when(tch_hrs %in% c(1, 2) ~ 1,
                                tch_hrs >= 3 ~ 0,
                                TRUE ~ NA_real_
      ),
      #SNAP spending
      spend_snap = case_when(spndsrc8 == 1 ~ 1,
      (spndsrc1 >= 0 | spndsrc2 >= 0 | spndsrc3 >= 0 | spndsrc4 >= 0 | spndsrc5 >= 0 | spndsrc6 >= 0 | spndsrc7 >= 0 | spndsrc8 >= 0) ~ 0
      )
    ) %>%
    # Needed for rowwise sum calculations in anxiety_signs and depression_signs var
    rowwise() %>%
    mutate(
      # Dummy var for sign of anxiety (based on >=3 score)
      anxiety_signs = case_when(
        sum(anxious_score, worry_score, na.rm = T) >= 3 ~ 1,
        is.na(anxious_score) & is.na(worry_score) ~ NA_real_,
        TRUE ~ 0
      ),
      # Dummy var for sign of depression (based on >=3 score)
      depression_signs = case_when(
        sum(interest_score, down_score, na.rm = T) >= 3 ~ 1,
        is.na(interest_score) & is.na(down_score) ~ NA_real_,
        TRUE ~ 0
      )
    ) %>%
    ungroup() %>%
    mutate(
      # Dummy Var for any signs of anxiety/depression
      depression_anxiety_signs = case_when(
        # Set 1 if respondent has either anxiety signs or depression sigs
        anxiety_signs == 1 | depression_signs == 1 ~ 1,
        is.na(anxiety_signs == 1) & is.na(depression_signs) ~ NA_real_,
        TRUE ~ 0
      ),
      # Turn MSA column into character
      est_msa = as.character(est_msa),
      # Add week_number column
      week_num = paste0("wk", week_num)
    ) %>%
    ### Append full state names
    left_join(tigris::fips_codes %>% select(state, state_code, state_name) %>%
      distinct(state_code, .keep_all = TRUE),
    by = c("est_st" = "state_code")
    ) %>%
    ### Append MSA Names
    left_join(msa_fips_codes, by = c("est_msa" = "CBSA Code")) %>%
    ### Append Replicate Weights
    left_join(rep_wt, by = "scram") %>%
    janitor::clean_names()

  # Check that cleanded data has same number of rows as raw data
  assert("Cleaned df has same # of rows as raw data", nrow(df) == nrow(df_clean))

  return(df_clean)
}


CUR_WEEK <- 13
week_vec <- c(13:CUR_WEEK)

# Read in all PUF files for the specified weeks, and write out one big PUF file. There will be a column named
# week_num that differentiates microdata from each week.

puf_all_weeks <- map_df(week_vec, download_and_clean_puf_data)

# Create public_use_files directory if it doesn't exist
dir.create("data/intermediate-data", showWarnings = F)

write_csv(puf_all_weeks, str_glue("data/intermediate-data/pulse_puf2_week_13_to_{CUR_WEEK}.csv"))
# Write out most recent CSV
write_csv(puf_all_weeks, here("data/intermediate-data", "pulse_puf2_all_weeks.csv"))


# Manually generate and write out data dictionary for appended columns
appended_column_data_dictionary <-
  tibble::tribble(
    ~col_name, ~description,
    "hisp_rrace", "Combination of Hispanic and Race column. Groups respondents into the following categories: Hispanic, White non Hispanic, Black non Hispanic, Asian non Hispanic, and Other race/two or more races",
    "uninsured", "Indicator variable for if a respondent is uninsured. This is 1 if the respondent reported that they have none of the available insurnace ooptions or only have insurance through the Indian Health Service. It is 0 if the respondents have some type of health insurance (excluding the Indian Health service)",
    "insured_public", "Indicator variable for if a respondent has public insurance. This is 1 if the respondent reported thaty had Medicare, Medicaid, or VA Health Insurance",
    "inc_loss", "Indicator variable for if a respondent (or anyone in their houshold) experienced a loss in employment income since March 13, 2020. This is essentially a recoding of the wrkloss variable with -88 and -99 coded as NA, 1 coded as 1 and 2 coded as 0",
    "expect_inc_loss", "Indicator variable for if a respondent (or anyone in their household) expects to experience a loss in employment income in the next 4 weeks due to the coronavirus. this is essentially a recoding of the expctloss variable with -88 and -99 coded as NA, 1 coded as 1 and 2 coded as 0",
    "payment_not_conf", "Indicator variable for if a respondent has little or no confidence in paying rent/mortgage next month or has already deferred payment for next months rent/mortgage. Note this excludes people who oen their homes free and clear or occupy thier house without payment of rent. They are coded as 1 if mortconf is  1,2 or ; s 0 if mortconf is 3 or 4; and NA otherwise",
    "rent_not_conf", "Indicator variable for if a respondent has little or no confidence in paying thier rent next month or has already deferred. This is a limited to renters (ie tenure ==3)",
    "mortgage_not_conf", "Indicator variable for if a respondent has little or no confidence in paying thier mortgage next month or has already deferred. This is a limited to owners paying mortgage (ie tenure ==2)",
    "rent_caughtup", "Indicator variable for if a respondent's household is currently caught up on rent. This is a limited to renters (ie tenure ==3)",
    "mortgage_caughtup", "Indicator variable for if a respondent's household is currently caught up on mortage. This is a limited to owners paying mortgage (ie tenure ==2)",
    "food_insufficient", "Indicator variable for if a respondents household has sometimes or often had not enough to eat in the last 7 days. This is essentially a recoding of the curfoodsuff variable where 3 and 4 are coded as 1, 1 and 3 are coded as 0, and -88 and -99 are coded as NA",
    "spend_savings", "Indicator variable for if a respondent reported using money from savings or selling assets in last 7 days to meet spending needs",
    "spend_credit", "Indicator variable for if a respondent reported using money from credit cards or loans in last 7 days to meet spending needs",
    "spend_ui", "Indicator variable for if a respondent reported using money from unemployment insurance (UI) benefit payments in last 7 days to meet spending needs",
    "spend_stimulus", "Indicator variable for if a respondent reported using money from stimulus (economic impact) payment in last 7 days to meet spending needs",
    "anxious_score", "A recoding of the anxious variable to correctly reflect the numerical scores used to determine symptoms of generalized anxiety disorder. Specifically not at all = 0, several days = 1, more than half the days = 2, and nearly every day = 3",
    "worry_score", "A recoding of the worry variable to correctly reflect the numerical scores used to determine symptoms of generalized anxiety disorder. Specifically not at all = 0, several days = 1, more than half the days = 2, and nearly every day = 3",
    "interest_score", "A recoding of the interest variable to correctly reflect the numerical scores used to determine symptoms of major depresive disorder. Specifically not at all = 0, several days = 1, more than half the days = 2, and nearly every day = 3",
    "down_score", "A recoding of the worry variable to correctly reflect the numerical scores used to determine symptoms of major depressive disorder. Specifically not at all = 0, several days = 1, more than half the days = 2, and nearly every day = 3",
    "anxiety_signs", "An indicator variable for if the respondent is showing signs of generalized anxiety disorder. This is coded as 1 if the sum of anxious_score and worry_score is >= 3. Respondents with missing responses to both questions are coded as NA and 0 otherwise",
    "depression_signs", "An indicator variable for if the respondent is showing signs of major depressive disroder. This is coded as 1 if the sum of down_score and interest_score is >= 3.  Respondents with missing responses to both questions are coded as NA and 0 otherwise",
    "depression_anxiety_signs", " An indicator variable if the respondent is showing either signs of major depressive disorder or generalized anxiety disorder. Respondents with missing responses to both anxiety_signs and depression_signs are coded as NA",
    "expense_dif", "Indicator variable for if a respondent reported difficulty for their household to pay for usual household expense n the last 7 days ",
    "telework", "Indicator for at least one adults in this household substitute some or all of their typical in-person work for telework because of the coronavirus pandemic",
    "metalhealth_unmet", "Indicator for needed but did not get counseling or therapy from a mental health professional in the past 4 weeks, for any reason",
    "eviction_risk", "Indicator for the likelihood of the household will have to leave this home or apartment within the next two months because of eviction",
    "foreclosure_risk", "Indicator for the likelihood of the household will have to leave this home within the next two months because of foreclosure",
    "learning_fewer", "Indicator for the student(s) spend less time on all learning activities relative to a school day before the coronavirus pandemic during the last 7 days ",
    "spend_snap", "Indicator for household members using SNAP to meet their spending needs in the past 7 days",
    "week_num", "The week number that the survey data is from",
    "state", "2 digit abbrevation of the state that respondents are from",
    "state_name", "The full name of the state that respondents are from",
    "csa_title", "The name of the larger Combined statistical area that the respondent is from. Note the Census only reports the Metropolitan Statistical Area (aks the CBSA)",
    "cbsa_title", "The full name of the Core based statistical area that the respondent is from"
    
  )

# Write out data dictionary
write_csv(
  appended_column_data_dictionary,
  "data/intermediate-data/pulse_puf2_appended_columns_data_dictionary.csv"
)
