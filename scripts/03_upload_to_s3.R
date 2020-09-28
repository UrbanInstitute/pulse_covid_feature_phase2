library(aws.s3)
library(tidyverse)

####---- Set AWS Parameters----------------
my_bucket_name <- "ui-census-pulse-survey"


# Set keys (Don't have to do this if your AWS creds are stored locally)
# If they're not stored locally, you need to store your AWS secret keys as a
# CSV in the following location

# secret_keys <- read_csv("data/raw-data/small/secret_keys.csv")

# key <- secret_keys$`Access key ID`
# secret_key <- secret_keys$`Secret access key`

# Sys.setenv(
#     "AWS_ACCESS_KEY_ID" = key,
#     "AWS_SECRET_ACCESS_KEY" = secret_key,
#     "AWS_DEFAULT_REGION" = "us-east-1"
# )

####---- Define data dictionaries----------------

# Manually define shortened msa names for feature as dataframe
# Note some of these names are slightly different from the names
# on the Census dashboard as they use the latest 2019 CBSA names
# from OMB and we use the 2018 names
# TODO: Update with 2019 CBSA names (and inform Comms when you do)
msa_transalation_list <-
  tibble::tribble(
    ~cbsa_title, ~cbsa_title_short,
    "Atlanta-Sandy Springs-Roswell, GA", "Atlanta",
    "Boston-Cambridge-Newton, MA-NH", "Boston",
    "Chicago-Naperville-Elgin, IL-IN-WI", "Chicago",
    "Dallas-Fort Worth-Arlington, TX", "Dallas/Fort Worth",
    "Detroit-Warren-Dearborn, MI", "Detroit",
    "Houston-The Woodlands-Sugar Land, TX", "Houston",
    "Los Angeles-Long Beach-Anaheim, CA", "Long Beach",
    "Miami-Fort Lauderdale-West Palm Beach, FL", "Miami/Fort Lauderdale",
    "New York-Newark-Jersey City, NY-NJ-PA", "New York",
    "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD", "Philadelphia",
    "Phoenix-Mesa-Scottsdale, AZ", "Phoenix",
    "Riverside-San Bernardino-Ontario, CA", "Riverside",
    "San Francisco-Oakland-Hayward, CA", "San Francisco/Oakland",
    "Seattle-Tacoma-Bellevue, WA", "Seattle",
    "Washington-Arlington-Alexandria, DC-VA-MD-WV", "Washington DC"
  )

# Manually create data dictionary for feature file
feature_data_dictionary <-
  tibble::tribble(
    ~variable.name, ~variable.type, ~definition,
    "geography", "string", "geography name",
    "metric", "string", "name of metric",
    "week_num", "string", "week interval",
    "race_var", "string", "race/ethnicity group",
    "mean", "float", "mean for metric/race/week/geography, if there are no observations for a given metric/race/week/geography, the mean will be null",
    "se", "float", "standard error for estimate, if mean is 0 then standard error will be null and if mean is 0 or 1 standard error will be 0 ",
    "moe_95", "float", "95% confidence margin of error",
    "moe_95_lb", "float", "lower bound of 95% confidence interval (mean - moe_95)",
    "moe_95_ub", "float", "upper bound of 95% confidence interval (mean + moe_95)",
    "geo_type", "string", "type of geography ('state' , 'msa', or 'national')",
    "sigdiff", "bool", "whether mean is significantly different from total mean for geography. If race_var is 'total' and geo_type is 'state' or 'msa', sigdiff representes whether the state/msa mean  is significantly different from the national mean. If geo_type is 'national' and race_var is not 'total', sigdiff represents whether the national mean for the given race/ethnicity group is significantly different from the overall national average. If geo_type is 'national' and race_var is 'total', sigdiff will be null"
  )



####---- Upload to S3----------------

s3_filepath <- "data/final-data/"

# put most recent public use file in S3
put_object(
  file = paste0("data/intermediate-data/", "pulse_puf_all_weeks.csv"),
  object = "pulse_puf_most_recent.csv",
  bucket = my_bucket_name,
  multipart = F
)

# put most recent data dictionary in S3
endweek_padded <- str_pad(endweek, width = 2, side = "left", pad = "0")
put_object(
  file = paste0(
    str_glue("data/raw-data/public_use_files/pulse2020_data.dictionary_CSV_{endweek_padded}.xlsx")
  ),
  object = "puf_week_most_recent_data_dictionary.xlsx",
  bucket = my_bucket_name,
  multipart = F
)

# put most recent feature file for point estimates in S3
put_object(
  file = paste0(s3_filepath, "point_all_to_current_week.csv"),
  object = "point_all_to_current_week.csv",
  bucket = my_bucket_name,
  multipart = F
)


# put most recent feature file for rolling estimates in S3
put_object(
  file = paste0(s3_filepath, "rolling_all_to_current_week.csv"),
  object = "rolling_all_to_current_week.csv",
  bucket = my_bucket_name,
  multipart = F
)

# put feature file data dictionary on S3
s3write_using(feature_data_dictionary,
  FUN = write_csv,
  bucket = my_bucket_name,
  object = "pulse_summary_files_data_dictionary.csv"
)



# put MSA translation list on S3
s3write_using(msa_transalation_list,
  FUN = write_csv,
  bucket = my_bucket_name,
  object = "msa_translation_list.csv"
)


# put appended column data dictionary on S3
put_object(
  file = "data/intermediate-data/pulse_puf_appended_columns_data_dictionary.csv",
  object = "appended_column_data_dictionary.csv",
  bucket = my_bucket_name,
  multipart = F
)
