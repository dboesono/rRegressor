# Load raw data from .csv file
boston_df <- read.csv("data-raw/dataset-70319.csv")

# Apply preprocessing...

# Save the cleaned data in the required R package location
usethis::use_data(boston_df, overwrite = TRUE)
