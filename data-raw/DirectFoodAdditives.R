## code to prepare `DirectFoodAdditives` dataset goes here

# Load in downloaded data from FDA's Substances Added to Food database
DirectFoodAdditives <- read.csv("data-raw/FDA_SubstancesAddedToFood.csv", header = FALSE)

# Extract column names
df_colnames <- DirectFoodAdditives[3,]

# Remove download information from data frame
# Remove the 'Other Names' column due to non-ASCII characters
DirectFoodAdditives <- DirectFoodAdditives[-(1:3),-(3)]

# Assign column names
colnames(DirectFoodAdditives) <- df_colnames[c(1:2,4:37)]

# Remove white space in CAS column
DirectFoodAdditives$`CAS Reg No (or other ID)` <- gsub(" ","",DirectFoodAdditives$`CAS Reg No (or other ID)`)

# Remove any non-ASCII characters from the Technical Effects column
DirectFoodAdditives$`Used for (Technical Effect)` <- gsub("<br />","",DirectFoodAdditives$`Used for (Technical Effect)`)

# Save data frame to data folder for use in ToCS
usethis::use_data(DirectFoodAdditives, overwrite = TRUE)
