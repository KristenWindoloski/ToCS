## code to prepare `IndirectFoodAdditives` dataset goes here

# Load in downloaded data from FDA's Inventory of Food Contact Substances List
IndirectFoodAdditives <- read.csv("data-raw/FDA_InventoryOfFoodContactSubstances.csv",
                                  header = FALSE)

# Extract column names
df_colnames <- IndirectFoodAdditives[3,]

# Remove download information from data frame
IndirectFoodAdditives <- IndirectFoodAdditives[-(1:3),-(32)]

# Assign column names
colnames(IndirectFoodAdditives) <- df_colnames[c(1:2,4:37)]

# Remove white space in CAS column
IndirectFoodAdditives$`CAS Registry No. (or other ID)` <- gsub(" ","",IndirectFoodAdditives$`CAS Registry No. (or other ID)`)

# Save data frame to data folder for use in ToCS
usethis::use_data(IndirectFoodAdditives, overwrite = TRUE)
