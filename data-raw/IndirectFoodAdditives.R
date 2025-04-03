## code to prepare `IndirectFoodAdditives` dataset goes here

# Load in downloaded data from FDA's Inventory of Food Contact Substances List
IndirectFoodAdditives <- read.csv("data-raw/FDA_InventoryOfFoodContactSubstances.csv",
                                  header = FALSE)

# Extract column names
df_colnames <- IndirectFoodAdditives[3,]

# Remove download information from data frame
IndirectFoodAdditives <- IndirectFoodAdditives[-(1:3),]

# Assign column names
colnames(IndirectFoodAdditives) <- df_colnames

# Save data frame to data folder for use in ToCS
usethis::use_data(IndirectFoodAdditives, overwrite = TRUE)
