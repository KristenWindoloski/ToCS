## code to prepare the datasets goes here

######################################
# DIRECT FOOD ADDITIVES
######################################

# Load in downloaded data from FDA's Substances Added to Food database
DirectFoodAdditives <- read.csv("data-raw/FDA_SubstancesAddedToFood.csv",
                                header = FALSE)

# Extract column names
df_colnames <- DirectFoodAdditives[5,]

# Remove download information from data frame
DirectFoodAdditives <- DirectFoodAdditives[-(1:5),]

# Assign column names
colnames(DirectFoodAdditives) <- df_colnames

# Remove white space in CAS column
DirectFoodAdditives$`CAS Reg No (or other ID)` <- gsub(" ",
                                                       "",
                                                       DirectFoodAdditives$`CAS Reg No (or other ID)`)

# Remove any non-ASCII characters from the Technical Effects column
DirectFoodAdditives$`Used for (Technical Effect)` <- gsub("<br />",
                                                          "",
                                                          DirectFoodAdditives$`Used for (Technical Effect)`)


######################################
# INDIRECT FOOD ADDITIVES
######################################

# Load in downloaded data from FDA's Inventory of Food Contact Substances List
IndirectFoodAdditives <- read.csv("data-raw/FDA_InventoryOfFoodContactSubstances.csv",
                                  header = FALSE)

# Extract column names
df_colnames <- IndirectFoodAdditives[5,]

# Remove download information from data frame
IndirectFoodAdditives <- IndirectFoodAdditives[-(1:5),]

# Assign column names
colnames(IndirectFoodAdditives) <- df_colnames

# Remove white space in CAS column
IndirectFoodAdditives$`CAS Registry No. (or other ID)` <- gsub(" ","",IndirectFoodAdditives$`CAS Registry No. (or other ID)`)

########################################
# SAVE DATA FRAMES TOGETHER
########################################

# Save data frame to sysdata file in R folder
usethis::use_data(DirectFoodAdditives, IndirectFoodAdditives, overwrite = TRUE, internal = TRUE)
