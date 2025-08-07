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


###############################################################
# DETERMINE HOW MANY COMPOUNDS AVAILABLE TO SIMULATE
###############################################################

allCAS <- unique(c(DirectFoodAdditives$`CAS Reg No (or other ID)`,IndirectFoodAdditives$`CAS Registry No. (or other ID)`))

rat3compss <- httk::get_cheminfo(model = "3compartmentss", species = "Rat", default.to.human = FALSE) #1026
rat1comp <- httk::get_cheminfo(model = "1compartment", species = "Rat", default.to.human = FALSE) #965
rat3comp <- httk::get_cheminfo(model = "3compartment", species = "Rat", default.to.human = FALSE) #965
ratpbtk <- httk::get_cheminfo(model = "pbtk", species = "Rat", default.to.human = FALSE) #965
ratfetalpbtk <- httk::get_cheminfo(model = "fetal_pbtk", species = "Rat", default.to.human = FALSE) #965

intrat3compss <- intersect(allCAS,rat3compss)
intrat1comp <- intersect(allCAS,rat1comp)
intrat3comp <- intersect(allCAS,rat3comp)
intratpbtk <- intersect(allCAS,ratpbtk)
intratfetal_pbtk <- intersect(allCAS,ratfetalpbtk)

load_sipes2017()
load_pradeep2020()
load_dawson2021()

human3compss <- httk::get_cheminfo(model = "3compartmentss", species = "Human") #1026
human1comp <- httk::get_cheminfo(model = "1compartment", species = "Human") #965
human3comp <- httk::get_cheminfo(model = "3compartment", species = "Human") #965
humanpbtk <- httk::get_cheminfo(model = "pbtk", species = "Human") #965
humanfetalpbtk <- httk::get_cheminfo(model = "fetal_pbtk", species = "Human") #965

inthuman3compss <- intersect(allCAS,human3compss)
inthuman1comp <- intersect(allCAS,human1comp)
inthuman3comp <- intersect(allCAS,human3comp)
inthumanpbtk <- intersect(allCAS,humanpbtk)
inthumanfetal_pbtk <- intersect(allCAS,humanfetalpbtk)


