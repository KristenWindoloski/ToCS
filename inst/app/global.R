
###################################################
# GENERATE INITIAL CONDITION GLOBAL VARIABLES
###################################################

ics <- names_ICs()
ic_names <- ics[[1]]
ic_comps <- ics[[2]]


###################################################
# GENERATE DATA GLOBAL VARIABLES
###################################################

physiology.data <- httk::physiology.data
tissue.data <- httk::tissue.data
chem.physical_and_invitro.data <- httk::chem.physical_and_invitro.data
