
# Set up a new environment in ToCS package
the <- new.env(parent = emptyenv())

# Define 'global' variables in ToCS (avoids global variables by using a
# special package-specific environment)
the$ics <- names_ICs()
the$ic_names <- the$ics[[1]]
the$ic_comps <- the$ics[[2]]

# Define 'global' variables in ToCS (avoids global variables by using a
# special package-specific environment)
the$chemdata <- httk::chem.physical_and_invitro.data
