
# Add all httk data sets that are global variables in httk (not seen within
# shiny app because 'library(httk)' is not used, so we have to include them here)

physiology.data <- httk::physiology.data
tissue.data <- httk::tissue.data
mecdt <- httk::mecdt
mcnally_dt <- httk::mcnally_dt
bmiage <- httk::bmiage
wfl <- httk::wfl
well_param <- httk::well_param

utils::globalVariables(c('physiology.data',
                         'tissue.data',
                         'mecdt',
                         'mcnally_dt',
                         'bmiage',
                         'wfl',
                         'well_param'))
