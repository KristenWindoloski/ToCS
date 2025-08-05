# --------------------------------
# Example 1 (Concentration-Time Profile)

rm(list = ls())
devtools::load_all(".")

ui_pars <- list(selectInput_Function = "Concentration-time profiles",
                selectInput_Species = "Human",
                selectInput_DefaultToHuman = "Yes",

                selectInput_InSilicoPars = "No, do not load in silico parameters",
                numericInput_SimTime = 5,
                selectInput_ReturnSamps = "Only return a specified dose quantile (default)",
                numericInput_Quantile = 0.95,

                selectInput_ADMEdoseunits = "mg/kg",
                numericInput_InitialDose = 1,
                selectInput_NumDoses = "Multiple doses",
                selectInput_MultipleDosesQ = "No",
                numericInput_MultiDoseAmount = 1,
                sliderInput_MultiDoseTime = 6,
                textInput_DoseMatrix = "0,5/24,10/24,1,29/24,34/24,2,53/24,58/24,3,77/24,82/24,4,101/24,106/24,9.9E-06,9.9E-06,9.9E-06,9.9E-06,9.9E-06,9.9E-06,9.9E-06,9.9E-06,9.9E-06,9.9E-06,9.9E-06,9.9E-06,9.9E-06,9.9E-06,9.9E-06",
                numericInput_DailyDose = 2,

                selectInput_CompPreference = "Choose from all available compounds",
                selectInput_HondaCond = "NULL",

                selectInput_InitialCondCustom = "No, keep the default amounts (default)",
                selectInput_rb2p = "Do not recalculate (default)",
                selectInput_RestrictClear = "Yes, include protein binding (default)",
                selectInput_AdjFub = "Yes, adjust the fraction of unbound plasma (default)",
                numericInput_MinFub = 0.0001,
                selectInput_Regression = "Use regressions (default)",
                numericInput_ClintPval = 0.05,
                numericInput_Alpha = 0.001,
                selectInput_Bioactive = "Total chemical concentration (default)",
                numericInput_Samples = 1000,

                selectInput_ODEmethod = "lsoda",
                numericInput_SolSteps = 4,
                sliderInput_RTol = -8,
                sliderInput_ATol = -12,

                numericInput_CacoDefault = 1.6,
                selectInput_Fabs = "Use the Caco2.Pab value selected above (default)",
                selectInput_Fgut = "Use the Caco2.Pab value selected above (default)",
                selectInput_Overwrite = "Do not overwrite in vivo values (default)",
                selectInput_Keep100 = "Do not keep Fabs and Fgut at 100% availability (default)",

                selectInput_SSoutunits = "uM",
                selectInput_OutConcSS = "blood",
                selectInput_OutConcIVIVE = "plasma",
                selectInput_TissueSS = "liver",
                selectInput_TissueIVIVE = "liver",
                textInput_OutputTimes = "",
                selectInput_IVIVEoutunits = "mgpkgpday")

run_ToCS(ui_pars)

# --------------------------------
# Example 2 (IVIVE)

rm(list = ls())
devtools::load_all(".")

ui_pars <- list(selectInput_Function = "In vitro in vivo extrapolation (IVIVE)",
                selectInput_Species = "Human",
                selectInput_DefaultToHuman = "Yes",

                selectInput_InSilicoPars = "Yes, load in silico parameters",
                numericInput_SimTime = 5,
                selectInput_ReturnSamps = "Only return a specified dose quantile (default)",
                numericInput_Quantile = 0.95,

                selectInput_ADMEdoseunits = "mg/kg",
                numericInput_InitialDose = 5,
                selectInput_NumDoses = "Single Dose",
                selectInput_MultipleDosesQ = "Select",
                numericInput_MultiDoseAmount = 1,
                sliderInput_MultiDoseTime = 6,
                textInput_DoseMatrix = "",
                numericInput_DailyDose = 2,

                selectInput_CompPreference = "Choose from all available compounds",
                selectInput_HondaCond = "Honda1",

                selectInput_InitialCondCustom = "No, keep the default amounts (default)",
                selectInput_rb2p = "Do not recalculate (default)",
                selectInput_RestrictClear = "Yes, include protein binding (default)",
                selectInput_AdjFub = "Yes, adjust the fraction of unbound plasma (default)",
                numericInput_MinFub = 0.0001,
                selectInput_Regression = "Use regressions (default)",
                numericInput_ClintPval = 0.05,
                numericInput_Alpha = 0.001,
                selectInput_Bioactive = "Total chemical concentration (default)",
                numericInput_Samples = 1000,

                selectInput_ODEmethod = "lsoda",
                numericInput_SolSteps = 4,
                sliderInput_RTol = -8,
                sliderInput_ATol = -12,

                numericInput_CacoDefault = 1.6,
                selectInput_Fabs = "Use the Caco2.Pab value selected above (default)",
                selectInput_Fgut = "Use the Caco2.Pab value selected above (default)",
                selectInput_Overwrite = "Do not overwrite in vivo values (default)",
                selectInput_Keep100 = "Do not keep Fabs and Fgut at 100% availability (default)",

                selectInput_SSoutunits = "uM",
                selectInput_OutConcSS = "blood",
                selectInput_OutConcIVIVE = "plasma",
                selectInput_TissueSS = "liver",
                selectInput_TissueIVIVE = "liver",
                textInput_OutputTimes = "",
                selectInput_IVIVEoutunits = "mgpkgpday")

run_ToCS(ui_pars)
