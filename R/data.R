#' List of Direct Food Additives
#'
#' A list of compounds including their purposes and regulations in the U.S. FDA's
#' Substances Added to Food database
#'
#' @format ## `DirectFoodAdditives`
#' A data frame with 3971 rows and 37 columns:
#' \describe{
#'   \item{CAS Reg No (or other ID)}{Chemical Abstract Service (CAS) Registry Number
#'   for the substance or a numerical code assigned by the Human Foods Program to
#'   those substances that do not have a CAS Registry Number (977nnn-nn-n series).}
#'   \item {Substance}{The name of the ingredient as recognized by the FDA's Human
#'   Foods Program}
#'   \item {Other Names}{Other name identifiers the substance is known by.}
#'   \item {Used for (Technical Effect)}{The physical or technical effect(s) the
#'   substance has in or on food.}
#'   \item {Reg col01, Reg col02, Reg col03, Reg col04, Reg col05, Reg col06}
#'   {Color additive regulations (21 CFR Parts 73, 74, 81, 82).}
#'   \item {Reg add01, Reg add02, Reg add03, Reg add04, Reg add05, Reg add06,
#'   Reg add07, Reg add08, Reg add09, Reg add10, Reg add11, Reg add12,Reg add13,
#'   Reg add14, Reg add16, Reg add17, Reg add18, Reg add19, Reg add20}{Food additive
#'   and GRAS regulations (21 CFR Parts 170-186).}
#'   \item {Reg prohibited189}{Prohibited substance regulation (21 CFR Part 189).}
#'   \item {Reg Administrative}{General administration regulation (21 CFR Part 2).}
#'   \item {regs Labeling & Standards}{Food labeling and standards regulations
#'   (21 CFR Parts 100-169).}
#'   \item{FEMA No}{The trade association, Flavor and Extract Manufacturers Association (FEMA),
#'   has established expert panels to evaluate and make independent determinations on the
#'   GRAS status of flavoring substances. The FEMA number is provided here as a reference
#'   to FEMA’s GRAS assessments.}
#'   \item {GRAS Pub No}{FEMA GRAS publication number.}
#'   \item {Most Recent GRAS Pub Update}{Most up to date GRAS publication number.}
#'   \item {FEMA status}{FEMA status of compound.}
#'   \item {JEFCA Flavor Number}{The Joint Expert Committee on Food Additives (JECFA)
#'   flavor number.}
#'   ...
#' }
#' @source <https://www.hfpappexternal.fda.gov/scripts/fdcc/index.cfm?set=FoodSubstances>
"DirectFoodAdditives"


#' List of Indirect Food Additives
#'
#' A list of compounds including their purposes and regulations in the U.S. FDA's
#' Inventory of Food Contact Substances database
#'
#' @format ## `IndirectFoodAdditives`
#' A data frame with 3647 rows and 51 columns:
#' \describe{
#'   \item{CAS Reg No (or other ID)}{Chemical Abstract Service (CAS) Registry Number
#'   for the substance or a numerical code assigned by the Human Foods Program to
#'   those substances that do not have a CAS Registry Number (977nnn-nn-n series).}
#'   \item {Substance}{The name of the ingredient as recognized by the FDA's Human
#'   Foods Program}
#'   \item {Reg col01, Reg col02, Reg col03, Reg col04, Reg col05, Reg col06}
#'   {Color additive regulations (21 CFR Parts 73, 74, 81, 82).}
#'   \item {Reg 01, Reg 02, Reg 03, Reg 04, Reg 05, Reg 06,
#'   Reg 07, Reg 08, Reg 09, Reg 10, Reg 11, Reg 12,Reg 13,
#'   Reg 14, Reg 16, Reg 17, Reg 18, Reg 19, Reg 20, Reg 21}{Food additive
#'   and GRAS regulations (21 CFR Parts 170-186).}
#'   \item {Reg prohibited189}{Prohibited substance regulation (21 CFR Part 189).}
#'   \item {regs Labeling-Standards}{Food labeling and standards regulations
#'   (21 CFR Parts 100-169).}
#'   \item {Other Names}{Other name identifiers the substance is known by.}
#'   \item {SYN01, SYN02, SYN03,SYN04, SYN05, SYN06,SYN07, SYN08, SYN09,SYN10,
#'   SYN12, SYN13,SYN14, SYN15, SYN16,SYN17, SYN18, SYN19}{Synonyms for the
#'   substance name.}
#'   ...
#' }
#' @source <http://www.hfpappexternal.fda.gov/scripts/fdcc/?set=IndirectAdditives>
"IndirectFoodAdditives"
