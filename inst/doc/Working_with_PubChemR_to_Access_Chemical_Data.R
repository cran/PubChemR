## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE--------------------------------------------------------------
#  # Install from CRAN
#  install.packages("PubChemR")
#  
#  # Or, install the development version from GitHub
#  install.packages("devtools")
#  devtools::install_github("selcukorkmaz/PubChemR")

## -----------------------------------------------------------------------------
# Load the PubChemR package
library(PubChemR)

## -----------------------------------------------------------------------------
# Example function call to verify setup
example_result <- pubchem_summary("aspirin", "name")
example_result$CIDs

## -----------------------------------------------------------------------------
pubchemSummary <- pubchem_summary(
  identifier = "aspirin",
  namespace = 'name',
  type = c("compound", "substance", "assay"),
  properties = "IsomericSMILES",
  include_synonyms = TRUE,
  include_sdf = FALSE,
  sdf_path = NULL
)
pubchemSummary$CIDs

## ---- eval = FALSE------------------------------------------------------------
#  # Save downloaded SDF file into a temporary folder.
#  pubchemSummary <- pubchem_summary(
#    identifier = "aspirin",
#    namespace = 'name',
#    type = c("compound", "substance", "assay"),
#    properties = "IsomericSMILES",
#    include_synonyms = TRUE,
#    include_sdf = TRUE,
#    sdf_path = NULL,
#    sdf_file_name = "Aspirin"
#  )
#  pubchemSummary$CIDs

## -----------------------------------------------------------------------------
getAIDs <- get_aids(
  identifier = "aspirin",
  namespace = "name"
)
head(getAIDs)

## -----------------------------------------------------------------------------
getCIDs <- get_cids(
  identifier = "aspirin",
  namespace = "name"
)
getCIDs

