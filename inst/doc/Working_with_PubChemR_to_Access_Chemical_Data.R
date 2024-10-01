## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = FALSE,
  class.output="scroll-100",
  cache.path = "cached/"
)

## ----install_package, eval=FALSE----------------------------------------------
#  install.packages("PubChemR")

## ----library, warning=FALSE---------------------------------------------------
library(PubChemR)

## ----install_devtools, eval=FALSE---------------------------------------------
#  install.packages("devtools")

## ----install_github, eval=FALSE-----------------------------------------------
#  devtools::install_github("selcukorkmaz/PubChemR")

## ----load_PubChemR------------------------------------------------------------
library(PubChemR)

## ----aids_by_cid--------------------------------------------------------------
aids_by_cid <- get_aids(
  identifier = c(2244, 2519, 3672),
  namespace = "cid",
  domain = "compound"
)

aids_by_cid

## ----aids_by_cid_getter-------------------------------------------------------
aids <- AIDs(object = aids_by_cid, .to.data.frame = TRUE)
aids

## -----------------------------------------------------------------------------
table(aids$CID)

## ----aids_by_ssid-------------------------------------------------------------
aids_by_sid <- get_aids(
  identifier = c(103414350, 103204295),
  namespace = "sid",
  domain = "substance"
)

aids_by_sid

## ----aids_by_sid_getter-------------------------------------------------------
AIDs(object = aids_by_sid, .to.data.frame = TRUE)

## ----aids_by_name-------------------------------------------------------------
aids_by_name <- get_aids(
  identifier = c("paracetamol", "naproxen", "diclofenac"),
  namespace = "name",
  domain = "compound"
)

aids_by_name

## ----AIDs_getter--------------------------------------------------------------
aids <- AIDs(object = aids_by_name, .to.data.frame = TRUE)
aids

## -----------------------------------------------------------------------------
table(aids$NAME)

## ----get_aids_by_smiles-------------------------------------------------------
aids_by_smiles <- get_aids(
  identifier = "CC(=O)OC1=CC=CC=C1C(=O)O",
  namespace = "smiles",
  domain = "compound"
)

aids_by_smiles

## ----AIDs_getter_smiles-------------------------------------------------------
AIDs(object = aids_by_smiles, .to.data.frame = TRUE)

## ----get_aids_by_inchikey-----------------------------------------------------
aids_by_inchikey <- get_aids(
  identifier = "GALPCCIBXQLXSH-UHFFFAOYSA-N",
  namespace = "inchikey",
  domain = "compound"
)

aids_by_inchikey

## ----AIDs_getter_inchikey-----------------------------------------------------
AIDs(object = aids_by_inchikey, .to.data.frame = TRUE)

## ----get_aids_by_formula------------------------------------------------------
aids_by_formula <- get_aids(
  identifier = "C15H12N2O2",
  namespace = "formula",
  domain = "compound"
)

aids_by_formula

## ----AIDs_getter_formula------------------------------------------------------
AIDs(object = aids_by_formula, .to.data.frame = TRUE)

## ----get_cids_by_name---------------------------------------------------------
cids_by_name <- get_cids(
  identifier = c("aspirin", "caffein", "ibuprofen"),
  namespace = "name",
  domain = "compound"
)

cids_by_name

## ----CIDs_getter_by_name------------------------------------------------------
CIDs(object = cids_by_name)

## ----get_cids_by_smiles-------------------------------------------------------
cids_by_smiles <- get_cids(
  identifier = "C([C@@H]1[C@H]([C@@H]([C@H](C(O1)O)O)O)O)O",
  namespace = "smiles",
  domain = "compound"
)

cids_by_smiles

## ----CIDs_by_smiles-----------------------------------------------------------
CIDs(object = cids_by_smiles)

## ----get_cids_by_inchikey-----------------------------------------------------
cids_by_inchikey <- get_cids(
  identifier = "HEFNNWSXXWATRW-UHFFFAOYSA-N",
  namespace = "inchikey",
  domain = "compound"
)

cids_by_inchikey

## ----CIDs_by_inchikey---------------------------------------------------------
CIDs(object = cids_by_inchikey)

## ----get_cids_by_formula------------------------------------------------------
cids_by_formula <- get_cids(
  identifier = "C15H12N2O2",
  namespace = "formula",
  domain = "compound"
)

cids_by_formula

## ----CIDs_getter_formula------------------------------------------------------
CIDs(object = cids_by_formula, .to.data.frame = TRUE)

## -----------------------------------------------------------------------------
sids_by_cid <- get_sids(
  identifier = c(2244, 2519, 3672),
  namespace = "cid",
  domain = "compound"
)

sids_by_cid

## -----------------------------------------------------------------------------
sids <- SIDs(object = sids_by_cid, .to.data.frame = TRUE)
sids

## -----------------------------------------------------------------------------
table(sids$`Compound ID`)

## -----------------------------------------------------------------------------
sids_by_aids <- get_sids(
  identifier = "1234",
  namespace = "aid",
  domain = "assay"
)

sids_by_aids

## -----------------------------------------------------------------------------
SIDs(object = sids_by_aids, .to.data.frame = TRUE)

## ----get_sids_by_name---------------------------------------------------------
sids <- get_sids(
  identifier = "aspirin",
  namespace = "name",
  domain = "compound"
)

sids

## ----sids_by_name-------------------------------------------------------------
SIDs(object = sids)

## ----get_sids_by_smiles-------------------------------------------------------
sids_by_smiles <- get_sids(
  identifier = "C([C@@H]1[C@H]([C@@H]([C@H](C(O1)O)O)O)O)O",
  namespace = "smiles",
  domain = "compound"
)

sids_by_smiles

## ----SIDs_by_smiles-----------------------------------------------------------
SIDs(object = sids_by_smiles)

## ----get_sids_by_inchikey-----------------------------------------------------
sids_by_inchikey <- get_sids(
  identifier = "BPGDAMSIGCZZLK-UHFFFAOYSA-N",
  namespace = "inchikey",
  domain = "compound"
)

sids_by_inchikey

## ----sids_by_inchikey---------------------------------------------------------
SIDs(object = sids_by_inchikey)

## ----get_sids_by_formula------------------------------------------------------
sids_by_formula <- get_sids(
  identifier = "C15H12N2O2",
  namespace = "formula",
  domain = "compound"
)

sids_by_formula

## ----SIDs_getter_formula------------------------------------------------------
SIDs(object = sids_by_formula, .to.data.frame = TRUE)

## ----get_assays_by_aid--------------------------------------------------------
assay_data <- get_assays(
  identifier = c(485314, 485341, 504466, 624202, 651820), 
  namespace = "aid"
)

assay_data

## ----reques_assay_data--------------------------------------------------------
request_args(object = assay_data)

## ----instance_assay_data------------------------------------------------------
aid_651820 <- instance(object = assay_data, .which = 651820)
aid_651820

## ----retrieve_aid-------------------------------------------------------------
retrieve(object = aid_651820, .slot = "aid", .to.data.frame = TRUE)

## ----retrieve_aid_source------------------------------------------------------
retrieve(object = aid_651820, .slot = "aid_source", .to.data.frame = TRUE)

## ----retrieve_name------------------------------------------------------------
retrieve(object = aid_651820, .slot = "name", .to.data.frame = FALSE)

## ----retrieve_description-----------------------------------------------------
retrieve(object = aid_651820, .slot = "description", .to.data.frame = FALSE, .verbose = TRUE)

## ----retrieve_protocol--------------------------------------------------------
retrieve(object = aid_651820, .slot = "protocol", .to.data.frame = FALSE, .verbose = TRUE)

## ----retrieve_comment---------------------------------------------------------
retrieve(object = aid_651820, .slot = "comment", .to.data.frame = FALSE, .verbose = TRUE)

## ----retrieve_xref------------------------------------------------------------
retrieve(object = aid_651820, .slot = "xref", .to.data.frame = FALSE)

## ----retrieve_results---------------------------------------------------------
retrieve(object = aid_651820, .slot = "results", .to.data.frame = TRUE)

## ----retrieve_revision--------------------------------------------------------
retrieve(object = aid_651820, .slot = "revision", .to.data.frame = FALSE)

## ----retrieve_activity_outcome_method-----------------------------------------
retrieve(object = aid_651820, .slot = "activity_outcome_method", .to.data.frame = FALSE)

## ----retrieve_project_category------------------------------------------------
retrieve(object = aid_651820, .slot = "project_category", .to.data.frame = FALSE)

## ----get_compounds_by_cid-----------------------------------------------------
compound_data <- get_compounds(
  identifier = c(2244, 5245),
  namespace = "cid"
)

compound_data

## ----request_by_cid-----------------------------------------------------------
request_args(object = compound_data)

## ----instance_by_cid----------------------------------------------------------
compound_2244 <- instance(object = compound_data, .which = 2244)
compound_2244

## ----retrieve_by_id-----------------------------------------------------------
retrieve(object = compound_2244, .slot = "id", .to.data.frame = TRUE)

## ----retrieve_by_atoms--------------------------------------------------------
retrieve(object = compound_2244, .slot = "atoms", .to.data.frame = FALSE)

## ----retrieve_by_bonds--------------------------------------------------------
retrieve(object = compound_2244, .slot = "bonds", .to.data.frame = FALSE)

## ----retrieve_by_coords-------------------------------------------------------
retrieve(object = compound_2244, .slot = "coords", .to.data.frame = FALSE)

## ----retrieve_by_props--------------------------------------------------------
retrieve(object = compound_2244, .slot = "props", .to.data.frame = TRUE)

## ----retrieve_by_count--------------------------------------------------------
retrieve(object = compound_2244, .slot = "count", .to.data.frame = TRUE)

## ----get_substances_by_name---------------------------------------------------
substance_data <- get_substances(
  identifier = "aspirin",   
  namespace = "name"
)

substance_data

## ----request_substance_by_name------------------------------------------------
request_args(object = substance_data)

## ----instance_by_substance_name-----------------------------------------------
substance_aspirin <- instance(object = substance_data, .which = "aspirin")

substance_aspirin

## ----retrieve_by_sid----------------------------------------------------------
retrieve(object = substance_aspirin, .slot = "sid", .to.data.frame = TRUE)

## ----retrieve_by_source-------------------------------------------------------
retrieve(object = substance_aspirin, .slot = "source", .to.data.frame = TRUE)

## ----retrieve_by_synonyms-----------------------------------------------------
retrieve(object = substance_aspirin, .slot = "synonyms", .to.data.frame = FALSE)

## ----retrieve_by_comment------------------------------------------------------
retrieve(object = substance_aspirin, .slot = "comment", .to.data.frame = FALSE, .verbose = TRUE)

## ----retrieve_by_xref---------------------------------------------------------
retrieve(object = substance_aspirin, .slot = "xref", .to.data.frame = FALSE, .verbose = TRUE)

## ----retrieve_by_compound-----------------------------------------------------
retrieve(object = substance_aspirin, .slot = "compound", .to.data.frame = FALSE)

## ----get_properties-----------------------------------------------------------
props <- get_properties(
  properties = c("mass", "molecular", "inchi"),
  identifier = c("aspirin", "ibuprofen"),
  namespace = "name",
  propertyMatch = list(
    .ignore.case = TRUE,
    type = "contain"
  )
)
props

## ----get_properties_aspirin---------------------------------------------------
retrieve(object = props, .which = "aspirin", .to.data.frame = TRUE)

## ----get_properties_ibuprofen-------------------------------------------------
retrieve(object = props, .which = "ibuprofen", .to.data.frame = FALSE)

## ----get_properties_combine---------------------------------------------------
retrieve(object = props, .to.data.frame = TRUE, .combine.all = TRUE)

## ----get_synonyms_by_name-----------------------------------------------------
synonyms <- get_synonyms(
  identifier = "aspirin",
  namespace = "name"
)

synonyms

## ----get_all_sources_substance------------------------------------------------
substance_sources <- get_all_sources(
  domain = "substance"
)

substance_sources

## ----get_sdf_by_name----------------------------------------------------------
get_sdf(
  identifier = "aspirin",
  namespace = "name",
  path = NULL,
  file_name = "aspirin_structure"
)

## ----download_jsson-----------------------------------------------------------
download(
  filename = "Aspirin",
  outformat = "JSON",
  path = tempdir(),
  identifier = "aspirin",
  namespace = "name",
  domain = "compound",
  overwrite = TRUE
)

