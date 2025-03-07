## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  # cache = TRUE,
  class.output="scroll-100",
  cache.path = "cached/"
)
library(PubChemR)

## ----eval=FALSE---------------------------------------------------------------
# get_pug_rest(
#   identifier = NULL,
#   namespace = "cid",
#   domain = "compound",
#   operation = NULL,
#   output = "JSON",
#   searchtype = NULL,
#   property = NULL,
#   options = NULL,
#   save = FALSE,
#   dpi = 300,
#   path = NULL,
#   file_name = NULL,
#   ...
# )

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "2244", namespace = "cid", domain = "compound", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemDataResult <- pubChemData(result)

## -----------------------------------------------------------------------------
pubChemDataResult$PC_Compounds[[1]]$id

## -----------------------------------------------------------------------------
pubChemDataResult$PC_Compounds[[1]]$atoms

## -----------------------------------------------------------------------------
pubChemDataResult$PC_Compounds[[1]]$bonds

## -----------------------------------------------------------------------------
pubChemDataResult$PC_Compounds[[1]]$coords

## -----------------------------------------------------------------------------
pubChemDataResult$PC_Compounds[[1]]$charge

## -----------------------------------------------------------------------------
pubChemDataResult$PC_Compounds[[1]]$props[[2]]

## -----------------------------------------------------------------------------
pubChemDataResult$PC_Compounds[[1]]$props[[3]]

## -----------------------------------------------------------------------------
pubChemDataResult$PC_Compounds[[1]]$props[[4]]

## -----------------------------------------------------------------------------
pubChemDataResult$PC_Compounds[[1]]$props[[5]]

## -----------------------------------------------------------------------------
pubChemDataResult$PC_Compounds[[1]]$props[[7]]

## -----------------------------------------------------------------------------
pubChemDataResult$PC_Compounds[[1]]$props[[13]]

## -----------------------------------------------------------------------------
pubChemDataResult$PC_Compounds[[1]]$props[[14]]

## -----------------------------------------------------------------------------
pubChemDataResult$PC_Compounds[[1]]$props[[15]]

## -----------------------------------------------------------------------------
pubChemDataResult$PC_Compounds[[1]]$props[[17]]

## -----------------------------------------------------------------------------
pubChemDataResult$PC_Compounds[[1]]$props[[18]]

## -----------------------------------------------------------------------------
pubChemDataResult$PC_Compounds[[1]]$props[[19]]

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = c("1","2","3","4","5"), namespace = "cid", domain = "compound", property = c("MolecularFormula","MolecularWeight","CanonicalSMILES"), output = "CSV")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "glucose", namespace = "name", domain = "compound", operation = "cids", output = "TXT")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "CCCC", namespace = "smiles", domain = "compound", operation = "cids", output = "TXT")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "5793", namespace = "cid", domain = "compound", operation = "cids", output = "TXT", searchtype = "fastidentity", options = list(identity_type = "same_connectivity"))
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "US20050159403A1", namespace = "xref/PatentID", domain = "substance", operation = "sids", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## ----eval=FALSE---------------------------------------------------------------
# result <- get_pug_rest(identifier = "2244", namespace = "cid", domain = "compound", output = "SDF")

## -----------------------------------------------------------------------------
get_pug_rest(identifier = "lipitor", namespace = "name", domain = "compound", output = "PNG")

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "2244", namespace = "cid", domain = "compound", property = "MolecularWeight", output = "TXT")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = c("1","2","3","4","5"), namespace = "cid", domain = "compound", property = c("MolecularWeight", "MolecularFormula", "HBondDonorCount", "HBondAcceptorCount", "InChIKey", "InChI"), output = "CSV")

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "vioxx", namespace = "name", domain = "compound", operation = "synonyms", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "2244", namespace = "cid", domain = "compound", operation = c("xrefs","MMDBID"), output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "137349406", namespace = "sid", domain = "substance", operation = c("xrefs","PatentID"), output = "TXT")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "504526", namespace = "aid", domain = "assay", operation = "description", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "1000", namespace = "aid", domain = "assay", operation = "summary", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "504526", namespace = "aid", domain = "assay", output = "CSV")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "504526", namespace = "aid", domain = "assay", operation = "JSON?sid=104169547,109967232", output = "JSON")
result

## -----------------------------------------------------------------------------
result <- pubChemData(result)
result$PC_AssaySubmit$assay

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "504526", namespace = "aid", domain = "assay", operation = "concise", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "504526", namespace = "aid", domain = "assay", operation = "doseresponse/CSV?sid=104169547,109967232", output = "CSV")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = c("490","1000"), namespace = "aid", domain = "assay", operation = "targets/ProteinGI,ProteinName,GeneID,GeneSymbol", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "USP2", namespace = "target/genesymbol", domain = "assay", operation = "aids", output = "TXT")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "EC50", namespace = "activity", domain = "assay", operation = "aids", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "1956,13649", namespace = "geneid", domain = "gene", operation = "summary", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "EGFR", namespace = "genesymbol", domain = "gene", operation = "summary", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "ERBB1", namespace = "synonym", domain = "gene", operation = "summary", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "1956,13649", namespace = "geneid", domain = "gene", operation = "summary", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "13649", namespace = "geneid", domain = "gene", operation = "aids", output = "TXT")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "13649", namespace = "geneid", domain = "gene", operation = "concise", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "13649", namespace = "geneid", domain = "gene", operation = "pwaccs", output = "TXT")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "P00533,P01422", namespace = "accession", domain = "protein", operation = "summary", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "ChEMBL:CHEMBL203", namespace = "synonym", domain = "protein", operation = "summary", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "P00533,P01422", namespace = "accession", domain = "protein", operation = "summary", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "P00533", namespace = "accession", domain = "protein", operation = "aids", output = "TXT")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "Q01279", namespace = "accession", domain = "protein", operation = "concise", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "P00533", namespace = "accession", domain = "protein", operation = "pwaccs", output = "TXT")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "Reactome:R-HSA-70171", namespace = "pwacc", domain = "pathway", operation = "summary", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "Reactome:R-HSA-70171,BioCyc:HUMAN_PWY-4983", namespace = "pwacc", domain = "pathway", operation = "summary", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "Reactome:R-HSA-70171", namespace = "pwacc", domain = "pathway", operation = "cids", output = "TXT")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "Reactome:R-HSA-70171", namespace = "pwacc", domain = "pathway", operation = "geneids", output = "TXT")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "Reactome:R-HSA-70171", namespace = "pwacc", domain = "pathway", operation = "accessions", output = "TXT")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "9606,2697049", namespace = "taxid", domain = "taxonomy", operation = "summary", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "Homo sapiens", namespace = "synonym", domain = "taxonomy", operation = "summary", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result) 

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "9606,10090,10116", namespace = "taxid", domain = "taxonomy", operation = "summary", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "2697049", namespace = "taxid", domain = "taxonomy", operation = "aids", output = "TXT")
result

## -----------------------------------------------------------------------------
pubChemData(result) 

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "1409578", namespace = "aid", domain = "assay", operation = "concise", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "CHEMBL3308376,CVCL_0045", namespace = "cellacc", domain = "cell", operation = "summary", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "HeLa", namespace = "synonym", domain = "cell", operation = "summary", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "CVCL_0030,CVCL_0045", namespace = "cellacc", domain = "cell", operation = "summary", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "HeLa", namespace = "synonym", domain = "cell", operation = "aids", output = "TXT")
result

## -----------------------------------------------------------------------------
pubChemData(result)

## -----------------------------------------------------------------------------
result <- get_pug_rest(identifier = "79900", namespace = "aid", domain = "assay", operation = "concise", output = "JSON")
result

## -----------------------------------------------------------------------------
pubChemData(result)

