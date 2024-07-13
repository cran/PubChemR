## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  class.output="scroll-100",
  cache.path = "cached/"
)
library(PubChemR)

## -----------------------------------------------------------------------------
result <- get_pug_view(annotation = "data", identifier = "1234", domain = "compound", output = "JSON")

## -----------------------------------------------------------------------------
result

## -----------------------------------------------------------------------------
retrieve(object = result, .slot = "RecordType",  .to.data.frame = FALSE)

## -----------------------------------------------------------------------------
retrieve(object = result, .slot = "RecordNumber",  .to.data.frame = FALSE)

## -----------------------------------------------------------------------------
retrieve(object = result, .slot = "RecordTitle",  .to.data.frame = FALSE)

## -----------------------------------------------------------------------------
section <- retrieve(object = result, .slot = "Section",  .to.data.frame = FALSE)
section

## -----------------------------------------------------------------------------
sectionList(section)

## -----------------------------------------------------------------------------
structures <- section(object = result, .id = "S1")

## -----------------------------------------------------------------------------
structures

## -----------------------------------------------------------------------------
retrieve(object = structures, .slot = "TOCHeading")

## -----------------------------------------------------------------------------
retrieve(object = structures, .slot = "Description")

## -----------------------------------------------------------------------------
retrieve(object = structures, .slot = "Section")

## -----------------------------------------------------------------------------
result <- get_pug_view(annotation = "data", identifier = "2244", domain = "compound", output = "JSON", heading = "Experimental Properties")

## -----------------------------------------------------------------------------
result

## -----------------------------------------------------------------------------
s1 <- section(object = result, .id = "S1")

## -----------------------------------------------------------------------------
s1

## -----------------------------------------------------------------------------
s1_s1 <- section(object = s1, .id = "S1")

## -----------------------------------------------------------------------------
s1_s1

## -----------------------------------------------------------------------------
s1_s1_s10 <- section(object = s1_s1, .id = "S10")

## -----------------------------------------------------------------------------
s1_s1_s10

## -----------------------------------------------------------------------------
retrieve(object = s1_s1_s10, .slot = "Information", .to.data.frame = FALSE)

## -----------------------------------------------------------------------------
result <- get_pug_view(annotation = "data", identifier = "1", domain = "assay", output = "JSON")

## -----------------------------------------------------------------------------
result

## -----------------------------------------------------------------------------
sectionList(object = result)

## -----------------------------------------------------------------------------
record_description <- section(object = result, .id = "S10")
record_description

## -----------------------------------------------------------------------------
record_description

## -----------------------------------------------------------------------------
retrieve(object = record_description, .slot = "Information")

## -----------------------------------------------------------------------------
result <- get_pug_view(annotation = "data", identifier = "1", domain = "gene", output = "JSON")

## -----------------------------------------------------------------------------
result

## -----------------------------------------------------------------------------
retrieve(object = result, .slot = "RecordTitle")

## -----------------------------------------------------------------------------
sectionList(object = result)

## -----------------------------------------------------------------------------
result <- get_pug_view(annotation = "annotations", identifier = "Viscosity", domain = "heading", output = "JSON")

## -----------------------------------------------------------------------------
result

## -----------------------------------------------------------------------------
annotations <- retrieve(object = result, .slot = "Annotation", .to.data.frame = FALSE)

## -----------------------------------------------------------------------------
head(annotations)

## -----------------------------------------------------------------------------
result <- get_pug_view(annotation = "annotations", identifier = NULL, domain = "heading", output = "JSON", heading = "Viscosity")
result

## -----------------------------------------------------------------------------
result <- get_pug_view(annotation = "annotations", identifier = "Boiling Point", domain = "heading", output = "JSON", headingType = "Compound")

## -----------------------------------------------------------------------------
result

## -----------------------------------------------------------------------------
annotations <- retrieve(object = result, .slot = "Annotation", .to.data.frame = FALSE)

## -----------------------------------------------------------------------------
head(annotations)

## -----------------------------------------------------------------------------
result <- get_pug_view(annotation = "annotations", identifier = "CAS", domain = "heading", output = "JSON", page = "10")

## -----------------------------------------------------------------------------
result

## -----------------------------------------------------------------------------
annotations <- retrieve(object = result, .slot = "Annotation", .to.data.frame = FALSE)

## -----------------------------------------------------------------------------
head(annotations)

## -----------------------------------------------------------------------------
result <- get_pug_view(annotation = "categories", identifier = "1234", domain = "compound", output = "JSON")
result

## -----------------------------------------------------------------------------
categories <- retrieve(object = result, .slot = "Categories", .to.data.frame = FALSE)

## -----------------------------------------------------------------------------
head(categories)

## -----------------------------------------------------------------------------
result <- get_pug_view(annotation = "neighbors", identifier = "1234", domain = "compound", output = "JSON")

## -----------------------------------------------------------------------------
result

## -----------------------------------------------------------------------------
retrieve(object = result, .slot = "NeighborsOfType", .to.data.frame = FALSE)

## -----------------------------------------------------------------------------
result <- get_pug_view(annotation = "literature", identifier = "1234", domain = "compound", output = "JSON")

## -----------------------------------------------------------------------------
result

## -----------------------------------------------------------------------------
retrieve(object = result, .slot = "AllURL", .to.data.frame = FALSE)

## -----------------------------------------------------------------------------
result <- get_pug_view(annotation = "linkout", identifier = "1234", domain = "compound", output = "JSON")
result

## -----------------------------------------------------------------------------
retrieve(result, .slot = "ObjUrl", .to.data.frame = FALSE)

## -----------------------------------------------------------------------------
result <- get_pug_view(annotation = "structure", identifier = "2244", domain = "compound", output = "JSON")
result

## -----------------------------------------------------------------------------
retrieve(object = result, .slot = "Structures", .to.data.frame = FALSE)

