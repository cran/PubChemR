make_test_instance <- function(identifier,
                               namespace = "name",
                               domain = "compound",
                               result = list(),
                               success = TRUE) {
  structure(
    list(
      result = result,
      request_args = list(identifier = identifier, namespace = namespace, domain = domain),
      success = success,
      error = NULL
    ),
    class = "PubChemInstance"
  )
}

test_that("identifier getters tolerate successful but empty payloads", {
  cid_hit <- make_test_instance(
    identifier = "aspirin",
    result = list(IdentifierList = list(CID = c(2244L, 1983L)))
  )
  cid_miss <- make_test_instance(
    identifier = "missing",
    result = list(IdentifierList = list(CID = integer(0)))
  )
  cids_obj <- structure(
    list(
      result = list(cid_hit, cid_miss),
      request_args = list(identifier = c("aspirin", "missing"), namespace = "name", domain = "compound"),
      success = c(TRUE, TRUE),
      has_hits = c(TRUE, FALSE),
      error = c("", "")
    ),
    class = "PubChemInstance_CIDs"
  )

  cids <- CIDs(cids_obj)
  expect_s3_class(cids, "tbl_df")
  expect_equal(cids$Name, c("aspirin", "aspirin"))
  expect_equal(cids$CID, c(2244L, 1983L))

  aids_hit <- make_test_instance(
    identifier = "aspirin",
    result = list(InformationList = list(Information = list(list(CID = 2244L, AID = c(111L, 222L)))))
  )
  aids_miss <- make_test_instance(
    identifier = "missing",
    result = list(InformationList = list(Information = list()))
  )
  aids_obj <- structure(
    list(
      result = list(aids_hit, aids_miss),
      request_args = list(identifier = c("aspirin", "missing"), namespace = "name", domain = "compound"),
      success = c(TRUE, TRUE),
      has_hits = c(TRUE, FALSE),
      error = c("", "")
    ),
    class = "PubChemInstance_AIDs"
  )

  aids <- AIDs(aids_obj)
  expect_s3_class(aids, "tbl_df")
  expect_equal(aids[[1]], c("aspirin", "aspirin"))
  expect_equal(aids$AID, c(111L, 222L))

  sids_obj <- structure(
    list(
      result = list(make_test_instance(
        identifier = "missing",
        result = list(InformationList = list(Information = list()))
      )),
      request_args = list(identifier = "missing", namespace = "name", domain = "compound"),
      success = TRUE,
      has_hits = FALSE,
      error = ""
    ),
    class = "PubChemInstance_SIDs"
  )
  syn_obj <- structure(
    list(
      result = list(make_test_instance(
        identifier = "missing",
        result = list(InformationList = list(Information = list()))
      )),
      request_args = list(identifier = "missing", namespace = "name", domain = "compound"),
      success = TRUE,
      has_hits = FALSE,
      error = ""
    ),
    class = "PubChemInstance_Synonyms"
  )

  expect_no_error(SIDs(sids_obj))
  expect_no_error(synonyms(syn_obj))
  expect_equal(nrow(SIDs(sids_obj)), 0)
  expect_equal(nrow(synonyms(syn_obj)), 0)
})

test_that("find_last_layer and PC_Substance print handle empty lists", {
  expect_equal(find_last_layer(list()), list())
  expect_null(find_last_layer(NULL))

  sub_obj <- structure(
    list(
      result = list(),
      request_args = list(domain = "compound", namespace = "cid", identifier = "2244"),
      success = TRUE,
      error = NULL
    ),
    class = "PC_Substance"
  )

  expect_no_error(capture.output(print(sub_obj)))
})
