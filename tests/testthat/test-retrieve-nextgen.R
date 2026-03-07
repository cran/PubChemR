test_that("retrieve works on PubChemResult payloads", {
  res <- pc_response(
    '{"IdentifierList":{"CID":[2244,3672]}}',
    request = list(domain = "compound", namespace = "name", identifier = "aspirin")
  )

  out_default <- retrieve(res)
  expect_s3_class(out_default, "tbl_df")
  expect_true("CID" %in% names(out_default))
  expect_equal(out_default$CID, c(2244, 3672))

  out_path <- retrieve(res, .slot = "IdentifierList/CID", .to.data.frame = FALSE)
  expect_equal(out_path, c(2244, 3672))
})

test_that("retrieve can extract error fields from failed PubChemResult", {
  fail_res <- pc_make_result(
    success = FALSE,
    request = list(domain = "compound", identifier = "2244"),
    error = pc_make_error("TransportError", "synthetic transport failure"),
    status = 500
  )

  err_code <- retrieve(fail_res, .slot = "error/code", .to.data.frame = FALSE)
  expect_equal(err_code, "TransportError")

  err_tbl <- retrieve(fail_res, .slot = "error", .to.data.frame = TRUE)
  expect_s3_class(err_tbl, "tbl_df")
  expect_true(all(c("code", "message") %in% names(err_tbl)))
})
