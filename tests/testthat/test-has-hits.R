test_that("has_hits returns named flags", {
  obj <- list(
    request_args = list(identifier = c("aspirin", "unknown")),
    has_hits = c(TRUE, FALSE),
    success = c(TRUE, FALSE)
  )

  out <- has_hits(obj)
  expect_type(out, "logical")
  expect_equal(unname(out), c(TRUE, FALSE))
  expect_equal(names(out), c("aspirin", "unknown"))
})

test_that("has_hits falls back to success when has_hits is absent", {
  obj <- list(
    request_args = list(identifier = c("id1", "id2")),
    success = c(TRUE, FALSE)
  )

  out <- has_hits(obj)
  expect_equal(unname(out), c(TRUE, FALSE))
})

test_that("has_hits supports .which filtering and validates identifiers", {
  obj <- list(
    request_args = list(identifier = c("id1", "id2")),
    has_hits = c(TRUE, FALSE),
    success = c(TRUE, FALSE)
  )

  out <- has_hits(obj, .which = "id2")
  expect_equal(unname(out), FALSE)
  expect_equal(names(out), "id2")

  expect_error(
    has_hits(obj, .which = "missing"),
    "Unknown instance identifier"
  )
})

test_that("request_args and has_hits support PubChemResult objects", {
  res <- pc_make_result(
    success = TRUE,
    request = list(identifier = "2244", namespace = "cid", domain = "compound"),
    data = list(IdentifierList = list(CID = c(2244L))),
    status = 200
  )

  expect_equal(request_args(res, "identifier"), "2244")
  flags <- has_hits(res)
  expect_true(unname(flags))
  expect_equal(names(flags), "2244")
})
