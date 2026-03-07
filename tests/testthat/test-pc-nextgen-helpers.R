test_that("pc_capabilities reports runtime flags", {
  cap <- pc_capabilities(check_network = FALSE)

  expect_s3_class(cap, "tbl_df")
  expect_equal(nrow(cap), 1)
  expect_true(all(c(
    "live_enabled", "offline_mode", "cache_dir", "cache_exists", "cache_writable",
    "optional_rcdk", "optional_chemminer", "optional_matrix", "network_reachable"
  ) %in% names(cap)))
  expect_type(cap$cache_writable, "logical")
})

test_that("example helper payloads are deterministic and usable", {
  payload <- pc_example_assaysummary_payload()
  expect_true(is.list(payload))
  expect_true(!is.null(payload$Table$Columns$Column))

  long <- pc_assay_activity_long(x = payload)
  expect_s3_class(long, "tbl_df")
  expect_equal(nrow(long), 3)
  expect_true(all(c("CID", "AID", "ActivityOutcome") %in% names(long)))

  feat <- pc_example_feature_table()
  expect_s3_class(feat, "tbl_df")
  expect_equal(nrow(feat), 3)
  expect_true(all(c("CID", "MolecularWeight", "XLogP") %in% names(feat)))
})
