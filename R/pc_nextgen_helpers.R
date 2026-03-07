#' Inspect Next-Generation Workflow Capabilities
#'
#' @description
#' Reports runtime capabilities relevant to next-generation workflows, including
#' offline policy, cache readiness, and optional dependency availability.
#'
#' @param cache_dir Cache directory to inspect. If `NULL`, uses
#'   `pc_config()$cache_dir`.
#' @param check_network Logical. If `TRUE`, performs a lightweight connectivity
#'   check against PubChem.
#' @param network_timeout Timeout in seconds for the optional network check.
#'
#' @details
#' Network checks are disabled by default to keep behavior deterministic and
#' CRAN-friendly. Use `check_network = TRUE` only in interactive contexts.
#'
#' @return A one-row tibble with capability flags and runtime settings.
#'
#' @examples
#' pc_capabilities()
#' @export
pc_capabilities <- function(cache_dir = NULL,
                            check_network = FALSE,
                            network_timeout = 2) {
  cache_dir <- cache_dir %||% .pc_state$config$cache_dir
  network_timeout <- pc_validate_positive_finite_numeric_scalar(
    network_timeout,
    "'network_timeout'"
  )

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  cache_exists <- dir.exists(cache_dir)
  cache_writable <- FALSE
  if (cache_exists) {
    probe <- tempfile(tmpdir = cache_dir, fileext = ".tmp")
    cache_writable <- isTRUE(tryCatch({
      writeLines("ok", probe)
      TRUE
    }, error = function(e) FALSE))
    unlink(probe, force = TRUE)
  }

  net_ok <- NA
  if (isTRUE(check_network)) {
    net_ok <- isTRUE(tryCatch({
      resp <- httr::GET(
        "https://pubchem.ncbi.nlm.nih.gov",
        httr::timeout(network_timeout),
        httr::user_agent(.pc_state$config$user_agent)
      )
      sc <- httr::status_code(resp)
      is.finite(sc) && sc >= 200 && sc < 500
    }, error = function(e) FALSE))
  }

  tibble::tibble(
    live_enabled = !isTRUE(.pc_state$config$offline),
    offline_mode = isTRUE(.pc_state$config$offline),
    cache_dir = cache_dir,
    cache_exists = cache_exists,
    cache_writable = cache_writable,
    optional_rcdk = pc_namespace_available("rcdk"),
    optional_chemminer = pc_namespace_available("ChemmineR"),
    optional_matrix = pc_namespace_available("Matrix"),
    network_reachable = as.logical(net_ok)
  )
}

#' Example Assay Summary Payload for Next-Generation Workflows
#'
#' @description
#' Returns a small deterministic payload shaped like
#' `compound/*/assaysummary/JSON`, suitable for local examples and tests.
#'
#' @return A named list containing `Table/Columns/Row` assay summary data.
#'
#' @examples
#' payload <- pc_example_assaysummary_payload()
#' tbl <- pc_assay_activity_long(x = payload)
#' nrow(tbl)
#' @export
pc_example_assaysummary_payload <- function() {
  list(
    Table = list(
      Columns = list(
        Column = c("AID", "SID", "CID", "Activity Outcome", "Activity Value [uM]")
      ),
      Row = list(
        list(Cell = c("1001", "9001", "2244", "Active", "1.20")),
        list(Cell = c("1002", "9002", "2244", "Inactive", "")),
        list(Cell = c("1003", "9003", "3672", "Inconclusive", "12.50"))
      )
    )
  )
}

#' Example Feature Table for Next-Generation Modeling Workflows
#'
#' @description
#' Returns a compact feature table with common compound descriptors for
#' deterministic, offline-friendly workflow examples.
#'
#' @return A tibble containing CID-level descriptor columns.
#'
#' @examples
#' pc_example_feature_table()
#' @export
pc_example_feature_table <- function() {
  tibble::tibble(
    CID = c("2244", "3672", "5957"),
    MolecularWeight = c(180.16, 206.28, 194.19),
    XLogP = c(1.2, 3.5, 0.1),
    TPSA = c(63.6, 37.3, 86.6),
    HBondDonorCount = c(1, 0, 1),
    HBondAcceptorCount = c(4, 2, 4)
  )
}
