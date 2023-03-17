################################################################################
#
#'
#' Get GitHub repositories
#'
#
################################################################################

get_repositories_ <- function(org, .limit = Inf) {
  gh::gh(
    endpoint = "/orgs/{org}/repos",
    org = org,
    .limit = .limit
  ) |>
    jsonlite::toJSON() |>
    jsonlite::fromJSON(flatten = TRUE) |>
    tibble::tibble()
}


get_repositories <- function(org, .limit = Inf) {
  lapply(
    X = org,
    FUN = get_repositories_,
    .limit = .limit
  ) |>
    (\(x) { names(x) <- org; x })() |>
    dplyr::bind_rows(.id = "organisation")
}
