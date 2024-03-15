#'
#' Get GitHub repositories
#' 
#' @param org A character value or vector of character values for name of 
#'   GitHub organisation from which to retrieve repository/ies information from.
#' @param .limit Number of results to return. Default to `Inf`.
#' 
#' @returns A tibble with rows for information for each repository within the
#'   specified organisation.
#' 
#' @examples
#' get_repositories("OxfordIHTM")
#' 
#' @export
#'

get_repositories_ <- function(org, .limit = Inf) {
  gh::gh(
    endpoint = "/orgs/{org}/repos",
    org = org,
    .limit = .limit
  ) |>
    jsonlite::toJSON() |>
    jsonlite::fromJSON(flatten = TRUE) |>
    lapply(unlist) |>
    (\(x) do.call(cbind, x))() |>
    data.frame() |>
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
