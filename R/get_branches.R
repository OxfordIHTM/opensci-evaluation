#'
#' Get information on branches of repositories from GitHub
#' 
#' @param owner A character value or a vector of character values for owner of
#'   repository/ies to get branches information from
#' @param repo A character value or a vector of character values for name of 
#'   repository/ies to get branch information from
#'   
#' @returns A tibble with each row containing information on branches in
#'   repository/ies
#'   
#' @examples
#' get_repo_branches("OxfordIHTM", "ihtm-hackathon-2023")
#' get_repos_branches(
#'   "OxfordIHTM", c("ihtm-hackathon-2023", "ihtm-hackathon-2024")
#' )
#'
#' @export
#'

get_repo_branches <- function(owner, repo) {
  gh::gh(
    endpoint = "/repos/{owner}/{repo}/branches",
    owner = owner, repo = repo
  ) |>
    jsonlite::toJSON() |>
    jsonlite::fromJSON() |>
    dplyr::bind_rows() |>
    tibble::tibble() |>
    dplyr::mutate(owner = owner, repository = repo)
}


get_repos_branches <- function(owner, repo) {
  if (length(owner) == 1) {
    owner <- rep(owner, length(repo))
  } 
  
  if (length(owner) < length(repo)) {
    stop(
      "Length of owner should be equal to length of repo. Try again.",
      call. = TRUE
    )
  }
  
  owner <- as.list(owner)
  repo <- as.list(repo)
  
  Map(
    f = get_repo_branches,
    owner = owner,
    repo = repo
  ) |>
    dplyr::bind_rows()
}
