################################################################################
#
#
#
################################################################################

get_repo_branches <- function(owner, repo) {
  gh::gh(
    endpoint = "/repos/{owner}/{repo}/branches",
    owner = owner, repo = repo
  ) |>
    jsonlite::toJSON() |>
    jsonlite::fromJSON(flatten = TRUE) |>
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
