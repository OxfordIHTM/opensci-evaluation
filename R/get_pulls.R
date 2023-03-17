



get_pull <- function(owner, repo, .limit = Inf) {
  gh::gh(
    endpoint = "/repos/{owner}/{repo}/pulls",
    owner = owner,
    repo = repo,
    .limit = .limit
  ) |>
    jsonlite::toJSON() |>
    jsonlite::fromJSON(flatten = TRUE) |>
    tibble::tibble()
}


get_pulls <- function(owner, repo, .limit = Inf) {
  if (length(owner) == 1) {
    owner <- rep(owner, length(repo))
  }
  
  if (length(owner) < length(repo)) {
    stop(
      "Lenth of owner should either be 1 or same as length of repo. Try again.",
      call. = TRUE
    )
  }
  
  owner <- as.list(owner)
  repo <- as.list(repo)
  
  Map(
    f = get_pull,
    owner = owner,
    repo = repo,
    .limit = .limit
  ) |>
    dplyr::bind_rows()
}