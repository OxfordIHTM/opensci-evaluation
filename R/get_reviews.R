

get_review <- function(owner, repo, .limit = Inf) {
  gh::gh(
    endpoint = "/repos/{owner}/{repo}/pulls/comments",
    owner = owner,
    repo = repo,
    .limit = .limit
  ) |>
    jsonlite::toJSON() |>
    jsonlite::fromJSON(flatten = TRUE) |>
    tibble::tibble()
}


get_reviews <- function(owner, repo, .limit = Inf) {
  if (length(owner) == 1) {
    owner <- rep(owner, length(repo))
  }
  
  if (length(owner) < length(repo)) {
    stop(
      "Length of owner can be of length 1 or the same as lenth of repo. Try again.",
      call. = TRUE
    )
  }
  
  owner <- as.list(owner)
  
  Map(
    f = get_review,
    owner = owner,
    repo = repo,
    .limit = .limit
  ) |>
    dplyr::bind_rows()
}