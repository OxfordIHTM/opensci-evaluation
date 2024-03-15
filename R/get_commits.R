#'
#' Get all commits information from a GitHub repository
#' 
#' @param owner GitHub repository owner username
#' @param repo GitHub repository name
#' @param branch GitHub repository branch in `repo` to get commits from
#' @param .limit Number of records to return. Set to retrieve all records by
#'   default.
#' 
#' @returns A data.frame with information on a number of commits made to 
#'   specified GitHub repository
#'   
#' @examples 
#' get_repo_commits(owner = "OxfordIHM", repo = "ihtm-hackathon-2023")
#' 
#' @export
#'

get_commits_ <- function(owner, 
                         repo, 
                         branch = "main",
                         .limit = Inf) {
  gh::gh(
    endpoint = "/repos/{owner}/{repo}/commits",
    owner = owner, 
    repo = repo,
    sha = branch,
    .limit = .limit
  ) |>
    jsonlite::toJSON() |>
    jsonlite::fromJSON(flatten = TRUE) |>
    lapply(unlist) |>
    (\(x) do.call(cbind, x))() |>
    data.frame() |>
    tibble::tibble() |>
    dplyr::mutate(
      owner = owner,
      repository = repo,
      branch = branch
    )
}


get_commits <- function(owner, 
                        repo,
                        branch, 
                        .limit = Inf) {
  if (length(owner) == 1) {
    owner <- rep(owner, length(branch))
  }
  
  if (length(owner) < length(branch)) {
    stop(
      "Length of owner should either be 1 or the same as length of branch.",
      call. = TRUE
    )
  }
  
  if (length(repo) == 1) {
    repo <- rep(repo, length(branch))
  }
  
  if (length(repo) < length(branch)) {
    stop(
      "Length of repo should either be 1 or the same as length of branch.",
      call. = TRUE
    )
  }
  
  owner <- as.list(owner)
  repo <- as.list(repo)
  branch <- as.list(branch)
  
  Map(
    f = get_commits_,
    owner = owner,
    repo = repo,
    branch = branch,
    .limit = .limit
  ) |>
    dplyr::bind_rows()
}




get_pull_commit <- function(owner, repo, commit, .limit = Inf) {
  gh::gh(
    endpoint = "/repos/{owner}/{repo}/commits/{commit_sha}/pulls",
    owner = owner,
    repo = repo,
    commit_sha = commit
  ) |>
    jsonlite::toJSON() |>
    jsonlite::fromJSON(flatten = TRUE) |>
    tibble::tibble() |>
    dplyr::mutate(
      owner = owner,
      repository = repo,
      sha = commit
    )
}


get_pull_commits <- function(owner, repo, commit, .limit = Inf) {
  if (length(owner) == 1) {
    owner <- rep(owner, length(commit))
  }
  
  if (length(owner) < length(commit)) {
    stop(
      "Length of owner should be either 1 or equal to length of commit. Try again.",
      call. = TRUE
    )
  }
  
  if (length(repo) == 1) {
    repo = rep(repo, length(commit))
  }
  
  if (length(repo) < length(commit)) {
    stop(
      "Length of repo should be either 1 or equal to length of commit. Try again.",
      call. = TRUE
    )
  }
  
  owner <- as.list(owner)
  repo <- as.list(repo)
  commit <- as.list(commit)
  
  Map(
    f = get_pull_commit,
    owner = owner,
    repo = repo,
    commit = commit,
    .limit = .limit
  ) |>
    dplyr::bind_rows()
}

