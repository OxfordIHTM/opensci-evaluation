

create_commits_by_pulls <- function(commits, pulls, pulls_commits) {
  commitsDF <- commits |>
    dplyr::select(
      sha, commit.author.date, commit.author.name, author.id, author.login
    ) |>
    tidyr::unnest(cols = dplyr::everything()) |>
    dplyr::rename(
      commit_date = commit.author.date,
      commit_author = commit.author.name,
      commit_author_id = author.id,
      commit_username = author.login
    ) |>
    dplyr::mutate(commit_date = as.Date(commit_date, format = "%F"))
  
  pullsDF <- pulls_commits |>
    dplyr::select(
      sha, id, number, created_at, merge_commit_sha, user.login, user.id
    ) |>
    tidyr::unnest(cols = dplyr::everything()) |>
    dplyr::rename(
      pr_id = id,
      pr_number = number,
      pr_create_date = created_at,
      pr_merge_sha = merge_commit_sha,
      pr_username = user.login,
      pr_user_id = user.id
    ) |>
    dplyr::mutate(pr_create_date = as.Date(pr_create_date, format = "%F"))
  
  dplyr::left_join(x = commitsDF, y = pullsDF, by = "sha")
}


calculate_commits_to_pull_rate <- function(commit_by_pulls) {
  
}

