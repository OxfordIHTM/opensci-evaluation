

calculate_commits_per_day <- function(commits) {
  commits |>
    dplyr::mutate(
      commit_date = as.Date(
        commit.author.date |>
          unlist(), 
        format = "%F"
      )
    ) |>
    dplyr::group_by(commit_date) |>
    dplyr::count() |>
    dplyr::ungroup()
}


calculate_commits_per_week <- function(commits) {
  commits |>
    dplyr::mutate(
      commit_date = as.Date(
        commit.author.date |>
          unlist(), 
        format = "%F"
      ),
      commit_week = cut(
        x = commit_date,
        breaks = "week",
        start.on.monday = TRUE
      )
    ) |>
    dplyr::group_by(commit_week) |>
    dplyr::count() |>
    dplyr::ungroup()
}


calculate_commits_per_author <- function(commits) {
  commits |>
    dplyr::select(
      commit.author.name, author.id, author.login, 
      commit.committer.name, committer.id
    ) |>
    tidyr::unnest(cols = everything()) |>
    dplyr::rename(
      author_name = commit.author.name,
      author_id = author.id,
      username = author.login,
      committer_name = commit.committer.name,
      committer_id = committer.id
    ) |>
    dplyr::group_by(author_id) |>
    dplyr::summarise(
      username = unique(username),
      n = n(),
      .groups = "drop"
    )
}

calculate_commits_per_author_per_day <- function(commits) {
  commits |>
    dplyr::select(
      commit.author.date, commit.author.name, author.id, author.login, 
      commit.committer.name, committer.id
    ) |>
    tidyr::unnest(cols = everything()) |>
    dplyr::rename(
      commit_date = commit.author.date,
      author_name = commit.author.name,
      author_id = author.id,
      username = author.login,
      committer_name = commit.committer.name,
      committer_id = committer.id
    ) |>
    dplyr::mutate(commit_date = as.Date(commit_date, format = "%F")) |>
    dplyr::group_by(commit_date, author_id) |>
    dplyr::summarise(
      username = unique(username),
      n = n(),
      .groups = "drop"
    )
}


calculate_commits_per_author_per_week <- function(commits) {
  commits |>
    dplyr::select(
      commit.author.date, commit.author.name, author.id, author.login, 
      commit.committer.name, committer.id
    ) |>
    tidyr::unnest(cols = everything()) |>
    dplyr::rename(
      commit_date = commit.author.date,
      author_name = commit.author.name,
      author_id = author.id,
      username = author.login,
      committer_name = commit.committer.name,
      committer_id = committer.id
    ) |>
    dplyr::mutate(
      commit_date = as.Date(commit_date, format = "%F"),
      commit_week = cut(
        x = commit_date,
        breaks = "week",
        start.on.monday = TRUE
      )
    ) |>
    dplyr::group_by(commit_week, author_id) |>
    dplyr::summarise(
      username = unique(username),
      n = n(),
      .groups = "drop"
    )
}

