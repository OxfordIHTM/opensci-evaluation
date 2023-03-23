################################################################################
#
#'
#' Calculate various GitHub commits metrics
#' 
#' @param commits A data.frame/tibble of commits from specified Oxford IHTM
#'   repositories participated in by students generated from the `get_commits()`
#'   functions
#' @param start_date A date value specifying the starting date from which to
#'   calculate the number of commits per day. Default is 2022-01-01
#'   
#' @return A data.frame/tibble of number of commits by day, by person or by
#'   person by day
#'   
#' @rdname calculate_commits
#'
#
################################################################################

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
    dplyr::ungroup() |>
    dplyr::mutate(
      ihtm_class = paste0("class-", lubridate::year(commit_date)),
      .before = commit_date
    )
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
    dplyr::ungroup() |>
    dplyr::mutate(
      ihtm_class = paste0("class-", lubridate::year(as.Date(commit_week))),
      .before = commit_week
    )
}

calculate_commits_per_author <- function(commits, class_list) {
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
    ) |>
    dplyr::right_join(
      y = class_list |> 
        subset(select = c(login, id, ihtm_class)), 
      by = c("username" = "login", "author_id" = "id")
    ) |>
    dplyr::relocate(ihtm_class, .before = author_id) |>
    dplyr::arrange(ihtm_class, username) |>
    dplyr::rename(id = author_id)
}

calculate_commits_per_author_per_day <- function(commits, class_list) {
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
    ) |>
    dplyr::right_join(
      y = class_list |> 
        subset(select = c(login, id, ihtm_class)), 
      by = c("username" = "login", "author_id" = "id")
    ) |>
    dplyr::relocate(ihtm_class, .before = commit_date) |>
    dplyr::rename(id = author_id) |>
    dplyr::arrange(ihtm_class, commit_date, username)
}


calculate_commits_per_author_per_week <- function(commits, class_list) {
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
    ) |>
    dplyr::right_join(
      y = class_list |> 
        subset(select = c(login, id, ihtm_class)), 
      by = c("username" = "login", "author_id" = "id")
    ) |>
    dplyr::relocate(ihtm_class, .before = commit_week) |>
    dplyr::rename(id = author_id) |>
    dplyr::arrange(ihtm_class, commit_week, username)
}

