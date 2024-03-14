################################################################################
#
#'
#' Create commits by pulls
#'
#
################################################################################

create_commits_by_pulls <- function(class_list,
                                    commits, 
                                    pulls, 
                                    pulls_commits) {
  classDF <- class_list |>
    dplyr::select(ihtm_class, login, id)
  
  commitsDF <- commits |>
    dplyr::select(
      sha, owner, repository, commit.author.date, commit.author.name, 
      author.id, author.login
    ) |>
    tidyr::unnest(cols = dplyr::everything()) |>
    dplyr::rename(
      commit_date = commit.author.date,
      commit_author = commit.author.name,
      commit_author_id = author.id,
      commit_username = author.login
    ) |>
    dplyr::mutate(
      commit_date = as.Date(commit_date, format = "%F"),
      commit_username = ifelse(
        is.na(commit_username) & 
          stringr::str_detect(
            string = repository, pattern = "working-with-data-in-r-"
          ),
        stringr::str_remove_all(
          string = repository,
          pattern = "working-with-data-in-r-"
        ),
        commit_username
      )
    ) |>
    dplyr::group_by(commit_author) |>
    tidyr::fill(commit_username, .direction = "up")
  
  commitsDF <- dplyr::left_join(
    x = classDF[ , c("ihtm_class", "login", "id")], 
    y = commitsDF, 
    by = c("login" = "commit_username"),
    multiple = "all"
  ) |>
    dplyr::mutate(
      commit_author_id = ifelse(is.na(commit_author_id), id, commit_author_id)
    ) |>
    dplyr::relocate(login, .after = commit_author_id) |>
    dplyr::rename(commit_username = login)
  
  pullsDF <- pulls_commits |>
    dplyr::select(
      sha, id, number, 
      created_at, merge_commit_sha, user.login, user.id
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
  
  dplyr::left_join(x = commitsDF, y = pullsDF, by = "sha", multiple = "all")
}


################################################################################
#
#'
#' Calculate rates
#'
#
################################################################################

calculate_pulls_rate <- function(commits_by_pulls) {
  commits_by_pulls |>
    dplyr::summarise(
      n_commits = n(),
      n_pulls = sum(!is.na(unique(pr_id))),
      pulls_rate = n_pulls / n_commits,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      pulls_rate = ifelse(
        is.infinite(pulls_rate), NA, pulls_rate
      )
    )
}

calculate_pulls_rate_per_author <- function(commits_by_pulls, class_list) {
  commits_by_pulls |>
    dplyr::group_by(commit_author_id, commit_username) |>
    calculate_pulls_rate() |>
    dplyr::right_join(
      y = class_list |>
        subset(select = c(login, id, ihtm_class)), 
      by = c("commit_username" = "login", "commit_author_id" = "id")
    ) |>
    dplyr::relocate(ihtm_class, .before = "commit_author_id") |>
    dplyr::rename(
      id = commit_author_id,
      username = commit_username
    ) |>
    dplyr::arrange(ihtm_class, username)
}

calculate_pulls_rate_per_time <- function(commits_by_pulls, 
                                          period = c("day", "week")) {
  period <- match.arg(period)
  
  if (period == "day") {
    commits_by_pulls |>
      dplyr::group_by(commit_date) |>
      calculate_pulls_rate() |>
      dplyr::mutate(
        ihtm_class = paste0("class-", lubridate::year(commit_date))
      ) |>
      dplyr::relocate(ihtm_class, .before = commit_date) |>
      dplyr::arrange(ihtm_class, commit_date)
  }
  
  if (period == "week") {
    commits_by_pulls |>
      dplyr::mutate(
        commit_week = cut(
          x = commit_date,
          breaks = "week",
          start.on.monday = TRUE
        )
      ) |>
      dplyr::group_by(commit_week) |>
      calculate_pulls_rate() |>
      dplyr::mutate(
        ihtm_class = paste0("class-", lubridate::year(as.Date(commit_week))),
        .before = commit_week
      ) |>
      dplyr::arrange(ihtm_class, commit_week)
  }
}


calculate_pulls_rate_per_author_per_time <- function(commits_by_pulls,
                                                     class_list,
                                                     period = c("day", "week")) {
  period <- match.arg(period)
  
  if (period == "day") {
    commits_by_pulls |>
      dplyr::group_by(commit_date, commit_author_id, commit_username) |>
      calculate_pulls_rate() |>
      dplyr::right_join(
        y = class_list |>
          subset(select = c(login, id, ihtm_class)), 
        by = c("commit_username" = "login", "commit_author_id" = "id")
      ) |>
      dplyr::relocate(ihtm_class, .before = commit_date) |>
      dplyr::arrange(ihtm_class, commit_date)
  }
  
  if (period == "week") {
    commits_by_pulls |>
      dplyr::mutate(
        commit_week = cut(
          x = commit_date,
          breaks = "week",
          start.on.monday = TRUE
        )
      ) |>
      dplyr::group_by(commit_week, commit_username, commit_author_id) |>
      calculate_pulls_rate() |>
      dplyr::right_join(
        y = class_list |>
          subset(select = c(login, id, ihtm_class)), 
        by = c("commit_username" = "login", "commit_author_id" = "id")
      ) |>
      dplyr::relocate(ihtm_class, .before = commit_week) |>
      dplyr::arrange(ihtm_class, commit_week)
  }
}


get_pull_merged <- function(owner, repo, pull, .limit = Inf) {
  check_merge <- ifelse(
    is.na(pull),
    NA,
    try(
      gh::gh(
        endpoint = "/repos/{owner}/{repo}/pulls/{pull_number}/merge",
        owner = owner,
        repo = repo,
        pull_number = pull,
        .limit = .limit
      ),
      silent = TRUE
    )
  )
  
  ifelse(
    is.na(check_merge), NA,
    ifelse(
      is.list(check_merge), "merged", "not merged"
    )
  )
}


get_pulls_merged <- function(commits_by_pulls, .limit = Inf) {
  owner <- as.list(commits_by_pulls$owner)
  repo <- as.list(commits_by_pulls$repository)
  pull <- as.list(commits_by_pulls$pr_number)
  
  commits_by_pulls |>
    dplyr::mutate(
      pr_merged =   Map(
        f = get_pull_merged,
        owner = owner,
        repo = repo,
        pull = pull,
        .limit = .limit
      )
    ) |>
    tidyr::unnest(pr_merged)
}




calculate_rates <- function(commits_by_pulls) {
  commits_by_pulls |>
    dplyr::mutate(pr_merged = ifelse(pr_merged == "merged", pr_id, NA)) |>
    dplyr::summarise(
      n_authors = sum(!is.na(unique(commit_username))),
      n_commits = n(),
      n_pulls = sum(!is.na(unique(pr_id))),
      n_merge = sum(!is.na(unique(pr_merged))),
      commits_rate = n_commits / n_authors,
      pulls_rate = commits_rate / n_pulls,
      #merge_rate = commits_rate / n_merge,
      merge_rate = n_pulls / n_merge,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      commits_rate = ifelse(
        is.infinite(commits_rate), NA, commits_rate
      ),
      pulls_rate = ifelse(
        is.infinite(pulls_rate), NA, pulls_rate
      ),
      merge_rate = ifelse(
        is.infinite(merge_rate), NA, merge_rate
      )
    )
}


calculate_rates_per_author <- function(commits_by_pulls, class_list) {
  commits_by_pulls |>
    dplyr::group_by(commit_author_id, commit_username) |>
    calculate_rates() |>
    dplyr::right_join(
      y = class_list |>
        subset(select = c(login, id, ihtm_class)), 
      by = c("commit_username" = "login", "commit_author_id" = "id")
    ) |>
    dplyr::relocate(ihtm_class, .before = "commit_author_id") |>
    dplyr::rename(
      id = commit_author_id,
      username = commit_username
    ) |>
    dplyr::arrange(ihtm_class, username)
}


calculate_rates_per_time <- function(commits_by_pulls, 
                                     period = c("day", "week")) {
  period <- match.arg(period)
  
  if (period == "day") {
    commits_by_pulls |>
      dplyr::group_by(commit_date) |>
      calculate_rates() |>
      dplyr::mutate(
        ihtm_class = paste0("class-", lubridate::year(commit_date))
      ) |>
      dplyr::relocate(ihtm_class, .before = commit_date) |>
      dplyr::arrange(ihtm_class, commit_date)
  }
  
  if (period == "week") {
    commits_by_pulls |>
      dplyr::mutate(
        commit_week = cut(
          x = commit_date,
          breaks = "week",
          start.on.monday = TRUE
        )
      ) |>
      dplyr::group_by(commit_week) |>
      calculate_rates() |>
      dplyr::mutate(
        ihtm_class = paste0("class-", lubridate::year(as.Date(commit_week))),
        .before = commit_week
      ) |>
      dplyr::arrange(ihtm_class, commit_week)
  }
}


calculate_rates_per_author_per_time <- function(commits_by_pulls,
                                                class_list,
                                                period = c("day", "week")) {
  period <- match.arg(period)
  
  if (period == "day") {
    commits_by_pulls |>
      dplyr::group_by(commit_date, commit_author_id, commit_username) |>
      calculate_rates() |>
      dplyr::right_join(
        y = class_list |>
          subset(select = c(login, id, ihtm_class)), 
        by = c("commit_username" = "login", "commit_author_id" = "id")
      ) |>
      dplyr::relocate(ihtm_class, .before = commit_date) |>
      dplyr::arrange(ihtm_class, commit_date)
  }
  
  if (period == "week") {
    commits_by_pulls |>
      dplyr::mutate(
        commit_week = cut(
          x = commit_date,
          breaks = "week",
          start.on.monday = TRUE
        )
      ) |>
      dplyr::group_by(commit_week, commit_username, commit_author_id) |>
      calculate_rates() |>
      dplyr::right_join(
        y = class_list |>
          subset(select = c(login, id, ihtm_class)), 
        by = c("commit_username" = "login", "commit_author_id" = "id")
      ) |>
      dplyr::relocate(ihtm_class, .before = commit_week) |>
      dplyr::arrange(ihtm_class, commit_week)
  }
}

