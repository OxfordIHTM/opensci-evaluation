################################################################################
#
#'
#' Create student list
#' 
#' @param org GitHub organisation name
#' @param team GitHub organisation team name
#' 
#' @return A data.frame listing students by their GitHub usernames
#' 
#' @examples 
#' create_student_list(
#'   org = "OxfordIHTM", team = "class-2023"
#' )
#'
#'
#
################################################################################

create_student_list <- function(org, team, .limit = Inf) {
  gh::gh(
    endpoint = "/orgs/{org}/teams/{team_slug}/members",
    org = org, team_slug = team, .limit = .limit
  ) |>
    dplyr::bind_rows() |>
    #dplyr::select(login, id, html_url) |>
    dplyr::filter(!login %in% c("ernestguevarra", "proochista"))
}


create_student_lists <- function(org, team, .limit = Inf) {
  if (length(org) == 1) {
    org <- rep(org, length(team))
  }
  
  if (length(org) < length(team)) {
    stop(
      "Length of org should be the same as length of team. Try again.",
      call. = TRUE
    )
  }
  
  org <- as.list(org)
  team <- as.list(team)
  
  Map(
    f = create_student_list,
    org = org,
    team = team,
    .limit = .limit
  ) |>
    (\(x) { names(x) <- unlist(team); x })() |>
    dplyr::bind_rows(.id = "ihtm_class")
}
