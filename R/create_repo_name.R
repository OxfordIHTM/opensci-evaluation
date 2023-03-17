################################################################################
#
#'
#' Create a GitHub repository name for GitHub Classroom assignments
#' 
#' @param repo_base_name Repository base name for GitHub Classroom assignment
#' @param username A single GitHub username or a vector of GitHub usernames for 
#'   persons participating in GitHub Classroom assignment
#'   
#' @return A single GitHub repository username or a vector of GitHub repository
#'   usernames
#'   
#' @examples
#' 
#' create_repository_name(
#'   repo_base_name = "working-with-data-in-r", username = "ernestguevarra"
#' )
#'
#
################################################################################

create_repository_name <- function(repo_base_name, username) {
  paste0(repo_base_name, "-", username)
}

