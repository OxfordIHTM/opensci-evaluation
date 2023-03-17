

get_repo_details <- function(branches) {
  branches |> 
    dplyr::pull(commit.url) |>
    stringr::str_split(pattern = "/", simplify = TRUE) |>
    (\(x) x[ , 5:6])()
}