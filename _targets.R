################################################################################
#
# Open Science and Reproducible Research in R Lecture Series Evaluation
#
################################################################################

## Load libraries and custom functions -----------------------------------------
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)


## Set workflow options --------------------------------------------------------


## Create targets and list targets objects -------------------------------------

### Data targets
data_targets <- tar_plan(
  ## Create class lists
  ihtm_class_list = create_student_lists(
    org = "OxfordIHTM", 
    team = paste0("class-", 2020:as.numeric(format(Sys.Date(), format = "%Y")))
  ),
  ## Get all repositories
  ihtm_repositories = get_repositories(org = "OxfordIHTM"),
  ## Get all branches
  ihtm_branches = get_repos_branches(
    owner = "OxfordIHTM", 
    repo = c(
      "ihtm-hackathon-2023", 
      ihtm_repositories |> 
        dplyr::filter(
          stringr::str_detect(name, "working-with-data-in-r-")
        ) |> 
        dplyr::pull(name)
    )
  ),
  ## Get repository commits per branch
  ihtm_commits = get_commits(
    owner = "OxfordIHTM", 
    repo = ihtm_branches$repository, 
    branch = ihtm_branches$name
  ),
  ## Get pull requests
  ihtm_pulls = get_pulls(
    owner = "OxfordIHTM", 
    repo = c(
      "ihtm-hackathon-2023", 
      ihtm_repositories |> 
        dplyr::filter(
          stringr::str_detect(name, "working-with-data-in-r-")
        ) |> 
        dplyr::pull(name)
    )
  ),
  ## Get pull requests per commit
  ihtm_pull_commits = get_pull_commits(
    owner = "OxfordIHTM",
    repo = ihtm_commits$repository,
    commit = ihtm_commits$sha
  )
)


### Processing targets
processing_targets <- tar_plan(
  
)


### Analysis targets
analysis_targets <- tar_plan(
  
)


### Output targets
output_targets <- tar_plan(
  
)


### Reporting targets
report_targets <- tar_plan(
  
)


### Deploy targets
deploy_targets <- tar_plan(
  
)


## List targets
list(
  data_targets,
  processing_targets,
  analysis_targets,
  output_targets,
  report_targets,
  deploy_targets
)
