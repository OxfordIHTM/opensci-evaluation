################################################################################
#
# Open Science and Reproducible Research in R Lecture Series Evaluation
#
################################################################################

# Load libraries and custom functions ------------------------------------------
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)


# Set workflow options ---------------------------------------------------------


# Create targets and list targets objects -------------------------------------

## Data targets ----
data_targets <- tar_plan(
  ### Create class lists
  ihtm_class_list = create_student_lists(
    org = "OxfordIHTM", 
    team = paste0("class-", 2020:as.numeric(format(Sys.Date(), format = "%Y")))
  ),
  ### Get all repositories
  ihtm_repositories = get_repositories(org = "OxfordIHTM"),
  ### Get all branches
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
  ### Get repository commits per branch
  ihtm_commits = get_commits(
    owner = "OxfordIHTM", 
    repo = ihtm_branches$repository, 
    branch = unlist(ihtm_branches$name)
  ),
  ### Get pull requests
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
  ### Get pull requests per commit
  ihtm_pull_commits = get_pull_commits(
    owner = "OxfordIHTM",
    repo = ihtm_commits$repository,
    commit = ihtm_commits$sha
  ),
  ### Get reviews
  ihtm_reviews = get_reviews(
    owner = "OxfordIHTM",
    repo = c(
      "ihtm-hackathon-2023", 
      ihtm_repositories |> 
        dplyr::filter(
          stringr::str_detect(name, "working-with-data-in-r-")
        ) |> 
        dplyr::pull(name)
    )
  )
)


## Processing targets ----
processing_targets <- tar_plan(
  ### IHTM commits metrics
  ihtm_commits_per_day = calculate_commits_per_day(
    ihtm_commits |>
      dplyr::filter(
        !author.login %in% c("ernestguevarra", "github-classroom[bot]")
      )
  ),
  ihtm_commits_per_week = calculate_commits_per_week(
    ihtm_commits |>
      dplyr::filter(
        !author.login %in% c("ernestguevarra", "github-classroom[bot]")
      )
  ),
  ihtm_commits_per_author = calculate_commits_per_author(
    commits = ihtm_commits |>
      dplyr::filter(
        !author.login %in% c("ernestguevarra", "github-classroom[bot]")
      ),
    class_list = ihtm_class_list
  ),
  ihtm_commits_per_author_per_day = calculate_commits_per_author_per_day(
    commits = ihtm_commits |>
      dplyr::filter(
        !author.login %in% c("ernestguevarra", "github-classroom[bot]")
      ),
    class_list = ihtm_class_list
  ),
  ihtm_commits_per_author_per_week = calculate_commits_per_author_per_week(
    commits = ihtm_commits |>
      dplyr::filter(
        !author.login %in% c("ernestguevarra", "github-classroom[bot]")
      ),
    class_list = ihtm_class_list
  ),
  ### IHTM pull request metrics
  ihtm_commits_by_pulls = create_commits_by_pulls(
    class_list = ihtm_class_list,
    commits = ihtm_commits,
    pulls = ihtm_pulls,
    pulls_commits = ihtm_pull_commits
  ) |>
    dplyr::filter(
      !commit_username %in% c("ernestguevarra", "github-classroom[bot]")
    ) |>
    get_pulls_merged(),
  ihtm_rates = calculate_rates(ihtm_commits_by_pulls),
  ihtm_rates_per_day = calculate_rates_per_time(
    commits_by_pulls = ihtm_commits_by_pulls, period = "day"
  ),
  ihtm_rates_per_week = calculate_rates_per_time(
    commits_by_pulls = ihtm_commits_by_pulls, period = "week"
  ),
  ihtm_rates_per_author = calculate_rates_per_author(
    commits_by_pulls = ihtm_commits_by_pulls,
    class_list = ihtm_class_list
  ),
  ihtm_rates_per_author_per_day = calculate_rates_per_author_per_time(
    commits_by_pulls = ihtm_commits_by_pulls,
    class_list = ihtm_class_list,
    period = "day"
  ),
  ihtm_rates_per_author_per_week = calculate_rates_per_author_per_time(
    commits_by_pulls = ihtm_commits_by_pulls,
    class_list = ihtm_class_list,
    period = "week"
  )
)


## Analysis targets
analysis_targets <- tar_plan(
  
)


## Output targets
output_targets <- tar_plan(
  
)


## Reporting targets
report_targets <- tar_plan(
  tar_render(
    name = rrr_evaluation_report_html_2023,
    path = "reports/rrr_evaluation_2023.Rmd",
    output_dir = "docs",
    knit_root_dir = here::here(),
    cue = tar_cue("always")
  ),
  tar_target(
    name = rrr_evaluation_report_pdf_2023,
    command = pagedown::chrome_print(
      input = rrr_evaluation_report_html_2023[1],
      output = "outputs/rrr_evaluation_2023.pdf"
    )
  )
)


## Deploy targets
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
