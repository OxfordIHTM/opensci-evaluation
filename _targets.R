################################################################################
#
# Seychelles General Targets Workflow
#
################################################################################

## Load libraries and custom functions -----------------------------------------
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)

## Create targets and list targets objects -------------------------------------

### Data targets
data_targets <- tar_plan(
  
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
