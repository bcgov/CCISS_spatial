# CCISS_spatial
A spatial module for the Climate Change Informed Species Selection (CCISS) tool for CWU development.

Directory Working_App contains files for initial spatial prototype that may or may not prove useful. Directory _functions contains R script with two functions for generating feasibility statistics from the BGC projection data. 

## Repository structure

**new_module**

CCISS Shiny app

The app requires the following environment variables.
```
BCGOV_UR="postgres"
BCGOV_PWD=""
BCGOV_DB=""
BCGOV_HOST=""
AWS_HOST=""
```

To set environment variables:
```
readRenviron("file path to .Renviron")
```

To install dependencies for the app:
```
setwd("./new_module")
renv::restore()
```

You will be prompted to activate the project. Answer Y to install or update R packages needed for the app. For details on using `renv`, please refer to [Collaborating with renv](https://rstudio.github.io/renv/articles/collaborating.html)

To run shiny app:
```
runApp("new_module")
```

<br/>

**database**

The database folder stores scripts to create and optimize species feasibility results tables in the database. Please refer to [CCISS Shiny App Database Deployment Runbook](/database/CCISS%20Shiny%20App%20Database%20Deployment%20Runbook.txt) for details on the procedure. To run R scripts in the folder, set working directory to the main folder

<br/>

---

[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](<Redirect-URL>)

Copyright 2021 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
