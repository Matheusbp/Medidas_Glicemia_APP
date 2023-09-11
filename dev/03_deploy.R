# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

# Test your app

## Run checks ----
## Check the package before sending to prod
devtools::check()
# rhub::check_for_cran()

# Deploy

## Local, CRAN or Package Manager ----
## This will build a tar.gz that can be installed locally,
## sent to CRAN, or to a package manager
devtools::build()
# renv::install(packages = "./deploy/MedidasGlicemia_0.0.0.9000.tar.gz")
remotes::install_local("./deploy/MedidasGlicemia_0.0.0.9000.tar.gz",upgrade="never", dependencies = "./renv.lock")
## RStudio ----

## Docker ----
## If you want to deploy via a generic Dockerfile
golem::add_dockerfile_with_renv(output_dir = "deploy", lockfile = "renv.lock", document = "DESCRIPTION")
