#### Info ####

#' Make a local repository by mirroring CRAN
#'
#' This function creates a CRAN-like repository from a local library using functions from miniCRAN package (https://CRAN.R-project.org/package=miniCRAN).
#'
#' @param pathtorepo The path to the folder where the repository is to be created.
#'
#' @param libpath The path to the library where the repository is made from. 
#' By default, it is set to "https://cloud.r-project.org/".
#'
#' @param Rvers The version of R for which the repository is made. 
#' By default, it is set to the current R version (major.minor).
#' 
#' @param dependencies Whether the function will look up dependencies for each package, by default it is set to FALSE.
#' This will take time and can add packages that are not available in the repository. You may have to add these packages separately from a different repository
#'
#' @return A message indicating the success of the repository creation.
#'
#' @examples
#' \dontrun{
#' create_repository(pathtorepo = "path/to/repo", libpath = "path/to/library", Rvers = "4.2")
#' }
#'
#' Created by Istvan Laszlo Gyimes (ig16036[at]essex.ac.uk)
#' R version 4.2.2
#' RStudio RStudio 2022.07.1+554 "Spotted Wakerobin" Release (7872775ebddc40635780ca1ed238934c3345c5de, 2022-07-22) for Windows Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.8 Chrome/69.0.3497.128 Safari/537.36
#' Last modified 20-NOV-2023
#' @export



#### Make a local repository by mirroring CRAN ####
CRANMaker <- function(pathtorepo, librepo = "https://cloud.r-project.org/", Rvers = paste0(R.version$major, ".", R.version$minor), dependencies = FALSE){
  # If provided path does not exist, we create it
  checkpoint <- 0
  while (checkpoint == 0) {
    if (!dir.exists(pathtorepo)) {
      shallwecreate <- readline("The folder for the repository does not exist, would you like to create it? [y/n] ")
      if (toupper(shallwecreate) == "Y") {
        cat(paste0("Creating folder: ", pathtorepo, ".\n"))
        dir.create(path = pathtorepo)
        checkpoint <- 1
      } else {
        shallwechange <- readline("Would you like to give a new path? [y/n] ")
        if (toupper(shallwechange) == "Y") {
          pathtorepo <- readline("Please add the new path here: ")
        } else {
          cat("Terminating function.\n")
          return(invisible())
        }
      }
    } else {
      checkpoint <- 1
    }
  }
  
  # Step 0 - Ensure that we have the packages we will use
  
  if (!require(miniCRAN)) {
    install.packages("miniCRAN", repos = librepo)
  }
  
  if (!require(tools)) {
    install.packages("tools", repos = librepo)
  }
  
  # Step 1 - Create a list of the packages to install
  
  packages_to_add <- data.frame(miniCRAN::pkgAvail(repos = librepo, type = "win.binary", Rversion = Rvers))$Package
  
  # Step 2 - Increase the list by adding the dependencies for each package # Note, this may include packages not avaiable on the library specified
  if (dependencies == TRUE) {
    packages_to_add_withdeps <- miniCRAN::pkgDep(pkg = packages_to_add, repos = librepo, type = "win.binary", depends = T, suggests = F, includeBasePkgs = F, quiet = T, Rversion =  Rvers)
    packages_to_make_repo_from <- unlist(as.list(packages_to_add_withdeps))
  } else {
    packages_to_make_repo_from <- packages_to_add
  }
  
  # Step 3 - Create a local repository with both source and binary versions
  miniCRAN::makeRepo(pkgs = packages_to_make_repo_from,
                               path = pathtorepo,
                               type = c("win.binary", "source"),
                               download = T,
                               writePACKAGES = F,
                               quiet = T,
                               repos = librepo,
                               Rversion =  Rvers)
  
  
  # Step 4 - Create a folder for archiving packages when updates are being done
  if(!file.exists(paste0(pathtorepo, "/Archive/"))){
    dir.create(path = paste0(pathtorepo, "/Archive/"))
  }
  tools::write_PACKAGES(dir = paste0(pathtorepo, "/bin/windows/contrib/", Rvers, "/"))
  tools::write_PACKAGES(dir = paste0(pathtorepo, "/src/contrib/"))
  if (!length(dir(paste0(pathtorepo, "/Archive/src/contrib/"))) == 0) {
    tools::write_PACKAGES(dir = paste0(pathtorepo, "/Archive/src/contrib/"))
  }
}
