#### Info ####

#' Make a local repository by mirroring CRAN
#'
#' This function creates a CRAN-like repository from a local library using functions from miniCRAN package (https://CRAN.R-project.org/package=miniCRAN).
#'
#' @param pathtorepo The path to the folder where the repository is to be created.
#'
#' @param libpath The path to the library where the repository is made from. 
#' The default mirror links are: "https://cran.r-project.org", "https://cloud.r-project.org/", "https://cran.rstudio.com/" and "ftp://cran.r-project.org/pub/R/"
#'
#' @param Rvers The version of R for which the repository is made. 
#' By default, it is set to the current R version (major.minor).
#' 
#' @param multiple.lib if TRUE, the script will cycle through multiple CRAN mirrors checking for packages, by default it is set to FALSE.
#' This will take time and can add packages that are not available in the one specific repository. The default mirror links are: "https://cran.r-project.org", "https://cloud.r-project.org/", "https://cran.rstudio.com/" and "ftp://cran.r-project.org/pub/R/"
#'
#' @return
#'
#' @examples
#' \dontrun{
#' CRANMaker(pathtorepo = "path/to/repo", libpath = "https://cran.r-project.org", Rvers = "4.2", dependencies = TRUE)
#' }
#'
#' Created by Istvan Laszlo Gyimes (ig16036[at]essex.ac.uk)
#' R version 4.2.2
#' RStudio RStudio 2022.07.1+554 "Spotted Wakerobin" Release (7872775ebddc40635780ca1ed238934c3345c5de, 2022-07-22) for Windows Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.8 Chrome/69.0.3497.128 Safari/537.36
#' Last modified 01-DEC-2023
#' @export

CRANMaker <- function(pathtorepo, librepo = c("https://cran.r-project.org", "https://cloud.r-project.org/", "https://cran.rstudio.com/", "ftp://cran.r-project.org/pub/R/"), Rvers = paste0(R.version$major, ".", R.version$minor), multiple.lib = FALSE){
  
  # Step 0 - Make sure that the folder for the repository exists, if not, create it
  
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
  
  # Step 0.1 - Set up the repository variable
  
  if (multiple.lib == FALSE) {
    librepo <- librepo[1]
    additional.libs <- list()
  } else {
    additional.libs <-  librepo[2:length(librepo)]
    librepo <- librepo[1]
  }
  
  # Step 0.2 - Ensure that we have the packages we will use
  
  if (!require(miniCRAN)) {
    install.packages("miniCRAN", repos = librepo)
  }
  
  if (!require(tools)) {
    install.packages("tools", repos = librepo)
  }
  
  if (!require(dplyr)) {
    install.packages("dplyr", repos = librepo)
  }
  
  # Step 1 - Create local repos for all R versions defined
  warnings_errors <- list()
  for (rv in Rvers) {
    cat(paste0("Repository for R ", rv, " is being made.\n"))
    
    # Step 2 - Create a list of the packages to install
      packages_to_make_repo_from_bin <- miniCRAN::pkgAvail(repos = librepo, type = "win.binary", Rversion = rv)[,1]
      packages_to_make_repo_from_src <- miniCRAN::pkgAvail(repos = librepo, type = "source", Rversion = rv)[,1]
      
      # Ensure that we do not download the same things again and again
      
      if (dir.exists(paste0(pathtorepo, "/src/contrib"))) {
        packages_we_have_src <- gsub(".tar.gz", "", dir(paste0(pathtorepo, "/src/contrib")))
        srccheck <- paste0(miniCRAN::pkgAvail(repos = librepo, type = "source", Rversion = rv)[,1], "_", miniCRAN::pkgAvail(repos = librepo, type = "source", Rversion = rv)[,2])
        packages_to_make_repo_from_src <- sub("_(.*)", "", srccheck[!srccheck %in% intersect(srccheck, packages_we_have_src)])
      }
    # Step 3 - Make the repository
      miniCRAN::makeRepo(pkgs = packages_to_make_repo_from_bin,
                         path = pathtorepo,
                         type = c("win.binary"),
                         download = T,
                         writePACKAGES = F,
                         quiet = T,
                         repos = librepo,
                         Rversion =  rv)
      miniCRAN::makeRepo(pkgs = packages_to_make_repo_from_src,
                         path = pathtorepo,
                         type = c("source"),
                         download = T,
                         writePACKAGES = F,
                         quiet = T,
                         repos = librepo,
                         Rversion =  rv)
    # Checkpoint - Do we have all packages from the list?
      for (lr in c(librepo, additional.libs)) {
        print(lr)
        packages_we_have_src <- gsub(".tar.gz", "", dir(paste0(pathtorepo, "/src/contrib")))
        packages_we_have_bin <- gsub(".zip", "", dir(paste0(pathtorepo, "/bin/windows/contrib/", rv)))
        srccheck <- paste0(miniCRAN::pkgAvail(repos = librepo, type = "source", Rversion = rv)[,1], "_", miniCRAN::pkgAvail(repos = librepo, type = "source", Rversion = rv)[,2])
        wincheck <- paste0(miniCRAN::pkgAvail(repos = librepo, type = "win.binary", Rversion = rv)[,1], "_", miniCRAN::pkgAvail(repos = librepo, type = "win.binary", Rversion = rv)[,2])
        
        if (length(srccheck[!srccheck %in% intersect(srccheck, packages_we_have_src)]) == 0 & length(wincheck[!wincheck %in% intersect(wincheck, packages_we_have_bin)]) == 0) {
          break
        } else {
          #If not, let's add the missing ones
          if (!length(srccheck[!srccheck %in% intersect(srccheck, packages_we_have_src)]) == 0) {
            pks <- sub("_(.*)", "", srccheck[!srccheck %in% intersect(srccheck, packages_we_have_src)])
            miniCRAN::addPackage(pkgs = pks,
                                 path = pathtorepo,
                                 repos = lr,
                                 type = "source",
                                 Rversion = rv,
                                 writePACKAGES = F,
                                 deps = F,
                                 quiet = T)
          }
          
          if (!length(wincheck[!wincheck %in% intersect(wincheck, packages_we_have_bin)]) == 0) {
            pks <- sub("_(.*)", "", wincheck[!wincheck %in% intersect(wincheck, packages_we_have_bin)])
            miniCRAN::addPackage(pkgs = pks,
                                 path = pathtorepo,
                                 repos = lr,
                                 type = "win.binary",
                                 Rversion = rv,
                                 writePACKAGES = F,
                                 deps = F,
                                 quiet = T)
          }
        }
      }
      
    # Step 4 - Write PACKAGES for the bin folder for each R version
    
    tools::write_PACKAGES(dir = paste0(pathtorepo, "/bin/windows/contrib/", rv, "/"), type = "win.binary")
    cat(paste0("Repository for R versions ", Rvers, " is done", "\n"))
  }
  
  # Step 5 - Create a folder for archiving packages when updates are being done
  
  if(!file.exists(paste0(pathtorepo, "/Archive/"))){
    dir.create(path = paste0(pathtorepo, "/Archive/"))
  }
  
  # Step 6 - Write the PACKAGES files
  
  # Step 6.1 - Write PACKAGES for src folder
  
  tools::write_PACKAGES(dir = paste0(pathtorepo, "/src/contrib/"), type = "source")
  
  # Step 6.2 - Write PACKAGES for Archive folder
  
  if (!length(dir(paste0(pathtorepo, "/Archive/src/contrib/"))) == 0) {
    tools::write_PACKAGES(dir = paste0(pathtorepo, "/Archive/src/contrib/"), type = "source")
  }
}



