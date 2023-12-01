#### Info ####

#' Make a local repository by mirroring CRAN
#'
#' This function creates a CRAN-like repository from a local library using functions from miniCRAN package (https://CRAN.R-project.org/package=miniCRAN).
#'
#' @param pathtorepo The path to the folder where the repository is.
#'
#' @param libpath The path to the library where the repository is made from. 
#' By default, it is set to "https://cloud.r-project.org/".
#'
#' @param Rvers The version of R for which the repository is made. 
#' By default, it is set to the current R version (major.minor).
#'
#' @return A message indicating the success of the repository creation.
#'
#' @examples
#' \dontrun{
#' CRANRepoUpdater(pathtorepo = "path/to/repo", Rvers = "4.3")
#' }
#'
#' Created by Istvan Laszlo Gyimes (ig16036[at]essex.ac.uk)
#' R version 4.2.2
#' RStudio RStudio 2022.07.1+554 "Spotted Wakerobin" Release (7872775ebddc40635780ca1ed238934c3345c5de, 2022-07-22) for Windows Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.8 Chrome/69.0.3497.128 Safari/537.36
#' Last modified 01-DEC-2023
#' @export
CRANRepoUpdater <- function(pathtorepo, librepo = "https://cloud.r-project.org/", Rvers = paste0(R.version$major, ".", R.version$minor)){
  if (!dir.exists(pathtorepo)) {
    cat("Repository not found.\n")
    return(invisible())
    }
  
  if (!require(miniCRAN)) {
    install.packages("miniCRAN", repos = librepo)
  } # We need miniCRAN available for R
  
  if (!require(tools)) {
    install.packages("tools", repos = librepo)
  }
  
  # Step 1 - Check if the Archive folder exists and if not, create one
  
  if (!dir.exists(paste0(pathtorepo, "/Archive/"))) {
    dir.create(path = paste0(pathtorepo, "/Archive/"))
  }
  
  # Step 2 - Check which packages have updates
  
  oldsrc <- as.matrix(miniCRAN::oldPackages(path = pathtorepo, repos = librepo, type = "source", Rversion = Rvers)[, 1:3]) # Any outdated source files?
  oldwinbin <- as.matrix(miniCRAN::oldPackages(path = pathtorepo, repos = librepo, type = "win.binary", Rversion = Rvers)[, 1:3]) # Any outdated windows binary files?
  
  # Step 3 - Add the old versions to the Archive folder (making sure that both old source and old windows binary files are added as source files to the Archive fodler)
    # Step 3.1 - Move the old source files
  
    movesrcs <- list.files(path = paste0(pathtorepo, "/src/contrib/"))
  
    movesrcs <- subset(movesrcs, movesrcs %in% paste0(oldsrc[,1], "_", oldsrc[,2], ".tar.gz"))
    file.copy(from = paste0(pathtorepo, "/src/contrib/", movesrcs), to = paste0(pathtorepo, "/Archive/src/contrib/"), overwrite = F)
    
    # Step 3.2 - Clean the list of old binary files
    
    oldwinbin2 <- paste0(oldwinbin[,1], "_", oldwinbin[,2], ".tar.gz")
    oldwinbin2 <- subset(oldwinbin2, !oldwinbin2 %in% dir(paste0(pathtorepo, "/Archive/src/contrib")))
    oldwinbin2 <- unlist(lapply(oldwinbin2, function(x) sub("_.*", "", x)))
    oldwinbin <- oldwinbin[oldwinbin[,1] %in% oldwinbin2,]
    # Step 3.3 - Download the archived package versions
    miniCRAN::addOldPackage(oldwinbin[,1], path = paste0(pathtorepo, "/Archive/"), vers = oldwinbin[,2], repos = librepo, type = "source", Rversion = Rvers, writePACKAGES = F)
  
  # Step 4 - Update the packages
  
  miniCRAN::updatePackages(path = pathtorepo, repos = librepo, type = "source", ask = F, quiet = T, oldPkgs = oldsrc[,1], Rversion = Rvers)
  
  miniCRAN::updatePackages(path = pathtorepo, repos = librepo, type = "win.binary", ask = F, quiet = T, oldPkgs = oldwinbin[,2], Rversion = Rvers)
  
    #Step 4.1 - Clean up duplicates
    archived <- dir(paste0(pathtorepo, "/Archive/src/contrib"))
    sources <- dir(paste0(pathtorepo, "/src/contrib"))
    duplicates <- sources[sources %in% archived]
    if (length(duplicates) > 0) {
      file.remove(paste0(paste0(pathtorepo, "/src/contrib"), duplicates))
    }
  # Step 5 - Make sure that the index files are up-to-date, note that this may take quite long
  tools::write_PACKAGES(dir = paste0(pathtorepo, "/bin/windows/contrib/", Rvers, "/"), type = "win.binary")
  tools::write_PACKAGES(dir = paste0(pathtorepo, "/src/contrib/"), type = "source")
  tools::write_PACKAGES(dir = paste0(pathtorepo, "/Archive/src/contrib/"), type = "source") # For the Archive
}
