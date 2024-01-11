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
#' @param multiple.lib if TRUE, the script will cycle through multiple CRAN mirrors checking for packages, by default it is set to FALSE.
#' This will take time and can add packages that are not available in the one specific repository. The default mirror links are: "https://cran.r-project.org", "https://cloud.r-project.org/", "https://cran.rstudio.com/" and "ftp://cran.r-project.org/pub/R/"
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

#### Update a local repository mirrored from CRAN ####

CRANRepoUpdater <- function(pathtorepo, librepo = c("https://cran.r-project.org", "https://cloud.r-project.org/", "https://cran.rstudio.com/", "ftp://cran.r-project.org/pub/R/"), Rvers = paste0(R.version$major, ".", R.version$minor), multiple.lib = FALSE){
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
  
  if (multiple.lib == FALSE) {
    librepo <- librepo[1]
    additional.libs <- list()
  } else {
    additional.libs <-  librepo[2:length(librepo)]
    librepo <- librepo[1]
  }
  # Step 1 - Check if the Archive folder exists and if not, create one
  
  if (!dir.exists(paste0(pathtorepo, "/Archive/"))) {
    dir.create(path = paste0(pathtorepo, "/Archive/"))
  }
  
  # Step 2 - Check which packages have updates
  for (rv in Rvers) {
    print(paste0("Updating R ", rv))
    oldsrc <- as.matrix(miniCRAN::oldPackages(path = pathtorepo, repos = librepo, type = "source", Rversion = rv)[, 1:3]) # Any outdated source files?
    oldwinbin <- as.matrix(miniCRAN::oldPackages(path = pathtorepo, repos = librepo, type = "win.binary", Rversion = rv)[, 1:3]) # Any outdated windows binary files?
    
    # Step 3 - Add the old versions to the Archive folder (making sure that both old source and old windows binary files are added as source files to the Archive folder)
    # Step 3.1 - Move the old source files
    
    movesrcs <- list.files(path = paste0(pathtorepo, "/src/contrib/"))
    
    movesrcs <- subset(movesrcs, movesrcs %in% paste0(oldsrc[,1], "_", oldsrc[,2], ".tar.gz"))
    file.copy(from = paste0(pathtorepo, "/src/contrib/", movesrcs), to = paste0(pathtorepo, "/Archive/src/contrib/"), overwrite = F)
    
    if (length(oldwinbin) > 0) {
      # Step 3.2 - Clean the list of old binary files
      oldwinbin2 <- paste0(oldwinbin[,1], "_", oldwinbin[,2], ".tar.gz")
      oldwinbin2 <- subset(oldwinbin2, !oldwinbin2 %in% dir(paste0(pathtorepo, "/Archive/src/contrib")))
      oldwinbin2 <- unlist(lapply(oldwinbin2, function(x) sub("_.*", "", x)))
      if (!is.null(oldwinbin2)) {
        oldwinbin <- dplyr::filter(data.frame(oldwinbin), data.frame(oldwinbin)[,1] %in% oldwinbin2)
      }else{
        oldwinbin <- data.frame(oldwinbin)
      }
      
      # Step 3.3 - Download the archived package versions
      for (packrun in 1:length(oldwinbin)) {
        miniCRAN::addOldPackage(oldwinbin[packrun,1], path = paste0(pathtorepo, "/Archive/"), vers = oldwinbin[packrun,2], repos = librepo, type = "source", Rversion = rv, writePACKAGES = F)
      }
    }
    # Step 4 - Update the packages
    print(paste0("Updating source packages for R version ", rv))
    miniCRAN::updatePackages(path = pathtorepo, repos = librepo, type = "source", ask = F, quiet = T, oldPkgs = oldsrc[,1], Rversion = rv)
    print(paste0("Updating windows binary packages for R version ", rv))
    miniCRAN::updatePackages(path = pathtorepo, repos = librepo, type = "win.binary", ask = F, quiet = T, oldPkgs = oldwinbin[,1], Rversion = rv)
    
    # Step 4.1 - Check if we have all packages from CRAN
    counter <- 1
    CRANava_bin <- miniCRAN::pkgAvail(repos = librepo, type = "win.binary", Rversion = rv, quiet = T)
    
    whatwehave_bin <- matrix(dir(path = paste0(pathtorepo, "/bin/windows/contrib/", rv))[grepl(".zip", dir(path = paste0(pathtorepo, "/bin/windows/contrib/", rv)))])
    whatwehave_bin <- matrix(unlist(strsplit(gsub(".zip", "", whatwehave_bin), "_")), ncol = 2, byrow = T)
    whatwedonthave_bin <- setdiff(whatwehave_bin[,1], CRANava_bin[,1])
    libsr <- c(librepo, additional.libs)
    while (!length(whatwedonthave_bin)==0 & counter <= length(libsr)) {
      print(paste0(counter, "/", length(libsr)))
      
      miniCRAN::addPackage(pkgs = whatwedonthave_bin,
                           path = pathtorepo,
                           repos = libsr[counter],
                           type = c("win.binary", "source"),
                           Rversion = rv,
                           writePACKAGES = F,
                           deps = F,
                           quiet = T)
      counter <- counter+1
      whatwehave_bin <- matrix(dir(path = paste0(pathtorepo, "/bin/windows/contrib/", rv))[grepl(".zip", dir(path = paste0(pathtorepo, "/bin/windows/contrib/", rv)))])
      whatwehave_bin <- matrix(unlist(strsplit(gsub(".zip", "", whatwehave_bin), "_")), ncol = 2, byrow = T)
      whatwedonthave_bin <- setdiff(whatwehave_bin[,1], CRANava_bin[,1])
    }

    
    # Step 4.2 -  Check if we really did update all of them
    whatwehave_bin <- matrix(dir(path = paste0(pathtorepo, "/bin/windows/contrib/", rv))[grepl(".zip", dir(path = paste0(pathtorepo, "/bin/windows/contrib/", rv)))])
    whatwehave_bin <- matrix(unlist(strsplit(gsub(".zip", "", whatwehave_bin), "_")), ncol = 2, byrow = T)
    diffbin <- setdiff(paste0(CRANava_bin[,1], "_", CRANava_bin[,2]), paste0(whatwehave_bin[,1], "_", whatwehave_bin[,2]))
    if(length(diffbin)>0){
      whatwedonthaveupdated_bin <- matrix(unlist(strsplit(diffbin, "_")), ncol = 2, byrow = T)
      
      if (length(whatwedonthaveupdated_bin) > 0) {
        counter <- 1
        libsr <- c(librepo, additional.libs)
        while (!length(whatwedonthaveupdated_bin)==0 & counter <= length(libsr)) {
          print(paste0(counter, "/", length(libsr)))
          
          miniCRAN::addPackage(pkgs = whatwedonthaveupdated_bin[,1],
                               path = pathtorepo,
                               repos = librepo,
                               type = c("win.binary", "source"),
                               Rversion = rv,
                               writePACKAGES = F,
                               deps = F,
                               quiet = T)
          counter <- counter+1
          whatwehave_bin <- matrix(dir(path = paste0(pathtorepo, "/bin/windows/contrib/", rv))[grepl(".zip", dir(path = paste0(pathtorepo, "/bin/windows/contrib/", rv)))])
          whatwehave_bin <- matrix(unlist(strsplit(gsub(".zip", "", whatwehave_bin), "_")), ncol = 2, byrow = T)
          whatwedonthaveupdated_bin <- matrix(unlist(strsplit(setdiff(paste0(CRANava_bin[,1], "_", CRANava_bin[,2]), paste0(whatwehave_bin[,1], "_", whatwehave_bin[,2])), "_")), ncol = 2, byrow = T)
        }
      }
    }
    
    # Step 4.3 - Write the PACKAGES files
    
    tools::write_PACKAGES(dir = paste0(pathtorepo, "/bin/windows/contrib/", rv, "/"), type = "win.binary")
    
    # Do the stes 4.1 - 4.3 for the source files as well
    counter <- 1
    CRANava_src <- miniCRAN::pkgAvail(repos = librepo, type = "source", Rversion = rv, quiet = T)
    whatwehave_src <- matrix(dir(path = paste0(pathtorepo, "/src/contrib/"))[grepl(".tar.gz", dir(path = paste0(pathtorepo, "/src/contrib/")))])
    whatwehave_src <- matrix(unlist(strsplit(gsub(".tar.gz", "", whatwehave_src), "_")), ncol = 2, byrow = T)
    whatwedonthave_src <- setdiff(whatwehave_src[,1], CRANava_src[,1])
    libsr <- c(librepo, additional.libs)
    while (!length(whatwedonthave_src)==0 & counter <= length(libsr)) {
      print(paste0(counter, "/", length(libsr)))
      
      miniCRAN::addPackage(pkgs = whatwedonthave_src,
                           path = pathtorepo,
                           repos = libsr[counter],
                           type = "source",
                           Rversion = rv,
                           writePACKAGES = F,
                           deps = F,
                           quiet = T)
      
      counter <-  counter+1
      whatwehave_src <- matrix(dir(path = paste0(pathtorepo, "/src/contrib/"))[grepl(".tar.gz", dir(path = paste0(pathtorepo, "/src/contrib/")))])
      whatwehave_src <- matrix(unlist(strsplit(gsub(".tar.gz", "", whatwehave_src), "_")), ncol = 2, byrow = T)
      whatwedonthave_src <- setdiff(whatwehave_src[,1], CRANava_src[,1])
    }
    
    # Check if we really did update all of them
    whatwehave_src <- matrix(dir(path = paste0(pathtorepo, "/src/contrib/"))[grepl(".tar.gz", dir(path = paste0(pathtorepo, "/src/contrib/")))])
    whatwehave_src <- matrix(unlist(strsplit(gsub(".tar.gz", "", whatwehave_src), "_")), ncol = 2, byrow = T)
    diffsrc <- setdiff(paste0(CRANava_src[,1], "_", CRANava_src[,2]), paste0(whatwehave_src[,1], "_", whatwehave_src[,2]))
    if (length(diffsrc)>0) {
      whatwedonthaveupdated_src <- matrix(unlist(strsplit(diffsrc, "_")), ncol = 2, byrow = T)
      if (length(whatwedonthaveupdated_src) > 0) {
        counter <- 1
        libsr <- c(librepo, additional.libs)
        while (!length(whatwedonthave_src)==0 & counter <= length(libsr)) {
          print(paste0(counter, "/", length(libsr)))
          miniCRAN::addPackage(pkgs = whatwedonthaveupdated_src[,1],
                               path = pathtorepo,
                               repos = librepo,
                               type = "source",
                               Rversion = rv,
                               writePACKAGES = F,
                               deps = F,
                               quiet = T)
          whatwehave_src <- matrix(dir(path = paste0(pathtorepo, "/src/contrib/"))[grepl(".tar.gz", dir(path = paste0(pathtorepo, "/src/contrib/")))])
          whatwehave_src <- matrix(unlist(strsplit(gsub(".tar.gz", "", whatwehave_src), "_")), ncol = 2, byrow = T)
          diffsrc <- setdiff(paste0(CRANava_src[,1], "_", CRANava_src[,2]), paste0(whatwehave_src[,1], "_", whatwehave_src[,2]))
          if (length(diffsrc)>0) {
            whatwedonthaveupdated_src <- matrix(unlist(strsplit(diffsrc, "_")), ncol = 2, byrow = T)
            counter <- counter+length(libsr)
          }
          
          counter <- counter+1
        }
      }
    }
  }
  
  
  # Step 5 - Write the PACKAGES files for the source and Archive files
  tools::write_PACKAGES(dir = paste0(pathtorepo, "/src/contrib/"), type = "source")
  tools::write_PACKAGES(dir = paste0(pathtorepo, "/Archive/src/contrib/"), type = "source") # For the Archive
}
