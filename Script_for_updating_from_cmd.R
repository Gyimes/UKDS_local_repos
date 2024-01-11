#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
ac <- data.frame(matrix(unlist(args), ncol = 2, byrow = T))
colnames(ac) <- c("Arg", "Val")
argsexpected <- data.frame(matrix(data = c("-ps", "-pr", "-libr", "-version", "-ml"), ncol = 1))
colnames(argsexpected) <- "Arg"
argsexpected <- merge(x = argsexpected, y = ac, all.x = T)
print(argsexpected)

# Organising arguments
# Step 1 - path to script
psfinder <- function(argsexpected){
  ps <- argsexpected[grepl("-ps", argsexpected[,1]),2]
  # Did the user add the script name?
  scriptchecker <- function(ps){
    if (grepl("CRANRepoUpdater.R",ps)) {
      fchecker <- file.exists(ps)
    } else{
      ps <- paste0(ps, "/CRANRepoUpdater.R")
      fchecker <- file.exists(ps)
    }
    out <- list()
    out[[1]] <- fchecker
    out[[2]] <- ps
    return(out)
  }
  fchecker <- scriptchecker(ps)[[1]]
  ps <- scriptchecker(ps)[[2]]
  user.exit <- 0
  while (fchecker == F & user.exit == 0) {
    user.input <- readline(prompt = "R Script not found, would you like to correct the path? y/n ")
    if (tolower(user.input) == "y") {
      ps <- readline(prompt = "Enter the correct path to the R script: ")
      fchecker <- scriptchecker(ps)[[1]]
      ps <- scriptchecker(ps)[[2]]
    } else {
      user.exit <- 1
      quit(save = "no")
    }
  }
  if (user.exit == 0) {
    source(ps)
  }
  return(invisible(NULL))
}

psfinder(argsexpected = argsexpected) # And so the script is in the global environment

ge <- ls()
if (!length(ge[grepl("CRANRepoUpdater", ge)]) == 1 ){
  print("ERROR: No script found")
}

# Step 2 - path to repo
pr <- argsexpected[grepl("-pr", argsexpected[,1]),2]
user.exit <- 0
while (user.exit == 0 & !file.exists(pr)) {
  user.input <- readline(promt = "Specified repository not found, would you like to correct the path? y/n ")
  if (tolower(user.input) == "y") {
    pr <- readline(promt = "Path to repository: ")
  } else {
    user.exit <- 1
    quit(save = "no")
  }
}

# Step 3 - version argument
if (is.na(argsexpected[grepl("-version", argsexpected[,1]),2])) {
  Rversi <- dir(paste0(pr, "/bin/windows/contrib/"))
}else{
  Rversi <- argsexpected[grepl("-version", argsexpected[,1]),2]
}

# Step 4 - librepo argument
if (!is.na(argsexpected[grepl("-version", argsexpected[,1]),2])) {
  librepo <- argsexpected[grepl("-libr", argsexpected[,1]),2]
}

# Step 5 - multiple lib argument
if (!is.na(argsexpected[grepl("-ml", argsexpected[,1]),2])) {
  multiple.lib <- argsexpected[grepl("-ml", argsexpected[,1]),2]
}

# Step 6 - summarise
cat("Updating R version(s): ", Rversi, "\n", "Path to repo: ", pr, "/n")
if (exists("librepo")) {
  if (exists("multiple.lib")) {
    CRANRepoUpdater(pathtorepo = pr, librepo = librepo, Rvers = Rversi, multiple.lib = multiple.lib)
  } else {
    CRANRepoUpdater(pathtorepo = pr, librepo = librepo, Rvers = Rversi)
  }
} else {
  if (exists("multiple.lib")) {
    CRANRepoUpdater(pathtorepo = pr, Rvers = Rversi, multiple.lib = multiple.lib)
  } else {
    CRANRepoUpdater(pathtorepo = pr, Rvers = Rversi)
  }
}
