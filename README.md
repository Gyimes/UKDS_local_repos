# UKDS_local_repos
These are my R functions for creating and managing offline R repositories for secure environments. The goal of these functions to make the creation of local repositories as easy as possible.

The idea of this project came when I was thinking about how to create a user friendly R on a PC or virtual environment without internet connection.

I have created multiple R functions to address the following needs:
  A) Create a local repository based on CRAN*
  B) Create a local repository based on a list of packages
  C) Maintain the local repository by updating existing packages (and add new packages from an online repository)
  D) Add additional packages from other repositories in case of need

*CRAN is deemed as a "safe" repository, so it can be used in a secure or trusted research environment (TRE) without further checks. Note, that if packages from other, less controlled repositories are to be added to a TRE, additional safety protocols are to be followed.

All of these functions are heavily relying on the miniCRAN package https://CRAN.R-project.org/package=miniCRAN or https://github.com/andrie/miniCRAN. Their fantastic functions allowed me to compile these functions I used to create offline repositories. 

These functions are to be compiled into an easy-to-use executable later, so people with no experience in R can also use them.

Functions

CRANMaker(pathtorepo, librepo, Rvers, dependencies)

This function mirrors a full repository downloading all package and all dependencies from the specified library ('libpath'). The CRAN-like repository will be created in the folder specified in the 'pathtorepo' variable.
You can define which version of R the repository is for with 'Rvers', by default it is set to the version of R you are running.
dependencies variable is used to decide whether or not look up dependencies for each package as well. Note, that this will most likely lead to warnings as packages that are not part of the library specified in 'libpath' can be picked up. These packages will not be added, as the function cannot find them. However, you will get a list of package names the function could not download. You can use this list to add these packages from another repository.

RepoUpdater(pathtorepo, librepo, Rvers, addnews)

This function takes an already created local repository and updates the packages on it. It also offers the opportunity to get the new packages from the repository as well. Note, that if you have packages from multiple repositories, it will need some tweaking

Addsinglepkg(pkg, pathtorepo, librepo, Rvers)

This function allows adding a single package to the offline repository.

RepoMaker(pkgs, pathtorepo, librepo, Rvers, dependencies)

This function creates a local repository from a list of packages instead of a whole repository

TBC
