# Copyright 2011-2014 Meik Michalke <meik.michalke@hhu.de>
#
# This file is part of the R package roxyPackage.
#
# roxyPackage is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# roxyPackage is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with roxyPackage.  If not, see <http://www.gnu.org/licenses/>.


#' Deal with old packages in your local repository
#' 
#' Use this function to move older versions of a package to a specified archive directory,
#' or remove them completely.
#' 
#' @note This function responds to \code{\link[roxyPackage:sandbox]{sandbox}}.
#'
#' @param repo.root Path to the repository root, i.e., the directory which contains the \code{src}
#'    and \code{bin} directories. Usually this path should start with "\code{file:///}".
#' @param to.dir Character string, name of the folder to move the old packages to.
#' @param keep An integer value defining the maximum nuber of versions to keep. Setting this to 0 will
#'    completely remove all packages from the repository, which is probably only useful in combination
#'    with the option \code{package}.
#' @param package A character vector with package names to check. If set, \code{archive.packages} will only
#'    take actions on these packages. If \code{NULL}, all packages are affected.
#' @param type A character vector defining the package formats to keep. Valid entries are \code{"source"},
#'    \code{"win.binary"} and \code{"mac.binary.leopard"}. By default, only the source packages are archived, all other
#'    packages are deleted.
#' @param archive.root Path to the archive root, i.e., the directory to which files should be moved. Usually 
#'    the Archive is kept i \code{repo.root}
#' @param overwrite Logical, indicated whether existing files in the archive can be overwritten.
#' @param reallyDoIt Logical, real actions are only taken if set to \code{TRUE}, otherwise the actions
#'    are only printed.
#' @seealso \code{\link[roxyPackage:sandbox]{sandbox}} to run archive.packages() in a sandbox.
#' @export
#' @examples
#' \dontrun{
#' # dry run, only prints what would happen, so you can check
#' # if that's really what you want
#' archive.packages("file:///var/www/repo")
#' 
#' # after we've confirmed that the right packages will be moved
#' # and deleted, let's actually commit the changes
#' archive.packages("file:///var/www/repo", reallyDoIt=TRUE)
#' 
#' # if we don't want a standard archive, but for instance a parallel
#' # archive repository, we can have it. let's move all but the latest two
#' # versions from /var/www/repo to /var/www/archive. to suppress the
#' # creation of a special archive directory, we set to.dir=""
#' archive.packages("file:///var/www/repo", to.dir="", keep=2,
#'   type=c("source", "win.binary", "mac.binary.leopard"),
#'   archive.root="/var/www/archive", reallyDoIt=TRUE)
#' }
archive.packages <- function(repo.root, to.dir="Archive", keep=1, package=NULL, type="source",
  archive.root=repo.root, overwrite=FALSE, reallyDoIt=FALSE){

  old.opts <- getOption("available_packages_filters")
  on.exit(options(available_packages_filters=old.opts))
  options(available_packages_filters=list(noFilter=function(x){x}))

  all.valid.types <- c("source", "win.binary", "mac.binary.leopard")
  if(any(!type %in% all.valid.types)){
    stop(simpleError("archive: invalid package type specified!"))
  } else {}

  if(isTRUE(check.sandbox())){
    message("preparing sandbox...")
    # prepare folder structure; this will also insure sane values and abort
    # if locations are invalid. the function returns a list of paths to use
    ## this needs some love...
    # check for "file://" in this function
    adjust.paths <- prepare.sandbox.archive(repo.root=repo.root, archive.root=archive.root, to.dir=to.dir)
    # replace paths with sandbox
    repo.root <- normalizePathByOS(path=adjust.paths[["repo.root"]], is.unix=isUNIX(), mustWork=TRUE, filePrefix=TRUE)
    archive.root <- adjust.paths[["archive.root"]]
    to.dir <- adjust.paths[["to.dir"]]
    sandbox.status()
  } else {}

  if(!grepl("^file:///", repo.root)){
    warning("'repo.root' does not start with 'file:///', are you sure this is correct?")
  }

  archInRepo <- identical(repo.root, archive.root)
  clean.repo.root <- gsub("/$", "", repo.root)
  clean.archive.root <- ifelse(isTRUE(archInRepo), clean.repo.root, gsub("/$", "", archive.root))

  ## create a complete inventory of the repository, at least for all packages queried
  in.repo <- list()
  repo.src <- file.path(clean.repo.root, "src", "contrib")
  archive.src <- ifelse(isTRUE(archInRepo), repo.src, file.path(clean.archive.root, "src", "contrib"))
  in.repo[["source"]] <- filter.repo.packages(available.packages(repo.src, type="source"), packages=package)

  repo.win <- listRDirs(file.path(clean.repo.root,  "bin", "windows", "contrib"), full.path=TRUE)
  if(isTRUE(archInRepo)){
    archive.win <- repo.win
  } else {
    archive.win <- file.path(clean.archive.root, "bin", "windows", "contrib",
      listRDirs(file.path(clean.repo.root, "bin", "windows", "contrib"), full.path=FALSE))
  }
  # in.repo[["win.binary"]] will be an empty list() if the directory doesn't exist
  in.repo[["win.binary"]] <- lapply(repo.win, function(this.R){
      return(filter.repo.packages(available.packages(this.R, type="win.binary"), packages=package))
    })

  # in.repo[["mac.binary.leopard"]] will be an empty list() if the directory doesn't exist
  repo.mac <- listRDirs(file.path(clean.repo.root, "bin", "macosx", "leopard", "contrib"), full.path=TRUE)
  if(isTRUE(archInRepo)){
    archive.mac <- repo.mac
  } else {
    archive.mac <- file.path(clean.archive.root, "bin", "macosx", "leopard", "contrib",
      listRDirs(file.path(clean.repo.root, "bin", "macosx", "leopard", "contrib"), full.path=FALSE))
  }
  in.repo[["mac.binary.leopard"]] <- lapply(repo.mac, function(this.R){
      return(filter.repo.packages(available.packages(this.R, type="mac.binary.leopard"), packages=package))
    })

  ## now go through all type entries in the inventory and check for each package seperately
  ## whether the maximum number of packages was reached

  for (this.type in all.valid.types){
    # iterate through in.repo$this.type$Package
    checkThisRepo <- in.repo[[this.type]]
    if(length(checkThisRepo) > 0){
      if(identical(this.type, "source")){
        for (this.package in unique(checkThisRepo[,"Package"])){
          presentPackages <- checkThisRepo[checkThisRepo[,"Package"] == this.package,]
          # even if there's only one package, ensure it's still a matrix
          if(!is.matrix(presentPackages)){
            presentPackages <- t(as.matrix(presentPackages))
          }
          presentVersions <- presentPackages[,"Version"]
          presentVersions <- presentVersions[order(package_version(presentVersions), decreasing=TRUE)]
          if(keep > 0){
            keepVersions <- presentVersions[1:keep]
          } else {
            keepVersions <- ""
          }
          moveVersions <- presentVersions[!presentVersions %in% keepVersions]
          if(length(moveVersions) > 0){
            if(this.type %in% type){
              mvToArchive(this.package, repo=repo.src, archive=file.path(archive.src, to.dir), versions=moveVersions, 
                type=this.type, overwrite=overwrite, reallyDoIt=reallyDoIt)
            } else {
              mvToArchive(this.package, repo=repo.src, archive=file.path(archive.src, to.dir), versions=moveVersions, 
                type=this.type, overwrite=overwrite, reallyDoIt=reallyDoIt, justDelete=TRUE)
            }
            # update PACKAGES
            if(isTRUE(reallyDoIt)){
              tools::write_PACKAGES(dir=gsub("^file:(/)+", "/", repo.src), type="source", verbose=FALSE, latestOnly=FALSE)
              message("archive: updated src/contrib/PACKAGES (source)")
            } else {
              message("archive: updated src/contrib/PACKAGES (source) (NOT RUN!)")
            }
          } else {}
        }
      } else {
        for (this.R.num in 1:length(checkThisRepo)){
          this.R <- checkThisRepo[[this.R.num]]
          for (this.package in unique(this.R[,"Package"])){
            presentPackages <- this.R[this.R[,"Package"] == this.package,]

            # even if there's only one package, ensure it's still a matrix
            if(!is.matrix(presentPackages)){
              presentPackages <- t(as.matrix(presentPackages))
            }
            presentVersions <- presentPackages[,"Version"]
            presentVersions <- presentVersions[order(package_version(presentVersions), decreasing=TRUE)]
            if(keep > 0){
              keepVersions <- presentVersions[1:keep]
            } else {
              keepVersions <- ""
            }
            moveVersions <- presentVersions[!presentVersions %in% keepVersions]
            if(length(moveVersions) > 0){
              if(identical(this.type, "win.binary")){
                this.repo <- repo.win[this.R.num]
                this.archive <- archive.win[this.R.num]
                writePCKtype <- "win.binary"
              } else {
                this.repo <- repo.mac[this.R.num]
                this.archive <- archive.mac[this.R.num]
                writePCKtype <-"mac.binary"
              }
              if(this.type %in% type){
                mvToArchive(this.package, repo=this.repo, archive=file.path(this.archive, to.dir), versions=moveVersions, 
                  type=this.type, overwrite=overwrite, reallyDoIt=reallyDoIt)
              } else {
                mvToArchive(this.package, repo=this.repo, archive=file.path(this.archive, to.dir), versions=moveVersions, 
                  type=this.type, overwrite=overwrite, reallyDoIt=reallyDoIt, justDelete=TRUE)
              }
              # update PACKAGES
              if(isTRUE(reallyDoIt)){
                tools::write_PACKAGES(dir=gsub("^file:(/)+", "/", this.repo), type=writePCKtype, verbose=FALSE, latestOnly=FALSE)
                message(paste0("archive: updated bin/PACKAGES (", this.type, ")"))
              } else {
                message(paste0("archive: updated bin/PACKAGES (", this.type, ") (NOT RUN!)"))
              }
            } else {}
          }
        }
      }
    } else {}
  }

  return(invisible(NULL))
}
