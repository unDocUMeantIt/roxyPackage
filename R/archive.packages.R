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
#' @param keep.revisions An integer value defining the maximum nuber of revisions to keep. This is only
#'    used when archiving Debian packages, i.e., if \code{type} includes \code{"deb"}. Setting this to
#'    0 or \code{NULL} will keep all revisions of package versions that are to be kept.
#' @param package A character vector with package names to check. If set, \code{archive.packages} will only
#'    take actions on these packages. If \code{NULL}, all packages are affected.
#' @param type A character vector defining the package formats to keep. Valid entries are \code{"source"},
#'    \code{"win.binary"}, \code{"mac.binary"}, and \code{"deb"}. By default, only the source packages are
#'    archived, all other packages are deleted, except for Debian repos, which currently can only be archived
#'    or be left as is.
#' @param archive.root Path to the archive root, i.e., the directory to which files should be moved. Usually 
#'    the Archive is kept in \code{repo.root}.
#' @param overwrite Logical, indicates whether existing files in the archive can be overwritten.
#' @param reallyDoIt Logical, real actions are only taken if set to \code{TRUE}, otherwise the actions
#'    are only printed.
#' @param graceful Logical, if \code{TRUE} the process will not freak out because of missing files. Use this
#'    for instance if you deleted files from the repo but did not update the package indices.
#' @param deb.options A named list of options that must be properly set if you want to archive Debian packages. After packages were
#'    removed from the repo, all Packages, Sources and Release files must be re-written and signed, and all of the following
#'    information is required: \code{distribution}, \code{component}, \code{gpg.key}, and \code{keyring}
#'    (which might be \code{NULL}). If you omit \code{gpg.version}, version 2 is assumed by default.
#'    See \code{\link[roxyPackage:debianize]{debianize}} for details.
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
#'   type=c("source", "win.binary", "mac.binary"),
#'   archive.root="/var/www/archive", reallyDoIt=TRUE)
#' }
archive.packages <- function(repo.root, to.dir="Archive", keep=1, keep.revisions=2, package=NULL, type="source",
  archive.root=repo.root, overwrite=FALSE, reallyDoIt=FALSE, graceful=FALSE,
  deb.options=list(
    distribution="unstable",
    component="main",
    gpg.version=2,
    gpg.key=NULL,
    keyring=NULL
  )){

  old.opts <- getOption("available_packages_filters")
  on.exit(options(available_packages_filters=old.opts))
  options(available_packages_filters=list(noFilter=function(x){x}))

  all.valid.types <- c("source", "win.binary", "mac.binary", "deb")
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
  in.repo[["win.binary"]] <- lapply(
    repo.win,
    function(this.R){
      return(filter.repo.packages(available.packages(this.R, type="win.binary"), packages=package))
    }
  )

  repo.mac <- listRDirs(file.path(clean.repo.root, "bin", "macosx", "contrib"), full.path=TRUE)
  if(isTRUE(archInRepo)){
    archive.mac <- repo.mac
  } else {
    archive.mac <- file.path(clean.archive.root, "bin", "macosx", "contrib",
      listRDirs(file.path(clean.repo.root, "bin", "macosx", "contrib"), full.path=FALSE))
  }
  # in.repo[["mac.binary"]] will be an empty list() if the directory doesn't exist
  in.repo[["mac.binary"]] <- lapply(
    repo.mac,
    function(this.R){
      return(filter.repo.packages(available.packages(this.R, type="mac.binary"), packages=package))
    }
  )

  ## now go through all type entries in the inventory and check for each package seperately
  ## whether the maximum number of packages was reached

  for (this.type in all.valid.types){
    if(identical(this.type, "deb") & "deb" %in% type){
      # remove "file://" from path
      deb.repo <- gsub("^file:(/)+", "/", clean.repo.root)
      if(dir.exists(file.path(deb.repo, "deb"))){
        # archiving Debian packages is done by a specialised internal function,
        # see roxyPackage-internal_debianize.R
        didArchiveSomething <- deb.archive.packages(repo.root=file.path(deb.repo, "deb"), to.dir=to.dir,
          keep.versions=keep, keep.revisions=keep.revisions, package=package,
          archive.root=clean.archive.root, overwrite=overwrite, reallyDoIt=reallyDoIt
        )
        # update Packages, Sources & Release files
        if(isTRUE(didArchiveSomething)){
          if(isTRUE(reallyDoIt)){
            # update package information
            deb.gen.package.index(
              repo=file.path(deb.repo, "deb"), binary=TRUE, distribution=deb.options[["distribution"]], component=deb.options[["component"]]
            )
            # update sources information
            deb.gen.package.index(
              repo=file.path(deb.repo, "deb"), binary=FALSE, distribution=deb.options[["distribution"]], component=deb.options[["component"]]
            )
            # default to gpg2 if no info given
            if(is.null(deb.options[["gpg.key"]])){
              deb.options[["gpg.key"]] <- 2
            } else {}
            deb.update.release(
              repo.root=deb.repo,
              gpg.key=deb.options[["gpg.key"]],
              keyring=deb.options[["keyring"]],
              gpg=Sys.which(GPGversion(key=deb.options[["gpg.key"]], version=deb.options[["gpg.version"]])),
              distribution=deb.options[["distribution"]],
              component=deb.options[["component"]]
            )
            message("archive: updated Debian Packages file")
          } else {
            message("archive: updated Debian Packages file (NOT RUN!)")
          }
        } else {}
      } else {}
    } else {
      # iterate through in.repo$this.type$Package
      checkThisRepo <- in.repo[[this.type]]
      if(length(checkThisRepo) > 0){
        if(identical(this.type, "source")){
          for (this.package in unique(checkThisRepo[,"Package"])){
            presentPackages <- archiveSubset(checkThisRepo, var="Package", values=this.package)
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
                  type=this.type, overwrite=overwrite, reallyDoIt=reallyDoIt, graceful=graceful
                )
              } else {
                mvToArchive(this.package, repo=repo.src, archive=file.path(archive.src, to.dir), versions=moveVersions, 
                  type=this.type, overwrite=overwrite, reallyDoIt=reallyDoIt, justDelete=TRUE, graceful=graceful
                )
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
          for (this.R.num in seq_along(checkThisRepo)){
            this.R <- checkThisRepo[[this.R.num]]
            for (this.package in unique(this.R[,"Package"])){
              presentPackages <- archiveSubset(this.R, var="Package", values=this.package)
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
                    type=this.type, overwrite=overwrite, reallyDoIt=reallyDoIt, graceful=graceful
                  )
                } else {
                  mvToArchive(this.package, repo=this.repo, archive=file.path(this.archive, to.dir), versions=moveVersions, 
                    type=this.type, overwrite=overwrite, reallyDoIt=reallyDoIt, justDelete=TRUE, graceful=graceful
                  )
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
  }

  return(invisible(NULL))
}
