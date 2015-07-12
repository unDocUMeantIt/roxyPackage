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


#' Download package dependencies
#' 
#' Tries to fetch all (missing) R packages to successfully build a Debian package. The packages are downloaded in source format
#' for you to \code{\link[roxyPackage:debianize]{debianize}}, but \code{dep4deb} can try to check for available Debian packages
#' instead.
#' 
#' The function works its way recursively through the dependencies of the dependencies, beginning with the original package given.
#' To make it easier for you to debianize the downloaded packages in a proper order, all downloads will be stored in numbered
#' subfolders of the main download folder, and you should work from the highest number backwards.
#' 
#' @param pck.source.dir Character string, path pointing to the root directory of your package sources, to a local R package source tarball, or
#'    a full URL to such a package tarball. Tarballs will be downloaded to \code{destdir}, if needed, extracted, and then checked for dependencies.
#'    Will be ignored if \code{pck.name} is not \code{NULL}.
#' @param pck.name Character string, the package name. This is an alternative to using \code{pck.source.dir}.
#' @param destdir File path to the directory where all downloaded files should be saved to.
#' @param repos Character vector, the base URL(s) of the repositories to use (see \code{\link[utils:download.packages]{download.packages}}).
#' @param all Logical, if \code{FALSE} only currently missing packages are downloaded, where "missing" means that there is no Debian package
#'    if these packages installed. If \code{TRUE} and \code{check.deb=FALSE}, all dependencies will be downloaded.
#' @param check.deb Logical, \code{TRUE} it will be checked if a debian package can be found, and if that is the case, its name is added
#'    to the results and the download skipped. If \code{all=FALSE}, packages will only be listed in the results if they are not installed.
#' @param origin A character string for the package origin, see \code{\link[roxyPackage:debianize]{debianize}}.
#' @param origin.alt A named list for more complex origin configuration, see \code{\link[roxyPackage:debianize]{debianize}}.
#' @param available An object as returned by \code{\link[utils:available.packages]{available.packages}} listing packages available at the
#'    repositories, or NULL which makes an internal call to available.packages.
#' @export
#' @return Returns a list with two elements:
#'    \describe{
#'      \item{dl}{A matrix as returned by \code{\link[utils:download.packages]{download.packages}}, listing all downloaded sources}
#'      \item{deb}{A character vector naming already available Debian packages}
#'    }
#' @examples
#' \dontrun{
#' dep4deb(pck.name="roxyPackage")
#' }

dep4deb <- function(pck.source.dir, pck.name=NULL, destdir=file.path(tempdir(),"roxyPackge","downloads"), repos=getOption("repos"),
  all=FALSE, check.deb=TRUE, origin="cran", origin.alt=list(), available=NULL){

  if(is.null(pck.name)){
    pck.source.dir <- deb.check.sources(src=pck.source.dir, dl.dir=destdir)
    # read the description file
    pck.dscrptn <- as.data.frame(read.dcf(file=file.path(pck.source.dir, "DESCRIPTION")), stringsAsFactors=FALSE)

    # fetch pakcage name and dependencies from that
    package.name <- as.character(pck.dscrptn[["Package"]])
    package.deps <- splitDepends(dep=c(as.character(pck.dscrptn[["Depends"]]), as.character(pck.dscrptn[["Imports"]])))
  } else {
    package.name <- package.deps <- pck.name
  }

  pre.results <- dl.missing.dep.src(deps=package.deps, destdir=destdir, subdir=package.name, order=1, recursive=TRUE,
    repos=repos, all=all, check.deb=check.deb, origin=origin, origin.alt=origin.alt, available=available)

  results <- list(dl=as.matrix(pre.results[["dl.result"]]), deb=pre.results[["deb"]])
  if(!is.null(results[["dl"]])){
    dl.folder <- file.path(destdir, package.name)
    message(paste("files were downloaded to:\n ", dl.folder))
  } else {}

  return(results)
}
