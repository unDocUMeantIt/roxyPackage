# Copyright 2020 Meik Michalke <meik.michalke@hhu.de>
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

#' Download ORCID icon
#' 
#' A simple wrapper for \code{\link[utils:download.file]{download.file}} to download
#' the official ORCID icon for use in a repository with authenticated ORCID IDs.
#' 
#' @note The \code{mode} argument is fixed to "wb" (binary) to ensure successful downloads
#' also on Windows systems.
#' 
#' @param repo The repository root directory to save the file.
#' @param filename Target filename.
#' @param url The URL to fetch the SVG file from.
#' @param method The download method to use, see \code{\link[utils:download.file]{download.file}}.
#' @param overwrite If \code{FALSE} (default) and the target file already exists, it will
#'    not be replaced with a newly downloaded one.
#' @param ... Further arguments to pass to \code{\link[utils:download.file]{download.file}},
#'    exept for \code{mode} (see Note).
#' @export
#' @examples
#' \dontrun{
#' orcid_icon_fetcher("/tmp")
#' }

orcid_icon_fetcher <- function(
  repo,
  filename="orcid.svg",
  url="https://ndownloader.figshare.com/files/8439047",
  method="auto",
  overwrite=FALSE,
  ...
){
  destfile <- file.path(repo, filename)
  if(any(!file.exists(destfile), isTRUE(overwrite))){
    download.file(
      url=url,
      destfile=destfile,
      method=method,
      mode="wb",
      ...
    )
  } else {
    message(paste0("file exists and was left untouched:\n  ", shQuote(destfile)))
    return(invisible(NULL))
  }
}
