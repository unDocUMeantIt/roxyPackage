# Copyright 2011-2015 Meik Michalke <meik.michalke@hhu.de>
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


#' Read/write ChangeLog files
#'
#' These functions ans methods can be used to manage ChangeLog files.
#'
#' The ChangeLog files used for R packages are usually required to have a standard format, if they are supposed to be parsed
#' by functions like \code{tools::news2Rd}:
#' \enumerate{
#'   \item entries are named "Changes in version <version number>" (and optionally a YYYY-MM-DD date string afterwards).
#'      The date string is mandatory if you want to use the ChageLog functions in roxyPackage. The version number can be given in
#'      both <major>.<minor>-<revision> or <major>.<minor>.<revision> format.
#'   \item they have single changes properly itemized, by indentation and then either \code{"o"}, \code{"-"} or \code{"*"} followed
#'     by space
#'   \item optionally have categories as subsections, like "Fixed", "Changed" or "Added"
#' }
#'
#' \code{readChangeLog} tries to read a given ChangeLog file and parse its content to generate a special ChangeLog object.
#'
#' \code{writeChangeLog} takes such a ChangeLog object to write it back to a file. If \code{file=NULL}, the log will be returned to stdout.
#'
#' \code{initChangeLog} generates a ChangeLog object from scratch, e.g., to get started with a new package.
#'
#' @param file Character string, path to the ChangeLog file to read.
#' @param head Character string, the headline text of the ChangeLog file (without the package name).
#' @param change Character string, the text introducing each ChnageLog entry for a package version.
#' @param item Character string, the text marking each entry item.
#' @return An object of class \code{ChangeLog}.
#' @aliases
#'    readChangeLog
#'    writeChangeLog
#'    initChangeLog
#' @include 00_classes_01_ChangeLog_internal.R
#' @include 01_methods_01_ChangeLog.R
#' @export
#' @rdname readChangeLog
#' @seealso \code{\link[roxyPackage:getChangeLogEntry]{getChangeLogEntry}}, \code{\link[roxyPackage:updateChangeLog]{updateChangeLog}}
#' @examples
#' \dontrun{
#' changelog <- readChangeLog("/home/user/myRsources/myRpackage/ChangeLog")
#' }
readChangeLog <- function(file, head="ChangeLog for package", change="changes in version", item="  -"){
  logObject <- parseChangeLog(file=file, head=head, change=change, item=item)
  return(logObject)
}


#' @param log An object of class \code{ChangeLog}.
#' @param lineEnd Integer number, indicates where to do line breaks.
#' @export
#' @rdname readChangeLog
writeChangeLog <- function(log, file=NULL, head="ChangeLog for package", change="changes in version", item="  -", lineEnd=78){
  breakBy <- paste0("\n", paste0(rep(" ", nchar(item) + 1), collapse=""))
  logObject <- pasteChangeLog(log=log, file=file, head=head, change=change, item=item, lineEnd=lineEnd, breakAt=c(" "), breakBy=breakBy)
  return(logObject)
}


#' @export
#' @param entry A (named) list of character vectors. The element names will become the ChangeLog
#'    sections, each vector element an item.
#' @param version Character string, version number to look up.
#' @param date The date of the ChangeLog entry in \code{YYYY-MM-DD} format. will be coerced into
#'    character. To keep the date stamp of a present entry, set \code{date=NULL}.
#' @param package Character string, the package name.
#' @rdname readChangeLog
#' @include 00_classes_01_ChangeLog_internal.R
initChangeLog <- function(entry=list(changed=c("initial release"), fixed=c("missing ChangeLog")), package="unknown", version="0.01-1", date=Sys.Date()){
  stopifnot(is.list(entry))
  newEntry <- new("ChangeLog.entry",
    version=version,
    date=as.character(date),
    entry=as(entry, "ChangeLog.items"))
  newLog <- new("ChangeLog",
    package=package,
    entries=list(newEntry)
  )
  # ok, this is ugly, but for what it's worth...
  slot(newLog, "fullLog") <- unlist(strsplit(pasteChangeLog(newLog), "\n"))
  return(newLog)
}
