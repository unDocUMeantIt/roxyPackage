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


# this file covers all internal functions to manage ChangeLogs


#' Read/write ChangeLog objects
#'
#' This methods can be used to manage ChangeLog objects.
#'
#' \code{getChangeLogEntry} takes a ChangeLog object and a version number string and returns the according entry.
#'
#' @param log An object of class \code{ChangeLog}.
#' @param ... Additional options, as of now only \code{version} is supported (see below).
#' @return An object of class \code{ChangeLog}.
#' @export
#' @examples
#' \dontrun{
#' changelog <- readChangeLog("/home/user/myRsources/myRpackage/ChangeLog")
#' CL.entry <- getChangeLogEntry(changelog, version="0.02-22")
#' }
#' @seealso \code{\link[roxyPackage:readChangeLog]{readChangeLog}},
#'   \code{\link[roxyPackage:updateChangeLog]{updateChangeLog}}
#' @docType methods
#' @import methods
#' @rdname getChangeLogEntry-methods
#' @include 00_classes_01_ChangeLog_internal.R
setGeneric("getChangeLogEntry", function(log, ...){
    standardGeneric("getChangeLogEntry")
  })

#' @rdname getChangeLogEntry-methods
#' @param version Character string, version number to look up.
#' @import methods
#' @aliases
#'    getChangeLogEntry,-methods
#'    getChangeLogEntry,ChangeLog-method
#'    getChangeLogEntry,ChangeLog,ANY,ANY,ANY,ANY-method
#' @include 00_classes_01_ChangeLog_internal.R
setMethod("getChangeLogEntry",
  signature=signature(log="ChangeLog"),
  function(log, version=NULL){
    if(!is.null(version)){
      entries <- slot(log, "entries")
      relevantEntry <- entries[[version]]
    } else {
      relevantEntry <- log
    }
    return(relevantEntry)
})

#' Update ChangeLog objects
#'
#' This method can be used to update ChangeLog objects.
#'
#' \code{updateChangeLog} takes a ChangeLog object and a version number string, replaces the complete
#' entry with the contents of \code{entry} and updates the time stamp to \code{date}.
#'
#' @param log An object of class \code{ChangeLog}.
#' @param entry A (named) list of character vectors. The element names will become the ChangeLog
#'    sections, each vector element an item.
#' @param version Character string, version number to look up.
#' @param date The date of the ChangeLog entry in \code{YYYY-MM-DD} format. will be coerced into
#'    character. To keep the date stamp of a present entry, set \code{date=NULL}.
#' @param append Logical, whether a present entry should be replaced or added to.
#' @return An object of class \code{ChangeLog}.
#' @export
#' @seealso \code{\link[roxyPackage:readChangeLog]{readChangeLog}}
#' @import methods
#' @docType methods
#' @rdname updateChangeLog-methods
# @examples
# \dontrun{
# changelog <- readChangeLog("/home/user/myRsources/myRpackage/ChangeLog")
# CL.entry <- getChangeLogEntry(changelog, version="0.02-22")
# }
#' @include 00_classes_01_ChangeLog_internal.R
setGeneric("updateChangeLog", function(log, entry, version, date=Sys.Date(), append=TRUE){
    standardGeneric("updateChangeLog")
  })

#' @rdname updateChangeLog-methods
#' @import methods
#' @aliases
#'    updateChangeLog,-methods
#'    updateChangeLog,ChangeLog-method
#'    updateChangeLog,ChangeLog,ANY,ANY,ANY,ANY-method
#' @include 00_classes_01_ChangeLog_internal.R
setMethod("updateChangeLog",
  signature=signature(log="ChangeLog"),
  function(log, entry, version, date=Sys.Date(), append=TRUE){
    # preparations
    entryList <- slot(log, "entries")
    stopifnot(is.list(entry))
    if(version %in% names(entryList) && isTRUE(append)){
      oldEntry <- entryList[[version]]
      oldEntry.items <- as(slot(oldEntry, "entry"), "list")
      newEntry <- as(mergeLists(list1=oldEntry.items, list2=entry, uniq=TRUE), "ChangeLog.items")
      if(is.null(date)){
        date <- slot(oldEntry, "date")
      } else {}
      entryList[[version]] <- new("ChangeLog.entry",
        version=version,
        date=as.character(date),
        entry=newEntry)
    } else {
      if(is.null(date)){
        if(!version %in% names(entryList)){
          # ignore date=NULL, since there is no present date
          date <- Sys.Date()
        } else {
          date <- slot(entryList[[version]], "date")
        }
      } else {}
      newEntry <- as(entry, "ChangeLog.items")
      entryList[[version]] <- new("ChangeLog.entry",
        version=version,
        date=as.character(date),
        entry=newEntry)
    }
    # check the order of entries to make sure new ones get on top
    entryList.order <- order(as.numeric_version(names(entryList)), decreasing=TRUE)
    slot(log, "entries") <- entryList[entryList.order]
    return(log)
  }
)
