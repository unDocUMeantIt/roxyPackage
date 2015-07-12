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


#' Convert ChangeLog/NEWS into NEWS.Rd
#' 
#' This function attempts to translate ASCII ChangeLog (or NEWS) files into NEWS.Rd files.
#' 
#' This should work for ChangeLog and NEWS files that
#' \enumerate{
#'   \item have entries named "Changes in version <version number>" (and optionally a YYYY-MM-DD date string afterwards)
#'   \item have single changes properly itemized, by indentation and then either \code{"o"}, \code{"-"} or \code{"*"} followed
#'     by space
#'   \item optionally have categories as subsections, like "Fixed" or "Added"
#' }
#' Any text string that isn't indented and doesn't start with "Changes in version" will likely be treated as a subsection.
#' The ChangeLog related functions and methods of this package, e.g. \code{\link[roxyPackage:initChangeLog]{initChangeLog}}, are
#' a convenient way to maintain R ChangeLogs in a proper format.
#' 
#' This function is basically a wrapper for the internal function \code{tools::news2Rd}.
#' 
#' @param log Character string, path to the ChangeLog or NEWS file to be converted.
#' @param news Character string, path to the NEWS.Rd file to be written.
#'   If \code{NULL}, results are written to \code{stdout()}.
#' @param codify Logical, whether to try to detect code snippets like function names and markup them accordingly.
#' @param overwrite Logical, whether to overwrite an existing NEWS.Rd file.
#' @return No return value, writes a file.
#' @seealso
#'    \code{\link[roxyPackage:initChangeLog]{initChangeLog}},
#'    \code{\link[roxyPackage:readChangeLog]{readChangeLog}},
#'    \code{\link[roxyPackage:updateChangeLog]{updateChangeLog}},
#'    \code{\link[roxyPackage:writeChangeLog]{writeChangeLog}}
#' @export
#' @examples
#' \dontrun{
#' cl2news(log="~/myFiles/myRPackage/ChangeLog", news="~/myFiles/myRPackage/inst/NEWS.Rd")
#' 
#' # use capture.output() to dump the results into a character vector
#' NEWS.object <- capture.output(cl2news(log="~/myFiles/myRPackage/ChangeLog"))
#' }

cl2news <- function(log, news=NULL, codify=TRUE, overwrite=TRUE){
  if(file_test("-f", log)){
    if(is.null(news)){
      tmp.NEWS.Rd <- stdout()
    } else {
      # unfortunately, neither checkNEWS(), readNEWS() nor news2Rd() are really good
      # at detecting strange ChangeLogs. in effect, an empty NEWS.Rd might be written
      # and cause errors in the "html" action, so we'll move that to a tempdir first
      tryCatch(NRd.out.dir <- tempdir(), error=function(e) stop(e))
      tmp.NEWS.Rd <- file.path(NRd.out.dir, "tmp.NEWS.Rd")
      on.exit(unlink(tmp.NEWS.Rd))
    }
    wrote.RD.file <- tryCatch(
      tools:::news2Rd(file=log, out=tmp.NEWS.Rd, codify=codify),
        error=function(e){
          warning(paste0("news: could not translate ", log," into NEWS.Rd file!"), call.=FALSE)
          return(FALSE)
        }
    )
    if(is.null(wrote.RD.file) & !is.null(news)){
      stopifnot(file.copy(tmp.NEWS.Rd, news, overwrite=overwrite))
      message(paste0("news: updated ", news, " from ChangeLog"))
    } else {}
  } else {
    warning(paste0("news: ", log," does not exist, no NEWS.Rd file created!"), call.=FALSE)
  }
  return(invisible(NULL))
}
