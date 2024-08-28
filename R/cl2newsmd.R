# Copyright 2024 Meik Michalke <meik.michalke@hhu.de>
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


#' Convert ChangeLog/NEWS into NEWS.md
#' 
#' Attempts to translate ASCII ChangeLog (or NEWS) files into NEWS.md files.
#' 
#' This should work for ChangeLog and NEWS files that
#' \enumerate{
#'   \item have entries named "Changes in version <version number>" (and optionally a parenthetical YYYY-MM-DD date string afterwards)
#'   \item have single changes properly itemized, by indentation and then either \code{"o"}, \code{"-"} or \code{"*"} followed
#'     by space
#'   \item optionally have categories as subsections, like "Changed", "Fixed", "Added", or "Removed"
#' }
#' Any text string that isn't indented and doesn't start with "Changes in version" will likely be treated as a subsection.
#' The ChangeLog related functions and methods of this package, e.g. \code{\link[roxyPackage:initChangeLog]{initChangeLog}}, are
#' a convenient way to maintain R ChangeLogs in a proper format.
#' 
#' This function uses the internal function \code{tools:::.news_reader_default} on the ChangeLog file provided.
#' 
#' @param log Character string, path to the ChangeLog or NEWS file to be converted.
#' @param news Character string, path to the NEWS.md file to be written.
#'   If \code{NULL}, results are written to \code{stdout()}.
#' @param codify Logical, whether to try to detect code snippets like function names and mark them up accordingly.
#' @param overwrite Logical, whether to overwrite an existing NEWS.md file.
#' @return Either writes a file without a return value, or returns the markdown string vector.
#' @seealso
#'    \code{\link[roxyPackage:initChangeLog]{initChangeLog}},
#'    \code{\link[roxyPackage:readChangeLog]{readChangeLog}},
#'    \code{\link[roxyPackage:updateChangeLog]{updateChangeLog}},
#'    \code{\link[roxyPackage:writeChangeLog]{writeChangeLog}}
#' @import utils
#' @export
#' @examples
#' \dontrun{
#' cl2newsmd(log="~/myFiles/myRPackage/ChangeLog", news="~/myFiles/myRPackage/inst/NEWS.md")
#' 
#' # dump the results into a character vector
#' NEWS.object <- cl2newsmd(log="~/myFiles/myRPackage/ChangeLog")
#' }

cl2newsmd <- function(
    log
  , news=NULL
  , codify=TRUE
  , overwrite=TRUE
){
  if(!file_test("-f", log)){
    warning(paste0("news: ", log," does not exist, no NEWS.md file created!"), call.=FALSE)
  } else if(any(is.null(news), if(!is.null(news)){!file_test("-f", news)}, isTRUE(overwrite))){
    # .news_reader_default is fetched from the tools package in roxyPackage-internal.R
    changelog_df <- .news_reader_default(log)

    changelog_list <- tapply(
        changelog_df[,c("Date","Category","Text")]
      , changelog_df[["Version"]]
      , function(changelog_df_v){
          v_result <- tapply(
              changelog_df_v[["Text"]]
            , changelog_df_v[["Category"]]
            , codify_md
            , codify=codify
            , simplify=FALSE
          )
          if(any(is.na(changelog_df_v[["Category"]]))){
            v_result[["__uncategorized__"]] <- codify_md(changelog_df_v[is.na(changelog_df_v[["Category"]]), "Text"], codify=codify)
          } else {}
          v_result[["date"]] <- changelog_df_v[["Date"]][[1]]
          return(v_result)
        }
        , simplify=FALSE
    )

    # sort by version
    changelog_list <- changelog_list[order(package_version(names(changelog_list)), decreasing=TRUE)]

    md_doc <- unlist(sapply(
        names(changelog_list)
      , function(v){
          this_log <- changelog_list[[v]]
          if(all(!is.null(this_log["date"]), !is.na(this_log["date"]))){
            result <- paste0("\n\n# Changes in version ", v, " (", this_log["date"], ")")
          } else {
            result <- paste0("\n\n# Changes in version ", v)
          }
          this_log["date"] <- NULL
          for(this_cat in names(this_log)){
            result <- paste0(
                result
              , if(!this_cat %in% "__uncategorized__"){paste0("\n\n## ", this_cat)} else {}
              , paste0("\n- ", this_log[[this_cat]])
            )
          }
          return(result)
        }
      , USE.NAMES=FALSE
    ))

    if(is.null(news)){
      return(md_doc)
    } else if(any(!file_test("-f", news), isTRUE(overwrite))){
      cat(md_doc, file=news)
      message(paste0("news: updated ", news, " from ChangeLog"))
    } else {
      message(paste0("news: unexpected case! skipped updating from ChangeLog"))
    }
  } else {
    message(paste0("news: file ", news, " exists and 'overwrite' is FALSE, skipped updating from ChangeLog"))
  }

  return(invisible(NULL))
}
