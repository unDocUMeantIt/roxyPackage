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


## function roxy.description()
# create package description
roxy.description <- function(val, description, version=NULL, date=NULL, R.vers=NULL){

  if(!is.null(version)){
    description[["Version"]] <- version
  } else {}
  if(!is.null(date)){
    description[["Date"]] <- date
  } else {}

  # prefer Author@R/Authors@R over Author/Maintainer
  pck.authors <- get.authors(description, maintainer=TRUE, contributor=TRUE)
  pck.author <- pck.authors[["aut"]]
  pck.maintainer <- pck.authors[["cre"]]
  pck.contributor <- pck.authors[["ctb"]]
  pck.maintainer.clean <- pck.authors[["cre.clean"]]

  ## basic package info
  if(identical(val, "package")){
    return(description[["Package"]])
  } else {}
  if(identical(val, "title")){
    return(description[["Title"]])
  } else {}
  if(identical(val, "depds")){
    return(description[["Depends"]])
  } else {}
  if(identical(val, "description")){
    # check if we need to add old Author/Maintainer fields
    if(is.null(R.vers) || isTRUE(R_system_version(R.vers) < "2.14")){
      if(!"Author" %in% names(description)){
        if(pck.contributor != "") {
          description[["Author"]] <- paste0(pck.author, ", with contributions from ", pck.contributor)
        } else {
          description[["Author"]] <- pck.author
        }
      } else {}
      if(!"Maintainer" %in% names(description)){
        description[["Maintainer"]] <- pck.maintainer
      } else {}
    } else {}
    return(description)
  } else {}

  checkFor <- c("Version","Date","Depends","Enhances","Encoding","License","LazyLoad","URL")
  desc.parts <- sapply(checkFor, function(this.entry){
      found <- ifelse(this.entry %in% names(description), paste0("\n#' ",this.entry,": \\tab ",description[[this.entry]],"\\cr"),"")
      found <- gsub("^[[:space:]]*#'[[:space:]]*$", ".\n", found)
      return(found)
    })
  # extra check for package type
  if("Type" %in% names(description)){
    if(description[["Type"]] %in% c("Package", "Frontend", "Translation")){
      pck.type <- description[["Type"]]
    } else {
      pck.type <- "Package"
    }
  } else {
    pck.type <- "Package"
  }

  pckg.package.v <- paste0(
      "#' ",description[["Title"]],".\n#'\n#' \\tabular{ll}{",
      "\n#' Package: \\tab ",description[["Package"]],"\\cr",
      "\n#' Type: \\tab ",pck.type,"\\cr",
      paste(desc.parts, collapse=""),
      "\n#' }\n#'",
      "\n#' ",gsub("\n#' #'\n#'","\n#'\n#'",gsub("\n[[:space:]]*", "\n#' ", description[["Description"]])),"\n#'",
      "\n#' @aliases ",description[["Package"]],"-package",
      "\n#' @name ",description[["Package"]],"-package",
      "\n#' @docType package",
      "\n#' @title The ",description[["Package"]]," Package",
      "\n#' @author ",pck.author,
      "\n#' @keywords package",
      "\nNULL\n")
  if(identical(val, "pckg.description")){
    return(pckg.package.v)
  } else {}
} ## end function roxy.description()
