## function writeStub()
writeStub <- function(file, path, content="", overwrite=FALSE){
  if(!dir.exists(path)){
    stop(simpleError("'path' does not exist!"))
  } else {}
  fullPath <- file.path(path, file)
  if(all(file.exists(fullPath), !isTRUE(overwrite))){
    stop(simpleError(paste0("File already exists: ", fullPath, " -- use 'overwrite=TRUE' to replace!")))
  } else {}
  cat(content, file=fullPath)
  return(invisible(NULL))
} ## end function writeStub()


## function copyright.part()
# similar to copyright.text() for debianization, but produces text for
# the preamble of files that are part of an R package
copyright.part <- function(pck.description, year=format(Sys.time(), "%Y")){
  id <- "# " # indentation
  el <- "#\n" # empty lines
  package <- getDescField(pck.description, field="Package")
  author <- gsub("@@", "@", getDescField(pck.description, field="Author"))
  license <- getDescField(pck.description, field="License")
  licenseText <- paste0(id, "Copyright (C) ", year, " ", author, "\n", el,
    id, "This file is part of the R package ", package, ".\n", el
  )
  if(checkLicence(license)){
    licenseInfo <- checkLicence(license, deb=TRUE, logical=FALSE)
    licenseText <- paste0(licenseText,
      id, package, " is free software: You can redistribute it and/or modify\n",
      id, "it under the terms of the ", licenseInfo[["name"]],
      if(grepl("GPL", licenseInfo[["name"]])){
        paste0(" as published\n", id, "by the Free Software Foundation")
      } else {},
      if(!is.na(licenseInfo[["version"]])){
        if(grepl("GPL", licenseInfo[["name"]])){
          nxt <- ", "
        } else {
          nxt <- paste0(",\n", id)
        }
        if(grepl(">", license)){
          paste0(nxt, "either version ", licenseInfo[["version"]], " of the License, or\n",
            id, "(at your option) any later version"
          )
        } else {
          paste0(nxt, "version ", licenseInfo[["version"]], " of the License")
        }
      } else {},
      ".\n", el,
      id, package, " is distributed in the hope that it will be useful,\n",
      id, "but WITHOUT ANY WARRANTY; without even the implied warranty of\n",
      id, "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n",
      id, "licence terms for more details.\n", el,
      id, "You should have received a copy of the license with the source\n",
      id, "package as the file COPYING or LICENSE.",
      if(grepl("GPL", licenseInfo[["name"]])){
        paste0("  If not, see\n", id, "<http://www.gnu.org/licenses/>.")
      } else {}
    )
  } else {
    licenseText <- paste0(licenseText,
      id, package, " is released under the terms of the \n",
      id, license, " license.\n", el,
      id, package, " is distributed in the hope that it will be useful,\n",
      id, "but WITHOUT ANY WARRANTY; without even the implied warranty of\n",
      id, "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n",
      id, "licence terms for more details.\n", el,
      id, "You should have received a copy of the license with the source\n",
      id, "package as the file COPYING or LICENSE."
    )
  }
  return(licenseText)
} ## end function copyright.part()


## function roxyParams()
# - params: a named list, each name is a parameter name, each value an object class
# - roxy: if TRUE returns parameter/slot definitions in roxygen syntax
# - type: can also be "representation" if roxy=FALSE to return representation() syntax
roxyParams <- function(params, roxy=TRUE, collapse="\n#' ", type="function"){
  stopifnot(isTRUE(length(names(params)) == length(params)))
  result <- sapply(seq_along(params), function(n){
      paramName <- names(params)[n]
      paramType <- params[[n]]
      if(isTRUE(roxy)){
        if(identical(paramName, "...")){
          paramText <- paste0("Additional arguments passed through to ", paramType)
        } else {
          paramText <- switch(paramType,
            "c"="A vector",
            "character"="Character string",
            "data.frame"="A data frame",
            "list"="A list",
            "logical"="Logical, if \\code{TRUE} ... if \\code{FALSE} ... ",
            "numeric"="Numeric",
            "matrix"="A matrix",
            paste0("An object of class ", paramType)
          )
        }
        roxyTag <- ifelse(type %in% c("S4class", "representation"), "@slot ", "@param ")
        result <- paste0(roxyTag, paramName, " ", paramText, ".")
      } else {
        if(identical(type, "representation")){
          result <- paste0(paramName, "=\"", paramType, "\"")
        } else {
          if(identical(paramName, "...")){
            result <- paramName
          } else {
            result <- paste0(paramName, "=", paramType, "()")
          }
        }
      }
      return(result)
    }
  )
  return(paste0(result, collapse=collapse))
} ## end function roxyParams()


## function roxySeealso()
roxySeealso <- function(seealso, collapse=",\n#'   "){
  stopifnot(isTRUE(length(names(seealso)) == length(seealso)))
  result <- paste0("\\code{\\link[", names(seealso), "]{", seealso, "}}")
  return(paste0("@seealso ", paste0(result, collapse=collapse)))
} ## end function roxySeealso()
