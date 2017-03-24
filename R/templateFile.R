# Copyright 2017 Meik Michalke <meik.michalke@hhu.de>
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


#' Create template file for new function/class/method
#' 
#' This function can be used to generate template files for new functions, S4 classes or methods.
#' 
#' Set the parameters to your needs, perhaps \code{setwd} into the target directory, and set \code{write=TRUE}
#' if you like what you see so far. The result should include a copyright note, insitial roxygen-style documentation
#' and some useful first lines of code, guessed from the provided arguments.
#'
#' @param name Character string, name of the function/method/class.
#' @param path Full path to the directory where the file should be added if \code{write=TRUE}.
#' @param pck.description Data frame holding the package description. Only the fields "Package", "Author" and "License" are needed/used (see Examples section).
#' @param year Character string, the year to use in the copyright information.
#' @param params A named list of parameters, where the element name is the parameter name and its value is the type of object expected as input.
#'    Some objects types are recognized, like "c", "character", "numeric", "logical", "matrix" or "data.frame".
#'    If an element is called \code{"..."}, the value is assumend to point to a function or method where additional arguments are passed to.
#'    If \code{type="S4class"}, this argument is used to define the slots.
#' @param seealso A named list, where element names define packages and values objects of that package to link.
#' @param return A named list, similar to \code{seealso}, but generating references for returned objects.
#' @param type Character string, either \code{"function"}, \code{"S4class"}, or \code{"S4method"}, depending on the template you want to create.
#' @param write Logical, if \code{TRUE} output will be written to a file in \code{path}, otherwise returned as a character string.
#' @param overwrite Logical, if \code{TRUE} and the output file already exists, it will be replaced with the generated template. Otherwise you'll just get an error.
#' @return If \code{write=TRUE}, writes a file in the \code{path} directory. If \code{write=FALSE}, returns a character string.
#' @export
#' @examples
#' pckg.dscrptn <- data.frame(
#'   Package="SquareTheCircle",
#'   Author="E.A. Dölle <doelle@@eternalwondermaths.example.org>",
#'   License="GPL (>= 3)",
#'   stringsAsFactors=FALSE
#' )
#' cat(
#'   templateFile(
#'     name="exampleFunction",
#'     pck.description=pckg.dscrptn
#'   )
#' )

templateFile <- function(
  name,
  path=getwd(),
  pck.description=data.frame(Package="", Author="", License="GPL (>= 3)", stringsAsFactors=FALSE),
  year=format(Sys.time(), "%Y"),
  params=list(obj="someClass", "..."="\\code{\\link[somepackage]{somefunction}}"),
  seealso=list(aPackage="aFunction"),
  return=list(aPackage="aFunction"),
  type="function",
  write=FALSE,
  overwrite=FALSE
){
  if(!type %in% c("function", "S4class", "S4method")){
    stop(simpleError("Invalid 'type'!"))
  } else {}
  licenseText <- copyright.part(pck.description=pck.description, year=year)
  roxyText <- paste(
    switch(type,
      "function"=paste0("#' Function ", name),
      "S4class"=paste0("#' S4 Class ", name),
      "S4method"=paste0("#' S4 Method ", name),
      paste0("#' ", name)
    ),
    "",
    "\\code{\\link[:]{}}",
    "",
    "@section <section>: ",
    "  \\describe{",
    "    \\item{\\code{}:} {}",
    "  }",
    "",
    "  \\itemize{",
    "    \\item{\\code{}} {:}",
    "  }",
    "",
    "@note ",
    "",
    roxyParams(params, type=type),
    if(type %in% "S4method"){
      paste(
        "@docType methods",
        "@import methods",
        roxyReturn(ret=return),
        sep="\n#' "
      )
    } else if(type %in% "S4class"){
      paste(
        paste0("@name ", name, ",-class"),
        paste0("@aliases ", name, ",-class ", name, "-class"),
        "@import methods",
        sep="\n#' "
      )
    } else {
      roxyReturn(ret=return)
    },
    "@references <ref>",
    roxySeealso(seealso=seealso),
    switch(type,
      "function"="@keywords functions",
      "S4class"="@keywords classes",
      "S4method"="@keywords methods",
      "@keywords <keywords>"
    ),
    switch(type,
      "S4class"=paste0("@rdname ", name, "-class"),
      "S4method"=paste0("@rdname ", name, "-method"),
      paste0("@rdname ", name)
    ),
    "@export",
    "@examples",
    "\\dontrun{",
    paste0(name, "()"),
    "}",
    sep="\n#' "
  )

  if(type %in% "S4class"){
    codeStub <- paste0("setClass(\n  \"", name, "\",\n",
      "  representation=representation(\n    ",
      roxyParams(params, roxy=FALSE, collapse=",\n    ", type="representation"),
      "\n  ),\n",
      "  prototype(\n    ",
      roxyParams(params, roxy=FALSE, collapse=",\n    ", type="S4class"),
      "\n  )#,\n",
      "#  contains=c()\n",
      ")\n\n",
      "# setValidity(\n#  \"", name, "\",\n",
      "#   function(object){\n",
      "#   }\n",
      "# )\n"
    )
  } else if(type %in% "S4method"){
    codeStub <- paste0("setGeneric(\n  \"", name, "\",\n  ",
      "function(\n    ",
      roxyParams(params, roxy=FALSE, collapse=",\n    "),
      "\n  ){\n    standardGeneric(\"", name, "\")\n  }\n)\n\n",
      "#' @rdname ", name, "-methods\n",
      "#' @aliases ", name, ",", params[[1]], "-method\n",
      "# @param <...>\n\n",
      "setMethod(\n  \"", name, "\",\n",
      "  signature(", names(params)[[1]], "=", params[[1]], "),\n",
      "  function(\n    ",
      roxyParams(params, roxy=FALSE, collapse=",\n    "),
      "\n  ){\n",
      "  }\n",
      ")\n"
    )
  } else {
    codeStub <- paste0(name, " <- function(\n  ",
      roxyParams(params, roxy=FALSE, collapse=",\n  "),
      "\n){\n}\n"
    )
  }

  result <- paste(licenseText, roxyText, codeStub, sep="\n\n")

  if(isTRUE(write)){
    # we'll add a vague scheme to file names, so that class defintions
    # come first, methods (likely for those classes) next, and
    # finally everthing else
    filename <- switch(type,
      "S4class"=paste0("01_class_",name, ".R"),
      "S4method"=paste0("02_method_", name, ".R"),
      paste0(name, ".R")
    )
    writeStub(
      file=filename,
      path=path,
      content=result,
      overwrite=overwrite
    )
  } else {
    return(result)
  }
}
