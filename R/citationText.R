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


# internal function to create contents of a CITATION file

citationText <- function(pck.dscr, pck.version, pck.date){

  pck.date <- gsub("([[:digit:]]{4})-.*", "\\1", pck.date, perl=TRUE)
  pck.title <- paste0(pck.dscr[["Package"]], ": ", pck.dscr[["Title"]])
  pck.url <- pck.dscr[["URL"]]

  if("Authors@R" %in% names(pck.dscr)){
    R.author <- pck.dscr[["Authors@R"]]
  } else {
    R.author <- pck.dscr[["Author@R"]]
  }
  if(is.null(R.author)){
    R.author <- paste0("\"", pck.dscr[["Author"]], "\"")
    bibtex.authors <- pck.dscr[["Author"]]
    txt.name <- R.author
  } else {
    txt.name <- c()
    R.auth.prsn <- eval(parse(text=paste(R.author)))
    all.authors <- get.by.role(R.auth.prsn)
    bibtex.authors <- paste(format(all.authors, include=c("given","family")), collapse = " and ")
    num.authors <- length(all.authors)
    for(this.author in 1:num.authors){
      R.auth.famil <- format(all.authors[[this.author]], include=c("family"))
      R.auth.given <- abbreviate(format(all.authors[[this.author]], include=c("given")), minlength=1, strict=TRUE, dot=TRUE)
      R.auth.full <- paste(R.auth.famil, R.auth.given, sep=", ")
      # cases:
      # 1 author
      # 1+x author
      # last author
      if(num.authors == 1 | this.author == num.authors){
        txt.name[length(txt.name)+1] <- R.auth.full
      } else if(this.author == num.authors - 1){
        txt.name[length(txt.name)+1] <- paste0(R.auth.full, " & ")
      } else {
        txt.name[length(txt.name)+1] <- paste0(R.auth.full, ", ")
      }
    }
  }

  cite.txt <- paste0("bibentry(\"Manual\",
    title=\"", pck.title, "\",
    author=\"", bibtex.authors, "\",
    year=\"", pck.date, "\",
    note=\"(Version ", pck.version ,")\",
    ", if(!is.null(pck.url)){paste0("url=\"", pck.url, "\",")} else {},"

    textVersion =
    paste(\"", paste(txt.name, collapse=""), " (", pck.date, "). \",
        \"", pck.title, " (Version ", pck.version, "). \"",
        if(!is.null(pck.url)){paste0(",
        \"Available from ", pck.url, "\"")} else {},",
        sep=\"\"),

    mheader = \"To cite ", pck.dscr[["Package"]], " in publications use:\")\n")

  return(cite.txt)
}
