# Copyright 2017 Meik Michalke <meik.michalke@hhu.de>
#
# This file is part of the R package koRpus.
#
# koRpus is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# koRpus is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with koRpus.  If not, see <http://www.gnu.org/licenses/>.

#' Convert vognettes from *.Rnw to *.Rmd
#' 
#' This is an enhanced R port of Perl code gists from GitHub [1, 2].
#' It tries its best to convert old Sweave vignettes into R Markdown.
#' Please do not expect 
#' 
#' @param file Path to an *.Rnw file to convert.
#' @param output Character string defining the R markdown output format.
#' @param engine Character string defining the \code{VignetteEngine} value.
#' @value A character string with the converted text. No file will be written.
#' @export
#' @references
#'  [1] https://gist.github.com/mikelove/5618f935ace6e389d3fbac03224860cd
#'  [2] https://gist.github.com/lgatto/d9d0e3afcc0a4417e5084e5ca46a4d9e
rnw2rmd <- function(file, output="rmarkdown::html_vignette", engine="knitr::rmarkdown"){
  txt <- txt_body <- readLines(file)

  title <- which(grepl("\\\\title{", txt, perl=TRUE))
  author <- which(grepl("\\\\author{", txt, perl=TRUE))
  vignette_meta <- which(grepl("%\\\\Vignette.+", txt, perl=TRUE))
  vignette_engine <- which(grepl("%\\\\VignetteEngine{.+", txt, perl=TRUE))
  # remove engine if set
  vignette_meta <- vignette_meta[!vignette_meta %in% vignette_engine]
  usepackage <- which(grepl("\\\\usepackage", txt, perl=TRUE))

  begin_document <- which(grepl("\\\\begin{document}", txt, perl=TRUE))
  end_document <- which(grepl("\\\\end{document}", txt, perl=TRUE))
  if(isTRUE(begin_document > 0)){
    txt_body <- txt[c((begin_document + 1):ifelse(isTRUE(end_document > 0), (end_document-1), length(txt)))]
  } else {}

  preamble <- list()
  if(isTRUE(title > 0)){
    preamble[["title"]] <- gsub("\\\\title{(.+?)}", "\"\\1\"", txt[[title]], perl=TRUE)
  } else {}
  if(isTRUE(author > 0)){
    preamble[["author"]] <- gsub("\\\\author{(.+?)}", "\"\\1\"", txt[[author]], perl=TRUE)
  } else {}
  preamble[["date"]] <- "`r Sys.Date()`"
  preamble[["output"]] <- output
  preamble[["vignette"]] <- paste(
    ">",
    paste0(txt[vignette_meta], collapse="\n  "),
    paste0("%\\VignetteEngine{", engine, "}"),
    paste0(txt[usepackage], collapse="\n  "),
    sep="\n  "
  )
#   if(isTRUE( > 0)){
#   } else {}
  preamble_rmd <- "---"
  for (thisPreamble in names(preamble)){
    preamble_rmd <- paste0(preamble_rmd, "\n", thisPreamble, ": ", preamble[[thisPreamble]])
  }
  preamble_rmd <- paste0(preamble_rmd, "\n---\n")

  pat <- list(
    c(from="\\\\maketitle", to=""),
    c(from="\\\\Robject{(.+?)}", to="`\\1`"),
    c(from="\\\\Rcode{(.+?)}", to="`\\1`"),
    c(from="\\\\Rclass{(.+?)}", to="*\\1*"),
    c(from="\\\\Rfunction{(.+?)}", to="`\\1`"),
    c(from="\\\\texttt{(.+?)}", to="`\\1`"),
    c(from="\\\\textit{(.+?)}", to="*\\1*"),
    c(from="\\\\textbf{(.+?)}", to="**\\1**"),
    c(from="\\\\emph{(.+?)}", to="*\\1*"),
    c(from="<<", to="```{r "),
    c(from=">>=", to="}"),
    c(from="@", to="```"),
    c(from="\\\\section{(.+?)}", to="# \\1"),
    c(from="\\\\subsection{(.+?)}", to="## \\1"),
    c(from="\\\\subsubsection{(.+?)}", to="### \\1"),
    c(from="\\\\Biocexptpkg{(.+?)}", to="`r Biocexptpkg(\"\\1\")`"),
    c(from="\\\\Biocannopkg{(.+?)}", to="`r Biocannopkg(\"\\1\")`"),
    c(from="\\\\Biocpkg{(.+?)}", to="`r Biocpkg(\"\\1\")`"),
    c(from="\\\\cite{(.+?)}", to="[\\@\\1]"),
    c(from="\\\\ref{(.+?)}", to="\\\\@ref(\\1)"),
    c(from="\\\\url{(.+?)}", to="<\\1>"),
    c(from="\\\\ldots", to="\\.\\.\\."),
    c(from="\\\\label{", to=" {#"), ## only for sections
    c(from="\\\\deseqtwo{}", to="DESeq2")
  )

  for (thisPat in pat){
    txt_body <- gsub(thisPat[["from"]], thisPat[["to"]], txt_body, perl=TRUE)
  }
  
  txt_body <- paste0(preamble_rmd, paste0(txt_body, collapse="\n"))
#   # replace Schunk environments
#   txt_body <- gsub("\\\\begin{Schunk}(.*?)\\\\end{Schunk}", "\\1", txt_body, perl=TRUE)
#   txt_body <- gsub("\\\\begin{Soutput}.*?\\\\end{Soutput}", "", txt_body, perl=TRUE)
#   txt_body <- gsub("\\\\begin{Sinput}(.*?)\\\\end{Sinput}", "```{r}\n\\1\n```", txt_body, perl=TRUE)
  
  
  return(txt_body)
}
