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
#' @params file Path to an *.Rnw file to convert.
#' @value A character string with the converted text. No file will be written.
#' @export
#' @references
#'  [1] https://gist.github.com/mikelove/5618f935ace6e389d3fbac03224860cd
#'  [2] https://gist.github.com/lgatto/d9d0e3afcc0a4417e5084e5ca46a4d9e
rnw2rmd <- function(file){
  txt <- readLines(file)

  pat <- list(
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
    txt <- gsub(thisPat[["from"]], thisPat[["to"]], txt, perl=TRUE)
  }
  
  return(txt)
}
