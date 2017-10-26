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
#' This is a much enhanced R port of Perl code gists from GitHub [1, 2].
#' It tries its best to convert old Sweave vignettes into R markdown.
#' Please do not expect it to do wonders, but to give you a good starting point
#' for conversions.
#' 
#' @param file Path to an *.Rnw file to convert.
#' @param output Character string defining the R markdown output format.
#' @param output_options A named character vector with additional options. If you need
#'    more than the default indentaion, you have to provide it directly (see default
#'    values for \code{toc_float}).
#' @param engine Character string defining the \code{VignetteEngine} value.
#' @param csl Character string defining a CSL style file for the bibliography.
#'    Please note that you will have to provide an existing file of that name in an
#'    appropriate location, like the *.Rmd file's directory. Ignored if \code{NULL},
#'    or if no bibliography was detected.
#' @param eval Logical, a default value for all R code chunks that are found. This is
#'    like a safety net to be able to disable all code by default. Setting the default
#'    value will be omitted if set to \code{NULL}.
#' @param replace An optional list of named character vectors with regular expressions
#'    to do custom replacements in the text body. The list must contain vectors with
#'    two character elements named \code{from} and \code{to}, to define what expressions
#'    should be replaced and with what.
#' @param write_file Logical, if set to \code{TRUE} results will be written to a file in
#'    the same directory as the input \code{file}, but with *.Rmd file ending. Default is
#'    \code{FALSE}, meaning results are returned as a character string.
#' @param overwrite Logical, whether existing files should be overwritten if \code{write_file=TRUE}.
#' @value A character string with the converted text. No file will be written by default,
#'    unless you set \code{write_file=TRUE}.
#' @export
#' @references
#'  [1] https://gist.github.com/mikelove/5618f935ace6e389d3fbac03224860cd
#'
#'  [2] https://gist.github.com/lgatto/d9d0e3afcc0a4417e5084e5ca46a4d9e
#' @examples
#' \dontrun{
#' rnw2rmd(file.path(find.package("roxyPackage"),"doc","roxyPackage_vignette.Rnw"))
#' }
rnw2rmd <- function(
  file,
  output="html_document",
  output_options=c(
    theme="cerulean",
    highlight="kate",
    toc="true",
    toc_float="\n      collapsed: false\n      smooth_scroll: false",
    toc_depth=3
  ),
  engine="knitr::rmarkdown",
  csl=NULL,
  eval=FALSE,
  replace=NULL,
  write_file=FALSE,
  overwrite=FALSE
){
  if(is.null(eval)){
    eval_print <- ""
  } else if(isTRUE(eval)) {
    eval_print <- ", eval=TRUE"
  } else {
    eval_print <- ", eval=FALSE"
  }
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
    c(from="<<", to=paste0("```{r", eval_print, " ")),
    c(from=">>=", to="}"),
    c(from="@", to="```"),
    c(from="\\s*\\\\begin{Sinput}", to=paste0("```{r", eval_print, "}")),
    c(from="\\s*\\\\end{Sinput}", to="```"),
    c(from="\\s*\\\\begin{Schunk}", to=""),
    c(from="\\s*\\\\end{Schunk}", to=""),
    c(from="\\s*\\\\begin{Soutput}", to="<!-- \\\\begin{Soutput}"),
    c(from="\\s*\\\\end{Soutput}", to="\\\\end{Soutput} -->"),
    c(from="\\s*\\\\begin{Verbatim}", to="```"),
    c(from="\\s*\\\\end{Verbatim}", to="```"),
    c(from="\\s*\\\\section{(.+?)}", to="# \\1"),
    c(from="\\s*\\\\subsection{(.+?)}", to="## \\1"),
    c(from="\\s*\\\\subsubsection{(.+?)}", to="### \\1"),
    c(from="\\s*\\\\paragraph{(.+?)}", to="#### \\1"),
    c(from="\\\\Biocexptpkg{(.+?)}", to="`r Biocexptpkg(\"\\1\")`"),
    c(from="\\\\Biocannopkg{(.+?)}", to="`r Biocannopkg(\"\\1\")`"),
    c(from="\\\\Biocpkg{(.+?)}", to="`r Biocpkg(\"\\1\")`"),
    c(from="\\\\cite{(.+?)}", to="[\\@\\1]"),
    c(from="\\\\cite<(.+?)>{(.+?)}", to="[\\1 \\@\\2]"),
    c(from="\\\\citeA{(.+?)}", to="\\@\\1"),
    c(from="\\\\citeNP{(.+?)}", to="\\@\\1"),
    c(from="\\\\citeNP<(.+?)>{(.+?)}", to="\\1 \\@\\2"),
    c(from="\\\\ref{(.+?)}", to="\\\\@ref(\\1)"),
    c(from="\\\\url{(.+?)}", to="<\\1>"),
    c(from="\\\\href{(.+?)}{(.+?)}", to="[\\2](\\1)"),
    c(from="\\\\ldots", to="\\.\\.\\."),
    c(from="\\\\label{", to=" {#"), # only for sections
    c(from="\\\\deseqtwo{}", to="DESeq2"),
    c(from="\\\\footnote{(.+?)}", to="^[\\1]"),
    c(from="\\\\bibliography{(.+?)}", to="# References"),
    c(from="\\\\bibliographystyle{(.+?)}", to=""),
    c(from="\\\\addcontentsline{(.+)}", to=""),
    c(from="\\\\\\$", to="\\$"),
    c(from="^(\\s*)%(.*)[^->]\\s*$", to="\\1<!-- \\2 -->"),
    c(from="\\\\,", to="")
  )
  if(is.list(replace)){
    pat <- append(pat, replace)
  } else {}

  txt <- txt_body <- readLines(file)

  title <- which(grepl("\\\\title{", txt, perl=TRUE))
  author <- which(grepl("\\\\author{", txt, perl=TRUE))
  bibliography <- which(grepl("^\\s*\\\\bibliography{", txt, perl=TRUE))
  vignette_meta <- which(grepl("%\\\\Vignette.+", txt, perl=TRUE))
  vignette_engine <- which(grepl("%\\\\VignetteEngine{.+", txt, perl=TRUE))
  # remove engine if set
  vignette_meta <- vignette_meta[!vignette_meta %in% vignette_engine]
  usepackage <- which(grepl("\\\\usepackage", txt, perl=TRUE))

  begin_document <- which(grepl("\\\\begin{document}", txt, perl=TRUE))
  end_document <- which(grepl("\\\\end{document}", txt, perl=TRUE))
  if(isTRUE(begin_document > 0)){
    txt_body <- txt[c((begin_document + 1):ifelse(isTRUE(end_document > 0), (end_document - 1), length(txt)))]
  } else {}
  begin_abstract <- which(grepl("\\\\begin{abstract}", txt_body, perl=TRUE))
  end_abstract <- which(grepl("\\\\end{abstract}", txt_body, perl=TRUE))

  preamble <- list()
  if(isTRUE(title > 0)){
    preamble[["title"]] <- gsub("\\\\title{(.+?)}", "\"\\1\"", txt[[title]], perl=TRUE)
  } else {}
  if(isTRUE(author > 0)){
    preamble[["author"]] <- gsub("\\\\author{(.+?)}", "\"\\1\"", txt[[author]], perl=TRUE)
  } else {}
  preamble[["date"]] <- "\"`r Sys.Date()`\""
  if(!is.null(output_options)){
    output_options_print <- paste(paste0("    ", names(output_options), ":"), output_options, collapse="\n")
  } else {}
  preamble[["output"]] <- paste0("\n  ", output, ":\n", output_options_print)
  if(isTRUE(bibliography > 0)){
    preamble[["bibliography"]] <- gsub("\\\\bibliography{(.+?)}", "\\1.bib", txt[[bibliography]], perl=TRUE)
    if(!is.null(csl)){
      preamble[["csl"]] <- csl
    } else {}
  } else {}
  if(isTRUE(begin_abstract > 0)){
    abstract_text <- txt_body[c((begin_abstract + 1):(end_abstract - 1))]
    for (thisPat in pat){
      abstract_text <- gsub(thisPat[["from"]], thisPat[["to"]], abstract_text, perl=TRUE)
    }
    preamble[["abstract"]] <- paste0(">\n  ", paste(abstract_text, collapse="\n  "))
    txt_body <- txt_body[-c((begin_abstract):(end_abstract))]
  } else {}
  preamble[["vignette"]] <- paste(
    ">",
    paste0(txt[vignette_meta], collapse="\n  "),
    paste0("%\\VignetteEngine{", engine, "}"),
    paste0(txt[usepackage], collapse="\n  "),
    sep="\n  "
  )

  preamble_rmd <- "---"
  for (thisPreamble in names(preamble)){
    preamble_rmd <- paste0(preamble_rmd, "\n", thisPreamble, ": ", preamble[[thisPreamble]])
  }
  preamble_rmd <- paste0(preamble_rmd, "\n---\n")

  for (thisPat in pat){
    txt_body <- gsub(thisPat[["from"]], thisPat[["to"]], txt_body, perl=TRUE)
  }
  
  txt_body <- nested_env(txt=txt_body)

  txt_body <- paste0(preamble_rmd, paste0(txt_body, collapse="\n"))

  # clean up multiple newlines
  txt_body <- gsub("[\\\n]{3,}?", "\\\n\\\n", txt_body, perl=TRUE)

  if(isTRUE(write_file)){
    normalPath <- normalizePath(file)
    outDir <- dirname(normalPath)
    outFile <- paste0(gsub("\\.[rRsS][nN][wW]$", "", basename(normalPath)), ".Rmd")
    outPath <- file.path(outDir, outFile)
    if(all(file.exists(outPath), !isTRUE(overwrite))){
      warning(paste0("file already exists, set 'overwrite=TRUE' if you want to replace it:\n  ", outPath), call.=FALSE)
    } else {
      message(paste0("writing to file:\n  ", outPath))
      cat(txt_body, file=outPath)
    }
    return(invisible(NULL))
  } else {}

  return(txt_body)
} ## end function rnw2rmd()


## internal function nested_env()
# iterates through a character vector and tries to replace
# itemize or enumerate blocks with R markdown equivalents
nested_env <- function(txt){
  begin_itemize <- paste0("\\s*\\\\begin{itemize}\\s*")
  begin_enumerate <- paste0("\\s*\\\\begin{enumerate}\\s*")
  level <- 0
  enum <- 0
  envir_in <- ""
  for (thisTxtNum in 1:length(txt)){
    if(isTRUE(grepl(begin_itemize, txt[thisTxtNum], perl=TRUE))){
      envir_in <- "itemize"
      level <- level + 1
      txt[thisTxtNum] <- gsub(begin_itemize, "", txt[thisTxtNum], perl=TRUE)
    } else if(isTRUE(grepl(begin_enumerate, txt[thisTxtNum], perl=TRUE))){
      envir_in <- "enumerate"
      enum <- 0
      level <- level + 1
      txt[thisTxtNum] <- gsub(begin_enumerate, "", txt[thisTxtNum], perl=TRUE)
    } else {}
    if(isTRUE(grepl("\\\\item", txt[thisTxtNum], perl=TRUE))){
      if(level > 2){
        warning("list depths of more than two levels are not supported in Rmarkdown, reducing to two levels -- please check!")
      } else {}
      if(envir_in %in% "itemize"){
        indent <- switch(as.character(level),
          "0"="",
          "1"="* ",
          "2"="    + ",
          "    + "
        )
        txt[thisTxtNum] <- gsub("\\s*\\\\item[(.+?)]\\s*", indent, txt[thisTxtNum], perl=TRUE)
        txt[thisTxtNum] <- gsub("\\s*\\\\item\\s*", indent, txt[thisTxtNum], perl=TRUE)
      } else if(envir_in %in% "enumerate"){
      message(level)
        enum <- enum + 1
        if(level > 1){
          indent <- paste0("    ", letters[enum], ". ")
        } else if(level > 0){
          indent <- paste0(enum, ". ")
        } else {
          indent <- ""
        }
        txt[thisTxtNum] <- gsub("\\s*\\\\item\\s*", indent, txt[thisTxtNum], perl=TRUE)
      } else {}
    } else {}
    if(envir_in %in% c("itemize","enumerate")){
      envir_end <- paste0("\\s*\\\\end{",envir_in,"}\\s*")
      if(isTRUE(grepl(envir_end, txt[thisTxtNum], perl=TRUE))){
        level <- level - 1
        txt[thisTxtNum] <- gsub(envir_end, "", txt[thisTxtNum], perl=TRUE)
      } else {}
    } else {}
  }
  if(level != 0){
    warning("looks like we were not able to correctly detect all levels of ", envir, " environments. is the input document valid?")
  } else {}
  return(txt)
} ## end internal function nested_env()
