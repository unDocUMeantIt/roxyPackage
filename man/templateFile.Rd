% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/templateFile.R
\name{templateFile}
\alias{templateFile}
\title{Create template file for new function/class/method}
\usage{
templateFile(
  name,
  path = getwd(),
  pck.description = data.frame(Package = "", Author = "", License = "GPL (>= 3)",
    stringsAsFactors = FALSE),
  year = format(Sys.time(), "\%Y"),
  params = list(obj = "someClass", ... = "\\\\code{\\\\link[somepackage]{somefunction}}"),
  seealso = list(aPackage = "aFunction"),
  return = list(aPackage = "aFunction"),
  type = "function",
  write = FALSE,
  overwrite = FALSE
)
}
\arguments{
\item{name}{Character string, name of the function/method/class.}

\item{path}{Full path to the directory where the file should be added if \code{write=TRUE}.}

\item{pck.description}{Data frame holding the package description. Only the fields "Package",
      "Author" and "License" are needed/used (see Examples section).}

\item{year}{Character string, the year to use in the copyright information.}

\item{params}{A named list of parameters,
      where the element name is the parameter name and its value is the type of object expected as input.
Some objects types are recognized, like "c", "character", "numeric", "logical",
      "matrix" or "data.frame".
If an element is called \code{"..."},
      the value is assumend to point to a function or method where additional arguments are passed to.
If \code{type="S4class"}, this argument is used to define the slots.}

\item{seealso}{A named list,
      where element names define packages and values objects of that package to link.}

\item{return}{A named list, similar to \code{seealso},
      but generating references for returned objects.}

\item{type}{Character string, either \code{"function"}, \code{"S4class"},
      or \code{"S4method"}, depending on the template you want to create.}

\item{write}{Logical, if \code{TRUE} output will be written to a file in \code{path},
      otherwise returned as a character string.}

\item{overwrite}{Logical, if \code{TRUE} and the output file already exists,
      it will be replaced with the generated template. Otherwise you'll just get an error.}
}
\value{
If \code{write=TRUE}, writes a file in the \code{path} directory. If \code{write=FALSE},
      returns a character string.
}
\description{
This function can be used to generate template files for new functions,
      S4 classes or methods.
}
\details{
Set the parameters to your needs, perhaps \code{setwd} into the target directory,
      and set \code{write=TRUE}
if you like what you see so far. The result should include a copyright note,
      insitial roxygen-style documentation
and some useful first lines of code, guessed from the provided arguments.
}
\examples{
pckg.dscrptn <- data.frame(
  Package="SquareTheCircle",
  Author="E.A. Dölle <doelle@eternalwondermaths.example.org>",
  License="GPL (>= 3)",
  stringsAsFactors=FALSE
)
cat(
  templateFile(
    name="exampleFunction",
    pck.description=pckg.dscrptn
  )
)
}
