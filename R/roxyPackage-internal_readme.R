# Copyright 2016-2017 Meik Michalke <meik.michalke@hhu.de>
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

# this file contains helper functions to write an initial REAMDE.md file

# some functions for easier construction
# headlines
md_h <- function(text, level=1){
  return(paste0(paste0(rep("#", level), collapse=""), " ", text, "\n\n"))
}
# urls
md_url <- function(text, url){
  return(paste0("[", text ,"](", url ,")"))
}

## function readme_text()
# generates the full text for the README.md file
readme_text <- function(
    package,
    description,
    url=NULL,
    license=NULL,
    year=format(Sys.time(), "%Y"),
    author=NULL,
    githubUser=NULL,
    githubRepo=NULL
  ){
    includeFlattr <- includeLicense <- ""

    if(all(!is.null(license), !is.null(author))){
      includeLicense <- paste0(
        "\n",
        md_h("License", level=2),
        copyright.text(
          license=license,
          package=package,
          year=year,
          author=author,
          deb=FALSE
        )
      )
    } else {}

    result <- paste0(
      md_h(package),
      includeFlattr,
      autoLineBreak(text=description, lineEnd=90, breakBy="\n"),
      if(!is.null(url)){
        paste0(
          "\n\nMore information on ", package,
          " is available on the ", md_url("project homepage", url),
          ".\n"
        )
      } else {},
      if(all(!is.null(githubUser), !is.null(githubRepo))){
        paste0(
          "\n",
          md_h("Installation", level=2),
          md_h("Installation via GitHub", level=3),
          "To install the package directly from GitHub, you can use `install_github()` from the ",
          md_url("devtools","https://github.com/hadley/devtools"),
          " package:\n\n",
          "```r\n",
          "library(devtools)\n",
          "install_github(\"", githubUser, "/", githubRepo, "\")\n",
          "```\n"
        )
      } else {},
      includeLicense
    )

    return(result)
} ## end function readme_text()
