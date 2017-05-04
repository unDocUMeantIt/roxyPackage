# Copyright 2016 Meik Michalke <meik.michalke@hhu.de>
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

# creates code for a flattr button
#  - md: if TRUE returns markdown, if FALSE returns as XiMpLe X(HT)ML node
readme_flattr <- function(
    user_id,
    url,
    title,
    language="en_GB",
    tags="github",
    category="software",
    buttonText="Flattr this git repo",
    img="https://api.flattr.com/button/flattr-badge-large.png",
    md=TRUE
  ){
    flattrURL <- paste0(
      "https://flattr.com/submit/auto?",
      "user_id=", user_id,
      "&url=", url,
      "&title=", title,
      if(!identical(language, "")){
        paste0("&language=", language)
      } else {},
      if(!identical(tags, "")){
        paste0("&tags=", tags)
      } else {},
      if(!identical(category, "")){
        paste0("&category=", category)
      } else {}
    )
    if(isTRUE(md)){
      result <- paste0(
        "[![", buttonText, "](", img, ")](",flattrURL,")"
      )
    } else {
      result <- XMLNode("a",
        XMLNode("img", attrs=list(src=img, alt=buttonText, style="max-width:100%;")),
        attrs=list(href=flattrURL, target="_blank")
      )
    }
    return(result)
}

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
    githubRepo=NULL,
    flattrUser=NULL,
    fl_url=NULL,
    fl_title=NULL,
    fl_language="en_GB",
    fl_tags="github",
    fl_category="software",
    fl_buttonText="Flattr this git repo",
    fl_img="https://api.flattr.com/button/flattr-badge-large.png"
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

    if(!is.null(flattrUser)){
      if(all(is.null(fl_url), !is.null(githubUser), !is.null(githubRepo))){
        # if no flattr url was given but we have all github info, we'll use that
        fl_url <- paste0("https://github.com/", githubUser, "/", githubRepo)
      } else {}
      if(all(!is.null(fl_url), !is.null(fl_title))){
        includeFlattr <- paste0(
          readme_flattr(
            user_id=flattrUser,
            url=fl_url,
            title=fl_title,
            language=fl_language,
            tags=fl_tags,
            category=fl_category,
            buttonText=fl_buttonText,
            img=fl_img
          ),
          "\n\n"
        )
      } else {}
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
