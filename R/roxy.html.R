# Copyright 2011-2020 Meik Michalke <meik.michalke@hhu.de>
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


## function imprint_node()
imprint_node <- function(imprint=NULL, privacy.policy=NULL){
  if(any(!is.null(imprint), !is.null(privacy.policy))){
    result <- XMLNode("span",
      if(!is.null(imprint)){
        XMLNode("a",
                "Imprint",
                attrs=list(href=imprint, target="_blank")
        )
      },
    if(all(!is.null(imprint), !is.null(privacy.policy))){
      XMLNode("span", "&nbsp;&#183;&nbsp;", attrs=list(class="dot"))
    },
      if(!is.null(privacy.policy)){
        XMLNode("a",
                "Privacy policy",
                attrs=list(href=privacy.policy, target="_blank")
        )
      },
      attrs=list(class="imprint")
    )
  } else {
    result <- NULL
  }
  return(result)
} ## end function imprint_node()


## function rx.tr()
# helper function to cretae HTML table rows with two columns
#' @import XiMpLe
rx.tr <- function(col1, col2, isList=FALSE){
  return(XMLNode("tr",
    XMLNode("td", col1, attrs=list(valign="top")),
    if(isTRUE(isList)){
      XMLNode("td", children=col2)
    } else {
      XMLNode("td", col2)
    }  
  ))
} ## function rx.tr()


## function rx.clean()
# helper function to remove mail adresses from author fields
rx.clean <- function(txt, nomail=TRUE, newline=TRUE, textmail=TRUE){
  if(isTRUE(nomail)){
    txt <- gsub("[[:space:]]*<[^@]*@[^>]*>", "", txt)
  } else if(isTRUE(textmail)){
    txt <- gsub("(<)([^@]*)(@)([^>]*)(>)", "<\\2 at \\4>", txt, perl=TRUE)
    txt <- entities(txt)
  } else {}
  if(isTRUE(newline)){
    txt <- gsub("\n", " ", txt)
  } else {}
  return(txt)
} ## end function rx.clean()


## function roxy.NEWS2HTML()
# newsRd: path to existing NEWS.Rd file
# newsHTML: path to NEWS.html file to be written
roxy.NEWS2HTML <- function(newsRd, newsHTML, pckg, css){
  if(file_test("-f", newsRd)){
    tools::Rd2HTML(Rd=newsRd, out=newsHTML, package=pckg, stylesheet=css)
    message(paste0("news: updated ", newsHTML, " from NEWS.Rd"))
  } else {
    warning(paste0("news: ", newsRd," does not exist, no NEWS.HTML file created!"), call.=FALSE)
    return(invisible(NULL))
  }
} ## end function roxy.NEWS2HTML()


## function rx.html.switch()
# mainly for use in roxy.html() to make huge HTML trees better readable
#' @import XiMpLe
rx.html.switch <- function(desc, field){
  if(field %in% dimnames(desc)[[2]]){
    switch(EXPR=field,
      Depends=rx.tr("Depends:", rx.clean(desc[,"Depends"])),
      Imports=rx.tr("Imports:", rx.clean(desc[,"Imports"])),
      LinkingTo=rx.tr("LinkingTo:", rx.clean(desc[,"LinkingTo"])),
      Suggests=rx.tr("Suggests:", rx.clean(desc[,"Suggests"])),
      Enhances=rx.tr("Enhances:", rx.clean(desc[,"Enhances"])),
      Additional_repositories=rx.tr("Additional repositories:", 
          if(isTRUE(grepl(",", desc[,"Additional_repositories"]))){
            # more than one extra repo
            XMLNode("span", .children=lapply(
                unlist(strsplit(desc[,"Additional_repositories"], ",")),
                function(thisURL){
                  URLnoSpace <- gsub("&", "&amp;", gsub("^[[:space:]]*|[[:space:]]*$","",thisURL))
                  return(XMLNode("a", URLnoSpace, attrs=list(href=URLnoSpace)))
                }
              )
            )
          } else {
            XMLNode("a", gsub("&", "&amp;", desc[,"Additional_repositories"]),
            attrs=list(href=gsub("&", "&amp;", desc[,"Additional_repositories"])))
         }
        ),
      BugReports=rx.tr("BugReports:", 
        if(substr(desc[,"BugReports"], 1, 4) != 'http'){
          rx.clean(desc[,"BugReports"], nomail=FALSE, textmail=TRUE)
        } else {
          XMLNode("a", gsub("&", "&amp;", desc[,"BugReports"]),
          attrs=list(href=gsub("&", "&amp;", desc[,"BugReports"])))
        }),
      URL=rx.tr("URL:", XMLNode("a",
        gsub("&", "&amp;", desc[,"URL"]),
        attrs=list(href=gsub("&", "&amp;", desc[,"URL"])))),
      NeedsCompilation=rx.tr("NeedsCompilation:", desc[,"NeedsCompilation"]),
      SystemRequirements=rx.tr("SystemRequirements:", rx.clean(desc[,"SystemRequirements"]))
    )
  } else {
    if(field %in% "NeedsCompilation"){
      return(rx.tr("NeedsCompilation:", "no"))
    } else {}
    return(NULL)
  }
} ## end function rx.html.switch()


## function debRepoInfo()
# generates HTML code with info how to install packages from the deb repository
#' @import XiMpLe
debRepoInfo <- function(URL, dist, comp, arch, version, revision, compression, repo, repo.name, repo.root,
  package=NULL, keyring.options=NULL, page.css="web.css", package.full=NULL, imprint=NULL, privacy.policy=NULL, deb.dir="deb"){
  mirror.list <- getURL(URL, purpose="mirror.list", deb.dir=deb.dir)
  debian.path <- getURL(URL, purpose="debian.path", deb.dir=deb.dir)
  deb.URL <- getURL(URL, purpose="debian", deb.dir=deb.dir)
  repo.path <- debRepoPath(dist=dist, comp=comp, arch=arch, URL=URL, deb.dir=deb.dir)
  # check for OpenPGP key info
  keyring.options <- mergeOptions(
    someFunction=debianizeKeyring,
    customOptions=keyring.options,
    newDefaults=list(
      repo.name=repo.name,
      repo.root=repo.root,
      distribution=dist,
      component=comp
    )
  )
  keyname <- eval(keyring.options[["keyname"]])
  if(!is.null(keyname)){
    keyInRepo <- deb.search.repo(
    pckg=keyname,
    repo=file.path(keyring.options[["repo.root"]], deb.dir),
    distribution=keyring.options[["distribution"]],
    component=keyring.options[["component"]],
    arch="all",
    boolean=TRUE)
  } else {
    keyInRepo <- FALSE
  }
  # check if URL points to a list of mirrors
  if(!is.null(mirror.list)){
    apt.base.txt <- paste(paste0("[ftp|http]://<mirror>", debian.path), dist, comp, sep=" ")
    instruction <- XMLNode("p",
      "Select a server near you from ",
      XMLNode("a", "this list of mirrors", attrs=list(href=mirror.list, target="_blank")),
      " and add the repository to your configuration (e.g.,",
      XMLNode("code", paste0("/etc/apt/sources.list.d/", repo, ".list")),
      "):"
    )
  } else {
    apt.base.txt <- paste(paste0(deb.URL, debian.path), dist, comp, sep=" ")
    instruction <- XMLNode("p", "Add the repository to your configuration (e.g.,", XMLNode("code", paste0("/etc/apt/sources.list.d/", repo, ".list")), "):")
  }
  if(isTRUE(keyInRepo)){
    apt.signed.by <- paste0(" [signed-by=/usr/share/keyrings/", keyname, ".asc]")
  } else {
    apt.signed.by <- ""
  }
  apt.types <- c(
    paste0("# for binary packages:\ndeb", apt.signed.by),
    paste0("# for source packages:\ndeb-src", apt.signed.by)
  )
  apt.full.txt <- paste(paste(apt.types, apt.base.txt, sep=" "), collapse="\n")
  # XiMpLe currently suffers from a layout glitch: it tries to auto-indent tag content, which doesn't look so great in <pre> tags
  # therefore, all values of <pre> tags start with an empty line to produce nice output in the web browser
  xml.obj.list <- list(
      XMLNode("h2", "Install R packages from this Debian repository"),
      XMLNode("h4", "Configure repository"),
      instruction,
      XMLNode("pre", paste0("&nbsp;", paste0("\n", apt.full.txt)), attrs=list(class="repo")) # the "&nbsp;" works around a glitch in XiMpLe
    )

  # if we do the update call before installing the OpenPGP package, we don't need to
  # call it again when installing the package.
  stillNeedsUpdate <- "sudo apt update\n"

  if(!is.null(keyname)){
    keyInRepo <- deb.search.repo(
      pckg=keyname,
      repo=file.path(keyring.options[["repo.root"]], deb.dir),
      distribution=keyring.options[["distribution"]],
      component=keyring.options[["component"]],
      arch="all",
      boolean=TRUE)
    if(isTRUE(keyInRepo)){
      gpg.txt <- paste0(stillNeedsUpdate, "sudo apt install ", keyname)
      stillNeedsUpdate <- ""
      xml.obj.list <- append(xml.obj.list,
        list(
          XMLNode("p", "You might have to temporarily replace", XMLNode("code", "deb"), "with", XMLNode("code", "deb [trusted=yes]"), "in your", XMLNode("code", paste0(repo, ".list")), "file until after you installed the following keyring package, or apt might refuse to install it because it doesn't know the included OpenPGP key yet. Make sure you remove the", XMLNode("code", "[trusted=yes]"), "after the keyring was successfully installed!"),
          XMLNode("h4", "Add OpenPGP key"),
          XMLNode("p", "To be able to make use of secure apt, install the repository's OpenPGP keyring package:"),
          XMLNode("pre", paste0("&nbsp;\n", gpg.txt), attrs=list(class="repo")) # the "&nbsp;" works around a glitch in XiMpLe
        ))
    } else {
      message("html: debian keyring package not in repo yet, skipping docs!")
    }
  } else {}

  if(!is.null(package)){
    pkg.txt <- paste0(stillNeedsUpdate, "sudo apt install ", package)
    xml.obj.list <- append(xml.obj.list,
      list(
        XMLNode("h4", "Install packages"),
        XMLNode("p", "You can then install the package:"),
        XMLNode("pre", paste0("&nbsp;\n", pkg.txt), attrs=list(class="repo")) # the "&nbsp;" works around a glitch in XiMpLe
      ))
  } else {}

  if(all(!is.null(package.full), !is.null(repo.path), is.null(mirror.list))){
    src.orig   <- debSrcFilename(pck=package, version=version, revision=revision, compression=compression, file="orig")
    src.debian <- debSrcFilename(pck=package, version=version, revision=revision, compression=compression, file="debian")
    src.dsc    <- debSrcFilename(pck=package, version=version, revision=revision, compression=compression, file="dsc")
    repo.path.src <- debRepoPath(dist=dist, URL=URL, source=TRUE, deb.dir=deb.dir)
    xml.obj.list <- append(xml.obj.list,
      list(
        XMLNode("h3", "Manual download"),
        XMLNode("p", "In case you'd rather like to download the package manually, here it is:"),
        XMLNode("ul", XMLNode("li", XMLNode("a", package.full, attrs=list(href=paste0(repo.path, "/", package.full))))),
        XMLNode("p", "Package source code:"),
        XMLNode("ul",
          XMLNode("li", XMLNode("a", src.orig, attrs=list(href=paste0(repo.path.src, "/", src.orig)))),
          XMLNode("li", XMLNode("a", src.debian, attrs=list(href=paste0(repo.path.src, "/", src.debian)))),
          XMLNode("li", XMLNode("a", src.dsc, attrs=list(href=paste0(repo.path.src, "/", src.dsc))))
        ),
        imprint_node(imprint=imprint, privacy.policy=privacy.policy)
      ))
  } else {}

  #############
  ## make HTML out of these strings
  #############
  html.page <- XMLTree(
    XMLNode("html",
      XMLNode("head",
        XMLNode("title", paste0("Install package ", package, " from Debain repository")),
        XMLNode("link", attrs=list(
          rel="stylesheet",
          type="text/css",
          href=page.css)),
        XMLNode("meta", attrs=list(
          "http-equiv"="Content-Type",
          content="text/html; charset=utf-8")),
        XMLNode("meta", attrs=list(
          name="generator",
          content="roxyPackage"))
      ),
      XMLNode("body", attrs=list(lang="en"), .children=xml.obj.list),
      attrs=list(xmlns="http://www.w3.org/1999/xhtml")),
    dtd=list(
      doctype="html PUBLIC",
      id="-//W3C//DTD XHTML 1.0 Strict//EN",
      refer="http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")
    )

  return(pasteXML(html.page, indent.by=" "))
} ## end function debRepoInfo()


## function URLs_in_DESCRIPTION()
# looks for patterns in the form of "<...>)" in a given character string and
# replaces it with proper XiMpLe hyperlinks. returns everything in a given result node
URLs_in_DESCRIPTION <- function(desc, result_node="p"){
  desc <- rx.clean(desc)
  if(isTRUE(grepl("\\<.+>\\)", desc))){
    # we split the string at each "(<"
    desc_split <- unlist(strsplit(desc, "<"))
    desc_list <- list()
    for (thisString in desc_split){
      # then look for the presence of a closing ">)" in each substring
      if(isTRUE(grepl(">\\)", thisString))){
        # if one is found, we assume that the start of this substring is
        # the start of a URL. we then split the string at the ">)", turn
        # the first half into a <a href> node and leave the rest as text
        thisString_split <- unlist(strsplit(thisString, ">\\)"))
        if(length(thisString_split) > 2){
          stop(simpleError("html: something very odd happened in your DESCRIPTION text. please check all URLs for proper '(<...>)' bracket structure!"))
        } else {}
        if(isTRUE(grepl("\\($", desc_list[[length(desc_list)]]))){
          # no extra space here
          desc_list[[length(desc_list)]] <- paste0(desc_list[[length(desc_list)]], "<")
        } else {
          desc_list[[length(desc_list) + 1]] <- "<"
        }
        desc_list[[length(desc_list) + 1]] <- XMLNode("a", attrs=list(href=thisString_split[1]), thisString_split[1])
        desc_list[[length(desc_list) + 1]] <- ">)"
        if(length(thisString_split) > 1){
          if(isTRUE(grepl("^[[:alnum:]]", thisString_split[2]))){
            desc_list[[length(desc_list) + 1]] <- thisString_split[2]
          } else {
            # no extra space here
            desc_list[[length(desc_list)]] <- paste0(desc_list[[length(desc_list)]], thisString_split[2])
          }
        } else {}
      } else {
        desc_list[[length(desc_list) + 1]] <- thisString
      }
    }
    result <- XMLNode(result_node, .children=desc_list)
  } else {
    result <- XMLNode(result_node, desc)
  }
  return(result)
} ## end function URLs_in_DESCRIPTION()


## function roxy.html()
# if index=TRUE, pckg is treated as a list
# for global repository index outside pckg dir, set index=TRUE and redirect="pckg/"
#' @import XiMpLe
roxy.html <- function(pckg, index=FALSE, css="web.css", R.version=NULL,
  url.src=NULL, url.win=NULL, url.mac=NULL, url.doc=NULL, url.vgn=NULL, title.vgn=NULL, url.deb.repo=NULL, main.path.mac=NULL, title=NULL,
  cite="", news="", changelog="", readme="", redirect="", rss.file=NULL, flattr.id=NULL, URL=NULL, imprint=NULL, privacy.policy=NULL) {

  rss.header <- rss.feed <- NULL

  if(isTRUE(index)){
    # treat pckg as a list
    if(!is.list(pckg)){
      pckg <- list(pckg)
    } else {}
    page.css <- css
    html.body.table <- lapply(as.list(pckg), function(this.pckg){
        pckg.name <- this.pckg[,"Package"]
        pckg.title <- this.pckg[,"Title"]
        table.row <- XMLNode("tr",
            XMLNode("td", XMLNode("a", pckg.name, attrs=list(href=paste0(redirect, pckg.name, "/index.html")))),
            XMLNode("td", pckg.title),
            attrs=list(valign="top")
          )
      })
    html.body <- XMLNode("body",
      XMLNode("h1", title),
      XMLNode("table", .children=html.body.table, attrs=list(summary=title)),
      imprint_node(imprint=imprint, privacy.policy=privacy.policy),
      attrs=list(lang="en"))
  } else {
    got.fields <- dimnames(pckg)[[2]]
    pckg.name <- pckg[,"Package"]
    title <- paste0(title, ": ", pckg.name)

    # check for RSS
    if(!is.null(rss.file)){
      rss.header <- XMLNode("link", attrs=list(
        rel="alternate",
        type="application/rss+xml",
        title=paste0("RSS (", title, ")"),  
        href=rss.file))

      rss.feed <- XMLNode("a",
        XMLNode("img", attrs=list(
            src="../feed-icon-14x14.png",
            alt=paste0("RSS feed for R package ", pckg.name)
          )),
        attrs=list(href=rss.file))
    } else {}

    pckg.title <- rx.clean(pckg[,"Title"])
    pckg.authors <- get.authors(pckg, maintainer=TRUE, all.participants=TRUE, check.orcid=TRUE)
    # check if there's a hyperlink in the participant string, which should be the case
    # for ORCIDs in a person's comment
    pckg.participants <- lapply(
      seq_along(pckg.authors[["participants"]]),
      function(this_prtc_num){
        this_prtc <- rx.clean(pckg.authors[["participants"]][this_prtc_num])
        if(this_prtc_num < length(pckg.authors[["participants"]])){
          this_prtc <- paste0(this_prtc, ", ")
        } else {}
        if(grepl("<a href=\"https://orcid.", this_prtc)){
          if(!grepl(paste0("https://orcid.org/", "([[:digit:]]{4}[-]){3}[[:digit:]]{3}[[:alnum:]]"), this_prtc)){
            warning("Invalid ORCID format, please check again!", call.=FALSE)
          } else {
            pckg.prt.start <- gsub("(.*)(<a href=\\\"https://orcid.*)", "\\1", this_prtc, perl=TRUE)
            pckg.prt.url <- gsub("(.*)(<a href=\\\"https://orcid.*)(.*</a>)(.*)", "\\2\\3", this_prtc, perl=TRUE)
            pckg.prt.end <- gsub("(.*</a>)(.*)", "\\2", this_prtc, perl=TRUE)
            this_prtc <- XMLNode("span",
              pckg.prt.start,
              node(parseXMLTree(pckg.prt.url, object=TRUE), node=list("a")),
              pckg.prt.end
            )
          }
        } else {}
        return(this_prtc)
      }
    )
    pckg.maintainer <- rx.clean(pckg.authors[["cre"]], nomail=FALSE, textmail=TRUE)

    pckg.materials <- list()
    if(file_test("-f", readme)){
      pckg.materials <- append(pckg.materials, XMLNode("a", "README", attrs=list(href="README.html")))
    } else {}
    if(file_test("-f", news)){
      if(length(pckg.materials)){
        pckg.materials <- append(pckg.materials, XMLNode("span", "&nbsp;"))
      } else {}
      pckg.materials <- append(pckg.materials, XMLNode("a", "NEWS", attrs=list(href=gsub("(.*)(NEWS)(.*)", "\\2\\3", news, perl=TRUE))))
    } else {}
    if(file_test("-f", changelog)){
      if(length(pckg.materials)){
        pckg.materials <- append(pckg.materials, XMLNode("span", "&nbsp;"))
      } else {}
      pckg.materials <- append(pckg.materials, XMLNode("a", "ChangeLog", attrs=list(href="ChangeLog")))
    } else {}
    if(!is.null(rss.feed)){
      pckg.materials <- append(pckg.materials, rss.feed)
    } else {}

    page.css <- paste0("../", css)
    html.body <- XMLNode("body",
      XMLNode("h2", paste0(pckg.name, ": ", pckg.title)),
      URLs_in_DESCRIPTION(pckg[,"Description"]),
      XMLNode("table",
        rx.tr("Version:", pckg[,"Version"]),
        rx.html.switch(desc=pckg, field="Depends"),
        rx.html.switch(desc=pckg, field="Imports"),
        rx.html.switch(desc=pckg, field="LinkingTo"),
        rx.html.switch(desc=pckg, field="Suggests"),
        rx.html.switch(desc=pckg, field="Enhances"),
        rx.html.switch(desc=pckg, field="Additional_repositories"),
        rx.tr("Published:", as.character(as.Date(getDescField(pckg, field=c("Date","Packaged","Date/Publication"))))),
        rx.tr("Author:", pckg.participants, isList=TRUE),
        rx.tr("Maintainer:", pckg.maintainer),
        rx.html.switch(desc=pckg, field="BugReports"),
        rx.tr("License:", pckg[,"License"]),
        rx.html.switch(desc=pckg, field="URL"),
        rx.html.switch(desc=pckg, field="NeedsCompilation"),
        rx.html.switch(desc=pckg, field="SystemRequirements"),
        if(file_test("-f", cite)){
          rx.tr("Citation:", XMLNode("a",
            paste0(pckg.name, " citation info"),
            attrs=list(href="citation.html")))
        },
        if(length(pckg.materials)){
          rx.tr("Materials:", pckg.materials)
        },
        attrs=list(summary=paste0("Package ", pckg.name, " summary."))),
      XMLNode("h4", "Downloads:"),
      XMLNode("table",
        if(!is.null(url.doc)){
          rx.tr("Reference manual:", XMLNode("a",
                                             url.doc,
                                             attrs=list(href=url.doc)))
        },
        if(!is.null(url.vgn)){
          if(is.null(title.vgn)){ title.vgn <- url.vgn }
          rx.tr("Vignettes:", XMLNode("", .children=mapply(function(this.url, this.title){
            XMLNode("", .children=list(
              XMLNode("a", this.title, attrs=list(href=this.url)),
              XMLNode("br")))},
            url.vgn, title.vgn, SIMPLIFY=FALSE)
          ))
        },
        if(!is.null(url.src)){
          rx.tr("Package source:", XMLNode("a",
            url.src,
            attrs=list(href=paste0("../../src/contrib/", url.src))))
        },
        if(length(url.win) > 0){
          rx.tr("Windows binaries:",
            lapply(
              length(url.win):1,
              function(this.num){
                this.R <- names(url.win)[this.num]
                XMLNode("span",
                  paste0("R ", this.R, ": "),
                  XMLNode("a",
                    ifelse(this.num > 1, paste0(url.win[[this.num]], ", "), url.win[[this.num]]),
                    attrs=list(href=paste0("../../bin/windows/contrib/", this.R, "/", url.win[[this.num]]))
                  )
                )
              }
            ),
            isList=TRUE
          )
        },
        if(length(url.mac) > 0){
          rx.tr("OS X binaries:",
            lapply(
              length(url.mac):1,
              function(this.num){
                this.R <- names(url.mac)[this.num]
                XMLNode("span",
                  paste0("R ", this.R, ": "),
                  XMLNode("a",
                    ifelse(this.num > 1, paste0(url.mac[[this.num]], ", "), url.mac[[this.num]]),
                    attrs=list(href=paste0("../../bin/macosx/", main.path.mac, "/", this.R, "/", url.mac[[this.num]]))
                  )
                )
              }
            ),
            isList=TRUE
          )
        },
        if(!is.null(url.deb.repo)){
          rx.tr("Debain binary package:", XMLNode("a", "Learn how to install Debian packages from this repository", attrs=list(href=url.deb.repo)))},
        attrs=list(summary=paste0("Package ", pckg.name, " downloads."))),
        imprint_node(imprint=imprint, privacy.policy=privacy.policy),
      attrs=list(lang="en"))
  }

  html.page <- XMLTree(
    XMLNode("html",
      XMLNode("head",
        XMLNode("title", title),
        XMLNode("link", attrs=list(
          rel="stylesheet",
          type="text/css",
          href=paste0(redirect, page.css))),
        XMLNode("meta", attrs=list(
          "http-equiv"="Content-Type",
          content="text/html; charset=utf-8")),
        if(!is.null(flattr.id)){
          XMLNode("meta", attrs=list(
            name="flattr:id",
            content=flattr.id))
        } else {},
        XMLNode("meta", attrs=list(
          name="generator",
          content="roxyPackage")),
        rss.header),
      html.body,
      attrs=list(xmlns="http://www.w3.org/1999/xhtml")),
    dtd=list(
      doctype="html PUBLIC",
      id="-//W3C//DTD XHTML 1.0 Strict//EN",
      refer="http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"))

  return(pasteXML(html.page))
} ## end function roxy.html()


## function roxy.html.cite()
# tries to create a HTML version of the citation file
# 'cite.obj' must be a citation object (i.e., inherit from class "bibentry")
#' @import XiMpLe
roxy.html.cite <- function(cite.obj, page.css="web.css", package="", imprint=NULL, privacy.policy=NULL){
  cite.mheader <- attr(cite.obj, "mheader")
  cite.text <- gsub("&", "&amp;", cite.obj$textVersion)
  cite.bibtex <- paste(paste0("\t\t\t", gsub("&", "&amp;", gsub("^  ", "\t", toBibtex(cite.obj)))), collapse="\n")

  html.page <- XMLTree(
    XMLNode("html",
      XMLNode("head",
        XMLNode("title", paste0(package, " citation info")),
        XMLNode("link", attrs=list(
          rel="stylesheet",
          type="text/css",
          href=page.css)),
        XMLNode("meta", attrs=list(
          "http-equiv"="Content-Type",
          content="text/html; charset=utf-8")),
        XMLNode("meta", attrs=list(
          name="generator",
          content="roxyPackage"))
      ),
      XMLNode("body",
      if(!is.null(cite.mheader) & !is.null(cite.text)){
        XMLNode("p", cite.mheader)
        XMLNode("blockquote", cite.text)},
      if(!is.null(cite.bibtex)){
        XMLNode("p", "Corresponding BibTeX entry:")
        XMLNode("pre", cite.bibtex)},
        imprint_node(imprint=imprint, privacy.policy=privacy.policy),
        attrs=list(lang="en")),
      attrs=list(xmlns="http://www.w3.org/1999/xhtml")),
    dtd=list(
      doctype="html PUBLIC",
      id="-//W3C//DTD XHTML 1.0 Strict//EN",
      refer="http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")
    )

  return(pasteXML(html.page))
} ## end function roxy.html.cite()


## function rx.css()
# CSS file generator
rx.css <- function(){
  paste("/* these CSS definitions are identical to those used for CRAN */
body {
  background: white;
  color: black;
}

a {
  background: white;
  color: blue;
}

h1 {
  background: white;
  color: rgb(55%, 55%, 55%);
  font-family: monospace;
  font-size: x-large;
  text-align: center;
}

h2 {
  background: white;
  color: rgb(40%, 40%, 40%);
  font-family: monospace;
  font-size: large;
}

h3, h4, h5 {
  background: white;
  color: rgb(40%, 40%, 40%);
  font-family: monospace;
}

em.navigation {
  font-weight: bold;
  font-style: normal;
  background: white;
  color: rgb(40%, 40%, 40%);
  font-family: monospace;
}

img.toplogo {
  vertical-align: middle;
}

span.check_ok {
  color: black;
}
span.check_ko {
  color: red;
}

span.BioC {
  color: #2C92A1;
}
span.Ohat {
  color: #8A4513;
}
span.Gcode {
  color: #5B8A00;
}
span.Rforge {
  color: #8009AA;
}

span.acronym {
  font-size: small;
}
span.env {
  font-family: monospace;
}
span.file {
  font-family: monospace;
}
span.option{
  font-family: monospace;
}
span.pkg {
  font-weight: bold;
}
span.samp {
  font-family: monospace;
}

/* these are custom CSS definitions of roxyPackage */

pre.repo {
  padding: 5pt;
  border: 1px dashed #000000;
  background-color: #DFDFFF;
  white-space: pre;
  white-space: -moz-pre-wrap; /* Mozilla, supported since 1999 */
  white-space: -pre-wrap; /* Opera 4 - 6 */
  white-space: -o-pre-wrap; /* Opera 7 */
  white-space: pre-wrap; /* CSS3 - Text module (Candidate Recommendation) http://www.w3.org/TR/css3-text/#white-space */
  word-wrap: break-word; /* IE 5.5+ */
}
span.imprint {
  float: right;
  font-size: small;
  padding-top: 75px;
  padding-right: 10px;
}
span.dot{
  font-weight: bold;
}
")
} ## end function rx.css()
