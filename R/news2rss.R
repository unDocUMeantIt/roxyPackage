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


#' Generate RSS feeds from R NEWS files
#' 
#' This function should take either HTML or Rd files and return a valid RSS 2.0 XML file.
#'
#' @param news Character string, path to the R NEWS file to be converted.
#' @param rss Character string, path to the RSS.xml file to be written. If \code{NULL}, results are written to stdout().
#' @param html Logical, whether \code{news} is in HTML or Rd format. If \code{NULL}, guess this from the file ending.
#' @param encoding Character string, how the feed is encoded.
#' @param channel A named character vector with information on this RSS feed:
#'    \describe{
#'      \item{\code{title}:}{Title of the feed, probably the package name.}
#'      \item{\code{link}:}{URL to the package web page, e.g. its repository site.}
#'      \item{\code{description}:}{Descriptions of the feed, e.g. the package.}
#'      \item{\code{language}:}{Optional, a valid RSS language code, see \code{http://www.rssboard.org/rss-language-codes}.}
#'      \item{\code{atom}:}{Optional, full URL to the RSS feed on the web, used for \code{atom:link rel="self"}.}
#'    }
#' @return No return value, writes a file or to stdout()
#' @export
#' @examples
#' \dontrun{
#' channel.info <- c(
#'   title="roxyPackage",
#'   link="http://R.reaktanz.de/pckg/roxyPackage",
#'   description=roxyPackage:::pckg.dscrptn[["Description"]],
#'   atom="http://R.reaktanz.de/pckg/roxyPackage/rss.xml")
#' rss.tree <- news2rss("~/R/roxyPackage/NEWS.Rd",
#'   channel=channel.info)
#' }

news2rss <- function(news, rss=NULL, html=NULL, encoding="UTF-8",
  channel=c(
    title="",
    link="",
    description="",
    language="",
    atom="")){

  # check if all necessary information is present
  for (need.info in c("title", "link", "description")){
    if(!need.info %in% names(channel)){
      missing.info <- TRUE
    } else if(identical(channel[[need.info]], "")){
      missing.info <- TRUE
    } else {
      missing.info <- FALSE
    }
    if(isTRUE(missing.info)){
      stop(simpleError(paste0("news: missing info on RSS channel: '", need.info, "', no RSS feed generated!")))
    } else {}
  }

  if(file_test("-f", news)){
    ## try to see if this is an Rd or HTML file
    if(is.null(html)){
      if(isTRUE(grepl("html$", news, ignore.case=TRUE))){
        html <- TRUE
      } else if(isTRUE(grepl("rd$", news, ignore.case=TRUE))){
        html <- FALSE
        # package <- gsub(".*\\\\name\\{([^\\}]+)\\}.*", "\\1", Rd.raw, perl=TRUE)
      } else {
        stop(simpleError("news: unknown file type, consider setting 'html' accordingly!"))
      }
    } else {}
    ## html should now be either TRUE or FALSE

    if(isTRUE(html)){
      html.raw <- paste(readLines(news), collapse=" ")
    } else {
      html.raw <- paste(capture.output(tools::Rd2HTML(Rd=news)), collapse=" ")
    }

    ## some ugly hacks
    # at least until R 2.14 <ul> and <ol> blocks missed a closing </li>, so
    # XiMpLe::parseXMLTree() comes to really strange results
    # we'll try to fix this, before we parse the object
    html.tags <- XiMpLe:::XML.single.tags(html.raw)
    tag.li.open <- tolower(html.tags) %in% "<li>"
    tag.li.close <- tolower(html.tags) %in% "</li>"
    if(sum(tag.li.open) > sum(tag.li.close)){
      tag.ul.close <- tolower(html.tags) %in% "</ul>"
      tag.ol.close <- tolower(html.tags) %in% "</ol>"
      if(sum(tag.li.open) == sum(tag.li.close) + sum(tag.ul.close) + sum(tag.ol.close)){
        # this should do it:
        html.tags[tag.ul.close] <- "</li></ul>"
        html.tags[tag.ol.close] <- "</li></ol>"
      } else {
        # hm, seems there's more wrong with this file, better stop here
        stop(simpleError("news: the NEWS.html file looks really strange, missing </li>s, please check!"))
      }
    }
    # <meta> and <link> probably are also not in XML format
    for (this.tag in c("link", "meta")){
      html.tags <- gsub(
        paste0("^<", this.tag, " ([^>]+)([^/])>"),
        paste0("<", this.tag, " \\1\\2 />"),
        html.tags,
        ignore.case=TRUE, perl=TRUE)
    }

    ## get HTML tree
    # if this works it should be relatively easy to make an RSS XML object from this object
    html.tree <- XiMpLe::parseXMLTree(html.tags, object=TRUE)

    # News for Package '*': XiMpLe::node(html.tags, node=list("html","body","h2"))
    # Changes in * version *: XiMpLe::node(html.tags, node=list("html","body","h3"))
    # 
    # everything between two <h3>s is news for one release

    # we only need the body child nodes to search for news
    # this node() call returns a named list
    html.body <- XiMpLe::node(html.tree, node=list("html","body"), what="children")
    # now go through the child nodes and return everything from one <h3> to another
    news.start <- which(names(html.body) %in% "h3")
    # how many news nodes are there?
    num.all.news <- length(news.start)
    # append the number of all nodes plus one as last "news.start" for the loop
    news.start <- c(news.start, length(html.body)+1)

    xml.channel.items <- unlist(sapply(1:num.all.news, function(num.this.news){
        this.news.start <- news.start[num.this.news]
        # news end the node before the next news start
        this.news.end <- news.start[num.this.news+1]-1
        this.news.range <- html.body[(this.news.start+1):this.news.end]
        this.news.title.raw <- slot(slot(html.body[[this.news.start]], "children")[[1]], "value")
        # try to generate a unique, unchanging guid -- we take version string
        this.news.guid <- XiMpLe::XMLNode("guid",
          gsub("[[:space:]]*", "", gsub(".*Changes in (.*) version (.*)", "\\1\\2", this.news.title.raw, ignore.case=TRUE, perl=TRUE)),
          attrs=list(isPermaLink="false"))
        # scan title for release dates
        this.news.hasDate <- grepl(".*(\\()?([[:alnum:]]{4}-[[:alnum:]]{2}-[[:alnum:]]{2})(\\))?.*", this.news.title.raw, perl=TRUE)
          if(isTRUE(this.news.hasDate)){
            this.news.date.raw <- gsub(".*(\\()?([[:alnum:]]{4}-[[:alnum:]]{2}-[[:alnum:]]{2})(\\))?.*", "\\2", this.news.title.raw, perl=TRUE)
            this.news.date <- XiMpLe::XMLNode("pubDate", dateRFC2822(this.news.date.raw))
            # remove date from the title
            this.news.title.raw <- gsub("[[:space:]]+(\\()?([[:alnum:]]{4}-[[:alnum:]]{2}-[[:alnum:]]{2})(\\))?", "", this.news.title.raw, perl=TRUE)
          } else {
            this.news.date <- NULL
          }
        this.news.title <- XiMpLe::XMLNode("title", this.news.title.raw)
        this.news.link <- XiMpLe::XMLNode("link", channel[["link"]])
        # place description in CDATA to be able to use HTML formatting
        this.news.range.CDATA <- XiMpLe::XMLNode("![CDATA[", .children=this.news.range)
        this.news.desc <-  XiMpLe::XMLNode("description", this.news.range.CDATA)
        this.news.node <- XiMpLe::XMLNode("item", this.news.title, this.news.link, this.news.date, this.news.guid, this.news.desc)
      }))

    xml.channel.title <- XiMpLe::XMLNode("title", channel[["title"]])
    xml.channel.link <- XiMpLe::XMLNode("link", channel[["link"]])
    roxyPackage.version <- read.dcf(file.path(roxyPackage.lib.dir(), "DESCRIPTION"), fields="Version")
    xml.channel.generator <- XiMpLe::XMLNode("generator", paste0("roxyPackage (", roxyPackage.version, ")"))
    xml.channel.desc <- XiMpLe::XMLNode("description", XiMpLe::XMLNode("![CDATA[", channel[["description"]]))
    xml.channel.lang <- xml.channel.atom <- NULL
    if("language" %in% names(channel)){
      if(!identical(channel[["language"]], "")){
        xml.channel.lang <- XiMpLe::XMLNode("language", channel[["language"]])
      } else {}
    } else {}
    rss.node.attrs <- list(version="2.0")
    if("atom" %in% names(channel)){
      if(!identical(channel[["atom"]], "")){
        xml.channel.atom <- XiMpLe::XMLNode("atom:link", attrs=list(href=channel[["atom"]], rel="self", type="application/rss+xml"))
        rss.node.attrs <- append(rss.node.attrs, list("xmlns:atom"="http://www.w3.org/2005/Atom"))
      } else {}
    } else {}
    xml.channel <- XiMpLe::XMLNode("rss",
      XiMpLe::XMLNode("channel",
        .children=append(
          list(
            xml.channel.title,
            xml.channel.link,
            xml.channel.atom,
            xml.channel.desc,
            xml.channel.generator,
            xml.channel.lang),
          xml.channel.items)),
      attrs=rss.node.attrs)
    rss.tree <- XiMpLe::XMLTree(xml.channel, xml=list(version="1.0", encoding=encoding))
# uncomment for debugging:
# rss.tree <- xml.channel.items
    if(!is.null(rss)){
      cat(pasteXML(rss.tree), file=rss)
      message(paste0("news: updated RSS feed ", rss))
      return(invisible(NULL))
    } else {
      return(rss.tree)
    }
  } else {
    warning(paste0("news: ", news," does not exist, no RSS file created!"), call.=FALSE)
  }
  return(invisible(NULL))
}
