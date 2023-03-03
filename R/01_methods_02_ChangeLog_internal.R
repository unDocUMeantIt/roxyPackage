# Copyright 2011-2015 Meik Michalke <meik.michalke@hhu.de>
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

## following stuff is internal only

setAs(from="ChangeLog.items", to="list", function(from){
    fromPlain <- slot(from, "plain")
    if(length(fromPlain) > 0){
      newList <- list(fromPlain)
    } else {
      newList <- list()
    }
    newList <- append(newList, slot(from, "sections"))
    return(newList)
  }
)

# transforms named lists into ChangeLog.items objects
setAs(from="list", to="ChangeLog.items", function(from){
    entryNames <- names(from)
    # if there are no names, put all to the 'plain' slot
    if(is.null(entryNames)){
      newEntry <- new("ChangeLog.items", plain=as.vector(unlist(from)))
    } else {
      thisPlain <- as.vector(unlist(from[names(from) %in% ""]))
      if(is.null(thisPlain)){
        thisPlain <- character()
      } else {}
      newEntry <- new("ChangeLog.items",
        plain=thisPlain,
        sections=from[!names(from) %in% ""])
    }
    return(newEntry)
  }
)


## function autoLineBreak()
# roxygen2 can produce long comment lines, and if we add a line break, all but the first line
# still has the needed comment character, so the Rd file is broken. to work around this, the
# "preserveCommentChar" option can be used. set it to "%" to ensure that comment lines always start with "%"
autoLineBreak <- function(text, lineEnd=78, breakAt=c(" "), breakBy="\n", preserveCommentChar=NULL){
  if(all(identical(breakAt, " "), is.null(preserveCommentChar))){
    # simple case, we can just leave it to strwrap()
    results <- paste0(strwrap(x=text, width=lineEnd), collapse=breakBy)
  } else {
    # TODO: replace chunks of this code with strwrap() as well, should be much more efficient
    results <- sapply(text, function(thisText){
      if(nchar(thisText) > lineEnd){
        if(is.null(preserveCommentChar)){
          needCommentChecks <- FALSE
        } else {
          # does this portion begin with a comment sign?
          needCommentChecks <- grepl(paste0("^[[:space:]]*", preserveCommentChar), thisText)
        }
        if(isTRUE(needCommentChecks)){
          lineStart <- gsub("^([[:space:]]*%)(.*)", "\\1", thisText)
          lineEnd <- lineEnd - nchar(lineStart)
          breakBy <- paste0(breakBy, lineStart)
        } else {}
        splittedText <- unlist(strsplit(thisText, ""))
        possibleBreaks <- splittedText %in% breakAt
        # now jump to the fist maximum line end, check if there's a place to break
        # and if not, take the last one before here
        thisPlace <- lineEnd
        while(thisPlace < length(splittedText)){
          thisRange <- (thisPlace-lineEnd+1):thisPlace
          relevantPlaces <- possibleBreaks[thisRange]
          # just in case: if there's nowhere to break lines, just skip
          if(sum(which(relevantPlaces)) < 1){
            thisPlace <- thisPlace + 1
            next
          } else {}
          maxBreakPlace <- thisRange[max(which(relevantPlaces))]
          splittedText[maxBreakPlace] <- breakBy
          thisPlace <- thisPlace + lineEnd
        }
        newText <- paste0(splittedText, collapse="")
        return(newText)
      } else {
        return(thisText)
      }
    })
  }
  return(as.character(results))
} ## end function autoLineBreak()


## function findItemsInChangeLog()
# log: character vector, result of readLines() in parseChangeLog()
findItemsInChangeLog <- function(logLines, item="  -"){
  itemRegExp <- paste0("^", item)
  numLines <- length(logLines)

  newLogLines <- c()
  thisLine <- 1
  while(thisLine < (numLines + 1)){
    if(!grepl(itemRegExp, logLines[thisLine + 1])){
      combinedLogLines <- trim(gsub(itemRegExp, "", logLines[thisLine]))
      while(!grepl(itemRegExp, logLines[thisLine + 1]) && thisLine < numLines){
        combinedLogLines <- paste(trim(combinedLogLines), trim(logLines[thisLine + 1]), collapse="", sep=" ")
        thisLine <- thisLine + 1
      }
      newLogLines <- c(newLogLines, combinedLogLines)
      thisLine <- thisLine + 1
    } else {
      newLogLines <- c(newLogLines, trim(gsub(itemRegExp, "", logLines[thisLine])))
      thisLine <- thisLine + 1
    }
  }

  results <- newLogLines
  return(results)
} ## end function findItemsInChangeLog()


## function parseChangeLog()
parseChangeLog <- function(file, head="ChangeLog for package", change="changes in version", item="  -"){
  cl <- readLines(file, warn=FALSE)
  results <- new("ChangeLog", fullLog=cl)

  # remove empty lines
  cl <- cl[!cl %in% ""]
  last.line <- length(cl)

  # begin the parsing
  # which package is this?
  cl.for.package <- cl[grep(paste0("^", head, "[[:space:]]+[[:alpha:]]+"), cl, ignore.case=TRUE)]
  if(is.null(cl.for.package) || identical(cl.for.package, "")){
    stop(simpleError("log: can't parse the specified ChangeLog, wrong format?"))
  } else {}
  slot(results, "package") <- gsub(paste0("^", head, "[[:space:]]+"), "", cl.for.package, ignore.case=TRUE)

  # separate individual entries
  cl.entries <- c(grep(paste0("^", change, "[[:space:]]+[[:alnum:]]+"), cl, ignore.case=TRUE), last.line + 1)
  entry.idx <- 1
  while(entry.idx < length(cl.entries)){
    this.entry <- cl[cl.entries[entry.idx]:(cl.entries[entry.idx+1]-1)]
    this.entry.version <- gsub(paste0("^", change, "[[:space:]]+([[:digit:]]+[.][[:digit:]]+[.-]?[[:alnum:]]+)[[:space:]]+[(][[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}[)].*"),
      "\\1", this.entry[1], ignore.case=TRUE, perl=TRUE)
    if(identical(this.entry[1], this.entry.version)){
      stop(simpleError("log: can't parse the specified ChangeLog, no valid version information found!"))
    } else {}
    this.entry.date <- gsub(paste0("^", change, "[[:space:]]+[[:digit:]]+[.][[:digit:]]+[.-]?[[:alnum:]]+[[:space:]]+[(]([[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2})[)].*"),
      "\\1", this.entry[1], ignore.case=TRUE, perl=TRUE)
    if(identical(this.entry[1], this.entry.date)){
      stop(simpleError("log: can't parse the specified ChangeLog, no valid date information found!"))
    } else {}
    slot(results, "entries")[[this.entry.version]] <- new("ChangeLog.entry",
      version=this.entry.version,
      date=this.entry.date)

    # now go through this entry and try to find sections
    this.entry <- this.entry[-1]
    this.entry.sect.indices <- c(grep("^[[:alpha:]]+:[[:space:]]*$", this.entry), length(this.entry) + 1)
    slot(slot(results, "entries")[[this.entry.version]], "entry") <- new("ChangeLog.items")
    # if this vector has more than one element, sections are present
    if(this.entry.sect.indices[1] > 1){
      end.of.plain <- this.entry.sect.indices[1]-1
      slot(slot(slot(results, "entries")[[this.entry.version]], "entry"), "plain") <- findItemsInChangeLog(this.entry[1:end.of.plain], item=item)
    } else {}
    if(length(this.entry.sect.indices) > 1){
      sect.idx <- 1
      while(sect.idx < length(this.entry.sect.indices)){
        sect.name <- gsub(":[[:space:]]*$", "", this.entry[this.entry.sect.indices[sect.idx]])
        sect.content <- this.entry[(this.entry.sect.indices[sect.idx]+1):(this.entry.sect.indices[sect.idx+1]-1)]
        slot(slot(slot(results, "entries")[[this.entry.version]], "entry"), "sections")[[sect.name]] <- findItemsInChangeLog(sect.content, item=item)
        sect.idx <- sect.idx + 1
      }
    } else {}

    entry.idx <- entry.idx + 1
  }

  return(results)
} ## end function parseChangeLog()


## methods pasteChangeLog()
#' @import methods
setGeneric("pasteChangeLog", function(log, ...){standardGeneric("pasteChangeLog")})
# single ChangeLog item
#' @import methods
setMethod("pasteChangeLog",
  signature=signature(log="ChangeLog.items"),
  function(log, item="  -", lineEnd=78, breakAt=c(" "), breakBy="\n    "){
    # adjust lineEnd
    lineEnd <- lineEnd - nchar(item) - 1

    # are there plain entries?
    if(length(slot(log, "plain")) > 0){
      this.entry.plain <- paste0(item, " ",
        autoLineBreak(slot(log, "plain"), lineEnd=lineEnd, breakAt=breakAt, breakBy=breakBy),
        "\n", collapse="")
    } else {
      this.entry.plain <- ""
    }
    # are there sections?
    if(length(slot(log, "sections")) > 0){
      this.entry.sections <- paste(sapply(seq_along(slot(log, "sections")), function(this.idx){
          this.sect <- paste0(item, " ",
            sapply(
              seq_along(slot(log, "sections")[[this.idx]]),
              function(this_item){
                autoLineBreak(slot(log, "sections")[[this.idx]][[this_item]], lineEnd=lineEnd, breakAt=breakAt, breakBy=breakBy)
              }
            ),
            "\n", collapse="")
          this.sect.name <- paste0(names(slot(log, "sections"))[this.idx], ":\n")
          this.sect.full <- paste0(this.sect.name, this.sect, collapse="")
          return(this.sect.full)
        }), collapse="")
    } else {
      this.entry.sections <- ""
    }
    this.entry.full <- paste0(this.entry.plain, this.entry.sections, collapse="")
    return(this.entry.full)
})


## function mergeLists()
# merges two ChangeLog entry lists into one
mergeLists <- function(list1, list2, uniq=TRUE){
  listAppend <- function(l1, l2){
    # if there were already plain items, these here need to get
    # into the first list entry
    plainInLists <- "" %in% names(l1) && is.null(names(l2))
    if(isTRUE(plainInLists)){
      l1[[1]] <- c(unlist(l2), unlist(l1[[1]]))
    } else {
      l1 <- append(l1, l2, after=0)
    }
    return(l1)
  }
  namesL1 <- names(list1)
  namesL2 <- names(list2)
  allNames <- unique(c(namesL1, namesL2))
  allNamesNoPlain <- allNames[!allNames %in% ""]
  havePlain <- "" %in% allNames || any(is.null(namesL1), is.null(namesL2))
  if(is.null(allNames)){
    newList <- append(list1, list2)
  } else {
    newList <- lapply(allNamesNoPlain, function(this.name){
      return(c(list1[[this.name]], list2[[this.name]]))
    })
    names(newList) <- allNamesNoPlain
    if(isTRUE(havePlain)){
      PlainList1 <- list1[namesL1 %in% ""]
      PlainList2 <- list2[namesL2 %in% ""]
      if(is.null(namesL2)){
        newList <- listAppend(newList, list2)
      } else if(length(PlainList2) > 0){
        newList <- listAppend(newList, PlainList2)
      } else {}
      if(is.null(namesL1)){
        newList <- listAppend(newList, list1)
      } else if(length(PlainList1) > 0){
        newList <- listAppend(newList, PlainList1)
      } else {}
    } else {}
  }
  # remove duplicates
  if(isTRUE(uniq)){
    newList <- lapply(newList, unique)
  } else {}
  return(newList)
} ## end function mergeLists()


# single ChangeLog entry
#' @import methods
setMethod("pasteChangeLog",
  signature=signature(log="ChangeLog.entry"),
  function(log, change="changes in version", item="  -", lineEnd=78, breakAt=c(" "), breakBy="\n    "){
    entry.head <- paste0(change, " ", slot(log, "version"), " (", slot(log, "date"), ")\n")
    entry.items <- pasteChangeLog(slot(log, "entry"), item=item, lineEnd=lineEnd, breakAt=breakAt, breakBy=breakBy)
    cl.entries <- paste0(entry.head, entry.items, "\n", collapse="")
    return(cl.entries)
})

# full ChangeLog
#' @import methods
setMethod("pasteChangeLog",
  signature=signature(log="ChangeLog"),
  function(log, file=NULL, head="ChangeLog for package", change="changes in version", item="  -", lineEnd=78, breakAt=c(" "), breakBy="\n    "){
    cl.title <- paste0(head, " ", slot(log, "package"), "\n\n")
    cl.entries <- paste(sapply(slot(log, "entries"), function(this.entry){
        pasteChangeLog(this.entry, change=change, item=item, lineEnd=lineEnd, breakAt=breakAt, breakBy=breakBy)
      }), collapse="")

    results <- paste0(cl.title, cl.entries)
    if(!is.null(file)){
      cat(results, file=file)
      return(invisible(NULL))
    } else {}
    return(results)
  }
) ## end method pasteChangeLog()



# show methods for ChangeLog objects
#' @import methods
setMethod("show", signature(object="ChangeLog.items"), function(object){
  cat(pasteChangeLog(object))
})
#' @import methods
setMethod("show", signature(object="ChangeLog.entry"), function(object){
  cat(pasteChangeLog(object))
})
#' @import methods
setMethod("show", signature(object="ChangeLog"), function(object){
  cat(pasteChangeLog(object))
})
