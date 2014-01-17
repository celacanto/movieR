library(XML)
library(stringr)

getCannesSelection <- function(year){
    
  cannesUrl <- paste("http://www.festival-cannes.com/en/archives/", year, "/allSelections.html", sep = "")
  canesHtml <- htmlParse(cannesUrl, encoding = "utf-8")
    
  selectionsListsRaw <- readHTMLList(canesHtml)
  selectionsLists <- head(selectionsListsRaw[-(1:8)], -1)
  
  if(length(selectionsLists) == 0) {
    # In 1950 and 1948 there were no Festival
    return("festival was not this year")
  }
  
  titleRaw <- lapply(selectionsLists, str_extract,  "^.*?\t")
  titleClean <- lapply(titleRaw, gsub, pattern = " ?\t", replacement = "")
  orName <- lapply(titleClean, gsub, pattern = " ?\\(.*\\)( with .*)?$", replacement = "")
  engNameRaw <- lapply(titleClean, str_extract, pattern = " ?\\(.*\\)( with .*)?$")
  engNameTrim <- lapply(engNameRaw, str_trim)
  engName <- lapply(engNameTrim, gsub, pattern = "\\(|\\)| with .*$", replacement = "")
  
  directorRaw <- lapply(selectionsLists, str_extract,  "\t .*")
  directorWithand <- lapply(directorRaw, gsub, pattern = ", \t{2,}", replacement = " / ")
  director <- lapply(directorWithand, gsub, pattern = "\t directed by ", replacement = "")
  
  
  selectionTypeRaw <- xpathSApply(canesHtml, "//h2", xmlValue)
  # There are some <h2> headers after the selections names. 
  # All theses shit <h2> headers are after the "select a year"
  selectAyearIndex <- grep("Select a year", selectionTypeRaw)
  selectionType <- selectionTypeRaw[1:(selectAyearIndex - 1)]
  
  selectionSize <- sapply(director, length)
  selection <- mapply(rep, x = selectionType, times = selectionSize)
  
  
  listVar <- list(selection, orName, engName, director)
  x <- as.data.frame(as.matrix(sapply(listVar, unlist)), stringsAsFactors = FALSE)
  rownames(x) <- NULL
  
  colnames(x) <- c("selection", "original.title", "english.title", "director")
  
  x$Year <- year
  
  return(x)
  
}

years <- 2013:1946
total <- length(years)
pb <- txtProgressBar(min = 0, max = total, style = 3)
cannesList <- list()
for(y in seq_len(total)){
  cannesList[[y]] <- getCannesSelection(years[y])
  setTxtProgressBar(pb, y)
}
close(pb)