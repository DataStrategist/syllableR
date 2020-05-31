### General functions for syllableCounter

#' @title Creates a .rds with the latest file.
#' @description Connects to svn.code.sf.net to grab that
#' @return RDS is saved. If all went well, "OK" is returned
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname getNewDictionary
#' @export
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom stringr str_split
getNewDictionary <- function(){
  site <- xml2::read_html("http://svn.code.sf.net/p/cmusphinx/code/trunk/cmudict/cmudict-0.7b")
  nodes <- rvest::html_nodes(site, "p")
  justText <- rvest::html_text(nodes)
  justText <- unlist(stringr::str_split(justText, "\n"))
  ## remove garbage and Split on double-space
  justText <- gsub("[^[:alnum:]///' ;]", "", justText)
  justText <- justText[substring(justText, 1,1) != ";"]
  outputDF <- as.data.frame(stringr::str_split(justText, "  ",simplify = TRUE), stringsAsFactors = FALSE)
  names(outputDF) <- c("word", "syllables")

  ## keep only numbers
  outputDF$mood <- gsub("[^0-9]", "",outputDF$syllables)
  outputDF <- outputDF[,c(1,3)]
  outputDF$word <- tolower(outputDF$word)
  saveRDS(outputDF, "dictionary.RDS")
  return("OK")
}

#' @title Takes a sentence and figures out the syllable inflection for each word
#' @description depending on the parameters, it'll either give back a string of
#' numbers, or a
#' @param sentence text to parse, Default: 'Damn that Little Mouse cause i'm
#' the Albatross'
#' @param explain if FALSE, just returns the syllable intonation, if TRUE, will
#' return the input sentence with the corresponding syllable numbers,
#' Default: FALSE
#' @return Will give you a list of 0, 1, 2 depending on where the emphasis falls
#' on each syllable.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}}
#' @rdname getStructure
#' @export
#' @importFrom purrr map
#' @importFrom dplyr %>%
getStructure <- function(sentence = "Damn that Little Mouse cause i'm the Albatross", explain = FALSE){
  dictionary <- readRDS("dictionary.RDS")

  ## split up sentence into a bunch of words
  split <- unlist(strsplit(tolower(sentence), "[^[:alnum:]']"))

  result <- unlist(split %>% purrr::map(~(. == dictionary$word))%>% purrr::map(~dictionary$mood[.]))

  if(explain){
    return(data.frame(word = split, syllables = result))
  } else {
    paste(result, collapse = "")
  }
}

#' @title Plays a string of syllables
#' @description WIP, tryng to figure out how to "sing" the inflection of a
#' sentance suitable for electronic music NLG
#' @param structure string of 0,1,2, the output from `getStructure()`
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_match_all}}
#'  \code{\link[Rmusic]{play_music}}
#' @rdname tonePlayer
#' @export
#' @importFrom stringr str_match_all
#' @importFrom Rmusic play_music
#' @importFrom dplyr %>%
tonePlayer <- function(structure){
  pitch <- gsub(0, "A", structure)
  pitch <- gsub(1, "B", pitch)
  pitch <- gsub(2, "C", pitch)

  pitch <- stringr::str_match_all(pitch, ".{1}") %>% unlist


  duration <- stringr::str_match_all(structure, ".{1}") %>% unlist

  duration <- gsub(0, .5, duration)
  duration <- gsub(1, 1, duration)
  duration <- gsub(2, 1, duration)

  Rmusic::play_music(pitch, as.numeric(duration))

}


