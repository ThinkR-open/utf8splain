
prepend_class <- function(., class){
  class(.) <- c(class,class(.))
  .
}

raw_to_bits <- function(.){
  paste(substr(as.character(rev(rawToBits(.))), 2, 2), collapse = "")
}

#' Split a utf-8 encoded string into bytes
#'
#' @param s a utf-8 encoded string
#'
#' @return a tibble (with extra class "tbl_bytes") with columns
#' - id : index of the byte
#' - byte: hex representation of the byte
#' - decimal: decimal reprsentation of the byte
#' - binary: binary representation of the byte
#'
#' @examples
#' bytes( "hello" )
#'
#' @importFrom tibble tibble
#' @importFrom purrr map_chr
#' @export
bytes <- function(s){
  bytes <- charToRaw(s)
  tibble(
      id      = seq_along(bytes),
      byte    = bytes,
      decimal = as.integer(bytes),
      binary  = map_chr(bytes, raw_to_bits)
    ) %>%
    prepend_class("tbl_bytes" )
}

#' @importFrom stringr str_replace
extract_rune <- function( bits ){
  map_chr( bits, ~ paste(substr(as.character(.),2,2), collapse = "" ) ) %>%
    str_replace( "^1+0", "" ) %>%
    paste( collapse = "")
}

#' Split a string into unicode runes
#'
#' @param s a utf-8 encoded string
#'
#' @return a tibble (with extra class "tbl_runes") with columns:
#' - id: index of the rune in the input
#' - rune: name of the rune, i.e. an hex representation prefixed by "U+"
#' - rune_binary: binary representation of the rune
#' - rune_decimal: decimal representation of the rune, i.e. the index of the character in the unicode table
#' - utf8_bytes: hex representation of the utf8 bytes
#' - utf8_binary: binary representation of the utf8 bytes
#' - description: description of the rune, from [uni::code]
#'
#' @examples
#' runes( "hello world" )
#'
#' @importFrom magrittr %>%
#' @importFrom purrr map map_int
#' @importFrom dplyr group_by summarise mutate select left_join
#' @importFrom tidyr nest
#' @export
runes <- function(s){
  bytes <- charToRaw(s)
  bits  <- map( bytes,~ rev(rawToBits(.)) )

  leading_1 <- map_int( bits, ~which.min(.)-1L )
  id <- cumsum(leading_1 != 1)

  tibble(
    bytes = map(bytes, force), # hacky until real raw support in dplyr
    bits,
    leading_1,
    id
  ) %>%
    group_by( id ) %>%
    summarise(
      bytes = list( unlist(bytes) ),
      bits  = list( bits )
    ) %>%
    mutate(
      rune_binary  = map_chr( bits, extract_rune ),
      rune_decimal = strtoi(rune_binary, base = 2),
      rune         = sprintf( "U+%04X", rune_decimal ),
      utf8_bytes   = map_chr( bytes, ~ paste( sprintf("%02X", as.integer(.)), collapse = " " ) ),
      utf8_binary  = map_chr( bits, ~ paste( map_chr(., ~paste(as.numeric(.), collapse = "") ), collapse = " " ) )
    ) %>%
    select( id, rune, rune_binary, rune_decimal, utf8_bytes, utf8_binary ) %>%
    left_join( select(uni::code, rune, description) , by = "rune" ) %>%
    structure( class = c( "tbl_runes", class(.) ) )
}

#' @importFrom grDevices grey
#' @importFrom crayon make_style bold
hide_encoding_one <- function(x){
  rx    <- "^(1*0)(.*)$"
  hide <- make_style( grey(0.9), grey = TRUE )
  left  <- hide( str_replace(x, rx, "\\1") )
  right <- str_replace(x, rx, "\\2")
  paste( paste0( left, right ), collapse = " " )
}

#' @importFrom stringr str_split
hide_encoding_bits <- function( binary ){
  str_split(binary, " ") %>%
    map_chr( hide_encoding_one )
}

#' @importFrom crayon bold blue red
#' @importFrom dplyr mutate_at vars mutate pull
#' @export
print.tbl_runes <- function(x, ...){
  n <- nrow(x)
  cat( "utf-8 encoded string with", n, "runes\n\n")

  txt <- x %>%
    select(rune, utf8_bytes, utf8_binary, description) %>%
    as.data.frame() %>%
    mutate_at(vars(utf8_bytes, utf8_binary), format, justify = "right") %>%
    mutate_at(vars(rune,description), format, justify = "left") %>%
    mutate( utf8_binary = hide_encoding_bits(utf8_binary)) %>%
    mutate( text = paste( bold(rune), red(utf8_bytes), utf8_binary, blue(description), sep = "   ") ) %>%
    pull()

  writeLines(txt)

  invisible(x)
}
