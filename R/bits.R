
raw_to_bits <- function(.){
  paste(substr(as.character(rev(rawToBits(.))), 2, 2), collapse = "")
}

#' @importFrom tibble tibble
#' @importFrom purrr map_chr
#' @export
bits <- function(s){
  bytes <- charToRaw(s)
  structure( tibble(
    decimal = as.integer(bytes),
    hex     = bytes,
    binary  = map_chr(bytes, raw_to_bits)
  ), class = c("tbl_bits", "tbl_df", "tbl", "data.frame") )
}

#' @importFrom stringr str_replace
extract_rune <- function( bits ){
  map_chr( bits, ~ paste(substr(as.character(.),2,2), collapse = "" ) ) %>%
    str_replace( "^1+0", "" ) %>%
    paste( collapse = "")
}

#' @importFrom magrittr %>%
#' @importFrom purrr map map_int
#' @importFrom dplyr group_by summarise mutate select
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
      hex          = map_chr( bytes, ~ paste( sprintf("%02X", as.integer(.)), collapse = " " ) )
    ) %>%
    select( id, rune, hex, rune_binary, rune_decimal ) %>%
    structure( class = c( "tbl_runes", class(.) ) )

}
