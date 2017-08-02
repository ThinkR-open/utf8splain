
raw_to_bits <- function(.){
  paste(substr(as.character(rev(rawToBits(.))), 2, 2), collapse = "")
}

#' @importFrom tibble tibble
#' @importFrom purrr map_chr
#' @export
bits <- function(s){
  structure( tibble(
    bytes = charToRaw(s),
    bits  = map_chr(bytes, raw_to_bits)
  ), class = c("tbl_bits", "tbl_df", "tbl", "data.frame") )
}
