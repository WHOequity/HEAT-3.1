.LANGS <- dplyr::filter(
  readxl::read_xlsx("data/langs-dictionary.xlsx"),
  !is.na(key)
)  

translate <- function(key, lang, html = TRUE) {
  if (!(lang %in% colnames(.LANGS))) {
    warning(
      'could not find language "', lang, '" defaulting to english', 
      call. = FALSE
    )
    lang <- "english"
  }
  
  msg <- sprintf("missing %s translation for `%s`", lang, key)
  
  if (!(key %in% .LANGS$key)) {
    return(key)
  }
  
  value <- .LANGS[.LANGS$key == key, lang, drop = TRUE]
  
  if (is.na(value)) {
    return(msg)
  }
  
  if (html) {
    HTML(value)
  } else {
    value
  }
}
