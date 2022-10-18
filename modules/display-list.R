
# generate html ul list from data list
getListHTML <- function (list) {
  rows_html <- paste(unlist(lapply(list, function(x) paste0("<li>", x, "</li>"))),collapse = "")
  innerhtml = paste('<ul class="display-list">', rows_html, '</ul>')
  return (innerhtml)
}
