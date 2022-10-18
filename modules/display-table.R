
# generate html grid from data frame
getTableHTML <- function (frames) {
  innerhtml = '<table class="display-table row-coloring">'
  header_html = '<tr class="display-table-row">'
  innerhtml = paste(innerhtml,header_html)
  for (col in colnames(frames)){
    header_item <- '<th class="display-table-header padding-y-1 padding-x-1 border border-base-dark">'
    cell <- sprintf("<span col-name='%s'>%s</span>", 
                    col,col)
    header_item = paste(header_item, cell, '</th>')
    innerhtml = paste(innerhtml,header_item)
  }
  
  rows_html <- paste(unlist(lapply(1:nrow(frames), function(row){
    row_html = '<tr class="display-table-row">'
    for (col in colnames(frames)){
      row_html = paste(row_html,'<td class="display-table-cell padding-y-1 padding-x-1 border border-base-dark">')
      cellVal <- frames[row,col]
      if (is.na(cellVal)){
        cellVal <- "Not Applicable"
      }
      cell <- sprintf("<span cell-value='%s'>%s</span>",
                      cellVal,cellVal)
      row_html = paste(row_html,cell)
      row_html = paste(row_html,'</td>')
    }
    row_html = paste(row_html,'</tr>')
  })),collapse = '')
  
  innerhtml = paste(innerhtml, rows_html)
  #print(header_html)
  innerhtml = paste(innerhtml, "</table>")
  return (innerhtml)
}


