searchUI <- function(id, placeholder, label, df, group, items){
  ns <- NS(id)
  
  div(
    if(group==""){
      selectizeInput(
        inputId = ns('search'),
        label = label,
        choices = NULL,
        multiple = FALSE,
        options = list(
          placeholder = placeholder,
          # you can search the data based on these fields
          searchField = c( as.character(items)),
          # the label that will be shown once value is selected
          labelField = as.character(items),
          # (each item is a row in data), which requires 'value' column (created by cbind at server side)
          render = I(paste0("{
                    option: function(item, escape) {
                return '<div>' + escape(item.",as.character(items),") +'</div>';
                       }
                  }"))
        )
      )
    }
    else{
      selectizeInput(
        inputId = ns('search'),
        label = label,
        choices = NULL,
        multiple = FALSE,
        options = list(
          placeholder = placeholder,
          # predefine all option groups
          optgroups = lapply(unique(df[[group]]), function(x) {
            list(value = as.character(x), label = as.character(x))
          }),
          # what field to sort according to groupes defined in 'optgroups'
          optgroupField = as.character(group),
          # you can search the data based on these fields
          searchField = c(as.character(group), as.character(items)),
          # the label that will be shown once value is selected
          labelField = as.character(items),
          # (each item is a row in data), which requires 'value' column (created by cbind at server side)
          render = I(paste0("{
                    option: function(item, escape) {
                return '<div>' + escape(item.",as.character(items),") +'</div>';
                       }
                  }"))
        )
      )
    }
  )
}

searchServer <- function(input,output,session,df,
                         clearEvent, filterBy,
                         filterVal){
  
  updateSelectizeInput(
    session = session,
    inputId = 'search',
    choices = cbind(df, value = seq_len(nrow(df))),
    selected=character(0),
    server = TRUE)
  
  dfReactive <- reactiveVal(NULL)
  
  
  observeEvent(filterVal(),{
    indexDf <- cbind(df, value = seq_len(nrow(df)))
    updateSelectizeInput(
      session = session,
      inputId = 'search',
      choices = indexDf[indexDf[[filterBy]]==filterVal(),],
      selected = character(0),
      server = TRUE)
    dfReact <- df %>%
      rowid_to_column("idx") %>%
      filter(idx %in% input$search)
    dfReactive(dfReact)
  },ignoreNULL = TRUE)
  
  observeEvent(clearEvent(),{
    if(input$search != "" | !is.null(filterVal())){
      updateSelectizeInput(
        session = session,
        inputId = 'search',
        choices = cbind(df, value = seq_len(nrow(df))),
        selected=character(0),
        server = TRUE)
      dfReact <- data.frame()
      dfReactive(dfReact)
    }
  },ignoreNULL = TRUE)
  
  observeEvent(c(input$search),{
    if(input$search != ""){
      dfReact <- df %>%
        rowid_to_column("idx") %>%
        filter(idx %in% input$search)
      dfReactive(dfReact)
    }
  },ignoreInit = TRUE)
  
  return(reactive({dfReactive()}))
}