if(getRversion() >= "2.15.1"){
  # To remove 'no visible binding for global variable ...' in data.table and lm commands
  utils::globalVariables(c('J', 'weights'))
}
