if(getRversion() >= "2.15.1"){
  # To remove 'no visible binding for global variable ...' in data.table and lm commands
  utils::globalVariables(c('J', 'weights', 'time', 'A', 'end_of_grace_period', 'count_not_adhered_until_now',
                           'A_model_eligible', 'C_artificial_temp', 'C_artificial',
                           'id'))
}
