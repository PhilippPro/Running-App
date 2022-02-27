my_seconds_to_period = function(x) {
  days = round(x %/% (60 * 60 * 24))
  hours = round((x - days*60*60*24) %/% (60 * 60))
  minutes = round((x - days*60*60*24 - hours*60*60) %/% 60)
  seconds = round(x - days*60*60*24 - hours*60*60 - minutes*60)
  days_str = ifelse(days == 0, "", paste0(days, "d "))
  hours_str = ifelse((hours == 0 & days == 0), "", paste0(hours, "H "))
  minutes_str = ifelse((minutes == 0 & days == 0 & hours == 0), "", paste0(minutes, "M "))
  seconds_str = paste0(seconds, "S")
  final_str = paste0(days_str, hours_str, minutes_str, seconds_str)
  return(final_str)
}