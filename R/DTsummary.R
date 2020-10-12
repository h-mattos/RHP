dt_summary = function(dt, max_cat = 10, fontSize = 1) {
  cols = names(dt)
  cols_class = unlist(lapply(dt, function(x) first(class(x))))
  cols = cols[cols_class %in% c('character', 'numeric', 'integer', 'logical', 'factor')]
  cols_class = cols_class[cols_class %in% c('character', 'numeric', 'integer', 'logical', 'factor')]
  out = data.table(Variable = paste0(cols, '\n[', cols_class, ']'), Stats = character(length(cols)), Freq = character(length(cols)), Valid = character(length(cols)))
  pb = txtProgressBar(max = length(cols), style = 3)
  for (col in seq_along(cols)) {
    if (cols_class[col] %in% c('character', 'logical', 'factor')) {
      temp = eval(parse(text = paste0('dt[!is.na(', cols[col], '), .N, .(var = ', cols[col], ')][order(-N)]')))
      if (nrow(temp) > max_cat) {
        temp[(max_cat + 1):.N, var := paste0('[', .N, ' Otros]')]
        temp = temp[, .(N = sum(N)), .(var)]
        temp[1:max_cat, var := paste0(stringr::str_pad(1:.N, 2, 'left', ' '), '. ', var)]
      } else {
        temp[, var := paste0(stringr::str_pad(1:.N, 2, 'left', ' '), '. ', var)]
      }
      out[col, Stats := temp[, paste0(var, collapse = '\n')]]
      out[col, Freq := paste0(temp$N, ' (', round(temp$N * 100/ sum(temp$N), 1), '%)', collapse = '\n')]
      out[col, Valid := paste0(sum(temp$N), '\n(', round(sum(temp$N) * 100 / nrow(dt), 1), '%)')]
    } else if (cols_class[col] %in% c('numeric', 'integer', 'Date')) {
      temp = eval(parse(text = paste0('dt[!is.na(', cols[col], '), .(var = ', cols[col], ')]')))
      out[col, Stats := temp[, paste0('mean (sd): ', round(mean(var), 2), ' (', round(sd(var), 2), ')\n',
                                                  'min < med < max:\n', round(min(var), 2), ' < ', round(median(var), 2), ' < ', round(max(var), 2), '\n',
                                                  'IQR (CV): ', round(diff(quantile(var, c(0.25, 0.75))), 2), ' (', round(sd(var) / mean(var), 2), ')')]]
      out[col, Freq := paste0(temp[, sum(!duplicated(var))], ' valores distintos')]
      out[col, Valid := paste0(nrow(temp), '\n(', round(nrow(temp) * 100 / nrow(dt), 1), '%)')]
    }
    setTxtProgressBar(pb, pb$getVal() + 1)
  }
  return(DT::formatStyle(DT::datatable(out[, lapply(.SD, stringr::str_replace_all, pattern = '\\n', replacement = '</br>')],
                                       escape = F, editable = F, options = list(sDom  = '<"top">flrt<"bottom">ip',
                                                                                pageLength = -1, lengthChange = FALSE), rownames = F),
                         fontSize = paste0(round(100 * fontSize), '%'), columns = 1:ncol(out)))
}
