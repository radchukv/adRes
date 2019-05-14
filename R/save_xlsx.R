#' Save a table in the *.xlsx format
#'
#' This function is called internally by other functions. It is a wrapper for
#' the function \code{\link[openxlsx:write.xlsx]{write.xlsx}} of the package \pkg{openxlsx}.
#'
#' @inheritParams parameters_definition
#' @return The table (but the return is invisible).
#' @export
#'
#' @examples
#' d <- data.frame(test = 1:5, id = LETTERS[1:5],
#'                 a= seq(from = 0.01, to = 0.05, by = 0.01),
#'                 test = 1:5, id = LETTERS[1:5],
#'                 p = c('<0.001', '0.002', '0.260', '<0.001', '0.987'))
#' dir.create('tables')
#' save_xlsx(table = d, table_basename = './tables/test1',
#'           typeTab = 'LRT')
#'
save_xlsx <- function(table, table_basename,
                      first_column_name = NULL, typeTab){
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    tryCatch({## the function can crash if no zip program is known to R
      if (!is.null(first_column_name)) {
        table <- eval(parse(text = paste0("cbind(", first_column_name, " = rownames(table), table)")))
      }
      wb <- openxlsx::createWorkbook("My name here")
      openxlsx::addWorksheet(wb, 'Sheet 1', gridLines = FALSE)
      openxlsx::writeData(wb, sheet = 1, table)
      hs <- openxlsx::createStyle(border = "TopBottom",
                                  textDecoration = "Bold",
                                  halign = "center")
      openxlsx::addStyle(wb, sheet = 1, hs, rows = 1, cols= 1:ncol(table))

      # format of the numeric values

      if (typeTab == 'efSizes'){
        spec <- list(col1 = 5, fmt = c('0.000', '0.000'))
      }
      if (typeTab == 'LRT'){
        spec <- list(col1 = 4, fmt = c('0.00', '0', 'TEXT', 'SCIENTIFIC', 'SCIENTIFIC'))
      }
      if (typeTab == 'heterog'){
        spec <- list(col1 = 3, fmt = c('0.000', '0.0', '0.0', 'TEXT'))
        }

      generstyle <- openxlsx::createStyle(numFmt = spec$fmt[1], halign = 'right')
      openxlsx::addStyle(wb, sheet = 1, style = generstyle, rows = 2:(nrow(table) + 1),
                         cols = spec$col1, gridExpand = TRUE, stack = TRUE)
      for (i in 2:length(spec$fmt)){
        generstyle <- openxlsx::createStyle(numFmt = spec$fmt[i], halign = 'right')
        openxlsx::addStyle(wb, sheet = 1, style = generstyle, rows = 2:(nrow(table) + 1),
                           cols = spec$col1 + i - 1, gridExpand = TRUE, stack = TRUE)
      }
      openxlsx::saveWorkbook(wb, paste0(table_basename, ".xlsx"), overwrite = TRUE)
      print(paste("The table has been created and is saved at location:",
                  normalizePath(paste0(table_basename, ".xlsx"))))
    },
    error = function(e) {
      print("Unfortunately, the export as *.xlsx did not work. The package openxlsx requires that a zip program is registered to R. Here is the original error message:")
      print(e)
      cat("\n")
    },
    finally = return(invisible(table)))
  } else {
    message("to save table in *.xlsx format, you need to run install.packages('openxlsx') !")
  }
}
