# edit_param_bh.R

#' @title Edit Beverton-Holt Parameters
#'
#' @description Edits Beverton-Holt Alpha and Beta parameters for specified functional groups in an Atlantis biology parameter file.
#'
#' @param bio_prm String. Path to the biology parameter file.
#' @param group_name Character vector. Atlantis functional group codes (e.g., c('MAK', 'HER')).
#' @param alpha Numeric vector. Alpha parameter values to insert. NA to skip.
#' @param beta Numeric vector. Beta parameter values to insert. NA to skip.
#' @param overwrite Logical. Whether to overwrite the existing file.
#' @param new_file_name String. Path to the new output parameter file if overwrite is FALSE.
#'
#' @return Modifies the specified .prm file. Returns nothing.
#' @export
edit_param_bh <- function(bio_prm, group_name, alpha = NA, beta = NA, overwrite = FALSE, new_file_name = NULL) {
  
  # Validation
  if (!file.exists(bio_prm)) {
    stop(sprintf("[edit_param_bh]: File %s not found.", bio_prm))
  }
  if (!overwrite && is.null(new_file_name)) {
    stop("[edit_param_bh]: new_file_name must be provided if overwrite is FALSE.")
  }
  if (!all(is.na(alpha)) && length(group_name) != length(alpha)) {
    stop("[edit_param_bh]: Length of 'alpha' vector must match length of 'group_name'.")
  }
  if (!all(is.na(beta)) && length(group_name) != length(beta)) {
    stop("[edit_param_bh]: Length of 'beta' vector must match length of 'group_name'.")
  }
  
  bio_lines <- readLines(bio_prm)
  
  for (i in seq_along(group_name)) {
    
    alpha_line_idx <- grep(paste0('^BHalpha_', group_name[i], '\\b'), bio_lines)
    beta_line_idx <- grep(paste0('^BHbeta_', group_name[i], '\\b'), bio_lines)
    
    if (length(alpha_line_idx) == 0) {
      stop(sprintf("[edit_param_bh]: Parameter BHalpha_%s not found in file.", group_name[i]))
    }
    if (length(beta_line_idx) == 0) {
      stop(sprintf("[edit_param_bh]: Parameter BHbeta_%s not found in file.", group_name[i]))
    }
    
    if (!all(is.na(alpha)) && !is.na(alpha[i])) {
      new_alpha <- paste0('BHalpha_', group_name[i], '\t', alpha[i])
      bio_lines[alpha_line_idx] <- new_alpha
    }
    
    if (!all(is.na(beta)) && !is.na(beta[i])) {
      new_beta <- paste0('BHbeta_', group_name[i], '\t', beta[i])  
      bio_lines[beta_line_idx] <- new_beta
    }
  }
  
  if (overwrite) {
    writeLines(bio_lines, con = bio_prm)
  } else {
    file.copy(bio_prm, new_file_name, overwrite = TRUE)
    writeLines(bio_lines, con = new_file_name)
  }
}