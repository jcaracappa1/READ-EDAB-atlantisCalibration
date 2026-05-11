# get_param_bh.R

#' @title Get Beverton-Holt Parameters
#'
#' @description Extracts all Beverton-Holt Alpha and Beta parameter values from an Atlantis biology parameter file.
#'
#' @param bio_prm String. Path to the biology parameter file.
#'
#' @return A data frame containing 'group', 'alpha', and 'beta' numeric values.
#' @export
get_param_bh <- function(bio_prm) {
  
  if (!file.exists(bio_prm)) {
    stop(sprintf("[get_param_bh]: File %s not found.", bio_prm))
  }
  
  bio_lines <- readLines(bio_prm)
  
  alpha_line <- bio_lines[grep('^BHalpha_', bio_lines)]
  beta_line <- bio_lines[grep('^BHbeta_', bio_lines)]
  
  alpha_group <- sapply(alpha_line, function(x) strsplit(x, '_|[[:space:]]+')[[1]][2], USE.NAMES = FALSE)
  alpha_vals <- sapply(alpha_line, function(x) {
    dum <- strsplit(x, '_|[[:space:]]+')[[1]]
    # Extract the first non-empty element after the group name
    return(dum[2 + which(dum[-c(1:2)] != '')[1]])
  }, USE.NAMES = FALSE) 
  
  alpha_df <- data.frame(
    group = alpha_group,
    alpha = as.numeric(alpha_vals), # Explicit coercion
    stringsAsFactors = FALSE
  )
  
  beta_group <- sapply(beta_line, function(x) strsplit(x, '_|[[:space:]]+')[[1]][2], USE.NAMES = FALSE)
  beta_vals <- sapply(beta_line, function(x) {
    dum <- strsplit(x, '_|[[:space:]]+')[[1]]
    return(dum[2 + which(dum[-c(1:2)] != '')[1]])
  }, USE.NAMES = FALSE) 
  
  beta_df <- data.frame(
    group = beta_group,
    beta = as.numeric(beta_vals), # Explicit coercion
    stringsAsFactors = FALSE
  )
  
  out_df <- dplyr::left_join(alpha_df, beta_df, by = "group")
  
  return(out_df)
}