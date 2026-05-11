# File: get_param_c_rate.R

#' @title Get C (consumption) parameters for age-structured groups
#' @description Retrieves age-structured consumption rate parameters from an Atlantis biology parameter file.
#' @param bio_prm String. Path to the Atlantis biology parameter file.
#' @param write_output Logical. Should the output be written to a CSV? Default is FALSE.
#' @param output_dir String. Directory to save the output CSV. Required if write_output is TRUE.
#' @param out_name String. Name of the output CSV file. Required if write_output is TRUE.
#' @return A data.frame containing the group names and their consumption rates per age class.
#' @export
get_param_c_rate <- function(bio_prm, write_output = FALSE, output_dir = NULL, out_name = NULL) {
  if (!file.exists(bio_prm)) {
    stop("get_param_c_rate: Biology parameter file not found.")
  }
  
  bio_lines <- readLines(bio_prm)
  bio_lines_id <- grep("^C_", bio_lines)
  
  if (length(bio_lines_id) == 0) {
    stop("get_param_c_rate: No C_ parameters found in biology file.")
  }
  
  bio_lines_vals1 <- bio_lines[bio_lines_id]
  which_invert <- grepl("_T15", bio_lines_vals1)
  bio_lines_vals1 <- bio_lines_vals1[!which_invert]
  bio_lines_id <- bio_lines_id[!which_invert]
  
  group_names <- unname(sapply(bio_lines_vals1, function(x) strsplit(x, "C_|\t10.00|\t| ")[[1]][2]))
  max_age <- max(as.numeric(sapply(bio_lines_vals1, function(x) strsplit(x, "C_|\t| ")[[1]][3])), na.rm = TRUE)
  
  c_mat <- matrix(NA, nrow = length(group_names), ncol = max_age)
  colnames(c_mat) <- paste0("C", 1:max_age)
  out_df <- data.frame(c_mat)
  out_df <- cbind(data.frame(group = group_names), out_df)
  
  for (i in seq_along(bio_lines_id)) {
    c_group <- bio_lines[bio_lines_id[i] + 1]
    c_split <- strsplit(c_group, split = "\t| |  ")[[1]]
    c_split <- c_split[c_split != ""] 
    
    c_out <- rep(NA, max_age)
    c_out[1:length(c_split)] <- as.numeric(c_split)
    out_df[i, 2:ncol(out_df)] <- c_out
  }
  
  if (write_output) {
    if (is.null(output_dir) || is.null(out_name)) {
      stop("get_param_c_rate: output_dir and out_name must be provided if write_output is TRUE.")
    }
    utils::write.csv(out_df, file = file.path(output_dir, paste0(out_name, ".csv")), row.names = FALSE)
  } else {
    return(out_df)
  }
}