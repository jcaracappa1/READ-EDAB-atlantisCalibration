# File: edit_param_c_rate.R

#' @title Edit C (consumption) parameters for age-structured groups
#' @description Modifies the age-structured consumption rate parameters inside an Atlantis biology parameter file.
#' @param bio_prm String. Path to the Atlantis biology parameter file.
#' @param new_c_rate data.frame or numeric vector. New consumption rates to inject.
#' @param overwrite Logical. Should the original parameter file be overwritten?
#' @param new_file_name String. Path to the new file if overwrite is FALSE.
#' @param single_group Logical. Are we editing a single functional group?
#' @param group_name String. Name of the functional group if single_group is TRUE.
#' @return None. Writes modifications directly to the specified file.
#' @export
edit_param_c_rate <- function(bio_prm, new_c_rate, overwrite = FALSE, new_file_name = NULL, single_group = FALSE, group_name = NA) {
  if (!file.exists(bio_prm)) {
    stop("edit_param_c_rate: Biology parameter file not found.")
  }
  
  if (!overwrite && is.null(new_file_name)) {
    stop("edit_param_c_rate: new_file_name must be provided if overwrite is FALSE.")
  }
  
  bio_lines <- readLines(bio_prm)
  bio_lines_id <- grep("^C_", bio_lines)
  
  if (length(bio_lines_id) == 0) {
    stop("edit_param_c_rate: No C_ parameters found in biology file.")
  }
  
  bio_lines_vals <- bio_lines[bio_lines_id]
  which_invert <- grepl("_T15", bio_lines_vals)
  bio_lines_vals <- bio_lines_vals[!which_invert]
  bio_lines_id <- bio_lines_id[!which_invert]
  
  group_names <- unname(sapply(bio_lines_vals, function(x) strsplit(x, "C_|\t10.00|\t| ")[[1]][2]))
  max_age <- max(as.numeric(sapply(bio_lines_vals, function(x) strsplit(x, "C_|\t| ")[[1]][3])), na.rm = TRUE)
  
  if (single_group) {
    if (is.na(group_name) || !(group_name %in% group_names)) {
      stop(paste("edit_param_c_rate: group_name", group_name, "not found."))
    }
    ind <- which(group_name == group_names)
    new_c_rate_vec <- new_c_rate[!is.na(new_c_rate)]
    
    if (length(new_c_rate_vec) > max_age) {
      stop(paste("edit_param_c_rate: Input vector length for group", group_name, "exceeds maximum age cohorts."))
    }
    
    c_string <- paste(new_c_rate_vec, collapse = "\t")
    bio_lines[bio_lines_id[ind] + 1] <- c_string
  } else {
    for (i in 1:nrow(new_c_rate)) {
      if (!(new_c_rate$group[i] %in% group_names)) {
        stop(paste("edit_param_c_rate: Group", new_c_rate$group[i], "not found in biology file."))
      }
      ind <- which(group_names == new_c_rate$group[i])
      
      c_string <- new_c_rate[i, 2:ncol(new_c_rate)]
      which_na <- which(is.na(c_string))
      if (length(which_na) > 0) {
        c_string <- c_string[-which_na]
      }
      
      if (length(c_string) > max_age) {
        stop(paste("edit_param_c_rate: Input vector length for group", new_c_rate$group[i], "exceeds maximum age cohorts."))
      }
      
      c_string <- paste(c_string, collapse = "\t")
      bio_lines[bio_lines_id[ind] + 1] <- c_string
    }
  }
  
  if (overwrite) {
    writeLines(bio_lines, con = bio_prm)
  } else {
    file.copy(bio_prm, new_file_name, overwrite = TRUE)
    writeLines(bio_lines, con = new_file_name)
  }
}