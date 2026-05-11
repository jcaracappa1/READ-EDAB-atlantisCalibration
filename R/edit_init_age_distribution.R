# edit_init_age_distribution.R

#' @title Edit Initial Age Distribution
#'
#' @description Modifies the initial age distribution and abundance for functional groups in the Atlantis initialization NetCDF file based on StockSmart data, box proportions, and other reference inputs.
#'
#' @param bio_prm String. Path to the biology parameter file.
#' @param fgs_file String. Path to the functional groups CSV file.
#' @param ss_conv_file String. Path to StockSmart conversions CSV file.
#' @param ss_cat_file String. Path to StockSmart units conversion CSV file.
#' @param box_prop_file String. Path to the group box proportions CSV file.
#' @param peak_age Numeric. Peak age for calculating age distribution.
#' @param steepness Numeric. Steepness parameter for age distribution.
#' @param prescribed_age_scale Logical. Whether to use a prescribed age scale.
#' @param age_scale Numeric vector. Optional prescribed age scale.
#' @param ref_run_dir String. Path to the reference run directory containing biomass outputs.
#' @param init_size_age String. Path to the initial size at age CSV output.
#' @param ss_adj_abund_file String. Path to output the adjusted abundance CSV.
#' @param groups Character vector. Optional subset of functional groups to edit.
#' @param init_file String. Path to the initial condition NetCDF file.
#' @param overwrite Logical. Whether to overwrite the original initialization file.
#' @param new_init_file String. Path for the new initialization NetCDF file if overwrite is FALSE.
#'
#' @return Modifies the specified NetCDF file and writes intermediate CSV files. Returns nothing.
#' @export
edit_init_age_distribution <- function(bio_prm,
                                       fgs_file,
                                       ss_conv_file,
                                       ss_cat_file,
                                       box_prop_file,
                                       peak_age,
                                       steepness,
                                       prescribed_age_scale = FALSE,
                                       age_scale = NA,
                                       ref_run_dir,
                                       init_size_age,
                                       ss_adj_abund_file,
                                       groups = NA,
                                       init_file,
                                       overwrite,
                                       new_init_file = NULL) {
  
  warning("Helper functions get_param_fspb, make_age_distribution, make_size_age_reference, make_init_size_age, edit_param_init_scalar must be refactored/written to ensure the package is functional.")
  
  # Validate
  if (!file.exists(init_file)) {
    stop(sprintf("[edit_init_age_distribution]: File %s not found.", init_file))
  }
  if (!file.exists(bio_prm)) {
    stop(sprintf("[edit_init_age_distribution]: Biology parameter file %s not found.", bio_prm))
  }
  
  # Prepare
  if (overwrite == FALSE && is.null(new_init_file)) {
    stop("[edit_init_age_distribution]: new_init_file must be provided if overwrite is FALSE.")
  }
  
  # Read in FSPB 
  fspb <- get_param_fspb(bio_prm)
  
  # Groups file
  fgs <- utils::read.csv(fgs_file) %>%
    dplyr::select(LongName, Code, Name, NumCohorts)
  
  # StockSmart unit conversions
  ss_unit_cases <- utils::read.csv(ss_cat_file)
  ss_unit_conv <- utils::read.csv(ss_conv_file)
  
  # Box proportions
  box_props <- utils::read.csv(box_prop_file)
  
  # External raw data reads
  men_dat <- utils::read.csv(file.path("data-raw", "data", "MenhadenBiomass.csv"), as.is = TRUE) %>%
    dplyr::rename(Value = "Biomass") %>%
    dplyr::mutate(Code = "MEN")
  
  stb_dat <- utils::read.csv(file.path("data-raw", "data", "StripedBassBiomass.csv"), as.is = TRUE) %>%
    dplyr::rename(Value = "Biomass") %>%
    dplyr::mutate(Code = "STB")
  
  lob_dat <- utils::read.csv(file.path("data-raw", "data", "LobsterBiomass.csv"), as.is = TRUE) %>%
    dplyr::rename(Value = "Biomass") %>%
    dplyr::mutate(Code = "LOB")
  
  non_ss_all <- dplyr::bind_rows(men_dat, stb_dat, lob_dat)
  
  #### Flag for predefined size at age ####
  if (all(is.na(age_scale))) {
    age_scale <- make_age_distribution(peak_age, steepness)
  }
  
  # Get initial biomass in mT
  init_biomass_age <- readRDS(file.path(ref_run_dir, "biomass_age.rds")) %>%
    dplyr::filter(time == 0) %>%
    dplyr::select(-time) %>%
    dplyr::rename(biomass_mt = "atoutput")
  
  init_biomass_invert <- readRDS(file.path(ref_run_dir, "biomass_age_invert.rds")) %>%
    dplyr::filter(time == 0) %>%
    dplyr::mutate(agecl = 1) %>%
    dplyr::select(-time) %>%
    dplyr::rename(biomass_mt = "atoutput")
  
  init_biomass_all <- dplyr::bind_rows(init_biomass_age, init_biomass_invert) %>%
    dplyr::left_join(fgs, by = c("species" = "LongName"))
  
  # Get initial RN and SN for age groups
  make_init_size_age(init_file = init_file, fgs_file = fgs_file, out_file = init_size_age)
  init_rn_sn <- utils::read.csv(init_size_age) %>%
    dplyr::left_join(fgs, by = "Code") %>%
    dplyr::mutate(tot_n = rn + sn,
                  ratio = rn / sn)
  
  ss_neus <- readr::read_csv(file.path("data", "functionalGroupNames.csv"))
  
  ss_raw <- stocksmart::stockAssessmentData %>%
    dplyr::filter(Metric == "Abundance" & RegionalEcosystem %in% c("Northeast Shelf", "Atlantic Highly Migratory"))
  
  data_no_ss <- ss_unit_cases %>%
    dplyr::filter(in.stocksmart == FALSE) %>%
    dplyr::left_join(ss_neus, by = "Code") %>%
    dplyr::left_join(non_ss_all, by = "Code")
  
  data_ss <- ss_unit_cases %>%
    dplyr::filter(in.stocksmart == TRUE) %>%
    dplyr::left_join(ss_neus, by = "Code") %>%
    dplyr::left_join(ss_raw, by = "Code") %>%
    dplyr::bind_rows(data_no_ss) %>%
    dplyr::filter(Year >= 1990) %>%
    dplyr::group_by(Code, Units, AssessmentYear, Case, min.cohort, sex.ratio.mf) %>%
    dplyr::summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(Case %in% c("Total", "SSB", "Adult", "Female")) %>%
    dplyr::left_join(ss_unit_conv, by = "Units") %>%
    dplyr::mutate(Value_new = Value * conversion)
  
  data_ss$numbers <- NA
  data_ss$biomass_tot <- NA
  
  for (i in 1:nrow(data_ss)) {
    # Convert different cases to biomass (except for numbers)
    if (fgs$NumCohorts[which(fgs$Code == data_ss$Code[i])] == 1) {
      data_ss$biomass_tot[i] <- data_ss$Value_new[i]
      next()
    }
    
    if (data_ss$Case[i] == "Total" && data_ss$new.units[i] == "mt") {
      data_ss$biomass_tot[i] <- data_ss$Value_new[i]
    } else if (data_ss$Case[i] == "SSB") {
      
      fspb_group <- fspb %>%
        dplyr::filter(group == data_ss$Code[i]) %>%
        reshape2::melt(id.vars = "group", value.name = "fspb") %>%
        tidyr::separate(variable, c("dum", "agecl"), sep = "_") %>%
        dplyr::mutate(agecl = as.numeric(agecl),
                      fspb = as.numeric(fspb))
      
      init_bio_group <- init_biomass_all %>%
        dplyr::filter(Code == data_ss$Code[i]) %>%
        dplyr::left_join(fspb_group, by = c("Code" = "group", "agecl")) %>%
        dplyr::mutate(ssb = biomass_mt * fspb)
      
      init_bio_group_tot <- init_bio_group %>%
        dplyr::group_by(species) %>%
        dplyr::summarise(biomass_mt_tot = sum(biomass_mt, na.rm = TRUE),
                         ssb_tot = sum(ssb, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(ssb_prop = biomass_mt_tot / ssb_tot)
      
      data_ss$biomass_tot[i] <- data_ss$Value_new[i] * init_bio_group_tot$ssb_prop[1]
      
    } else if (data_ss$Case[i] == "Adult") {
      
      init_bio_group <- init_biomass_all %>%
        dplyr::filter(Code == data_ss$Code[i])
      
      init_bio_age <- init_bio_group %>%
        dplyr::filter(agecl >= data_ss$min.cohort[i]) %>%
        dplyr::group_by(Code) %>%
        dplyr::summarise(biomass_mt_age_tot = sum(biomass_mt, na.rm = TRUE), .groups = "drop")
      
      init_bio_age_prop <- init_bio_group %>%
        dplyr::group_by(Code) %>%
        dplyr::summarise(biomass_mt_tot = sum(biomass_mt, na.rm = TRUE), .groups = "drop") %>%
        dplyr::left_join(init_bio_age, by = "Code") %>%
        dplyr::mutate(biomass_age_prop = biomass_mt_tot / biomass_mt_age_tot)
      
      data_ss$biomass_tot[i] <- data_ss$Value_new[i] * init_bio_age_prop$biomass_age_prop[1]
      
    } else if (data_ss$Case[i] == "Female") {
      data_ss$biomass_tot[i] <- data_ss$Value_new[i] * 2
    } else if (data_ss$Case[i] == "Total" && data_ss$new.units[i] == "num") {
      data_ss$numbers[i] <- data_ss$Value_new[i]
    }
  }
  
  # Clean up SS data reference table
  data_ss_final <- data_ss %>%
    dplyr::group_by(Code) %>%
    dplyr::summarise(numbers = mean(numbers, na.rm = TRUE),
                     biomass_tot = mean(biomass_tot, na.rm = TRUE), .groups = "drop")
  
  data_combined <- data_ss_final %>%
    dplyr::left_join(fgs, by = "Code")
  
  ## Separate by group type
  data_invert <- data_combined %>% 
    dplyr::filter(NumCohorts == 1) %>%
    dplyr::mutate(biomass_n = biomass_tot * 1E9 / 20 / 5.7)
  
  data_invert_age <- data_combined %>% 
    dplyr::filter(NumCohorts == 2)
  
  data_age <- data_combined %>%
    dplyr::filter(NumCohorts == 10) %>%
    dplyr::mutate(biomass_n = biomass_tot * 1E9 / 20 / 5.7)
  
  if (overwrite == TRUE) {
    init_nc <- ncdf4::nc_open(init_file)  
  } else {
    file.copy(init_file, new_init_file, overwrite = TRUE)
    init_nc <- ncdf4::nc_open(new_init_file)
  }
  
  varnames <- names(init_nc$var)
  
  ref_data_ls <- list()
  for (i in 1:nrow(data_age)) {
    
    ind_n <- init_rn_sn %>% dplyr::filter(LongName == data_age$LongName[i]) 
    ind_n_age <- ind_n$tot_n
    
    # Get prop at age
    dat_age <- numeric()
    for (j in 1:10) {
      group_name <- grep(paste0("^", data_age$Name[i], j, "_Nums$"), varnames, value = TRUE)
      if (length(group_name) > 0) {
        dat_age[j] <- sum(ncdf4::ncvar_get(init_nc, group_name)[1, ], na.rm = TRUE)
      }
    }
    
    if (prescribed_age_scale == TRUE) {
      dat_age_prop <- age_scale
    } else {
      dat_age_prop <- dat_age / sum(dat_age)
    }
    
    if (is.na(data_age$biomass_n[i])) {
      ref_nums <- round(data_age$numbers[i] * dat_age_prop)
    } else {
      ref_nums <- round((data_age$biomass_n[i] / ind_n_age) * dat_age_prop)  
    }
    
    ref_data_ls[[i]] <- data.frame(Code = data_age$Code[i], agecl = 1:10, ref_nums = ref_nums)
  }
  
  ncdf4::nc_close(init_nc)
  ref_num_age <- dplyr::bind_rows(ref_data_ls)
  
  if (any(is.na(groups))) {
    age_groups <- unique(ref_num_age$Code)  
  } else {
    age_groups <- groups
  }
  
  num_box_age_all_ls <- list()
  for (i in seq_along(age_groups)) {
    group_num <- ref_num_age %>%
      dplyr::filter(Code == age_groups[i])
    group_box_prop <- box_props[, as.character(age_groups[i])]
    
    num_box_age_group <- as.data.frame(group_num$ref_nums %*% t(group_box_prop))
    colnames(num_box_age_group) <- 0:29
    num_box_age_group$agecl <- 1:10
    num_box_age_all_ls[[i]] <- num_box_age_group %>%
      tidyr::gather(key = "box", value = "measurement", -agecl) %>%
      dplyr::mutate(Code = age_groups[i])
  }
  
  num_box_age_all <- dplyr::bind_rows(num_box_age_all_ls)
  
  # Reset Init Scalar to 1 for changed groups
  init_scalar <- get_param_init_scalar(run_prm = file.path("currentVersion", "at_run.prm"),
                                       groups_file = file.path("currentVersion", "neus_groups.csv"),
                                       write_output = FALSE)
  new_init_scalar <- init_scalar
  for (i in seq_along(age_groups)) {
    new_init_scalar$init_scalar[which(as.character(new_init_scalar$group) == as.character(age_groups[i]))] <- 1 
  }
  
  new_init_scalar$init_scalar <- as.numeric(as.character(new_init_scalar$init_scalar))
  edit_param_init_scalar(run_prm = file.path("currentVersion", "at_run.prm"),
                         groups_file = file.path("currentVersion", "neus_groups.csv"),
                         new_init_scalar = new_init_scalar,
                         overwrite = TRUE)
  
  # Write new box-age numbers values to init_nc
  if (overwrite == TRUE) {
    init_nc <- ncdf4::nc_open(init_file, write = TRUE)
  } else {
    init_nc <- ncdf4::nc_open(new_init_file, write = TRUE)
  }
  
  varnames <- names(init_nc$var)
  
  for (i in seq_along(age_groups)) {
    for (j in 1:10) {
      num_age_group <- num_box_age_all %>%
        dplyr::filter(Code == age_groups[i], agecl == j) %>%
        dplyr::left_join(fgs, by = "Code")
      
      nc_name <- grep(paste0("^", num_age_group$Name[1], j, "_Nums$"), varnames, value = TRUE)
      
      if (length(nc_name) > 0) {
        new_init <- matrix(NA, nrow = 5, ncol = 30)
        new_init[1, ] <- num_age_group$measurement
        ncdf4::ncvar_put(nc = init_nc, varid = nc_name, vals = new_init)
      }
    }
  }
  
  ncdf4::nc_close(init_nc)
}