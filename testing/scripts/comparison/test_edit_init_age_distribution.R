# compare_old_new.R
# Placeholder inputs needing manual specification if files are missing:
# - data-raw/data/MenhadenBiomass.csv
# - data-raw/data/StripedBassBiomass.csv
# - data-raw/data/LobsterBiomass.csv
# - data/functionalGroupNames.csv
# - data-raw/StockSmart_Conversions.csv

library(dplyr)
library(ncdf4)
library(stocksmart)

# Set directories
input_dir <- "testing/inputs"
out_dir <- "testing/outputs"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Load legacy and new functions
source("https://raw.githubusercontent.com/NEFSC/READ-EDAB-neusAtlantis/refs/heads/dev_branch/R/Calibration_Tools/edit_init_age_distribution.R")
edit_init_age_distribution_old = edit_init_age_distribution
source("R/edit_init_age_distribution.R")


# Run old function wrapper (assuming parameters are properly scoped to dummy test files inside the repo context)
# bio.prm =  here::here('currentVersion','at_biology.prm')
# fgs.file = here::here('currentVersion','neus_groups.csv')
# init.file = here::here('currentVersion','neus_init.nc')
# ss.cat.file = here::here('data','StockSmart_Abundance_Units_Conversion.csv')
# ss.conv.file = here::here('data-raw','StockSmart_Conversions.csv')
# box.prop.file = here::here('diagnostics','Group_Box_Proportions.csv')
# peak.age = 3
# steepness = 1.5
# ref.run.dir = here::here('Atlantis_Runs','Dev_07282022','Post_Processed','Data/')
# prescribed.age.scale = T
# age.scale =  c(0.005,0.03,0.2,0.2,0.175,0.15,0.1,0.075,0.05,0.015)
# # init.size.age =  here::here('currentVersion','vertebrate_init_length_cm.csv')
# init.size.age = here::here('diagnostics','Initial_Size_Age.csv')
# ss.adj.abund.file =  here::here('diagnostics','StockSmart_Adjusted_Abundance.csv')
# overwrite = F
# new.init.file = here::here('currentVersion','neus_init_test.nc')

source("https://raw.githubusercontent.com/NEFSC/READ-EDAB-neusAtlantis/refs/heads/dev_branch/R/Calibration_Tools/edit_param_FSPB.R")
legacy_result <- edit_init_age_distribution_old(bio.prm = file.path(input_dir,'at_biology.prm'),
                                                fgs.file = file.path(input_dir, 'neus_groups.csv'),
                                                ss.conv.file= file.path(input_dir, 'StockSmart_Abundance_Units_Conversion.csv'),
                                                ss.cat.file = file.path(input_dir, 'StockSmart_Conversions.csv'),
                                                box.prop.file = file.path(input_dir, 'Group_Box_Proportions.csv'),
                                                peak.age =3,
                                                steepness=1.5,
                                                prescribed.age.scale = T,
                                                age.scale = c(0.005,0.03,0.2,0.2,0.175,0.15,0.1,0.075,0.05,0.015),
                                                ref.run.dir = paste0(input_dir,'/Data/'),
                                                init.size.age = paste0(input_dir, 'vertebrate_init_length_cm.csv'),
                                                ss.adj.abund.file = paste0(input_dir,'StockSmart_Adjusted_Abundance.csv'),
                                                groups = 'MAK',
                                                init.file= paste0(input_dir,'neus_init.csv'),
                                                overwrite +F,
                                                new.init.file = paste0(output_dir,'neus_init_old.nc')
)
 

# Run new function wrapper
# ... new_result <- edit_init_age_distribution(...)

# Compare outputs
# Ensure that the modified NetCDF file from the new output matches the old output mathematically
# all.equal(ncdf4::nc_open("old.nc"), ncdf4::nc_open("new.nc"))