#############################################################################################

#                    		GNU GENERAL PUBLIC LICENSE
#                      		Version 3, 29 June 2007

# 	Copyright (C) 2007 Free Software Foundation, Inc. <https://fsf.org/>
# 	Everyone is permitted to copy and distribute verbatim copies
# 	of this license document, but changing it is not allowed.

#############################################################################################

# Modular Load Functions - Sources individual dam files
# This replaces the monolithic load_functions.R with a modular approach

# Load constants first
source("scripts/dams/constants.R")

# Load all dam files in alphabetical order
dam_files <- list.files("scripts/dams", pattern = "_dam\\.R$", full.names = TRUE)
dam_files <- sort(dam_files)

# Source each dam file
for (dam_file in dam_files) {
  if (dam_file != "scripts/dams/constants.R") {  # Skip constants as it's already loaded
    source(dam_file)
  }
}

cat("Loaded", length(dam_files), "dam files successfully\n")
