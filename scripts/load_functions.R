#############################################################################################

#                                      MIT License

#    Copyright (c) [YEAR] [YOUR NAME / ORGANIZATION]

#    Permission is hereby granted, free of charge, to any person obtaining a copy
#    of this software and associated documentation files (the "Software"), to deal
#    in the Software without restriction, including without limitation the rights
#    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#    copies of the Software, and to permit persons to whom the Software is
#    furnished to do so, subject to the following conditions:

#    The above copyright notice and this permission notice shall be included in all
#    copies or substantial portions of the Software.

#    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#    SOFTWARE.

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
