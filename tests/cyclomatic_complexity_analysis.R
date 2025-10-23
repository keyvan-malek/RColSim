#!/usr/bin/env Rscript

# Cyclomatic Complexity Analysis for RColSim
# This script analyzes the cyclomatic complexity of all R files in the project

library(cyclocomp)
library(DT)
library(knitr)

# Function to get all R files in the project
get_r_files <- function(root_dir) {
  r_files <- list.files(root_dir, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  # Filter out files in __MACOSX and other system directories
  r_files <- r_files[!grepl("__MACOSX|\\._", r_files)]
  return(r_files)
}

# Function to analyze cyclomatic complexity for a single file
analyze_file_complexity <- function(file_path) {
  tryCatch({
    # Get relative path for display
    rel_path <- gsub(paste0(getwd(), "/"), "", file_path)
    
    # Calculate cyclomatic complexity
    complexity_result <- cyclocomp(file_path)
    
    # Extract function information
    functions_info <- list()
    
    if (length(complexity_result) > 0) {
      for (i in seq_along(complexity_result)) {
        func_name <- names(complexity_result)[i]
        func_complexity <- complexity_result[[i]]
        
        functions_info[[func_name]] <- list(
          name = func_name,
          complexity = func_complexity,
          file = rel_path
        )
      }
    }
    
    # Calculate file-level metrics
    total_complexity <- sum(complexity_result)
    num_functions <- length(complexity_result)
    avg_complexity <- if (num_functions > 0) total_complexity / num_functions else 0
    max_complexity <- if (num_functions > 0) max(complexity_result) else 0
    
    return(list(
      file_path = rel_path,
      total_complexity = total_complexity,
      num_functions = num_functions,
      avg_complexity = round(avg_complexity, 2),
      max_complexity = max_complexity,
      functions = functions_info
    ))
    
  }, error = function(e) {
    cat("Error analyzing file:", file_path, "-", e$message, "\n")
    return(list(
      file_path = gsub(paste0(getwd(), "/"), "", file_path),
      total_complexity = 0,
      num_functions = 0,
      avg_complexity = 0,
      max_complexity = 0,
      functions = list(),
      error = e$message
    ))
  })
}

# Main analysis function
main <- function() {
  cat("Starting Cyclomatic Complexity Analysis for RColSim...\n")
  cat("=====================================================\n\n")
  
  # Get all R files
  r_files <- get_r_files(".")
  cat("Found", length(r_files), "R files to analyze\n")
  if (length(r_files) > 0) {
    cat("Files found:\n")
    for (i in 1:min(5, length(r_files))) {
      cat("  -", r_files[i], "\n")
    }
    if (length(r_files) > 5) {
      cat("  ... and", length(r_files) - 5, "more\n")
    }
  }
  cat("\n")
  
  # Analyze each file
  all_results <- list()
  all_functions <- list()
  
  for (file in r_files) {
    cat("Analyzing:", file, "\n")
    result <- analyze_file_complexity(file)
    all_results[[length(all_results) + 1]] <- result
    
    # Collect all functions for global analysis
    if (length(result$functions) > 0) {
      for (func in result$functions) {
        all_functions[[length(all_functions) + 1]] <- func
      }
    }
  }
  
  # Create summary data frames
  file_summary <- data.frame(
    File = sapply(all_results, function(x) x$file_path),
    Total_Complexity = sapply(all_results, function(x) x$total_complexity),
    Num_Functions = sapply(all_results, function(x) x$num_functions),
    Avg_Complexity = sapply(all_results, function(x) x$avg_complexity),
    Max_Complexity = sapply(all_results, function(x) x$max_complexity),
    stringsAsFactors = FALSE
  )
  
  # Sort by total complexity (descending) - only if we have results
  if (nrow(file_summary) > 0) {
    file_summary <- file_summary[order(file_summary$Total_Complexity, decreasing = TRUE), ]
  }
  
  function_summary <- data.frame(
    File = sapply(all_functions, function(x) x$file),
    Function = sapply(all_functions, function(x) x$name),
    Complexity = sapply(all_functions, function(x) x$complexity),
    stringsAsFactors = FALSE
  )
  
  # Sort by complexity (descending) - only if we have results
  if (nrow(function_summary) > 0) {
    function_summary <- function_summary[order(function_summary$Complexity, decreasing = TRUE), ]
  }
  
  # Print summary statistics
  cat("\n=====================================================\n")
  cat("SUMMARY STATISTICS\n")
  cat("=====================================================\n")
  cat("Total Files Analyzed:", length(all_results), "\n")
  cat("Total Functions Found:", length(all_functions), "\n")
  
  if (length(all_results) > 0) {
    cat("Total Project Complexity:", sum(file_summary$Total_Complexity), "\n")
    cat("Average Complexity per File:", round(mean(file_summary$Total_Complexity), 2), "\n")
    if (length(all_functions) > 0) {
      cat("Average Complexity per Function:", round(mean(function_summary$Complexity), 2), "\n")
      cat("Highest Function Complexity:", max(function_summary$Complexity), "\n")
    } else {
      cat("Average Complexity per Function: N/A (no functions found)\n")
      cat("Highest Function Complexity: N/A (no functions found)\n")
    }
  } else {
    cat("No files were successfully analyzed.\n")
  }
  
  # Identify high complexity files and functions
  high_complexity_files <- file_summary[file_summary$Total_Complexity > 50, ]
  high_complexity_functions <- function_summary[function_summary$Complexity > 10, ]
  
  cat("\n=====================================================\n")
  cat("HIGH COMPLEXITY FILES (Total Complexity > 50)\n")
  cat("=====================================================\n")
  if (nrow(high_complexity_files) > 0) {
    print(high_complexity_files)
  } else {
    cat("No files with total complexity > 50\n")
  }
  
  cat("\n=====================================================\n")
  cat("HIGH COMPLEXITY FUNCTIONS (Complexity > 10)\n")
  cat("=====================================================\n")
  if (nrow(high_complexity_functions) > 0) {
    print(head(high_complexity_functions, 20))  # Show top 20
    if (nrow(high_complexity_functions) > 20) {
      cat("... and", nrow(high_complexity_functions) - 20, "more functions\n")
    }
  } else {
    cat("No functions with complexity > 10\n")
  }
  
  # Save detailed results to CSV files
  write.csv(file_summary, "tests/cyclomatic_complexity_by_file.csv", row.names = FALSE)
  write.csv(function_summary, "tests/cyclomatic_complexity_by_function.csv", row.names = FALSE)
  
  cat("\n=====================================================\n")
  cat("DETAILED RESULTS SAVED TO:\n")
  cat("- cyclomatic_complexity_by_file.csv\n")
  cat("- cyclomatic_complexity_by_function.csv\n")
  cat("=====================================================\n")
  
  # Return results for further analysis
  return(list(
    file_summary = file_summary,
    function_summary = function_summary,
    all_results = all_results,
    all_functions = all_functions
  ))
}

# Run the analysis
if (interactive()) {
  results <- main()
} else {
  results <- main()
}
