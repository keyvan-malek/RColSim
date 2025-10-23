#!/usr/bin/env Rscript

# Simple Cyclomatic Complexity Analysis for RColSim
# This script analyzes the cyclomatic complexity using a custom approach

library(stringr)

# Function to count control flow statements in R code
count_control_flow <- function(code_lines) {
  # Remove comments and empty lines
  code_lines <- code_lines[!grepl("^\\s*#", code_lines)]
  code_lines <- code_lines[!grepl("^\\s*$", code_lines)]
  
  # Count various control flow constructs
  if_count <- sum(str_count(code_lines, "\\bif\\s*\\("))
  else_count <- sum(str_count(code_lines, "\\belse\\b"))
  for_count <- sum(str_count(code_lines, "\\bfor\\s*\\("))
  while_count <- sum(str_count(code_lines, "\\bwhile\\s*\\("))
  repeat_count <- sum(str_count(code_lines, "\\brepeat\\b"))
  break_count <- sum(str_count(code_lines, "\\bbreak\\b"))
  next_count <- sum(str_count(code_lines, "\\bnext\\b"))
  switch_count <- sum(str_count(code_lines, "\\bswitch\\s*\\("))
  try_catch_count <- sum(str_count(code_lines, "\\btryCatch\\s*\\("))
  
  # Count logical operators that increase complexity
  and_count <- sum(str_count(code_lines, "\\&\\&"))
  or_count <- sum(str_count(code_lines, "\\|\\|"))
  
  # Calculate cyclomatic complexity
  # Base complexity is 1, then add for each decision point
  complexity <- 1 + if_count + for_count + while_count + repeat_count + 
                switch_count + and_count + or_count + try_catch_count
  
  return(complexity)
}

# Function to extract functions from R code
extract_functions <- function(file_path) {
  tryCatch({
    code <- readLines(file_path, warn = FALSE)
    
    functions <- list()
    
    # Find function definitions
    func_pattern <- "^\\s*(\\w+)\\s*<-\\s*function\\s*\\("
    func_matches <- grep(func_pattern, code, value = TRUE)
    
    for (match in func_matches) {
      func_name <- str_extract(match, "^\\s*(\\w+)\\s*<-\\s*function")
      func_name <- str_extract(func_name, "\\w+")
      
      # Find the start of the function
      func_start <- grep(paste0("^\\s*", func_name, "\\s*<-\\s*function\\s*\\("), code)[1]
      
      if (!is.na(func_start)) {
        # Find the end of the function (simplified approach)
        brace_count <- 0
        func_end <- func_start
        
        for (i in func_start:length(code)) {
          line <- code[i]
          
          # Count opening braces
          open_braces <- str_count(line, "\\{")
          close_braces <- str_count(line, "\\}")
          
          brace_count <- brace_count + open_braces - close_braces
          
          if (brace_count == 0 && i > func_start) {
            func_end <- i
            break
          }
        }
        
        # Extract function code
        func_code <- code[func_start:func_end]
        
        # Calculate complexity for this function
        complexity <- count_control_flow(func_code)
        
        functions[[func_name]] <- list(
          name = func_name,
          complexity = complexity,
          start_line = func_start,
          end_line = func_end
        )
      }
    }
    
    return(functions)
    
  }, error = function(e) {
    cat("Error processing file:", file_path, "-", e$message, "\n")
    return(list())
  })
}

# Function to analyze file complexity
analyze_file <- function(file_path) {
  tryCatch({
    code <- readLines(file_path, warn = FALSE)
    
    # Get relative path for display
    rel_path <- gsub(paste0(getwd(), "/"), "", file_path)
    
    # Extract functions
    functions <- extract_functions(file_path)
    
    # Calculate file-level complexity
    file_complexity <- count_control_flow(code)
    
    # Calculate function-level metrics
    total_func_complexity <- sum(sapply(functions, function(f) f$complexity))
    num_functions <- length(functions)
    avg_func_complexity <- if (num_functions > 0) total_func_complexity / num_functions else 0
    max_func_complexity <- if (num_functions > 0) max(sapply(functions, function(f) f$complexity)) else 0
    
    return(list(
      file_path = rel_path,
      file_complexity = file_complexity,
      num_functions = num_functions,
      total_func_complexity = total_func_complexity,
      avg_func_complexity = round(avg_func_complexity, 2),
      max_func_complexity = max_func_complexity,
      functions = functions
    ))
    
  }, error = function(e) {
    cat("Error analyzing file:", file_path, "-", e$message, "\n")
    return(list(
      file_path = gsub(paste0(getwd(), "/"), "", file_path),
      file_complexity = 0,
      num_functions = 0,
      total_func_complexity = 0,
      avg_func_complexity = 0,
      max_func_complexity = 0,
      functions = list(),
      error = e$message
    ))
  })
}

# Main analysis function
main <- function() {
  cat("Starting Simple Cyclomatic Complexity Analysis for RColSim...\n")
  cat("============================================================\n\n")
  
  # Get all R files
  r_files <- list.files(".", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  r_files <- r_files[!grepl("__MACOSX|\\._", r_files)]
  
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
    result <- analyze_file(file)
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
    File_Complexity = sapply(all_results, function(x) x$file_complexity),
    Num_Functions = sapply(all_results, function(x) x$num_functions),
    Total_Func_Complexity = sapply(all_results, function(x) x$total_func_complexity),
    Avg_Func_Complexity = sapply(all_results, function(x) x$avg_func_complexity),
    Max_Func_Complexity = sapply(all_results, function(x) x$max_func_complexity),
    stringsAsFactors = FALSE
  )
  
  # Sort by file complexity (descending)
  if (nrow(file_summary) > 0) {
    file_summary <- file_summary[order(file_summary$File_Complexity, decreasing = TRUE), ]
  }
  
  function_summary <- data.frame(
    File = sapply(all_functions, function(x) x$name),
    Function = sapply(all_functions, function(x) x$name),
    Complexity = sapply(all_functions, function(x) x$complexity),
    Start_Line = sapply(all_functions, function(x) x$start_line),
    stringsAsFactors = FALSE
  )
  
  # Sort by complexity (descending)
  if (nrow(function_summary) > 0) {
    function_summary <- function_summary[order(function_summary$Complexity, decreasing = TRUE), ]
  }
  
  # Print summary statistics
  cat("\n============================================================\n")
  cat("SUMMARY STATISTICS\n")
  cat("============================================================\n")
  cat("Total Files Analyzed:", length(all_results), "\n")
  cat("Total Functions Found:", length(all_functions), "\n")
  
  if (length(all_results) > 0) {
    cat("Total File Complexity:", sum(file_summary$File_Complexity), "\n")
    cat("Average File Complexity:", round(mean(file_summary$File_Complexity), 2), "\n")
    cat("Average Function Complexity:", round(mean(function_summary$Complexity), 2), "\n")
    cat("Highest Function Complexity:", max(function_summary$Complexity), "\n")
  } else {
    cat("No files were successfully analyzed.\n")
  }
  
  # Identify high complexity files and functions
  high_complexity_files <- file_summary[file_summary$File_Complexity > 30, ]
  high_complexity_functions <- function_summary[function_summary$Complexity > 10, ]
  
  cat("\n============================================================\n")
  cat("HIGH COMPLEXITY FILES (File Complexity > 30)\n")
  cat("============================================================\n")
  if (nrow(high_complexity_files) > 0) {
    print(high_complexity_files)
  } else {
    cat("No files with complexity > 30\n")
  }
  
  cat("\n============================================================\n")
  cat("HIGH COMPLEXITY FUNCTIONS (Complexity > 10)\n")
  cat("============================================================\n")
  if (nrow(high_complexity_functions) > 0) {
    print(head(high_complexity_functions, 20))  # Show top 20
    if (nrow(high_complexity_functions) > 20) {
      cat("... and", nrow(high_complexity_functions) - 20, "more functions\n")
    }
  } else {
    cat("No functions with complexity > 10\n")
  }
  
  # Save detailed results to CSV files
  write.csv(file_summary, "tests/simple_complexity_by_file.csv", row.names = FALSE)
  write.csv(function_summary, "tests/simple_complexity_by_function.csv", row.names = FALSE)
  
  cat("\n============================================================\n")
  cat("DETAILED RESULTS SAVED TO:\n")
  cat("- simple_complexity_by_file.csv\n")
  cat("- simple_complexity_by_function.csv\n")
  cat("============================================================\n")
  
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
