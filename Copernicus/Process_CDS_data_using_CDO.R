# Create the output directory, suppressing warnings if it already exists
dir.create("./out", showWarnings = FALSE)

# Clear the workspace and run garbage collection
rm(list = ls()); gc()

# Define project parameters
Project <- "CMIP5"
Period <- "historical" # "historical", "RCP85"
Var <- "sfcWind"
Domain <- c(9, 21, 47, 54) # Longitude and Latitude bounds

# Read model names
List_of_models <- read.table("../Inputs/model_names.csv", header = TRUE, sep = ",")

# Flag to unzip files if needed
unzip_files <- FALSE

# Function to remove objects and run garbage collection
rm_gc <- function(x) {
  rm(x)
  gc()
}

# Unzip the downloaded files if needed
if (unzip_files) {
  Files <- list.files(path = paste0(Project, "/", Period, "/", Var), pattern = "\\.zip$")
  
  for (file in Files) {
    system(paste("unzip", paste0(Project, "/", Period, "/", Var, "/", file), "-d", paste0(Project, "/", Period, "/", Var)))
  }
}

# Extract models within project from the list of names
ID <- which(List_of_models$Project == Project)
Model <- List_of_models$Model[ID]

# Create an empty data.frame with 12 rows and columns corresponding to the models, filled with NA
Data_out <- as.data.frame(matrix(NA, nrow = 12, ncol = length(Model)))
colnames(Data_out) <- List_of_models$Abbreviation[ID]

# Loop through the models
for (M in 1:length(Model)) {
  # Define file pattern
  file_pattern <- paste0(gsub("-", "[-]?", Model[M]), ".*\\.nc$")
  
  # List files matching the pattern
  Files <- list.files(
    path = paste0(Project, "/", Period, "/", Var),
    pattern = file_pattern,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(Files) > 0) {
    # Merge files across time
    merged_file <- paste0("merged_", Model[M], ".nc")
    system(paste("cdo mergetime", paste(Files, collapse = " "), merged_file))
    
    # Select the time period
    selected_file <- paste0("selected_", Model[M], ".nc")
    if (Period == "historical") {
      system(paste("cdo seldate,1981-01-01,2005-12-31", merged_file, selected_file))
    } else if (Period == "RCP85") {
      system(paste("cdo seldate,2076-01-01,2100-12-31", merged_file, selected_file))
    }
    
    # Select the spatial region
    spatial_file <- paste0("spatial_", Model[M], ".nc")

    # Properly format the sellonlatbox coordinates
    domain_coords <- paste(Domain, collapse = ",")

    # Create the full command with no spaces in the operator
    cmd <- paste0("cdo sellonlatbox,", domain_coords, " ", selected_file, " ", spatial_file)

    # Execute the command
    system(cmd, intern = FALSE, ignore.stderr = FALSE)

    # Compute the monthly mean
    monthly_mean_file <- paste0("monthly_mean_", Model[M], ".nc")
    system(paste("cdo ymonmean", spatial_file, monthly_mean_file))
    
    # Extract information from the final file
    info_command <- paste("cdo infon", monthly_mean_file)
    info_output <- system(info_command, intern = TRUE)
    
    # Filter out lines with 'average_DT' if necessary
    info_output <- info_output[!grepl("average_DT", info_output)]
    
    # Process and store the mean values into Data_out
    # Convert the output into a string with newlines
    info_text <- paste(info_output[-1], collapse = "\n")

    # Use read.fwf to parse the string into a data frame
    output_table <- read.fwf(textConnection(info_text),
                         widths = c(5, 14, 9, 8, 10, 9, 12, 12, 12, 12),
                         col.names = c("Index", "Date", "Time", "Level", "Gridsize", "Miss",
                                       "Minimum", "Mean", "Maximum", "Parameter"),
                         stringsAsFactors = FALSE)

    # Drop the 'Index' column if not needed
    output_table <- output_table[, -1]

    Data_out[, M] <- output_table$Mean
    
    # Remove intermediate .nc files
    file.remove(merged_file)
    file.remove(selected_file)
    file.remove(spatial_file)
    file.remove(monthly_mean_file)
  }
}

# Write the results to a CSV file
write.table(Data_out, paste0("out/", Project, "_", Period, "_", Var, ".csv"), sep = ",", row.names = FALSE)
