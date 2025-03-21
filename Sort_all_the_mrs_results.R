rm(list=ls())
# Set the source and destination folders
source_folder <- "E:/A_MRI_UBSN/MRI/MRS_scan/data_check"  # Folder containing subject directories
destination_folder <- "E:/A_MRI_UBSN/MRI/MRS_results/3to2MM"  # Folder where the files will be moved to


# Get a list of subject directories
subject_directories <- list.dirs(source_folder, full.names = FALSE, recursive = FALSE)

# Iterate over each subject directory
for (subject_dir in subject_directories) {
  # Get the subject ID from the directory name
  subject_id <- basename(subject_dir)
  
  # Construct the source and destination paths
  source_path <- file.path(source_folder, subject_id,"MEGA_output","QuantifyResults")
  
  
  files <- list.files(source_path, full.names = TRUE)
  
  # Iterate over each file
  for (file in files) {
    # Extract the file name
    file_name <- basename(file)
    
    # Add the subject ID prefix to the file name
    new_file_name <- paste0(subject_id, "_", file_name)
    
    # Create the destination file path with the new file name
    destination_file <- file.path(destination_folder, new_file_name)
    
    # Copy the file to the destination folder
    file.copy(file, destination_file)
  }
  
}
