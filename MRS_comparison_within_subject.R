rm(list=ls())
library(readxl)
library(stringr)
# Set the source and destination folders
source_folder <- "E:/A_MRI_UBSN/MRI/MRS_scan/data_check"  # Folder containing subject directories
destination_folder <- "E:/A_MRI_UBSN/MRI/MRS_results/3to2MM"  # Folder where the files will be moved to

# Get a list of subject directories
subject_directories <- list.dirs(source_folder, full.names = FALSE, recursive = FALSE)


# Extract the first three characters from each filename and ensure uniqueness
unique_prefixes<- unique(gsub("(_.*$)", "", filenames))

str_preflix<-"_diff1_";
meta_types<-paste(str_preflix,'rawWaterScaled_Voxel_1_Basis_1.tsv',sep='')#tNAA,NAAG,NAA,Glu

filenames <- list.files(destination_folder, pattern = meta_types, full.names = TRUE)

# Create an empty list to store the data frames
df_list <- list()
n=1;

# Iterate over each filename
for (filename in filenames) {
  # Extract the prefix from the filename
  prefix <- str_extract(basename(filename), "\\d+") # get the subject_code
  
  Group <-str_extract(basename(filename), "^[A-Za-z]+")# get the subject group, P is for post scan of glaucoma, R is for baseline glaucoma, H is for healthy
  
  # Load the file as a data frame
  df <- read.delim(filename, sep = "\t") #if this is tsv file
  print(filename)
  #df <- read.csv(filename, sep = ",")
  # Add the prefix and first character as new columns in the data frame
  df <- cbind(ID = prefix, Group = Group, df)
  
  # Append the data frame to the list
  df_list[[n]] <- df
  n=n+1
}

# Merge all data frames into one
tcr <- do.call(rbind, df_list)
tcr<-subset(tcr,!Group %in% c('RH1','RH2'))
tcr[,3:length(tcr)] <- lapply(tcr[,3:length(tcr)], as.numeric)
rownames(tcr) <- NULL

tcr1<-subset(tcr,Group=='R')

demo <-  read_excel("subject_info (dominant eye).xlsx")



for (i in seq_len(nrow(tcr1))) {
  # Assign values to left_md and right_md
  tcr1$left_md[i] <- demo[demo$`subject id` == as.numeric(tcr1$ID[i]), 3]
  tcr1$right_md[i] <- demo[demo$`subject id` == as.numeric(tcr1$ID[i]), 6]
  # Print statements for debugging
  print(as.numeric(tcr1$ID[i]))
}
# Check the updated tcr1 data frame

tcr1$left_md <- as.numeric(tcr1$left_md)
tcr1$right_md <- as.numeric(tcr1$right_md)
tcr1$mean_md <- rowMeans(tcr1[, c("left_md", "right_md")], na.rm = TRUE)


# Assuming columns 34 to 36 are the glaucoma severity indices
severity_indices <- tcr1[, 34:36]

# Calculate correlations with each metabolic measure (columns 3 to 33)
cor_results <- sapply(tcr1[, 3:33], function(x) cor(x, severity_indices, use = "complete.obs"))

# Print correlation results
print(cor_results)



# Assuming tcr1 is already defined and severity_indices is set
severity_indices <- tcr1[, 34:36]

# Create a list to store results
cor_results <- list()

# Loop through each metabolic measure (columns 3 to 33)
for (i in 3:33) {
  metabolic_measure <- tcr1[, i]
  # Calculate correlation and test for significance
  test_result <- cor.test(metabolic_measure, severity_indices, use = "complete.obs")
  
  # Store the correlation coefficient and p-value
  cor_results[[i - 2]] <- list(
    correlation = test_result$estimate,  # Correlation coefficient
    p_value = test_result$p.value         # P-value
  )
}

# Print the correlation results
for (j in seq_along(cor_results)) {
  cat(sprintf("Metabolic Measure %d: Correlation = %.3f, p-value = %.3f\n", 
              j + 2, cor_results[[j]]$correlation, cor_results[[j]]$p_value))
}


# Example for the first metabolic measure (column 3)
model <- lm(tcr1[, 5] ~ tcr1[, 34] + tcr1[, 35] + tcr1[, 36])
summary(model)

# Loop through metabolic measures and fit linear models
results <- list()
n=1;
for (i in c(7,9,10)) {
  print(colnames(tcr1)[i])
  # formula <- as.formula(paste("tcr1[,", i, "] ~ tcr1[, 34] + tcr1[, 35] + tcr1[, 36]"))
  formula <- as.formula(paste("tcr1[,", i, "] ~ tcr1[, 36]"))
  model <- lm(formula)
  results[[n]] <- summary(model)
  n=n+1
}
# Print results for all models
results


