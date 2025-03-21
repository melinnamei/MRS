# Set the folder path
rm(list=ls())
library(stringr)
setwd('G:/A_MRI_UBSN/MRI/MRS_results/3to2MM')
folder_path <- getwd()

# Get the list of filenames in the folder
filenames <- list.files(folder_path, full.names = FALSE)

# Extract the first three characters from each filename and ensure uniqueness
unique_prefixes<- unique(gsub("(_.*$)", "", filenames))

str_preflix<-"_diff1_";

meta_types<-paste(str_preflix,'rawWaterScaled_Voxel_1_Basis_1.tsv',sep='')#tNAA,NAAG,NAA,Glu

filenames <- list.files(folder_path, pattern = meta_types, full.names = TRUE)

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

tcr<-subset(tcr,!Group %in% c('RH'))

#tcr<-subset(tcr,!ID %in% c('142','148','84','99')) # previous cut off date: 2024-Nov-11


rownames(tcr) <- NULL

tcr[,3:length(tcr)] <- lapply(tcr[,3:length(tcr)], as.numeric)

tcr1<-subset(tcr,Group!='H')

tcr1<-subset(tcr1,Group!='RH')

tcr1 <- tcr1[, sapply(tcr1, function(x) length(unique(x)) != 1)]




# Define the outcomes
outcomes <- c('tNAA','NAA','NAAG','Glu','GSH','Glx','GABA','GABAplus')
# Keep first two columns and only the specified outcomes
tcr1 <- tcr1[, c(names(tcr1)[1:2], outcomes[outcomes %in% names(tcr1)])]

# Add new column for Glx/GABA ratio
tcr1$`Glx/GABA` <- tcr1$Glx / tcr1$GABA

# If you want to handle potential division by zero or NA values:
tcr1$`Glx/GABA` <- ifelse(tcr1$GABA == 0 | is.na(tcr1$GABA),
                              NA,
                              tcr1$Glx / tcr1$GABA)


# First create a function to run all tests
analyze_outcomes <- function(df, col_name) {
  # Shapiro-Wilk test for normality for each group
print(col_name)
    group1_norm <- shapiro.test(df[df$Group == unique(df$Group)[1], col_name])$p.value
  group2_norm <- shapiro.test(df[df$Group == unique(df$Group)[2], col_name])$p.value
  
  # If both groups are normal (p > 0.05), use t-test, else use Wilcoxon
  if(group1_norm > 0.05 & group2_norm > 0.05) {
    test_result <- t.test(get(col_name) ~ Group, data = df)
    test_type <- "t-test"
  } else {
#    test_result <- wilcox.test(get(col_name) ~ Group, data = df)
    test_result <- t.test(get(col_name) ~ Group, data = df)
    
        test_type <- "test-t-Wilcoxon"
  }
  
  # Calculate means for each group
  means <- tapply(df[[col_name]], df$Group, mean, na.rm = TRUE)
  sds <- tapply(df[[col_name]], df$Group, sd, na.rm = TRUE)
  
  return(c(
    Variable = col_name,
    Test = test_type,
    P_value = test_result$p.value,
    Mean_Group1 = means[1],
    SD_Group1 = sds[1],
    Mean_Group2 = means[2],
    SD_Group2 = sds[2]
  ))
}

# Run analysis for all outcomes (columns 3-33)
results_list <- lapply(names(tcr1)[3:length(tcr1)], function(col) {
  analyze_outcomes(tcr1, col)
})

# Convert results to data frame
results_df <- do.call(rbind, results_list)
results_df <- as.data.frame(results_df)

# Convert numeric columns
numeric_cols <- colnames(results_df)[3:7]
results_df[numeric_cols] <- lapply(results_df[numeric_cols], as.numeric)

# Add significance markers
results_df$Significant <- ifelse(results_df$P_value < 0.05, "*", "")

# Sort by p-value
results_df <- results_df[order(results_df$P_value), ]

# Adjust p-values for multiple comparisons
results_df$P_adjusted <- p.adjust(results_df$P_value, method = "BH")

# Print results
print(results_df)

# Optional: Create visualizations for significant outcomes
library(ggplot2)

# Function to create boxplot for a specific outcome
create_boxplot <- function(data, outcome) {
  ggplot(data, aes(x = Group, y = .data[[outcome]], fill = Group)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.5) +
    theme_minimal() +
    ggtitle(outcome) +
    theme(legend.position = "none")
}

# Create plots for significant outcomes (p < 0.05)
significant_vars <- results_df$Variable[results_df$P_value < 0.8]
for(var in significant_vars) {
  print(create_boxplot(tcr1, var))
}
