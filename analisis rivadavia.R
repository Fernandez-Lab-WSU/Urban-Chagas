rm(list=ls())

db <- read.csv(here::here("dbrivadavia.csv"))

summary(db)

db <- subset(db,!is.na(db[,1]))

cat_vars <- c("ID",
              "Block",
              "Unit",
              "STATUS",
              "INF",
              "SITE",
              "SPR",
              "wall_material",
              "wall_covering",
              "roof_material",
              "roof_covering",
              "watertank",
              "watertank_pigeon",
              "palm",
              "objects",
              "insecticide",
              "commercial",
              "permethrin",
              "other",
              "travel_rural",
              "dwelling_status",
              "chicken_coop",
              "rats",
              "idcerca_NE",
              "idcerca_NW",
              "idcerca_S")


db[cat_vars] <- lapply(db[cat_vars], as.factor)

# Convert the Date column from character to Date
db$DATE <- as.Date(db$DATE, format = "%d/%m/%Y")
db$SPR.DATE <- as.Date(db$SPR.DATE, format = "%d/%m/%Y")

summary(db)

dbEV <- subset(db,db$STATUS == "EV")

summary(dbEV)


# Function to create stacked bar plots
library(ggplot2)

# Function to create stacked bar plots and ignore NAs
create_stacked_bar_plot <- function(data, categorical_var, target_var) {
  # Remove rows with NA in the current categorical variable
  data <- data[!is.na(data[[categorical_var]]), ]
  
  ggplot(data, aes_string(x = target_var, fill = categorical_var)) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(y = "Proportion", fill = categorical_var, x = target_var) +
    theme_minimal() +
    theme(legend.position = "none",
              plot.title = element_text(size = 8),
              axis.title.x = element_text(size = 8),
              axis.title.y = element_text(size = 8),
              axis.text.x = element_text(size = 7),
              axis.text.y = element_text(size = 7)
            ) +
    ggtitle(categorical_var)
}

# List of categorical variables
categorical_vars <- setdiff(cat_vars, c(
                            "ID",
                            "Block",
                            "Unit",
                            "STATUS",
                            "INF",
                            "SITE",
                            "SPR",
                            "education_level",
                            "employer",
                            "employee",
                            "selfemployed",
                            "housework",
                            "not.employed",
                            "pension",
                            "benefit",
                            "health",
                            "idcerca_NE",
                            "idcerca_NW",
                            "idcerca_S"))

# Create a list to store the plots
plots <- list()

# Create plots for each remaining categorical variable and store in the list
for (var in categorical_vars) {
  plots[[var]] <- create_stacked_bar_plot(db, var, "INF")
}

library(gridExtra)
# Arrange the plots in a 4x4 grid
grid.arrange(grobs = plots, ncol = 4, nrow = 4)

#now numeric variables:
numeric_var <- c("dog_num",
                 "cat_num",
                 "chicken_num",
                 "other_num",
                 "dist_lum",
                 "num_lum_50m",
                 "num_lum_100m",
                 "dist.infest",
                 "dist_NW",
                 "dist_S",
                 "num_infest_50m",
                 "num_infest_100m",
                 "num_S",
                 "num_S_infest_50m",
                 "num_S_infest_100m",
                 "num_NW",
                 "num_NW_infest_50m",
                 "num_NW_infest_100m",
                 "percent_green_50",
                 "percent_green_100")
numeric_var
  
# Function to create violin plots with jittered points
create_violin_plot <- function(data, numeric_var, target_var) {
  # Remove rows with NA in the current numeric variable
  data <- data[!is.na(data[[numeric_var]]), ]
  
  ggplot(data, aes_string(x = target_var, y = numeric_var, fill = target_var)) +
    geom_violin(trim = FALSE) +
    geom_jitter(width = 0.2, size = 0.6, alpha = 0.6) +
    labs(y = numeric_var, fill = target_var, x = target_var) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 8),
      axis.title.x = element_text(size = 8),
      axis.title.y = element_text(size = 8),
      axis.text.x = element_text(size = 7),
      axis.text.y = element_text(size = 7)
    ) +
    ggtitle(numeric_var)
}

# Create a list to store the plots
plots2 <- list()

# Create plots for each numeric variable and store in the list
for (var in numeric_var) {
  plots2[[var]] <- create_violin_plot(db, var, "INF")
}

# Arrange the plots in a 4x4 grid
grid.arrange(grobs = plots2, ncol = 5, nrow = 4)

library(dplyr)
# Initialize an empty data frame to store results
results <- data.frame(Variable = character(),
                      p_value = numeric(),
                      stringsAsFactors = FALSE)

# Perform Mann-Whitney test for each numeric variable
for (var in numeric_var) {
  test_result <- wilcox.test(dbEV[[var]] ~ dbEV$INF, data = dbEV)
  results <- rbind(results, data.frame(Variable = var, p_value = test_result$p.value))
}

# Display the results
results <- results %>%
  mutate(Significance = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ ""
  ))

print(results)

library(knitr)
library(kableExtra)
# Format p-values to four decimal places
results$p_value <- formatC(results$p_value, format = "f", digits = 4)

# Format the table for Word document
formatted_table <- results %>%
  kable("html", col.names = c("Variable", "P-value", "Significance"), align = "c") %>%
  kable_styling(full_width = FALSE, position = "left", bootstrap_options = c("striped", "hover", "condensed")) %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(3, bold = TRUE, color = "red")

# Print the formatted table
formatted_table

##correlation matrix animals
library(FactoMineR)
library(factoextra)
library(corrplot)
  # Extract numeric variables related to animals
  animal_vars <- c("dog_num", "cat_num", "chicken_num", "other_num")
  
  # Compute the correlation matrix
  cor_matrix <- cor(dbEV[animal_vars], use = "complete.obs")
  
  # Set the diagonal elements to NA
  diag(cor_matrix) <- NA
  
  # Print the correlation matrix
  print(cor_matrix)
  
  # Visualize the correlation matrix without the diagonal elements
  corrplot(cor_matrix, method = "circle", na.label = " ")

##FDMA to include rats and pigeons-------------------
  
  # Step 1: Install and load the missMDA package
  library(missMDA)
  
  # Extract the relevant variables, including categorical variables
  famd_vars <- c("dog_num", "cat_num", "chicken_num", "other_num", "rats")
  
  # Ensure the categorical variables are factors
  dbEV[famd_vars] <- lapply(dbEV[famd_vars], function(x) if(is.character(x)) as.factor(x) else x)
  
  # Step 2: Impute missing values using imputeFAMD
  imputed_data <- imputeFAMD(dbEV[famd_vars], ncp = 4) # ncp is the number of dimensions to use for imputation
  
  # Step 3: Perform FAMD on the imputed dataset
  famd_res <- FAMD(imputed_data$completeObs, ncp = 4, graph = FALSE)
  
  # Print the summary of FAMD results
  summary(famd_res)
  
  library(factoextra)
  # Visualize the FAMD results
  fviz_famd_ind(famd_res, habillage = "rats", palette = "jco", repel = TRUE) + 
    labs(title = "Individuals factor map - Grouped by Rats")
  
  fviz_famd_var(famd_res, repel = TRUE) + 
    labs(title = "Variables factor map")
  
  # Visualize the categories of categorical variables
  fviz_famd_var(famd_res, choice = "quali.var", repel = TRUE) + 
    labs(title = "Categorical Variables factor map")
  
  # Visualize the contributions of quantitative variables
  fviz_famd_var(famd_res, choice = "quanti.var", repel = TRUE) + 
    labs(title = "Quantitative Variables factor map")
  
  # Visualize the FAMD results with individuals colored by the INF variable
  fviz_famd_ind(famd_res, habillage = dbEV$INF, palette = "jco", repel = TRUE) +
    labs(title = "Individuals factor map - Grouped by INF")
 
   # Visualize the FAMD results with individuals colored by the INF variable and overlay quantitative variables
  fviz_famd(famd_res, 
            habillage = dbEV$INF, # Color individuals by INF
            palette = "jco", 
            repel = TRUE,
            addEllipses = TRUE) + # Add confidence ellipses
    labs(title = "FAMD - Individuals grouped by INF and Quantitative Variables Overlayed")
  
  # Add quantitative variables to the plot
  fviz_famd_var(famd_res, choice = "quanti.var", repel = TRUE) +
    labs(title = "FAMD - Quantitative Variables")
  
  ##SEE THE pca
  # Extract the quantitative variables for PCA
  quant_vars <- c("dog_num", "cat_num", "chicken_num", "other_num")
  quant_data <- dbEV[quant_vars]
  
  # Ensure the data does not contain NA values
  quant_data <- na.omit(quant_data)
  
  # Perform PCA
  pca_res <- prcomp(quant_data, scale. = TRUE)
  
  # Print summary of PCA results
  summary(pca_res)
  
  # Plot individuals
  fviz_pca_ind(pca_res, repel = TRUE) +
    labs(title = "PCA - Individuals")
  
  # Plot variables
  fviz_pca_var(pca_res, repel = TRUE) +
    labs(title = "PCA - Variables")
  
  # Combined plot of individuals and variables
  fviz_pca_biplot(pca_res, repel = TRUE) +
    labs(title = "PCA - Biplot")
  #THE pca DOESN'T REALLY ADD MUCH EITHER
  
  #do a manU with the Dim1 and Dim2 of the FAMD and the PCA and add it to the result formatted table
 
  # Extract FAMD coordinates for Dim 1 and Dim 2
  famd_coords <- as.data.frame(famd_res$ind$coord)
  colnames(famd_coords) <- paste0("FAMD_Dim", 1:4)
  dbEV <- cbind(dbEV, famd_coords)
  
    # Extract PCA coordinates for PC1 and PC2
  # Perform PCA Analysis
  quant_vars <- c("dog_num", "cat_num", "chicken_num", "other_num")
  quant_data <- dbEV[quant_vars]
  
  # Perform PCA and ensure row alignment
  pca_res <- prcomp(na.omit(quant_data), scale. = TRUE)
  pca_coords <- as.data.frame(matrix(NA, nrow = nrow(dbEV), ncol = 2))
  rownames(pca_coords) <- rownames(dbEV)
  pca_coords[complete.cases(quant_data), ] <- pca_res$x[, 1:2]
  colnames(pca_coords) <- paste0("PCA_PC", 1:2)
  dbEV <- cbind(dbEV, pca_coords)
  
  
  # Define all variables for Mann-Whitney U tests
  all_vars <- c(numeric_var, colnames(famd_coords), colnames(pca_coords))
  # Remove variables containing "NA"
  all_vars <- subset(all_vars, !is.na(all_vars))
  
  
  # Initialize an empty data frame to store results
  results <- data.frame(Variable = character(),
                        p_value = numeric(),
                        Significance = character(),
                        stringsAsFactors = FALSE)
  
  # Perform Mann-Whitney test for each variable
  for (var in all_vars) {
    test_result <- wilcox.test(dbEV[[var]] ~ dbEV$INF, data = dbEV)
    results <- rbind(results, data.frame(Variable = var, 
                                         p_value = formatC(test_result$p.value, format = "f", digits = 4), 
                                         Significance = ifelse(test_result$p.value < 0.05, ifelse(test_result$p.value < 0.01, ifelse(test_result$p.value < 0.001, "***", "**"), "*"), "")))
  }
  
  # Format the table for Word document
  formatted_table <- results %>%
    kable("html", col.names = c("Variable", "P-value", "Significance"), align = "c") %>%
    kable_styling(full_width = FALSE, position = "left", bootstrap_options = c("striped", "hover", "condensed")) %>%
    column_spec(1, bold = TRUE) %>%
    column_spec(3, bold = TRUE, color = "red")
  
  # Print the formatted table
  formatted_table
  
  library(ggplot2)
  library(gridExtra)
  
  # Function to create violin plots with jittered points
  create_violin_plot <- function(data, numeric_var, target_var) {
    # Remove rows with NA in the current numeric variable
    data <- data[!is.na(data[[numeric_var]]), ]
    
    ggplot(data, aes_string(x = target_var, y = numeric_var, fill = target_var)) +
      geom_violin(trim = FALSE) +
      geom_jitter(width = 0.2, size = 0.6, alpha = 0.6) +
      labs(y = numeric_var, fill = target_var, x = target_var) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7)
      ) +
      ggtitle(numeric_var)
  }
  
  # Create a list to store the plots
  plots2 <- list()
  
  # Create plots for each variable in all_vars and store in the list
  for (var in all_vars) {
    plots2[[var]] <- create_violin_plot(dbEV, var, "INF")
  }
  
  # Arrange the plots in a grid
  grid.arrange(grobs = plots2, ncol = 5, nrow = 6)
  
  
  #--------------------------------
##Hierarchical model: type of house
  library(cluster)
  # Define the variables related to house type
  house_vars <- c("wall_material", "wall_covering", "roof_material", "roof_covering")
  
  # Ensure the variables are treated as factors
  dbEV[house_vars] <- lapply(dbEV[house_vars], as.factor)
  
  # Impute missing values
  imputed_data <- imputeMCA(dbEV[house_vars], ncp = 3) # Adjust ncp as needed
  
  # Perform Multiple Correspondence Analysis (MCA) on imputed data
  mca_res <- MCA(imputed_data$completeObs, graph = TRUE)
  
  # Extract MCA coordinates for individuals
  mca_coords <- as.data.frame(mca_res$ind$coord)
  colnames(mca_coords) <- paste0("MCA_Dim", 1:ncol(mca_coords))
  
  # Add the MCA results back to the original data frame
  # Create a new data frame with NA for the new columns
  #mca_results <- data.frame(matrix(NA, nrow = nrow(dbEV), ncol = ncol(mca_coords)))
  #colnames(mca_results) <- colnames(mca_coords)
  
  # Fill the new data frame with MCA results for the rows without NA in the imputed dataset
  #original_complete_cases <- complete.cases(dbEV[house_vars])
  #mca_results[original_complete_cases, ] <- mca_coords
  
  # Combine the original data frame with the MCA results
  dbEV <- cbind(dbEV, mca_coords)
  
  # Print the first few rows to verify the results
  head(dbEV)
  
  # Plot the individuals
  fviz_mca_ind(mca_res, 
               label = "none",       # Hide individual labels
               habillage = dbEV$INF,   # Color by groups if needed
               addEllipses = TRUE,   # Add confidence ellipses
               ellipse.level = 0.95, # Confidence level
               palette = "jco")      # Color palette
  
  # Plot the variables
  fviz_mca_var(mca_res, 
               repel = TRUE,         # Avoid text overlapping
               col.var = "black")    # Color of variables
  
  # Plot a biplot showing both individuals and variables
  fviz_mca_biplot(mca_res, 
                  repel = TRUE,      # Avoid text overlapping
                  col.var = "black", # Color of variables
                  habillage = "none",# Color by groups if needed
                  palette = "jco")   # Color palette
  # Perform hierarchical clustering
  diss_matrix <- daisy(mca_coords, metric = "euclidean")
  hc_res <- hclust(diss_matrix, method = "ward.D2")
  
  # Cut the dendrogram to obtain 3 clusters
  clusters <- cutree(hc_res, k = 3)
  
  # Create a table summarizing the clusters
  cluster_table <- data.frame(Cluster = clusters, dbEV[house_vars])
  print("Cluster summary table:")
  print(table(cluster_table$Cluster))

  # Add the cluster assignments as a new categorical variable to dbEV
  dbEV$house_type <- as.factor(clusters)
  
  # Print the first few rows to verify the results
  head(dbEV)
  
  # Plot the individuals colored by house_type
  fviz_mca_ind(mca_res, 
               label = "none",         # Hide individual labels
               habillage = dbEV$house_type,   # Color by house_type
               addEllipses = TRUE,     # Add confidence ellipses
               ellipse.level = 0.95,   # Confidence level
               palette = "jco")        # Color palette
  
  # Plot the variables
  fviz_mca_var(mca_res, 
               repel = TRUE,           # Avoid text overlapping
               col.var = "black")      # Color of variables
  
  # Plot a biplot showing both individuals and variables colored by house_type
  fviz_mca_biplot(mca_res, 
                  repel = TRUE,        # Avoid text overlapping
                  col.var = "black",   # Color of variables
                  habillage = dbEV$house_type, # Color by house_type
                  palette = "jco")     # Color palette

  # Create stacked bar plots for each house variable without NAs
  plot_list <- lapply(house_vars, function(var) {
    ggplot(na.omit(dbEV[, c(var, "house_type")]), aes_string(x = var, fill = "house_type")) +
      geom_bar(position = "fill") +
      labs(y = "Proportion", fill = "House Type") +
      theme_minimal() +
      ggtitle(paste("Distribution of", var))
  })
  
  # Arrange the plots in a grid
  library(gridExtra)
  do.call(grid.arrange, c(plot_list, ncol = 2))
  
  # Create stacked bar plots for each house variable with house_type on the x-axis and without NAs
  plot_list <- lapply(house_vars, function(var) {
    ggplot(na.omit(dbEV[, c(var, "house_type")]), aes_string(x = "house_type", fill = var)) +
      geom_bar(position = "fill") +
      labs(y = "Proportion", fill = var) +
      theme_minimal() +
      ggtitle(paste("Distribution of", var, "by House Type"))
  })
  
  # Arrange the plots in a grid
  library(gridExtra)
  do.call(grid.arrange, c(plot_list, ncol = 2))
  
  # Load necessary packages
  library(FactoMineR)
  library(factoextra)
  library(missMDA)
  library(dplyr)
  
  # Define the additional variables for MCA
  additional_vars <- c("education_level", "pension", "benefit", "health")
  
  # Recode health category 4 as 3, and specify .default to handle other values
  dbEV$health <- recode(as.numeric(dbEV$health), `4` = 2, `3` = 2, .default = as.numeric(dbEV$health))
  
  # Recode education_level categories 4 and 5 as 3, and specify .default to handle other values
  dbEV$education_level <- recode(as.numeric(dbEV$education_level), `4` = 3, `5` = 3, .default = as.numeric(dbEV$education_level))
  
  # Print the tables for the recoded variables
  table(dbEV$health)
  table(dbEV$education_level)
  table(dbEV$benefit)
  table(dbEV$pension)
  
  # Create the income_work variable
  dbEV$income_work <- with(dbEV, ifelse(employer == 1 | employee == 1 | selfemployed == 1, "income_work", 
                                        ifelse(housework == 1 | not.employed == 1, "not_income_work", NA)))
  table(dbEV$income_work)
  
  # Ensure the variables are treated as factors
  dbEV[additional_vars] <- lapply(dbEV[additional_vars], as.factor)
  dbEV$income_work <- as.factor(dbEV$income_work)
  
  print(table(dbEV$health))
  print(table(dbEV$education_level))
  print(table(dbEV$benefit))
  print(table(dbEV$pension))
  
  # Combine income_work with additional variables for MCA
  mca_vars <- c(additional_vars, "income_work")
  
  # Check for missing values in the combined data
  missing_values <- sapply(dbEV[mca_vars], function(x) sum(is.na(x)))
  print("Missing values in each variable:")
  print(missing_values)
  
  # Impute missing values
  imputed_data <- imputeMCA(dbEV[mca_vars], ncp = 3) # Adjust ncp as needed
  
  # Perform Multiple Correspondence Analysis (MCA) on imputed data
  mca_res <- MCA(imputed_data$completeObs, graph = TRUE)
  
  # Visualize the contribution of variables to the first dimension
  fviz_contrib(mca_res, choice = "var", axes = 1, top = 10)
  
  # Visualize the contribution of variables to the second dimension
  fviz_contrib(mca_res, choice = "var", axes = 2, top = 10)
  
  # If you want to see the contributions for more dimensions, you can add similar lines for other dimensions
  # For example, to see the contribution to the third dimension:
  fviz_contrib(mca_res, choice = "var", axes = 3, top = 10)
  
  # Extract MCA coordinates for individuals
  mca_coords <- as.data.frame(mca_res$ind$coord)
  colnames(mca_coords) <- paste0("MCA_SEE_Dim", 1:ncol(mca_coords))
  
  # Add the MCA results back to the original data frame
  # Create a new data frame with NA for the new columns
  #mca_results <- data.frame(matrix(NA, nrow = nrow(dbEV), ncol = ncol(mca_coords)))
  #colnames(mca_results) <- colnames(mca_coords)
  
  # Fill the new data frame with MCA results for the rows without NA in the imputed dataset
  #original_complete_cases <- complete.cases(dbEV[mca_vars])
  #mca_results[original_complete_cases, ] <- mca_coords
  
  # Combine the original data frame with the MCA results
  dbEV <- cbind(dbEV, mca_coords)
  
  # Print the first few rows to verify the results
  head(dbEV)
  
  # Visualize the biplot of the MCA results
  fviz_mca_biplot(mca_res, 
                  repel = TRUE,        # Avoid text overlapping
                  col.var = "black",   # Color of variables
                  habillage = "none", # Color by income_work
                  palette = "jco",     # Color palette
                  addEllipses = FALSE,  # Add confidence ellipses
                  ellipse.level = 0.95) # Confidence level
  
  # Get the variance explained by each dimension
  eigenvalues <- mca_res$eig
  print("Eigenvalues and variance explained by each dimension:")
  print(eigenvalues)

  # Extract the first three dimensions of the MCA coordinates for individuals
  mca_coords <- as.data.frame(mca_res$ind$coord[, 1:3])
  colnames(mca_coords) <- paste0("MCA_SEE_Dim", 1:3)
  
  # Perform hierarchical clustering on the first three dimensions
  diss_matrix <- dist(mca_coords)  # Compute dissimilarity matrix
  hc_res <- hclust(diss_matrix, method = "ward.D2")
  
  # Plot the dendrogram
  plot(hc_res, labels = FALSE, main = "Hierarchical Clustering Dendrogram")
  
  # Cut the dendrogram to obtain clusters (e.g., 3 clusters)
  num_clusters <- 3
  clusters <- cutree(hc_res, k = num_clusters)
  
  # Add the cluster results back to the original data frame
  dbEV$SEE <- as.factor(clusters)
  table(dbEV$SEE)
  
  # Print the first few rows to verify the new cluster variable
  head(dbEV)
  
  # Visualize the biplot of the MCA results with clusters
  fviz_mca_biplot(mca_res, 
                  repel = TRUE,        # Avoid text overlapping
                  col.var = "black",   # Color of variables
                  habillage = dbEV$SEE, # Color by clusters
                  palette = "jco",     # Color palette
                  addEllipses = TRUE,  # Add confidence ellipses
                  ellipse.level = 0.95) # Confidence level
  
    # Create stacked bar plots for each house variable without NAs
  plot_list <- lapply(mca_vars, function(var) {
    ggplot(na.omit(dbEV[, c(var, "SEE")]), aes_string(x = var, fill = "SEE")) +
      geom_bar(position = "fill") +
      labs(y = "Proportion", fill = "SEE") +
      theme_minimal() +
      ggtitle(paste("Distribution of", var))
  })
  
  # Arrange the plots in a grid
    do.call(grid.arrange, c(plot_list, ncol = 2))
  
  # Create stacked bar plots for each house variable with house_type on the x-axis and without NAs
  plot_list <- lapply(mca_vars, function(var) {
    ggplot(na.omit(dbEV[, c(var, "SEE")]), aes_string(x = "SEE", fill = var)) +
      geom_bar(position = "fill") +
      labs(y = "Proportion", fill = var) +
      theme_minimal() +
      ggtitle(paste("Distribution of", var, "by SEE"))
  })
  
  # Arrange the plots in a grid
    do.call(grid.arrange, c(plot_list, ncol = 2))
  
  ##interaccion dwelling status y socio-economic
  dbEV$dwelling_status <- as.factor(dbEV$dwelling_status)
  
  # Create a contingency table between the cluster (SEE) and dwelling_status
  contingency_table <- table(SEE = dbEV$SEE, DwellingStatus = dbEV$dwelling_status)
  
  # Print the contingency table with variable titles and margins
  print("Contingency Table:")
  contingency_table_with_margins <- addmargins(contingency_table)
  print(ftable(contingency_table_with_margins))
  
  # Perform the chi-square test of independence
  chisq_test <- chisq.test(contingency_table)
  
  # Print the results of the chi-square test
  print("Chi-square Test Results:")
  print(chisq_test)
  
  # Convert the contingency table to a data frame for ggplot2
  contingency_df <- as.data.frame(contingency_table)
  colnames(contingency_df) <- c("SEE", "DwellingStatus", "Count")
  
  # Create a bar plot of the proportions
  ggplot(contingency_df, aes(x = SEE, y = Count, fill = DwellingStatus)) +
    geom_bar(stat = "identity", position = "fill") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(y = "Proportion", x = "Socio-Economic Status (SEE)", fill = "Dwelling Status") +
    ggtitle("Proportions of Dwelling Status within SEE Groups") +
    theme_minimal()
  
  
  # Load necessary packages
  library(vcd)
  
  # Create a mosaic plot of the contingency table
  mosaic(~SEE + DwellingStatus, data = contingency_table,
         shade = TRUE, legend = TRUE,
         main = "Mosaic Plot of Dwelling Status by SEE")
  
  
##interaction between socio-economic and type of house
  dbEV$house_type <- as.factor(dbEV$house_type)
  
  # Create a contingency table between the cluster (SEE) and dwelling_status
  contingency_table <- table(SEE = dbEV$SEE, HouseType = dbEV$house_type)
  
  # Print the contingency table with variable titles and margins
  print("Contingency Table:")
  contingency_table_with_margins <- addmargins(contingency_table)
  print(ftable(contingency_table_with_margins))
  
  # Perform the chi-square test of independence
  chisq_test <- chisq.test(contingency_table)
  
  # Print the results of the chi-square test
  print("Chi-square Test Results:")
  print(chisq_test)

  # Convert the contingency table to a data frame for ggplot2
  contingency_df <- as.data.frame(contingency_table)
  colnames(contingency_df) <- c("SEE", "HouseType", "Count")
  
  # Create a bar plot of the proportions
  ggplot(contingency_df, aes(x = SEE, y = Count, fill = HouseType)) +
    geom_bar(stat = "identity", position = "fill") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(y = "Proportion", x = "Socio-Economic Status (SEE)", fill = "HouseType") +
    ggtitle("Proportions of House Type within SEE Groups") +
    theme_minimal()
  
  # Load necessary packages
  library(vcd)
  
  # Create a mosaic plot of the contingency table
  mosaic(~SEE + HouseType, data = contingency_table,
         shade = TRUE, legend = TRUE,
         main = "Mosaic Plot of House Type by SEE")
  
  ##insecticide use (and type) vs socioeconomic
  dbEV$insecticide <- as.factor(dbEV$insecticide)
  
  # Create a contingency table between the cluster (SEE) and dwelling_status
  contingency_table <- table(SEE = dbEV$SEE, Insecticide = dbEV$insecticide)
  
  # Print the contingency table with variable titles and margins
  print("Contingency Table:")
  contingency_table_with_margins <- addmargins(contingency_table)
  print(ftable(contingency_table_with_margins))
  
  # Perform the chi-square test of independence
  chisq_test <- chisq.test(contingency_table)
  
  # Print the results of the chi-square test
  print("Chi-square Test Results:")
  print(chisq_test)
  
  # Convert the contingency table to a data frame for ggplot2
  contingency_df <- as.data.frame(contingency_table)
  colnames(contingency_df) <- c("SEE", "Insecticide", "Count")
  
  # Create a bar plot of the proportions
  ggplot(contingency_df, aes(x = SEE, y = Count, fill = Insecticide)) +
    geom_bar(stat = "identity", position = "fill") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(y = "Proportion", x = "Socio-Economic Status (SEE)", fill = "Insecticide") +
    ggtitle("Proportions of House Type within SEE Groups") +
    theme_minimal()
  
  # Load necessary packages
  library(vcd)
  
  # Create a mosaic plot of the contingency table
  mosaic(~SEE + Insecticide, data = contingency_table,
         shade = TRUE, legend = TRUE,
         main = "Mosaic Plot of Insecticide use by SEE")
  
#MODELO 1:
#type of house/animals/socio-economic/chickcoop/palm/watertank/objects/travelrural/insecticide - check interaction with socio-economic and travel

  # Define the categorical variables to be tested
  categorical_var <- c("SEE", "house_type", "insecticide", "commercial", "permethrin", "watertank", "palm", "chicken_coop", "objects", "travel_rural")
  
  # Initialize an empty data frame to store results
  results_chisq <- data.frame(Variable = character(),
                              p_value = numeric(),
                              stringsAsFactors = FALSE)
  
    # Perform Chi-square test for each categorical variable
  for (var in categorical_var) {
    # Remove rows with NA values in the specific variables
    valid_data <- dbEV %>% select(all_of(var), INF) %>% na.omit()
    
    # Create contingency table
    contingency_table <- table(valid_data[[var]], valid_data$INF)
    
    # Perform chi-square test
    test_result <- chisq.test(contingency_table)
    
    # Store results
    results_chisq <- rbind(results_chisq, data.frame(Variable = var, p_value = test_result$p.value))
  }
  
  # Display the results
  results_chisq <- results_chisq %>%
    mutate(Significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    ))
  
  # Format p-values to four decimal places
  results_chisq$p_value <- formatC(results_chisq$p_value, format = "f", digits = 4)
  
  # Format the table for Word document
  formatted_table_chisq <- results_chisq %>%
    kable("html", col.names = c("Variable", "P-value", "Significance"), align = "c") %>%
    kable_styling(full_width = FALSE, position = "left", bootstrap_options = c("striped", "hover", "condensed")) %>%
    column_spec(1, bold = TRUE) %>%
    column_spec(3, bold = TRUE, color = "red")
  
  # Print the formatted table
  formatted_table_chisq
  
  #ahora si el modelo

  ##modelo 1
  library(logistf)
  #sanjuan$numperr_cat <- relevel(sanjuan$numperr_cat, ref = "0")
  dbEV_mod1 <- select(dbEV, INF, house_type, chicken_coop, objects, FAMD_Dim1)
  dbEV_mod1 <- dbEV_mod1[complete.cases(dbEV_mod1), ]
  
  dbEV_mod1$house_type <- relevel(factor(dbEV_mod1$house_type), ref = "2")
  fit1<-logistf(data=dbEV_mod1, INF ~ house_type + chicken_coop + objects + FAMD_Dim1, control = logistf.control(maxit = 10000, maxstep = 1), na.action = na.fail)
  summary(fit1)
  
  # Note: VIF computation with logistf is not directly supported, 
  # so we use a standard logistic regression model to compute VIF
  
  # Fit a standard logistic regression model for VIF calculation
  fit_standard <- glm(INF ~ house_type + chicken_coop + objects + FAMD_Dim1, 
                      data = dbEV_mod1, 
                      family = binomial, 
                      na.action = na.fail)
  
  # Compute VIF using the car package
  vif_values <- car::vif(fit_standard)
  
  # Display the VIF values
  print(vif_values)
  
  library(MuMIn)
  MMI <- dredge(fit1)
  MMI
  write.table(MMI, "clipboard", sep="\t")
  
  MAS <- model.avg(MMI)
  summary(MAS)
  sw(MAS)
  
  #Coefficients and confidence intervals
  ci<-confint(MAS)
  table.coef <- exp(cbind (OR=coef(MAS),ci))
  print(table.coef, digits = 3)
  write.table(table.coef, "clipboard", sep="\t")
  
#MODELO 2:
#Modelo espacial

  dbEV_mod2 <- select(dbEV, INF, dist_lum, num_S_infest_50m, num_S_infest_100m, dist.infest)
  dbEV_mod2 <- dbEV_mod2[complete.cases(dbEV_mod2), ]
  
  fit2<-logistf(data=dbEV_mod2, INF ~ dist_lum + num_S_infest_100m, control = logistf.control(maxit = 10000, maxstep = 1), na.action = na.fail)
  summary(fit2)
  
  # Note: VIF computation with logistf is not directly supported, 
  # so we use a standard logistic regression model to compute VIF
  
  # Fit a standard logistic regression model for VIF calculation
  fit_standard <- glm(INF ~ dist_lum + num_S_infest_50m + num_S_infest_100m + dist.infest, 
                      data = dbEV_mo21, 
                      family = binomial, 
                      na.action = na.fail)
  
  # Compute VIF using the car package
  vif_values <- car::vif(fit_standard)
  
  # Display the VIF values
  print(vif_values)
  
  #Coefficients and confidence intervals
  ci<-confint(fit2)
  table.coef <- exp(cbind (OR=coef(fit2),ci))
  print(table.coef, digits = 3)
  write.table(table.coef, "clipboard", sep="\t")

  
  #MODELO 3:
  #Modelo espacial
  
  dbEV_mod3 <- select(dbEV, INF, house_type, chicken_coop, objects, FAMD_Dim1, dist_lum, num_S_infest_50m, num_S_infest_100m, dist.infest)
  dbEV_mod3 <- dbEV_mod3[complete.cases(dbEV_mod3), ]
  
  fit3<-logistf(data=dbEV_mod3, INF ~ house_type + chicken_coop + objects + FAMD_Dim1*num_S_infest_100m, control = logistf.control(maxit = 10000, maxstep = 1), na.action = na.fail)
  summary(fit3)
  
  # Note: VIF computation with logistf is not directly supported, 
  # so we use a standard logistic regression model to compute VIF
  
  #Coefficients and confidence intervals
  ci<-confint(fit3)
  table.coef <- exp(cbind (OR=coef(fit3),ci))
  print(table.coef, digits = 3)
  write.table(table.coef, "clipboard", sep="\t")
  
  library(MuMIn)
  MMI <- dredge(fit3)
  MMI
  write.table(MMI, "clipboard", sep="\t")
  
  MAS <- model.avg(MMI)
  summary(MAS)
  sw(MAS)
  
  #Coefficients and confidence intervals
  ci<-confint(MAS)
  
  # Extracting p-values from the summary of the conditional average model
  summary_MAS <- summary(MAS)
  pval <- summary_MAS$coefmat.subset[, "Pr(>|z|)"]
  
  # Combining OR, CI, and p-values into a table
  table.coef <- cbind(exp(cbind (OR=coef(MAS),ci)),pval)
  print(table.coef, digits = 3)
  write.table(table.coef, "clipboard", sep="\t")