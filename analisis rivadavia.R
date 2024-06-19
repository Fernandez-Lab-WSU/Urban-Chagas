
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
                            "idcerca_NE",
                            "idcerca_S",
                            "education_level",
                            "employer",
                            "employee",
                            "selfemployed",
                            "housework",
                            "not.employed",
                            "pension",
                            "benefit",
                            "health"))

# Create a list to store the plots
plots2 <- list()

# Create plots for each remaining categorical variable and store in the list
for (var in numeric_vars) {
  plots2[[var]] <- create_stacked_bar_plot(db, var, "INF")
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
                 "dist_NE",
                 "dist_S",
                 "num_infest_50m",
                 "num_infest_100m",
                 "num_S",
                 "num_S_infest_50m",
                 "num_S_infest_100m",
                 "num_NE",
                 "num_NW_infest_50m",
                 "num_NW_infest_100m")
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
for (var in numeric_vars) {
  plots2[[var]] <- create_violin_plot(db, var, "INF")
}

# Arrange the plots in a 4x4 grid
grid.arrange(grobs = plots2, ncol = 5, nrow = 4)



##grafico de variables num por inf/noinf
##correlation matrix animals - PCA or FDMA to include rats and pigeons
##Hierarchical model: type of house
##MCA: socio-economic - dejar dwelling status outside and check connection
##interaction between socio-economic and type of house
##insecticide use (and type) vs socioeconomic
#MODELO 1:
#type of house/animals/socio-economic/chickcoop/palm/watertank/objects/travelrural/insecticide - check interaction with socio-economic and travel

#MODELO 2:
#Modelo espacial

#MODELO 3: 
#integracion

