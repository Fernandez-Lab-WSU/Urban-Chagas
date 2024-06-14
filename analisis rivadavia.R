
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
    ggtitle(paste("Proportion of", categorical_var, "by", target_var))
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

categorical_vars

# Create a list to store the plots
plots <- list()

# Create plots for each remaining categorical variable and store in the list
for (var in categorical_vars) {
  plots[[var]] <- create_stacked_bar_plot(db, var, "INF")
}

library(gridExtra)
# Arrange the plots in a 4x4 grid
grid.arrange(grobs = plots, ncol = 4, nrow = 4)





