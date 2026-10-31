# Muneef Muhammed

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(shinyWidgets)
library(caret)
library(cluster)
library(pROC)
library(factoextra)
library(rpart)
library(caTools)

# Load the dataset
data <- read.csv("data/countries_and_death_causes.csv")

non_countries <- c("American Samoa", "Bermuda", "Cook Islands", "Greenland", 
                   "Guam", "Northern Mariana Islands", "Puerto Rico", 
                   "Tokelau", "United States Virgin Islands", "World", "Palestine")

countries <- data[!data$Code == '', 1]
countries_filtered <- setdiff(countries, non_countries)

filtered_data <- data[data$Entity %in% countries_filtered, ] %>%
  dplyr::select(-Code)

# Create the death_rate_binary variable based on some numeric death rate column
selected_features_reduced_3 <- filtered_data[, c("Diet.high.in.sodium", "Alochol.use", 
                                                 "Diet.low.in.nuts.and.seeds", "Diet.low.in.Vegetables", 
                                                 "Air.pollution", "Drug.use")]

# Apply log transformation to numeric columns
log_transformed_data <- filtered_data %>%
  mutate(across(where(is.numeric), ~ log(. + 1)))

# Create the binary target variable
selected_features_reduced_3$death_rate_binary <- ifelse(
  log_transformed_data$High.systolic.blood.pressure > mean(log_transformed_data$High.systolic.blood.pressure, na.rm = TRUE), 
  1, 0
)

selected_features_reduced_3$death_rate_binary <- factor(selected_features_reduced_3$death_rate_binary)

# Train the KNN model for later use
set.seed(123)
train_index <- createDataPartition(selected_features_reduced_3$death_rate_binary, p = 0.7, list = FALSE)
train_data <- selected_features_reduced_3[train_index, ]
test_data <- selected_features_reduced_3[-train_index, ]

knn_model <- train(death_rate_binary ~ ., data = train_data, method = "knn", preProcess = c("center", "scale"), 
                   trControl = trainControl(method = "cv", number = 5, allowParallel = FALSE))


# Define the UI for the Shiny App
ui <- fluidPage(
  titlePanel("Cause of Death Trends"),
  
  tabsetPanel(id = "tabs",
              tabPanel(
                "Visualization",
                sidebarLayout(
                  sidebarPanel(
                    selectInput("data_type", "Select Data Type:", choices = c("Full Data", "Country", "Region")),
                    selectInput("analysis_type", "Select Analysis Type:", choices = c("Country-based", "Cause of Death-based")),
                    conditionalPanel(
                      condition = "input.analysis_type == 'Country-based'",
                      selectInput("global_cause", "Select Cause of Death:", choices = colnames(filtered_data)[4:ncol(filtered_data)]),
                      selectInput("country_order", "Display:", choices = c("Top", "Bottom")),
                      sliderInput("num_countries", "Number of Countries to Display:", min = 5, max = length(unique(filtered_data$Entity)), value = 20, step = 1)
                    ),
                    conditionalPanel(
                      condition = "input.analysis_type == 'Cause of Death-based'",
                      selectInput("country", "Select Country:", choices = unique(countries_filtered)),
                      pickerInput("causes", "Select Causes of Death:", choices = colnames(filtered_data)[4:ncol(filtered_data)], 
                                  multiple = TRUE, options = list(`actions-box` = TRUE), selected = colnames(filtered_data)[4])
                    )
                  ),
                  mainPanel(
                    plotOutput("mainPlot")
                  )
                )
              ),
              
              # Single Variable Model Tab
              tabPanel(
                "Single Variable Model",  
                sidebarLayout(
                  sidebarPanel(
                    pickerInput("single_var", "Select Variables for Single Variable Model:", 
                                choices = colnames(selected_features_reduced_3)[1:6], 
                                multiple = TRUE, options = list(`actions-box` = TRUE))  # Allows selecting multiple variables
                  ),
                  mainPanel(
                    h4("ROC Curve for Selected Variables"),
                    plotOutput("rocPlot"),
                    h4("Model Summary and Comparison"),
                    tableOutput("comparisonTable"),
                    verbatimTextOutput("singleVarModelOutput")
                  )
                )
              ),
              
              tabPanel(
                "Multi Variable Model",
                sidebarLayout(
                  sidebarPanel(
                    selectInput("feature_set", "Select Feature Set:", choices = c("Feature Set 1")),
                    pickerInput("models", "Select Models to Compare for Feature Set 1:", 
                                choices = c("Logistic Regression", "Decision Tree", "KNN"), 
                                multiple = TRUE, 
                                selected = "Logistic Regression",  
                                options = list(`actions-box` = TRUE))
                  ),
                  mainPanel(
                    h4("Selected Features:"),
                    uiOutput("selectedFeatures"),  # Display selected features as bullet points
                    h4("ROC Curve Comparison"),
                    plotOutput("rocPlotMulti"),
                    h4("Model Comparison Table"),
                    tableOutput("modelComparisonTable")
                  )
                )
              ),
              
              tabPanel(
                "Clustering",
                sidebarLayout(
                  sidebarPanel(
                    numericInput("num_clusters", "Select Number of Clusters:", min = 2, max = 10, value = 3)
                  ),
                  mainPanel(
                    h4("Clustering Results"),
                    plotOutput("clusterPlot"),
                    plotOutput("elbowPlot")
                  )
                )
              )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Dynamically update the country/region selectInput based on the selected data type
  observeEvent(input$data_type, {
    if (input$data_type == "Country") {
      updateSelectInput(session, "country",
                        label = "Select Country:",
                        choices = unique(countries_filtered))
    } else if (input$data_type == "Region") {
      updateSelectInput(session, "country",
                        label = "Select Region:",
                        choices = non_countries)
    } else {
      updateSelectInput(session, "country",
                        label = "Select Country/Region:",
                        choices = c(unique(countries_filtered), non_countries))
    }
  })
  
  # Visualization Tab
  output$mainPlot <- renderPlot({
    req(input$analysis_type, input$data_type)
    
    # Filter data based on the selected data type (Full Data, Country, or Region)
    if (input$data_type == "Country") {
      plot_data <- filtered_data %>%
        filter(Entity %in% countries_filtered)
    } else if (input$data_type == "Region") {
      plot_data <- data %>% 
        filter(Entity %in% non_countries) # Filter for regions from the full data
    } else {
      plot_data <- filtered_data
    }
    
    req(nrow(plot_data) > 0) # Ensure plot_data is not empty
    
    if (input$analysis_type == "Cause of Death-based") {
      req(input$country, input$causes)
      
      country_data <- plot_data %>%
        filter(Entity == input$country)
      
      long_data <- country_data %>%
        select(Year, all_of(input$causes)) %>%
        gather(key = "Cause", value = "Deaths", -Year)
      
      cause_totals <- long_data %>%
        group_by(Cause) %>%
        summarise(Total_Deaths = sum(Deaths, na.rm = TRUE)) %>%
        arrange(desc(Total_Deaths))
      
      long_data$Cause <- factor(long_data$Cause, levels = cause_totals$Cause)
      
      ggplot(long_data, aes(x = Year, y = Deaths, color = Cause)) +
        geom_line(size = 1) +
        geom_point() +
        labs(title = paste("Causes of Death in", input$country), x = "Year", y = "Frequency of Deaths", color = "Cause of Death") +  
        theme_minimal() +
        scale_y_continuous(labels = comma)
      
    } else if (input$analysis_type == "Country-based") {
      req(input$global_cause, input$num_countries, input$country_order)
      
      global_data <- plot_data %>%
        group_by(Entity) %>%
        summarise(Total_Deaths = sum(!!sym(input$global_cause), na.rm = TRUE)) %>%
        arrange(desc(Total_Deaths))
      
      if (input$country_order == "Top") {
        global_data <- global_data %>% slice_head(n = input$num_countries)
      } else {
        global_data <- global_data %>% slice_tail(n = input$num_countries)
      }
      
      ggplot(global_data, aes(x = reorder(Entity, Total_Deaths * if (input$country_order == "Top") 1 else -1), y = Total_Deaths)) +
        geom_bar(stat = "identity", fill = "skyblue", color = "black") +
        geom_text(aes(label = comma(Total_Deaths)), vjust = 0.5, color = "black", size = 4, hjust = -0.1) +
        coord_flip() +
        labs(title = paste("Total", input$global_cause, "Deaths (", input$country_order, input$num_countries, "Countries)"), 
             x = "Country/Region", y = "Total Deaths") +
        theme_minimal() +
        scale_y_continuous(labels = comma, expand = expansion(mult = c(0.05, 0.2))) +
        theme(axis.text.y = element_text(size = 10))
    }
  })
  
  # Clustering logic
  observeEvent(input$tabs == "Clustering", {
    
    # Use the filtered data for clustering
    dataset <- reactive({
      # Select numeric columns, excluding "Year" and any non-numeric columns
      numeric_data <- filtered_data %>%
        select_if(is.numeric) %>%
        select(-Year) # Exclude 'Year' if present in your data
      numeric_data <- data.frame(lapply(numeric_data, as.numeric), check.names = FALSE)
      return(numeric_data)
    })
    
    cluster_data_scaled <- reactive({
      # Apply log transformation and scaling to the numeric data
      log_transformed_data <- log(dataset() + 1)  # Add 1 to avoid log(0)
      scale(log_transformed_data)
    })
    
    output$clusterPlot <- renderPlot({
      set.seed(123)
      # Perform K-means clustering
      kmeans_model <- kmeans(cluster_data_scaled(), centers = input$num_clusters, nstart = 25)
      
      # Visualize the clusters
      fviz_cluster(kmeans_model, data = cluster_data_scaled(), geom = "point", ellipse.type = "convex", ggtheme = theme_minimal()) +
        labs(title = paste("K-Means Clustering with", input$num_clusters, "Clusters"))
    })
  })
  
  # Single Variable Model logic
  observeEvent(input$tabs == "Single Variable Model", {
    
    train_test_data <- reactive({
      set.seed(123)
      train_index <- createDataPartition(selected_features_reduced_3$death_rate_binary, p = 0.7, list = FALSE)
      train_data <- selected_features_reduced_3[train_index, ]
      test_data <- selected_features_reduced_3[-train_index, ]
      list(train_data = train_data, test_data = test_data)
    })
    
    selected_variables <- reactive({
      req(input$single_var)
      input$single_var
    })
    
    auc_values <- reactiveVal(data.frame(Variable = character(), AUC = numeric(), Accuracy = numeric(), stringsAsFactors = FALSE))
    
    output$rocPlot <- renderPlot({
      req(input$single_var)
      variables <- selected_variables()
      colors <- rainbow(length(variables))
      auc_data <- data.frame(Variable = character(), AUC = numeric(), Accuracy = numeric(), stringsAsFactors = FALSE)
      
      for (i in seq_along(variables)) {
        var <- variables[i]
        formula <- as.formula(paste("death_rate_binary ~", var))
        train_data <- train_test_data()$train_data
        test_data <- train_test_data()$test_data
        
        single_var_model <- glm(formula, family = "binomial", data = train_data)
        single_var_probabilities <- predict(single_var_model, newdata = test_data, type = "response")
        single_var_predictions <- ifelse(single_var_probabilities > 0.5, 1, 0)
        
        roc_curve <- roc(test_data$death_rate_binary, single_var_probabilities, quiet = TRUE)
        auc_value <- auc(roc_curve)
        accuracy_value <- mean(single_var_predictions == test_data$death_rate_binary)
        
        auc_data <- rbind(auc_data, data.frame(Variable = var, AUC = as.numeric(auc_value), Accuracy = accuracy_value))
        
        if (i == 1) {
          plot(roc_curve, col = colors[i], lwd = 2, main = "ROC Curve Comparison", legacy.axes = TRUE)
        } else {
          plot(roc_curve, col = colors[i], lwd = 2, add = TRUE, legacy.axes = TRUE)
        }
      }
      
      legend("bottomright", legend = variables, col = colors, lwd = 2, title = "Variables", cex = 0.5)
      auc_values(auc_data)
    })
    
    output$comparisonTable <- renderTable({
      req(input$single_var)
      auc_data <- auc_values()
      req(nrow(auc_data) > 0)
      auc_data_sorted <- auc_data[order(-auc_data$AUC), ]
      auc_data_sorted
    })
    
    output$singleVarModelOutput <- renderPrint({
      req(input$single_var)
      auc_data <- auc_values()
      req(nrow(auc_data) > 0)
      auc_data_sorted <- auc_data[order(-auc_data$AUC), ]
      best_variable <- auc_data_sorted$Variable[1]
      train_data <- train_test_data()$train_data
      test_data <- train_test_data()$test_data
      best_model <- glm(as.formula(paste("death_rate_binary ~", best_variable)), family = "binomial", data = train_data)
      best_probabilities <- predict(best_model, newdata = test_data, type = "response")
      best_predictions <- ifelse(best_probabilities > 0.5, 1, 0)
      confusion_matrix <- confusionMatrix(as.factor(best_predictions), as.factor(test_data$death_rate_binary))
    })
  })
  
  # Multivariable Model Tab Logic (Feature Set 1)
  observeEvent(input$tabs == "Multi Variable Model", {
    
    train_test_data <- reactive({
      set.seed(123)
      train_index <- createDataPartition(selected_features_reduced_3$death_rate_binary, p = 0.7, list = FALSE)
      train_data <- selected_features_reduced_3[train_index, ]
      test_data <- selected_features_reduced_3[-train_index, ]
      list(train_data = train_data, test_data = test_data)
    })
    
    feature_set_1 <- c("Diet.high.in.sodium", "Alochol.use", "Diet.low.in.nuts.and.seeds", "Diet.low.in.Vegetables", "Air.pollution", "Drug.use")
    
    output$selectedFeatures <- renderUI({
      tags$ul(
        lapply(feature_set_1, function(feature) {
          tags$li(feature)
        })
      )
    })
    
    trained_models <- reactive({
      req(input$models)
      models <- list()
      train_data <- train_test_data()$train_data
      selected_features <- feature_set_1
      
      if ("Logistic Regression" %in% input$models) {
        logistic_model <- glm(death_rate_binary ~ ., data = train_data[, c(selected_features, "death_rate_binary")], family = "binomial")
        models$logistic <- list(model = logistic_model, type = "logistic")
      }
      
      if ("Decision Tree" %in% input$models) {
        tree_model <- rpart(death_rate_binary ~ ., data = train_data[, c(selected_features, "death_rate_binary")], method = "class")
        models$tree <- list(model = tree_model, type = "tree")
      }
      
      if ("KNN" %in% input$models) {
        models$knn_model <- list(model = knn_model, type = "knn")
      }
      
      return(models)
    })
    
    model_metrics <- reactive({
      metrics_list <- list()
      test_data <- train_test_data()$test_data
      
      for (model_name in names(trained_models())) {
        model_info <- trained_models()[[model_name]]
        model <- model_info$model
        model_type <- model_info$type
        
        if (model_type == "logistic") {
          prob <- predict(model, newdata = test_data, type = "response")
        } else if (model_type == "tree") {
          prob <- predict(model, newdata = test_data, type = "prob")[, 2]
        } else if (model_type == "knn") {
          prob <- predict(model, newdata = test_data, type = "prob")[, 2]
        }
        
        roc_curve <- roc(test_data$death_rate_binary, prob)
        auc_value <- auc(roc_curve)
        predicted_class <- ifelse(prob > 0.9, 1, 0)
        accuracy <- mean(predicted_class == test_data$death_rate_binary)
        
        metrics_list[[model_name]] <- list(auc = auc_value, accuracy = accuracy, roc = roc_curve)
      }
      
      return(metrics_list)
    })
    
    output$modelComparisonTable <- renderTable({
      metrics <- model_metrics()
      metrics_df <- data.frame(
        Model = names(metrics),
        AUC = sapply(metrics, function(x) x$auc),
        Accuracy = sapply(metrics, function(x) x$accuracy)
      )
      return(metrics_df)
    })
    
    output$rocPlotMulti <- renderPlot({
      metrics <- model_metrics()
      colors <- c("blue", "red", "green")
      i <- 1
      for (model_name in names(metrics)) {
        roc_curve <- metrics[[model_name]]$roc
        if (i == 1) {
          plot(roc_curve, col = colors[i], lwd = 2, main = "ROC Curve Comparison", legacy.axes = TRUE)
        } else {
          plot(roc_curve, col = colors[i], lwd = 2, add = TRUE, legacy.axes = TRUE)
        }
        i <- i + 1
      }
      legend("bottomright", legend = names(metrics), col = colors[1:length(metrics)], lwd = 2)
    })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
