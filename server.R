library(shinydashboard)
library(MuMIn)
library(modelr)
library(maps)
library(mapproj)
library(ggthemes)
library(caret)

# Read in EPEST and CDL datasets
epest_state <- read_csv("epest_state.csv") %>% 
  select(-X1)
cdl <- read_csv("cdl.csv")

# Summarize EPest at the national level
epest_nation <- epest_state %>%
  group_by(compound, year) %>%
  summarize(epest_low_kg = sum(epest_low_kg, na.rm = TRUE),
            epest_high_kg = sum(epest_high_kg, na.rm = TRUE))

# Read in state borders for US map
us_states <- map_data("state")

# create base choropleths for CDL
us_states_yr_08_18 <- bind_cols(us_states[rep(row.names(us_states), 11), ],
                          year = rep(2008:2018, each = dim(us_states)[1]))
us_choropleth_08_18 <- ggplot(us_states_yr_08_18, aes(long, lat, group = group))

# read in BIP data set and munge
bip <- read_csv("bip.csv") %>%
  select(-X1, -region) %>%
  filter(!state %in% c("Hawaii", "Alaska", "Puerto Rico", "Other", "MultiStateOperation", NA),
         mso_calc_type != "MSO only") %>%
  mutate(state = tolower(state),
         year_chr = year,
         year = as.numeric(substr(year_chr, 1, 4))) %>%
  arrange(year)

# Rename column names for BIP
 colnames(bip)[3:dim(bip)[2] - 2] <- gsub("season.", "", colnames(bip)[3:dim(bip)[2] - 2])
 colnames(bip)[3:dim(bip)[2] - 2] <- gsub("\\.", "_", colnames(bip)[3:dim(bip)[2] - 2])

# define bip subcategories
bip_subs <- c("Average Losses (%)" = "avg_loss",
              "Beekeepers" = "beekeepers",
              "Colonies" = "colonies",
              "Exclusive Beekeepers (%)" = "exclusive_beekeepers",
              "Exclusive Colonies (%)" = "exclusive_colonies",
              "Backyard Operations" = "operation_backyard",
              "Commercial Operations" = "operation_commercial",
              "Sideline Operations" = "operation_sideline",
              "Total Losses (%)" = "total_loss")

# create base choropleths for BIP
us_states_bip <- bind_cols(us_states[rep(row.names(us_states), 12), ],
                                year_chr = rep(unique(bip$year_chr), each = dim(us_states)[1]))
us_choropleth_bip <- ggplot(us_states_bip, aes(long, lat, group = group))

###########################################################################################
# Random forest 
# prepare epest data
epest_rf <- epest_state %>% 
  select(-epest_low_kg) %>% 
  filter(year > 2007) %>% 
  spread(key = compound, value = epest_high_kg) 

epest_rf[is.na(epest_rf)] <- 0

# prepare cdl data
cdl_rf <- cdl %>% 
  select(year, state, category, acreage) %>% 
  filter(year != 2018, state != "national") %>% 
  spread(key = category, value = acreage)

cdl_rf[is.na(cdl_rf)] <- 0

# prepare bee data
bip_rf <- bip %>%
  filter(mso_calc_type == "MSO in", 
         year != 2018, 
         year != 2007,
         !is.na(winter_total_loss),
         state != "district of columbia") %>% 
  select(year, state, winter_total_loss)

# create dataset for random forest
data_rf <- left_join(bip_rf, epest_rf, by = c("state", "year")) %>%
  filter(!(state == "california" & year == 2017)) %>% 
  left_join(cdl_rf, by = c("state", "year"))

# create test and training datasets
train_rf_rows <- sample(1:nrow(data_rf), size = nrow(data_rf) * 0.8)
test_rf_rows <- setdiff(1:nrow(data_rf), train_rf_rows)
train_rf <- data_rf[train_rf_rows, ]
test_rf <- data_rf[test_rf_rows, ]

# train control method defined
train_ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

function(input, output) {
  
  ########################################################################################
  # EPEST
  # Print out the selected EPEST compounds
  output$selected_epest_cmpds <- renderText({
    if(length(input$compounds) == 0) {
      "none"
    } else if(length(input$compounds) == 1) {
      input$compounds
    } else if(length(input$compounds) == 2) {
      paste0(input$compounds[1], " & ", input$compounds[2])
    } else {
      cmpds <- input$compounds
      for(i in 1:(length(cmpds) - 2)) {
        cmpds[i] <- paste0(cmpds[i], "; ")
      }
      cmpds[length(cmpds)] <- paste0(" & ", cmpds[length(cmpds)])
      cmpds
    }
  })
  
  # create choropleths with EPEST data
  epest_ch <- reactive({

    # User input to pick low or high EPEST estimates
    if(input$epest_choice == "High") {
      graph_fill <- "epest_high_kg"
      fill_label <- "EPEST High (kg)"
    } else {
      graph_fill <- "epest_low_kg"
      fill_label <- "EPEST Low (kg)"
    }

    # create epest sub-dataset for plotting
    epest_plot <- epest_state %>%
      filter(compound %in% input$compounds) %>%
      select(compound, year, state, graph_fill) %>%
      spread(key = compound, value = graph_fill) %>%
      group_by(year, state) %>%
      mutate(epest = sum(!!!syms(input$compounds), na.rm = TRUE))

    # Number of years's worth of maps to create
    if(length(input$compounds) == 0) {
      yr_rep <-  1
      yr_span <- NA
    } else {
      yr_rep <- max(epest_plot$year) - min(epest_plot$year) + 1
      yr_span <- min(epest_plot$year):max(epest_plot$year)
    }

    # create map data by year
    us_states_yr <- bind_cols(us_states[rep(row.names(us_states), yr_rep), ],
                              year = rep(yr_span, each = dim(us_states)[1]))

    # add EPEST data to map/year data
    plotdata <- left_join(us_states_yr,
                          select(epest_plot, year, state, epest),
                          by = c("region" = "state", "year"))

    # Plot choropleths EPEST by year
    ggplot(data = plotdata, aes(long, lat, group = group, fill = epest)) +
      geom_polygon(color = "black") +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      scale_fill_continuous(low = "white", high = "darkred", na.value = "white",
                            limits = c(0, NA)) +
      theme_map() +
      facet_wrap(~ year) +
      theme(legend.position = "bottom", strip.background = element_blank()) +
      labs(fill = fill_label) +
      theme(strip.text.x = element_text(size = 12),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12),
            legend.key.width = unit(8, "line"))
  })
  
  # render epest choropleth plots
  output$epest_choropleths <- renderPlot(epest_ch())
  
  # Download EPEST choropleth
  output$epest_choropleth_dl.png <- downloadHandler(
    filename = "epest_choropleth.png",
    content = function(file) {
      ggsave(epest_ch(), filename = file, device = "png")
      })
  
  # Generate plot of EPEST values by year for the whole country
  output$epest_us <- renderPlot({
    epest_nation %>% 
      filter(compound %in% input$compounds) %>% 
      group_by(year) %>% 
      summarize(epest_high = sum(epest_high_kg),
                epest_low = sum(epest_low_kg)) %>% 
      ggplot(aes(x = year)) +
      geom_bar(aes(y = epest_high), stat = "identity", fill = "darkred") +
      geom_bar(aes(y = epest_low), stat = "identity", fill = "black") +
      xlab("Year") +
      ylab("Nationwide EPEST (kg)")
  })
  
  ########################################################################################
  # EPEST National
  # create data subset
  get_epest_natl <- reactive({
    
    # User input to pick low or high EPEST estimates
    if(input$epest_choice == "High") {
      epest_natl <- select(epest_nation, -epest_low_kg) %>% 
        rename(mass = epest_high_kg)
    } else {
      epest_natl <- select(epest_nation, -epest_high_kg) %>% 
        rename(mass = epest_low_kg)
    }
    
    # filter by years and mass cutoff
    epest_natl <- epest_natl %>% 
      filter(year >= input$epest_yr_natl[1], year <= input$epest_yr_natl[2]) %>% 
      group_by(compound) %>% 
      summarize(total_mass = sum(mass),
                avg_mass = mean(mass),
                min_mass = min(mass),
                max_mass = max(mass),
                med_mass = median(mass)) %>% 
      filter(total_mass >= 10^input$epest_mass_natl)
  })
  
  # Render new compound selection in UI
  output$cmpd_select <- renderUI({
    checkboxGroupInput("compounds_natl", "Pick compounds to examine",
                       choices = unique(get_epest_natl()$compound),
                       selected = unique(get_epest_natl()$compound))
  })
  
  # Render numerical summaries
  output$epest_natl_table <- renderTable({
    get_epest_natl() %>% 
      arrange(-total_mass) %>% 
      select(compound, min_mass, med_mass, avg_mass, max_mass)
  })
  
  ########################################################################################
  # CDL Maps
  # Print out the selected CDL categories
  output$selected_cdl_cats <- renderText({
    if(length(input$land_use) == 0) {
      "none"
    } else if(length(input$land_use) == 1) {
      input$land_use
    } else if(length(input$land_use) == 2) {
      paste0(input$land_use[1], " & ", input$land_use[2])
    } else {
      cmpds <- input$land_use
      for(i in 1:(length(cmpds) - 2)) {
        cmpds[i] <- paste0(cmpds[i], ", ")
      }
      cmpds[length(cmpds)] <- paste0(" & ", cmpds[length(cmpds)])
      cmpds
    }
  })
  
  # Generate choropleths for CDL
  output$cdl_choropleths <- renderPlot({
    
    # create cdl subset of data for plotting
    cdl_plot <- cdl %>% 
      filter(state != "national" & category %in% input$land_use) %>% 
      spread(key = category, value = acreage) %>% 
      group_by(state, year) %>% 
      mutate(acres = sum(!!!syms(input$land_use), na.rm = TRUE))
    
    # add EPEST data to map/year data
    plotdata <- left_join(us_states_yr_08_18,
                          select(cdl_plot, year, state, acres),
                          by = c("region" = "state", "year"))
    
    # Plot choropleths CDL acreages by year
    us_choropleth_08_18 +
      geom_polygon(data = plotdata, aes(fill = acres), color = "black") +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      scale_fill_continuous(low = "white", high = "darkred", na.value = "white",
                            limits = c(0, NA)) +
      theme_map() +
      facet_wrap(~ year) +
      theme(legend.position = "bottom", strip.background = element_blank()) +
      labs(fill = "CDL Acreage") +
      theme(strip.text.x = element_text(size = 12),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12),
            legend.key.width = unit(8, "line"))
  })
  
  # Generate CDL nationwide bar plot
  output$cdl_us <- renderPlot({
    cdl %>% 
      filter(state == "national", category %in% input$land_use) %>% 
      group_by(year) %>% 
      summarize(acres = sum(acreage)) %>% 
      ggplot(aes(x = year)) +
      geom_bar(aes(y = acres), stat = "identity", fill = "black") +
      xlab("Year") +
      ylab("CDL Acreage")
  })
  
  ########################################################################################
  # CDL Table
  # Print out the selected CDL categories
  output$selected_cdl_cats_2 <- renderText({
    if(length(input$land_use_2) == 0) {
      "none"
    } else if(length(input$land_use_2) == 1) {
      input$land_use_2
    } else if(length(input$land_use_2) == 2) {
      paste0(input$land_use_2[1], " & ", input$land_use_2[2])
    } else {
      cmpds <- input$land_use_2
      for(i in 1:(length(cmpds) - 2)) {
        cmpds[i] <- paste0(cmpds[i], ", ")
      }
      cmpds[length(cmpds)] <- paste0(" & ", cmpds[length(cmpds)])
      cmpds
    }
  })
  
  # Function to create selected CDL table
  cdl_table_data <- reactive({
    cdl %>% 
      filter(category %in% input$land_use_2) %>% 
      spread(key = category, value = acreage) %>% 
      group_by(state, year) %>% 
      mutate(total_acres = sum(!!!syms(input$land_use_2), na.rm = TRUE))
  })
  
  # Render CDL table
  output$cdl_table <- renderDT(cdl_table_data(), filter = "top")
  
  # Download CDL table
  output$select_cdl_table.csv <- downloadHandler(
    filename = "select_cdl_table.csv",
    content = function(file) {
      write_csv(cdl_table_data(), file)
    })
  
  ########################################################################################
  # BIP
  # Generate BIP Choropleths
  output$bip_choropleths <- renderPlot({
    
    # create bip subset of data for plotting
    if(input$mso == "Exclude") {
      plot_bip <- bip %>%
        filter(mso_calc_type == "MSO out")
    } else {
      plot_bip <- bip %>% 
        filter(mso_calc_type == "MSO in")
    }
    
    bip_measure <- paste(tolower(input$colony_loss_span), 
                         input$bip_category,
                         sep = "_")

    plot_data <- left_join(as_tibble(us_states_bip),
                           select(plot_bip, year_chr, state, bip_measure),
                           by = c("region" = "state", "year_chr"))
    
    # plot choropleths of BIP data by year
    us_choropleth_bip +
      geom_polygon(data = plot_data, aes(fill = !!sym(bip_measure))) +
      facet_wrap(~ year_chr) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      scale_fill_continuous(low = "white", high = "darkred", na.value = "grey",
                            limits = c(0, NA)) +
      theme_map() +
      theme(legend.position = "bottom", strip.background = element_blank()) +
      labs(fill = bip_subs[input$bip_category]) +
      theme(strip.text.x = element_text(size = 12),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12),
            legend.key.width = unit(8, "line"))
  })
  
  ########################################################################################
  # Simple Correlations
  # Create dataset for plotting
  simple_plotdata <- reactive({
    
    if(input$simple_x_axis == "Pesticide Use (EPEST)") {
      x_values <- epest_state %>% 
        ungroup() %>% 
        filter(compound == input$x_axis_value_epest) %>% 
        select(year, state, epest_high_kg) %>% 
        rename(x_val = epest_high_kg)
    } else if(input$simple_x_axis == "Land Use (CDL)") {
      x_values <- cdl %>%
        filter(category == input$x_axis_value_cdl,
               state != "national") %>% 
        select(year, state, acreage) %>% 
        rename(x_val = acreage)
    } else {
      x_val <- paste(tolower(input$x_axis_value_bip1), 
                     input$x_axis_value_bip3,
                     sep = "_")
      if(input$x_axis_value_bip2 == "Exclude") {
        x_values <- bip %>%
          filter(mso_calc_type == "MSO out") %>% 
          mutate(x_val = !!sym(x_val)) %>% 
          select(year, state, x_val)
      } else {
        x_values <- bip %>% 
          filter(mso_calc_type == "MSO in") %>% 
          mutate(x_val = !!sym(x_val)) %>% 
          select(year, state, x_val) 
      }
    }
    
    if(input$simple_y_axis == "Pesticide Use (EPEST)") {
      y_values <- epest_state %>% 
        ungroup() %>% 
        filter(compound == input$y_axis_value_epest) %>% 
        select(year, state, epest_high_kg) %>% 
        rename(y_val = epest_high_kg)
    } else if(input$simple_y_axis == "Land Use (CDL)") {
      y_values <- cdl %>%
        filter(category ==input$y_axis_value_cdl,
               state != "national") %>% 
        select(year, state, acreage) %>% 
        rename(y_val = acreage)
    } else {
      y_val <- paste(tolower(input$y_axis_value_bip1), 
                     input$y_axis_value_bip3,
                     sep = "_")
      if(input$y_axis_value_bip2 == "Exclude") {
        y_values <- bip %>%
          filter(mso_calc_type == "MSO out") %>% 
          mutate(y_val = !!sym(y_val)) %>% 
          select(year, state, y_val)
      } else {
        y_values <- bip %>% 
          filter(mso_calc_type == "MSO in") %>% 
          mutate(y_val = !!sym(y_val)) %>% 
          select(year, state, y_val)
      }
    }
    
    left_join(x_values, y_values, by = c("year", "state")) %>% 
      filter(!is.na(y_val)) %>% 
      mutate(year = as.integer(year))
  })
   
  # Create simple correlation plot 
  output$simple_correlation <- renderPlotly({
    ggplot(data = simple_plotdata(), aes(x_val, y_val)) + 
      geom_point() +
      geom_smooth(method = lm) 
  })
  
  # Create data table for plotting values
  output$corr_table <- renderDataTable(simple_plotdata())
    
  ########################################################################################
  # Multiple Linear Regression
  
  # mlr calculations
  mlr_data <- reactive({
    
    # set high or low EPEST
    if(input$epest_choice_mlr == "High") {
      epest_cat_mlr <- "epest_high_kg"
    } else {
      epest_cat_mlr <- "epest_low_kg"
    }
    
    # crease epest subset based on user input
    epest_mlr <- epest_state %>% 
      filter(compound %in% input$compounds_mlr) %>% 
      select(compound, year, state, epest_cat_mlr) %>% 
      spread(key = compound, value = epest_cat_mlr) 
    
    epest_mlr[is.na(epest_mlr)] <- 0
    
    # create cdl subset based on user input
    cdl_mlr <- cdl %>% 
      filter(state != "national", category %in% input$land_use_mlr) %>% 
      select(category, year, state, acreage) %>% 
      spread(key = category, value = acreage)
    
    cdl_mlr[is.na(cdl_mlr)] <- 0
    
    # create bip subset based on user input
    if(input$mso_mlr == "Exclude") {
      bip_mlr <- filter(bip, mso_calc_type == "MSO out")
    } else {
      bip_mlr <- filter(bip, mso_calc_type == "MSO in")
    }
    
    if(input$bip_category_mlr == "Average Losses (%)") {
      bip_measure_mlr <- paste0(tolower(input$colony_loss_span_mlr), "_avg_loss")
    } else {
      bip_measure_mlr <- paste0(tolower(input$colony_loss_span_mlr), "_total_loss")
    }
    
    bip_mlr <- select(bip_mlr, state, year, bip_measure_mlr)
    
    # combine mlr datasets
    left_join(epest_mlr, cdl_mlr, by = c("state", "year")) %>% 
      left_join(bip_mlr, by = c("state", "year")) %>% 
      drop_na() %>% 
      select(-state) %>% 
      rename(loss = !!sym(bip_measure_mlr))
  })
    
  # set up validation data sets
  train1 <- reactive({
    train1 <- sample(1:nrow(mlr_data()), size = nrow(mlr_data())*0.8)
  })
  
  mlr_train <- reactive({mlr_data()[train1(), ]})
  
  # fit mlr model
  mlr_calc <- reactive({lm(loss ~ ., data = mlr_train())})
  
  # Render summary of MLR
  output$mlr_summary <- renderPrint({summary(mlr_calc())})
  
  # Render actual versus predicted plot for MLR
  output$mlr_plot <- renderPlot({
    test <- setdiff(1:nrow(mlr_data()), train1())
    mlr_test <- mlr_data()[test, ]
    
    mlr_test_pred <- predict(mlr_calc(), newdata = mlr_test)
    mlr_train_pred <- predict(mlr_calc(), newdata = mlr_train())

    mlr_values <- bind_rows(tibble(Predicted = mlr_test_pred, Actual = mlr_test$loss,
                                  Group = "Test"),
                           tibble(Predicted = mlr_train_pred, Actual = mlr_train()$loss,
                                  Group = "Train"))

    mlr_test_rmse <- round(sqrt(mean((mlr_test_pred - mlr_test$loss)^2)), 1)
    mlr_train_rmse <- round(sqrt(mean((mlr_train_pred - mlr_train()$loss)^2)), 1)

    mlr_test_label <- paste0("Test (RMSE = ", mlr_test_rmse, ")")
    mlr_train_label <- paste0("Train (RMSE = ", mlr_train_rmse, ")")

    ggplot(mlr_values, aes(Actual, Predicted, color = Group)) +
      geom_point() +
      geom_abline(slope = 1, intercept = 0) +
      scale_color_discrete(labels = c(mlr_test_label, mlr_train_label))
  })
  
  ########################################################################################
  # Random Forest
  
  # fit RF method
  rf_fit <- reactive ({
    train(winter_total_loss ~ ., data = train_rf, method = "rf",
          trControl = train_ctrl,
          tuneGrid = data.frame(mtry = as.numeric(input$rf_mtry)))
  })
  
  # Render summary of RF fit
  output$rf_summary <- renderPrint({print(rf_fit())})
  
  # Render actual versus predicted plot for RF
  output$rf_plot <- renderPlot({
    rf_test_pred <- predict(rf_fit(), newdata = select(test_rf, -winter_total_loss))
    rf_train_pred <- predict(rf_fit(), newdata = select(train_rf, -winter_total_loss))
    
    rf_values <- bind_rows(tibble(Predicted = rf_test_pred, Actual = test_rf$winter_total_loss, 
                     Group = "Test"),
              tibble(Predicted = rf_train_pred, Actual = train_rf$winter_total_loss, 
                     Group = "Train"))
    
    rf_test_rmse <- round(sqrt(mean((rf_test_pred - test_rf$winter_total_loss)^2)), 1)
    rf_train_rmse <- round(sqrt(mean((rf_train_pred - train_rf$winter_total_loss)^2)), 1)
    
    rf_test_label <- paste0("Test (RMSE = ", rf_test_rmse, ")")
    rf_train_label <- paste0("Train (RMSE = ", rf_train_rmse, ")")
    
    ggplot(rf_values, aes(Actual, Predicted, color = Group)) +
      geom_point() +
      geom_abline(slope = 1, intercept = 0) +
      scale_color_discrete(labels = c(rf_test_label, rf_train_label))
  })
  
  ########################################################################################
  # Principal Component Analysis
  
  # pca calculations
  pca_calc <- reactive({
    
    # set high or low EPEST
    if(input$epest_choice_pca == "High") {
      epest_cat_pca <- "epest_high_kg"
    } else {
      epest_cat_pca <- "epest_low_kg"
    }
    
    # crease epest subset based on user input
    epest_pca <- epest_state %>% 
      filter(compound %in% input$compounds_pca) %>% 
      select(compound, year, state, epest_cat_pca) %>% 
      spread(key = compound, value = epest_cat_pca) 
    
    epest_pca[is.na(epest_pca)] <- 0
    
    # create cdl subset based on user input
    cdl_pca <- cdl %>% 
      filter(state != "national", category %in% input$land_use_pca) %>% 
      select(category, year, state, acreage) %>% 
      spread(key = category, value = acreage)
    
    cdl_pca[is.na(cdl_pca)] <- 0
    
    # combine datasets
    pca_data <- left_join(epest_pca, cdl_pca, by = c("state", "year")) %>% 
      drop_na() %>% 
      ungroup() %>% 
      select(-state, -year) 
    
    # Principal component analysis
    prcomp(pca_data)
  })
  
  # render pca output
  output$pca_summary <- renderPrint({
    if(dim(pca_calc()$rotation)[1] > 5) {
      print(pca_calc()$rotation[, 1:5])
    } else {
      print(pca_calc()$rotation)
    }
  })
  
  # render PCA biplot
  output$pca_biplot <- renderPlot(biplot(pca_calc()))
  
  # render PCA screeplot
  output$pca_screeplot <- renderPlot({
    par(mfrow = c(1, 2))
    plot(pca_calc()$sdev^2/sum(pca_calc()$sdev^2), xlab = "Principal Component",
         ylab = "Proportion of Variance Explained", ylim = c(0, 1), type = 'b')
    plot(cumsum(pca_calc()$sdev^2/sum(pca_calc()$sdev^2)), xlab = "Principal Component",
         ylab = "Cum. Prop of Variance Explained", ylim = c(0, 1), type = 'b')
    })
  }