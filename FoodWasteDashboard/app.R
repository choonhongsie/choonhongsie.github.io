library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)
library(rsconnect)
library(highcharter)
library(randomcoloR)

packageVersion('rsconnect')

remove_background_theme <- function() {
    theme(
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank()
    )
}
FWD_UNEP_source <- read_csv("src/data/UNEP Food Waste Statistics.csv")
FWD_SG <- read_csv("src/data/Food Waste Statistics.csv")
WASTE_SG <- read_csv("src/data/Waste Stats Singapore.csv")

# FWD_UNEP
#Create new column for combined estimate in tonnes/year
FWD_UNEP <- mutate(FWD_UNEP_source, combined_estimate_tonnes_per_year = 
                       FWD_UNEP_source$`Household estimate (tonnes/year)` + 
                       FWD_UNEP_source$`Retail estimate (tonnes/year)` + 
                       FWD_UNEP_source$`Food service estimate (tonnes/year)`)
FWD_UNEP <- FWD_UNEP[, c(1,2,13,3,4,5,6,7,8,9,10,11,12)]

# rename column names to lowercase and include underscores for easier reference
new_col_names <- c("country", 
                   "combined_estimate_kpcpy", 
                   "combined_estimate_tpy", 
                   "household_estimate_kpcpy", 
                   "household_estimate_tpy", 
                   "retail_estimate_kpcpy", 
                   "retail_estimate_tpy", 
                   "food_service_estimate_kpcpy", 
                   "food_service_estimate_tpy", 
                   "estimate_confidence", 
                   "m49_code", 
                   "region", 
                   "source")
names(FWD_UNEP) <- new_col_names

# Remove source column
FWD_UNEP <- FWD_UNEP[, -13]

# Find total global food waste in 2021
total_foodwaste_2021 <- sum(FWD_UNEP$combined_estimate_tpy)

total_foodwaste_2021 <- format(total_foodwaste_2021, big.mark = ",", scientific = FALSE)
dataT <- FWD_UNEP

# Subset the dataframe to select the top 10 countries with the highest and lowest combined per-capita food waste
# order() sort the data in ascending order from the lowest index
topCountries <- tail(FWD_UNEP[order(FWD_UNEP$combined_estimate_kpcpy), ], 10)
bottomCountries <- head(FWD_UNEP[order(FWD_UNEP$combined_estimate_kpcpy), ], 10)

# Top 5 countries including Singapore of household estimate tonnes per year
top_5_countries <- FWD_UNEP %>% select(country, household_estimate_tpy) %>% arrange(desc(household_estimate_tpy)) %>% slice(1:5)
additional_row <- FWD_UNEP %>% filter(country == "Singapore") %>% select(country, household_estimate_tpy)

combined <- bind_rows(top_5_countries, additional_row)

# Group the data by the 'region' column
FWD_UNEP_region <- FWD_UNEP %>% group_by(region)

# total food waste kg/capital/year
total_FW_region <- FWD_UNEP_region %>%
    summarize(total_combined_estimate_kpcpy = sum(combined_estimate_kpcpy),
              total_household_estimate_kpcpy = sum(household_estimate_kpcpy),
              total_retail_estimate_kpcpy = sum(retail_estimate_kpcpy),
              total_food_service_estimate_kpcpy = sum(food_service_estimate_kpcpy))

# total food waste tonnes/year
total_FW_region_tpy <- FWD_UNEP_region %>%
    summarize(total_combined_estimate_tpy = sum(combined_estimate_tpy),
              total_household_estimate_tpy = sum(household_estimate_tpy),
              total_retail_estimate_tpy = sum(retail_estimate_tpy),
              total_food_service_estimate_tpy = sum(food_service_estimate_tpy))

# Calculate averages for each per-capita food waste column
avg_FW_region <- FWD_UNEP_region %>%
    summarize(
        avg_combined_kpcpy = mean(combined_estimate_kpcpy, na.rm = TRUE),
        avg_household_kpcpy = mean(household_estimate_kpcpy, na.rm = TRUE),
        avg_retail_kpcpy = mean(retail_estimate_kpcpy, na.rm = TRUE),
        avg_food_service_kpcpy = mean(food_service_estimate_kpcpy, na.rm = TRUE))

total_household <- sum(FWD_UNEP$household_estimate_tpy)
total_retail <- sum(FWD_UNEP$retail_estimate_tpy)
total_food_service <- sum(FWD_UNEP$food_service_estimate_tpy)

# Create a data frame
total_FW_by_sector <- data.frame(
    category = c("Household", "Retail", "Food Service"),
    value = c(total_household, total_retail, total_food_service),
    color = c("#d62023", "#3fb619", "#8FB8FDff")
)

# avg_FW_region
# Order the data by combined averages
avg_FW_region <- avg_FW_region[order(avg_FW_region$avg_combined_kpcpy, decreasing = TRUE), ]

# Use for selectInput choices
regions <- unique(FWD_UNEP$region)

# FWD_SG ------------------------------------------------------------------------
# Rename Column Names
new_col_names_sg <- c("year", "disposed_tonnes", "recycled_tonnes", "generated_tonnes", "recycling_rate_percent")
names(FWD_SG) <- new_col_names_sg

# Remove NA in the rows
FWD_SG <- na.omit(FWD_SG)

# WASTE_SG ------------------------------------------------------------------------
# Rename Column Names
new_col_names_waste_sg <- c("waste_type", "generated_tonnes", "recycled_tonnes", "recycling_rate_percent", "disposed_tonnes")
names(WASTE_SG) <- new_col_names_waste_sg
WASTE_SG <- WASTE_SG %>% filter(waste_type != "Overall")

ui <- dashboardPage(
    skin="green",
    dashboardHeader(title="Food Waste Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            id = "sidebar",
            menuItem("Dataset 1 (UNEP)", tabName = "dataset1", icon = icon("database")),
            menuItem("Visualisation", tabName = "vis10", icon = icon("chart-line")),
            conditionalPanel("input.sidebar == 'vis10' && input.t20 == 'dataset1-region'", selectInput(inputId = "selectInput_Map" , label ="Select the Region" , choices = regions)),
            tags$br(),
            
            menuItem("Dataset 2 (Recycling Stats)", tabName = "dataset2", icon = icon("database")),
            menuItem("Visualisation", tabName = "vis2", icon = icon("chart-line")),
            conditionalPanel("input.sidebar == 'vis2'", sliderInput(inputId = "sliderInput1", label = "Year", min = min(FWD_SG$year), max = max(FWD_SG$year), value = c(min(FWD_SG$year), max(FWD_SG$year)), step = 1)),
            tags$br(),
            
            menuItem("Dataset 3 (Waste Type)", tabName = "dataset3", icon = icon("database")),
            menuItem("Visualisation", tabName = "visWatseType", icon = icon("chart-line"))
            
        )),
    dashboardBody(
        fluidRow(
            tags$head(tags$style(HTML(".dataTB_UNEP {overflow: scroll; height: 720px;}"))),
            tabItems(
                # 1st tabItem - Data Information
                tabItem(
                    tabName = "dataset1",
                    tabBox(id="t1", width = 12,
                           tabPanel("Dataset 1 (UNEP)",
                                    fluidRow(
                                        box(style = "overflow: scroll;", status = "success", tags$img(src="images/Food Waste Background image.jpg", width = 550, height = 300), "Image Source: ",
                                            tags$a("Freepik", href = "https://www.freepik.com/free-photo/close-up-hand-holding-wooden-board_14351726.htm#query=food%20waste&position=26&from_view=search&track=ais"), align= "right"),
                                        box(style = "overflow: scroll;", status = "success", title = "Dataset", "This dataset comes from ",
                                            tags$a("Kaggle (Orignal source from UNEP 2021 Food Waste)", href = "https://www.kaggle.com/datasets/joebeachcapital/food-waste/")),
                                        valueBox(ncol(FWD_UNEP_source), "Total Number of Columns", icon = icon("table-columns"), color = "green", width = 3),
                                        valueBox(nrow(FWD_UNEP_source), "Total Number of Rows", icon = icon("table-list"), color = "green", width = 3),
                                        box(status = "success", title = "Column Names", uiOutput("FWD_UNEP_colNamesText"))
                                    ) # end of fluidRow
                           ), # end of first tabPanel
                           tabPanel("Data", icon = icon("table"), tags$div(class = "dataTB_UNEP", status = "success", dataTableOutput("dataTB_UNEP"))),
                           tabPanel("Structure", icon = icon("uncharted"), verbatimTextOutput("structure"))
                    )), # end of 1st tabItem
                # 2nd tabItem - Visualisation
                tabItem(
                    tabName = "vis10",
                    tabBox(id="t20", width = 12,
                           tabPanel("Region", value = "dataset1-region",
                               fluidRow(
                                 #plotOutput("plottest1"), downloadButton(outputId = "down", label = "Download the plot"),
                                   box(style = "overflow: scroll;", value = "map", status = "success", title = "World Map", highchartOutput("mapChart"), 
                                       #downloadButton("downloadData", "Download"), 
                                       #downloadButton("downloadPlot", "Download World Map"), 
                                       width = 6),
                                   box(value = "dataset1_pie", status = "success", title = "Pie Chart", highchartOutput("dataset1_pie"), width = 6),
                                   infoBox("Household", textOutput("household_Total"), icon = icon("house"), color = "green", width = 3),
                                   infoBox("Retail", textOutput("retail_Total"), icon = icon("utensils"), color = "orange", width = 3),
                                   infoBox("Food Service", textOutput("food_service_Total"), icon = icon("store"), color = "blue", width = 3),
                                   box(status = "success", title = "Column Chart", highchartOutput("bar"), width = 6),
                                   box(value = "map", status = "success", title = "Horizontal Bar Chart", highchartOutput("dataset1_sideBar"), width = 6)
                               ) # end of FluidRow
                           ), # end of tabPanel
                           tabPanel("World",
                                    fluidRow(
                                        box(status = "success", title = "Kg/Capital/Year", plotlyOutput("dataset1_world_chart1"), width = 6),
                                        box(status = "success", title = "Tonnes/Year", plotlyOutput("dataset1_world_chart2"), width = 6),
                                        box(status = "success", title = "Kg/Capital/Year", plotlyOutput("dataset1_world_chart3"), width = 6)
                                    ) # end of fluidRow
                                ) # end of tabPanel
                    )), # end of 2nd tabItem
                
                # Dataset 2 (Recycling Stats) ------------------------------------------------------------------------------------------------
                # 3rd tabItem - Data Information
                tabItem(
                    tabName = "dataset2",
                    tabBox(id="t3", width = 12,
                           tabPanel("Dataset 2 (Recycling Stats)",
                                    fluidRow(
                                        box(style = "overflow: scroll;", status = "success", tags$img(src="images/Food Waste SG background Image.png", width = 600, height = 300), "Image Source: ",
                                            tags$a("NEA", href = "https://www.nea.gov.sg/our-services/waste-management/3r-programmes-and-resources/food-waste-management"), align= "right"),
                                        box(status = "success", title = "Dataset", "This dataset comes NEA: ",
                                            tags$a("2003 - 2017, ", href = "https://www.nea.gov.sg/docs/default-source/our-services/waste-management/wastestats-2003-20164197a3fd04d34770bafba09393d0fdf0.pdf"),
                                            tags$a("2017 - 2021, ", href = "https://www.nea.gov.sg/docs/default-source/default-document-library/waste-and-recycling-statistics-2017-to-2021.pdf"),
                                            tags$a("2022", href = "https://www.nea.gov.sg/our-services/waste-management/waste-statistics-and-overall-recycling")),
                                        valueBox(ncol(FWD_SG), "Total Number of Columns", icon = icon("table-columns"), color = "green", width = 3),
                                        valueBox(nrow(FWD_SG), "Total Number of Rows", icon = icon("table-list"), color = "green", width = 3),
                                        box(status = "success", title = "Column Names", uiOutput("FWD_SG_colNamesText"))
                                    ) # end of fluidRow
                           ),
                           tabPanel("Data", icon = icon("table"), dataTableOutput("dataTB_SG")),
                           tabPanel("Structure", icon = icon("uncharted"), verbatimTextOutput("structure_SG"))
                    )), #end of 3rd TabItem
                # 4th tabItem - Visualisation
                tabItem(
                    tabName = "vis2",
                    tabBox(id="t4", width = 12,
                           fluidRow(
                               box(style = "overflow: scroll;", status = "success", tags$h3(tags$strong("Waste Statistics and Overall Recycling Table in Singapore")), width = 12),
                               box(value = "map", status = "success", title = "Line Chart", highchartOutput("line"), width = 6),
                               box(status = "success", title = "Area Chart", highchartOutput("area"), width = 6)
                           ) # end of FluidRow
                    )), # end of 4th tabItem
                
                # Dataset 3 (Waste Type) ------------------------------------------------------------------------------------------------
                # 5th tabItem - Data Information
                tabItem(
                    tabName = "dataset3",
                    tabBox(id="t5", width = 12,
                           tabPanel("Dataset 3 (Waste Type)",
                                    # content in About
                                    fluidRow(
                                        box(status = "success", tags$img(src="images/Food Waste SG background Image.png", width = 600, height = 300), "Image Source: ",
                                            tags$a("NEA", href = "https://www.nea.gov.sg/our-services/waste-management/3r-programmes-and-resources/food-waste-management"), align= "right"),
                                        box(status = "success", title = "Dataset", "This dataset comes NEA: ",
                                            tags$a("2022", href = "https://www.nea.gov.sg/our-services/waste-management/waste-statistics-and-overall-recycling")),
                                        
                                        valueBox(ncol(WASTE_SG), "Total Number of Columns", icon = icon("table-columns"), color = "green", width = 3),
                                        valueBox(nrow(WASTE_SG), "Total Number of Rows", icon = icon("table-list"), color = "green", width = 3),
                                        box(status = "success", title = "Column Names", uiOutput("WASTE_SG_colNamesText"))
                                    )),
                           tabPanel("Data", icon = icon("table"), dataTableOutput("dataTB_WASTE_SG")),
                           tabPanel("Structure", icon = icon("uncharted"), verbatimTextOutput("structure_WASTE_SG"))
                    )), # end of 5th tabItem
                # 6th tabItem - Visualisation
                tabItem(
                    tabName = "visWatseType",
                    tabBox(id="tbWasteType", width = 12,
                           fluidRow(
                               box(status = "success", tags$h3(tags$strong("Waste Type Statistics in Singapore")), width = 12),
                               box(value = "map", status = "success", title = "Donut Chart", highchartOutput("pie"), width = 6),
                               box(status = "success", title = "Treemap (Colour of Treemap is Random)", highchartOutput("treemap"), width = 6)
                           ) # end of FluidRow
                    )) # end of 6th tabItem
            )# end all tabItems
        )))

#-------------------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
    # FWD_UNEP
    # Structure 
    output$structure <- renderPrint(FWD_UNEP %>% str())
    
    # DataTable
    output$dataTB_UNEP <- DT::renderDataTable({
        datatable(FWD_UNEP)
    })
    
    output$outputText <- renderPrint(input$var1)
    
    # Create a reactive function to calculate the totals by region
    region_totals <- reactive({
        region_data <- FWD_UNEP %>%
            group_by(region) %>%
            summarize(
                Household_Total = sum(household_estimate_tpy),
                Retail_Total = sum(retail_estimate_tpy),
                Food_Service_Total = sum(food_service_estimate_tpy)
            )
        
        return(region_data)
    })
    
    # Render the bar graph by region
    output$bar <- renderHighchart ({
        data <- region_totals()
        data <- data %>% filter(region == input$selectInput_Map)
        
        # Reshape data to a longer format
        data_long <- data %>%
            pivot_longer(cols = c(Household_Total, Retail_Total, Food_Service_Total),
                         names_to = "Food_Waste_Type",
                         values_to = "Total_Waste")
        
        # Renaming Categories
        data_long$Food_Waste_Type <- recode(data_long$Food_Waste_Type,
                                            "Household_Total" = "Household",
                                            "Retail_Total" = "Retail",
                                            "Food_Service_Total" = "Food Service")
        
        hchart(data_long, type = "column", hcaes(x = Food_Waste_Type, y = Total_Waste)) %>% 
            hc_title(text = "Total Food Waste by Region ") %>%
            hc_subtitle(text = "Total Food Waste (Tonnes/Year)") %>% 
            hc_tooltip(pointFormat = "<b>{point.name}</b>: {point.y} tonnes/year") %>% 
            hc_xAxis(title = list(text = "Food Waste Sector"), labels = list(enabled = TRUE)) %>%  
            hc_yAxis(title = list(enabled = FALSE)) %>% 
            hc_plotOptions(column = list(colorByPoint = TRUE)) %>% 
            hc_colors(c("green", "orange", "blue"))
    })
    
    # output$bar <- renderPlotly({
    #     data <- region_totals()
    #     data <- data %>% filter(region == input$selectInput_Map)
    #     
    #     p <- ggplot() +
    #         geom_bar(stat = "identity", aes(x = c("Household", "Retail", "Food Service"), 
    #                                         y = c(as.numeric(data$Household_Total), 
    #                                               as.numeric(data$Retail_Total), 
    #                                               as.numeric(data$Food_Service_Total)), 
    #                                         fill = c("#3fb619", "orange", "blue"))) +
    #         labs(
    #             title = "Total Combined Food Waste Type by Region",
    #             x = "Food Waste Sector",
    #             y = "Total Food Waste (Tonnes)"
    #         ) +
    #         scale_fill_manual(
    #             values = c("orange" = "orange", "#3fb619" = "#3fb619", "blue" = "blue"),
    #             name = "Food Waste Types",
    #             breaks = c("orange", "#3fb619", "blue"),
    #             labels = c("Retail", "Household", "Food Service")
    #         ) +
    #         remove_background_theme() +
    #         theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),legend.position = "none") 
    #     
    #     
    #     ggplotly(p,tooltip = "y") %>%
    #         config(displayModeBar = FALSE)
    # })
    
    FWD_UNEP_column_names <- reactive({
        names(FWD_UNEP_source)
    })
    
    output$FWD_UNEP_colNamesText <- renderPrint({
        cat(paste(FWD_UNEP_column_names(), collapse = "<br>"), "\n")
    })
    
    output$household_Total <- renderText({
        data1 <- region_totals()
        data1 <- data1 %>% filter(region == input$selectInput_Map)
        scales::comma(data1$Household_Total)
    })
    output$retail_Total <- renderText({
        data2 <- region_totals()
        data2 <- data2 %>% filter(region == input$selectInput_Map)
        scales::comma(data2$Retail_Total)
    })
    output$food_service_Total <- renderText({
        data3 <- region_totals()
        data3 <- data3 %>% filter(region == input$selectInput_Map)
        scales::comma(data3$Food_Service_Total)
    })
    # Map 
    output$mapChart <- renderHighchart ({
        countries_by_region <- FWD_UNEP %>% 
            filter(region == input$selectInput_Map)
        
        highchart() %>% 
            hc_title(text = "Household Food Waste by Region") %>%
            hc_add_series_map(worldgeojson, countries_by_region, value = "household_estimate_tpy", joinBy = c("name", "country"), mapData = "map",
                              borderColor = "#A0A0A0",
                              borderWidth = 0.5,
                              states = list(hover = list(color = "green", borderColor = "black", shadow = FALSE)),
                              nullColor = "grey") %>%
            hc_colorAxis(minColor = "white", maxColor = "#d62023") %>%
            hc_legend(valueDecimals = 0, valueSuffix = "%") %>%
            hc_mapNavigation(enabled = TRUE) %>% 
            hc_subtitle(text = "Total Food Waste (Tonnes/Year)") %>% 
            hc_exporting(enabled = TRUE, filename = "Dataset 1 (UNEP) - World Map")
    })
    # Pie Chart
    output$dataset1_pie <- renderHighchart ({
        highchart() %>%
            hc_chart(type = "pie") %>%
            hc_add_series(total_FW_by_sector, "pie", hcaes(y = "value", name = "category", color = "color")) %>% 
            hc_tooltip(
                pointFormat = "<b>{point.name}</b>: {point.y} tonnes/year ({point.percentage:.1f}%)"
            ) %>%  hc_plotOptions(
                pie = list(
                    dataLabels = list(
                        enabled = TRUE,
                        format = "{point.percentage:.1f}%",
                        style = list(fontWeight = "bold")
                    ), 
                    showInLegend = TRUE
                )
            ) %>% 
            hc_title(text = "Total Food Waste by Sector by Region") %>%
            hc_legend(enabled = TRUE) %>%
            hc_subtitle(text = "Total Food Waste (Tonnes/Year)") %>% 
            hc_exporting(enabled = TRUE, filename = "Dataset 1 (UNEP) - Pie Chart")
    })
    # Horizontal Bar Chart
    output$dataset1_sideBar <- renderHighchart ({
        countries_by_region <- FWD_UNEP %>% filter(region == input$selectInput_Map) %>%
            arrange(-combined_estimate_tpy)         
        hchart(countries_by_region, type = "bar", hcaes(x = country, y = combined_estimate_tpy), color = "#d62023") %>% 
            hc_title(text = "Total Food Waste by Region ") %>%
            hc_subtitle(text = "Total Food Waste (Tonnes/Year)") %>% 
            hc_tooltip(pointFormat = "<b>{point.name}</b>: {point.y} tonnes/year") %>% 
            hc_xAxis(title = list(enabled = FALSE)) %>%  
            hc_yAxis(title = list(enabled = FALSE)) %>% 
            hc_exporting(enabled = TRUE, filename = "Dataset 1 (UNEP) - Horizontal Bar Chart")
    })
    
    output$dataset1_world_chart1 <- renderPlotly ({
    # Show how different categories of per-capita food waste compare in each region
    total_FW_region <- ggplot(total_FW_region, aes(y = reorder(region, total_combined_estimate_kpcpy), x = total_combined_estimate_kpcpy, fill = "Combined")) +
      geom_col(aes(x = total_household_estimate_kpcpy, fill = "Household",
                   text = paste0("Household: ", total_household_estimate_kpcpy, "kg")), width = 0.7) +
      geom_col(aes(x = total_food_service_estimate_kpcpy, fill = "Food Service",
                   text = paste0("Food Service: ", total_food_service_estimate_kpcpy, "kg")), width = 0.7) +
      geom_col(aes(x = total_retail_estimate_kpcpy, fill = "Retail",
                   text = paste0("Retail: ", total_retail_estimate_kpcpy, "kg")), width = 0.7) +
      labs(title = str_wrap("Total Food Waste Sector by Region", width = 35),
        y = "",
        x = "Total Per-Capita Food Waste (Kg/Year)") +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
      scale_fill_manual(
        values = c("Household" = "#d62023", "Retail" = "#3fb619", "Food Service" = "#8FB8FDff"),
        name = "Food Waste Types",
        labels = c("Retail", "Household", "Food Service")) +
      guides(fill = guide_legend(title = "Food Waste Sector")) +
      theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"), axis.text.x = element_text(angle = 0, hjust = 1), legend.position = "bottom",plot.margin = margin(t = 15)) +
      remove_background_theme()

    ggplotly(total_FW_region,tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
    layout(legend = list(orientation = "h", x = -0.2, y = -0.2))
    })
    
    output$dataset1_world_chart2 <- renderPlotly ({
        # Show how different categories of per-capita food waste compare in each region
        total_FW_region_tpy <- ggplot(total_FW_region_tpy, aes(y = reorder(region, total_combined_estimate_tpy), x = total_combined_estimate_tpy, fill = "Combined")) +
            geom_col(aes(x = total_household_estimate_tpy, fill = "Household",
                         text = paste0("Household: ", total_household_estimate_tpy, "kg")), width = 0.7) +
            geom_col(aes(x = total_food_service_estimate_tpy, fill = "Food Service",
                         text = paste0("Food Service: ", total_food_service_estimate_tpy, "kg")), width = 0.7) +
            geom_col(aes(x = total_retail_estimate_tpy, fill = "Retail",
                         text = paste0("Retail: ", total_retail_estimate_tpy, "kg")), width = 0.7) +
            labs(title = str_wrap("Total Food Waste Sector by Region", width = 35),
                 y = "",
                 x = "Total Food Waste (Tonnes/Year)") +
            theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
            scale_fill_manual(
                values = c("Household" = "#d62023", "Retail" = "#3fb619", "Food Service" = "#8FB8FDff"),
                name = "Food Waste Types",
                labels = c("Retail", "Household", "Food Service")) +
            guides(fill = guide_legend(title = "Food Waste Sector")) +
            theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"), axis.text.x = element_text(angle = 0, hjust = 1), legend.position = "bottom",plot.margin = margin(t = 15)) +
            remove_background_theme()
        
        ggplotly(total_FW_region_tpy, tooltip = "text") %>%
            config(displayModeBar = FALSE) %>%
            layout(legend = list(orientation = "h", x = -0.2, y = -0.2))
    })
    
    output$dataset1_world_chart3 <- renderPlotly ({
        # Show how different categories of per-capita food waste compare in each region average
        avg_FW_region <- ggplot(avg_FW_region, aes(y = reorder(region, avg_combined_kpcpy), x = avg_combined_kpcpy, fill = "Combined")) +
            geom_col(aes(x = avg_household_kpcpy, fill = "Household", 
                         text = paste0("Household: ", avg_household_kpcpy, "kg")), width = 0.7) +
            geom_col(aes(x = avg_food_service_kpcpy, fill = "Food Service", 
                         text = paste0("Food Service: ", avg_food_service_kpcpy, "kg")), width = 0.7) +
            geom_col(aes(x = avg_retail_kpcpy, fill = "Retail", 
                         text = paste0("Retail: ", avg_retail_kpcpy, "kg")), width = 0.7) +
            labs(title = str_wrap("Relative Proportions of Food Waste Sector by Region", width=30),
                 y = "",
                 x = "Average Per-Capita Food Waste (kg/year)")+
            theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
            scale_fill_manual(
                values = c("Household" = "#d62023", "Retail" = "#3fb619", "Food Service" = "#8FB8FDff"),
                name = "Food Waste Types",
                labels = c("Retail", "Household", "Food Service")
            ) +
            guides(fill = guide_legend(title = "Food Waste Sector")) +
            remove_background_theme() +
            theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"), 
                  legend.position = "bottom", plot.margin = margin(t = 15))
        
        ggplotly(avg_FW_region,tooltip = "text") %>%
            config(displayModeBar = FALSE)%>%
            layout(aspectratio = list(x = 1, y = 1),legend = list(orientation = "h", x = -0.2, y = -0.2))
    })
    
    # data <- mtcars
    # 
    # output$downloadData <- downloadHandler(
    #   filename = function() {
    #     paste("Dataset1 (UNEP) - World Map", ".png", sep="")
    #   },
    #   content = function(file) {
    #     write.csv(data, file)
    #   }
    # )
    # output$plottest1 <- renderPlot({
    #   plot(x=x(), y=y())
    # })
    # output$down <- downloadHandler(
    #   filename =  function() {
    #     paste("iris", "png", sep=".")
    #   },
    #   content = function(file) {
    #     png(file) 
    #     plot(iris[,1], iris[,1]) 
    #     dev.off()  
    #   } 
    # )
    #-------------------------------------------------------------------------------------------------------------    
    # FWD_SG
    # Structure 
    output$structure_SG <- renderPrint(FWD_SG %>% str())
    
    # DataTable
    output$dataTB_SG <- DT::renderDataTable({
        datatable(FWD_SG)
    })
    # Column Names of FWD_SG
    FWD_SG_column_names <- reactive({
        names(FWD_SG)
    })
    # print Column Names of FWD_SG
    output$FWD_SG_colNamesText <- renderPrint({
        cat(paste(FWD_SG_column_names(), collapse = "<br>"), "\n")
    })
    # To use in Line and Area Chart
    FWD_SG_pivot_longer <- reactive({
      FWD_SG %>% pivot_longer(cols = disposed_tonnes:disposed_tonnes:recycled_tonnes:generated_tonnes,
                              names_to = "Sector",
                              values_to = "Tonnes") %>% 
        mutate(Sector = case_when(
          Sector == "disposed_tonnes" ~ "Disposed",
          Sector == "generated_tonnes" ~ "Generated",
          Sector == "recycled_tonnes" ~ "Recycled",
          TRUE ~ as.character(Sector)
        ))
    })
    # Line Chart
    output$line <- renderHighchart({
      FWD_SG <- FWD_SG_pivot_longer()

      FWD_SG %>% filter(year >= input$sliderInput1[1] & year <= input$sliderInput1[2]) %>%
        hchart('spline', hcaes(x = 'year', y = 'Tonnes', group = "Sector")) %>% 
        hc_exporting(enabled = TRUE, filename = "Dataset 2 (Recycling Stats) - Line Chart") 
    })
    # Area Chart
    output$area <- renderHighchart({
      FWD_SG <- FWD_SG_pivot_longer()
        
      FWD_SG %>% filter(year >= input$sliderInput1[1] & year <= input$sliderInput1[2]) %>%
        hchart('areaspline', hcaes(x = 'year', y = 'Tonnes', group = "Sector")) %>% 
        hc_exporting(enabled = TRUE, filename = "Dataset 2 (Recycling Stats) - Area Chart") 
    })
    #-------------------------------------------------------------------------------------------------------------
    # WASTE_SG 
    # Structure 
    output$structure_WASTE_SG <- renderPrint(WASTE_SG %>% str())
    
    # DataTable
    output$dataTB_WASTE_SG <- DT::renderDataTable({
        datatable(WASTE_SG)
    })
    # Column Names of WASTE_SG
    WASTE_SG_column_names <- reactive({
        names(WASTE_SG)
    })
    # Print Column Names of WASTE_SG
    output$WASTE_SG_colNamesText <- renderPrint({
        cat(paste(WASTE_SG_column_names(), collapse = "<br>"), "\n")
    })
    # Pie Chart by Waste Type
    output$pie <- renderHighchart({
        first_5_rows <- head(WASTE_SG, 5) %>% select(waste_type, generated_tonnes)
        
        grouped_sum <- WASTE_SG %>%
            slice(6:14) %>%
            summarise(waste_type = "Others", generated_tonnes = sum(generated_tonnes))
        
        final_result <- bind_rows(first_5_rows, grouped_sum)
        
        highchart() %>%
            hc_chart(type = "pie") %>%
            hc_add_series(final_result, "pie", hcaes(y = "generated_tonnes", name = "waste_type", color = "waste_type"), center = c(50, 50), 
                          innerSize="50%") %>% 
            hc_tooltip(
                pointFormat = "<b>{point.name}</b>: {point.y} tonnes/year ({point.percentage:.1f}%)"
            ) %>%  hc_plotOptions(
                pie = list(
                    dataLabels = list(
                        enabled = TRUE,
                        format = "<b>{point.name}</b>:<br>({point.percentage:.1f}%)",
                        style = list(fontSize = "10px", fontWeight = "bold")
                    )
                ),
                showInLegend = TRUE
            ) %>% 
            hc_title(text = "Total Generated Waste by Type in Singapore") %>%
            hc_legend(enabled = TRUE) %>%
            hc_subtitle(text = "Total Waste Type (Tonnes/Year)") %>% 
            hc_exporting(enabled = TRUE, filename = "Dataset 3 (Waste Type) - Donut Chart") 
    })
    #  Treemap Chart by Waste Type
    output$treemap <- renderHighchart({
        # Calculate percentage
        WASTE_SG$percentage <- (WASTE_SG$generated_tonnes / sum(WASTE_SG$generated_tonnes)) * 100
        
        # Create treemap chart with generated_tonnes and percentage in the tooltip
        WASTE_SG %>%
            hchart("treemap", hcaes(x = waste_type, value = generated_tonnes, color = generated_tonnes)
            ) %>%
            hc_tooltip(pointFormat = "<b>{point.name}</b>: {point.generated_tonnes} tonnes/year ({point.percentage:.1f}%)") %>% 
            hc_plotOptions(
                treemap = list(
                    dataLabels = list(
                        enabled = TRUE,
                        format = "<b>{point.name}</b>:<br>({point.percentage:.1f}%)",
                        style = list(fontSize = "10px", fontWeight = "bold")
                    )
                )
            ) %>% 
            hc_colorAxis(minColor = randomColor(1),maxColor = randomColor(1)) %>% 
            hc_title(text = "Total Generated Waste by Type in Singapore") %>%
            hc_legend(enabled = FALSE) %>%
            hc_subtitle(text = "Total Waste Type (Tonnes/Year)") %>% 
            hc_exporting(enabled = TRUE, filename = "Dataset 3 (Waste Type) - Treemap")
    })
}

# Run the application
shinyApp(ui,server)

