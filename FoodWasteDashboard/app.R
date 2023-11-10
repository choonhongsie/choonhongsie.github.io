library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)
library(rsconnect)

packageVersion('rsconnect')

remove_background_theme <- function() {
    theme(
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank()
    )
}
FWD_UNEP <- read_csv("src/data/UNEP Food Waste Statistics.csv")
FWD_SG <- read_csv("src/data/Food Waste Statistics.csv")

# FWD_UNEP
#Create new column for combined estimate in tonnes/year
FWD_UNEP <- mutate(FWD_UNEP, combined_estimate_tonnes_per_year = FWD_UNEP$`Household estimate (tonnes/year)` + FWD_UNEP$`Retail estimate (tonnes/year)` + FWD_UNEP$`Food service estimate (tonnes/year)`)
FWD_UNEP <- FWD_UNEP[, c(1,2,13,3,4,5,6,7,8,9,10,11,12)]

# Changing column names to make them lowercase and include underscores for easier reference
new_col_names <- c("country", "combined_estimate_kpcpy", "combined_estimate_tpy", "household_estimate_kpcpy", "household_estimate_tpy", "retail_estimate_kpcpy", "retail_estimate_tpy", "food_service_estimate_kpcpy", "food_service_estimate_tpy", "estimate_confidence", "m49_code", "region", "source")
names(FWD_UNEP) <- new_col_names

# Get source column
dataSources <- FWD_UNEP %>% select(country, m49_code, source)

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

total_FW_region <- FWD_UNEP_region %>%
    summarize(total_combined_estimate_kpcpy = sum(combined_estimate_kpcpy),
              total_household_estimate_kpcpy = sum(household_estimate_kpcpy),
              total_retail_estimate_kpcpy = sum(retail_estimate_kpcpy),
              total_food_service_estimate_kpcpy = sum(food_service_estimate_kpcpy))

# Calculate averages for each per-capita food waste column
avg_FW_region <- FWD_UNEP_region %>%
    summarize(
        avg_combined_kpcpy = mean(combined_estimate_kpcpy, na.rm = TRUE),
        avg_household_kpcpy = mean(household_estimate_kpcpy, na.rm = TRUE),
        avg_retail_kpcpy = mean(retail_estimate_kpcpy, na.rm = TRUE),
        avg_food_service_kpcpy = mean(food_service_estimate_kpcpy, na.rm = TRUE))

# avg_FW_region
# Order the data by combined averages
avg_FW_region <- avg_FW_region[order(avg_FW_region$avg_combined_kpcpy, decreasing = TRUE), ]

# FWD_SG
new_col_names_sg <- c("year", "disposed_tonnes", "recycled_tonnes", "generated_tonnes", "recycling_rate_percent")
names(FWD_SG) <- new_col_names_sg

# my_data <- FWD_UNEP
# my_data_region <- unique(sort(my_data$region))
# FWD_UNEP %>% select(country, region) %>% filter(region == "South-eastern Asia")

regions <- unique(FWD_UNEP$region)


region_SA <- FWD_UNEP %>% filter(region=="Southern Asia") %>% group_by(region) %>% select(household_estimate_tpy, retail_estimate_tpy, food_service_estimate_tpy)
region_SA_household <- sum(region_SA$household_estimate_tpy)
region_SA_retail <- sum(region_SA$retail_estimate_tpy)
region_SA_food_sevice <- sum(region_SA$food_service_estimate_tpy)


region_SEA <- FWD_UNEP %>% filter(region=="South-eastern Asia") %>% group_by(region) %>% select(household_estimate_tpy, retail_estimate_tpy, food_service_estimate_tpy)
region_SEA_household <- sum(region_SA$household_estimate_tpy)
region_SEA_retail <- sum(region_SA$retail_estimate_tpy)
region_SEA_food_sevice <- sum(region_SA$food_service_estimate_tpy)
# 
ui <- dashboardPage(
    skin="green",
    dashboardHeader(title="Food Waste Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            id = "sidebar",
            menuItem("Dataset 1", tabName = "data1", icon = icon("database")),
            menuItem("Visualisation", tabName = "vis1", icon = icon("chart-line")),
            conditionalPanel("input.sidebar == 'vis1' && input.t2 == 'trends'",selectInput(inputId = "var1" , label ="Select the Region" , choices = regions)),
            menuItem("Choropleth Map", tabName = "map", icon=icon("map")),
            tags$br(),
            
            menuItem("Dataset 2", tabName = "data2", icon = icon("database")),
            menuItem("Visualisation", tabName = "vis2", icon = icon("chart-line"))
            
            
            # conditionalPanel("input.sidebar == 'vis1' && input.t2 == 'trends'", selectInput(inputId = "var1" , label ="Select the Region" , choices = c("1","2"))
        )),
    dashboardBody(
        tabItems(
            # First tab item
            tabItem(
                tabName = "data1",
                tabBox(id="t1", width = 12,
                       tabPanel("Dataset 1",
                                # content in About
                                fluidRow(
                                    column(width = 8,
                                           tags$img(src="images/Food Waste Background image.jpg", width = 600, height = 300),
                                           tags$br(),
                                           tags$a("image by Freepik"), align = "center"),
                                    column(width = 8,
                                           tags$br(),
                                           tags$p("This data set comes from UNEP 2021 Food Waste"))
                                )), # end of first tabPanel
                       tabPanel("Data", icon = icon("table"), dataTableOutput("dataTB_UNEP")),
                       tabPanel("Structure", icon = icon("uncharted"), verbatimTextOutput("structure")),
                )), # end of first tabItem
            # Second tab item
            tabItem(
                tabName = "vis1",
                tabBox(id="t2", width = 12,
                       tabPanel("Countries by Region", value="trends", plotlyOutput("bar"))
                )), # end of Second tabItem
            # Third tab item - Map
            # Fourth tab item
            tabItem(
                tabName = "data2",
                tabBox(id="t2", width = 12,
                       tabPanel("Dataset 2",
                                # content in About
                                fluidRow(
                                    column(width = 8,
                                           tags$img(src="images/Food Waste SG background Image.png", width = 600, height = 300),
                                           tags$br(),
                                           tags$a("image by Freepik"), align = "center"),
                                    column(width = 8,
                                           tags$br(),
                                           tags$p("This data set comes from UNEP 2021 Food Waste"))
                                )),
                       tabPanel("Data", icon = icon("table"), dataTableOutput("dataTB_SG")),
                       tabPanel("Structure", icon = icon("uncharted"), verbatimTextOutput("structure_SG")),
                       tabPanel("test", icon = icon("uncharted"), verbatimTextOutput("test")),
                )),
            # Fifth tab item
            tabItem(
                tabName = "vis2",
                tabBox(id="t3", width = 12,
                       tabPanel("Countries by Region", value="trends", plotlyOutput("bar2"))
                )) # end of Second tabItem
        )# end all tabItems
    ))
# Define server logic required to draw a histogram
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
    output$bar <- renderPlotly({
        data <- region_totals()
        data <- data %>% filter(region == input$var1)
        
        p <- ggplot() +
            geom_bar(stat = "identity", aes(x = c("Household", "Retail", "Food Service"), 
                                            y = c(as.numeric(data$Household_Total), 
                                                  as.numeric(data$Retail_Total), 
                                                  as.numeric(data$Food_Service_Total)), 
                                            fill = c("#3fb619", "orange", "blue"))) +
            geom_text(
                aes(
                    x = c("Household", "Retail", "Food Service"),
                    y = c(as.numeric(data$Household_Total),
                          as.numeric(data$Retail_Total),
                          as.numeric(data$Food_Service_Total)), 
                    hjust = 0.5, vjust = 2, fontface = "bold",
                    label = scales::comma(c(as.numeric(data$Household_Total),
                                            as.numeric(data$Retail_Total),
                                            as.numeric(data$Food_Service_Total))))
            ) +
            labs(
                title = "Total Combined Food Waste Type by Region",
                x = "Source",
                y = "Total Food Waste (Tonnes)"
            ) +
            scale_fill_manual(
                values = c("orange" = "orange", "#3fb619" = "#3fb619", "blue" = "blue"),
                name = "Food Waste Types",
                breaks = c("orange", "#3fb619", "blue"),
                labels = c("Retail", "Household", "Food Service")
            ) +
            remove_background_theme() +
            theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) 
        
        
        ggplotly(p,tooltip = "y") %>%
            config(displayModeBar = FALSE)
    })
    # FWD_SG
    # Structure 
    output$structure_SG <- renderPrint(FWD_SG %>% str())
    
    # DataTable
    output$dataTB_SG <- DT::renderDataTable({
        datatable(FWD_SG)
    })
}

# Run the application
shinyApp(ui,server)

