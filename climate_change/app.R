library(shiny)
library(shinythemes)
library(PPBDS.data)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(gganimate)
library(plotly)
library(sf)
library(rgdal)
library(rnaturalearth)
library(rnaturalearthdata)
library(gtsummary)
library(magick)
library(rstanarm)
library(broom.mixed)
library(gt)

# The libraries included above proved incredibly useful to completing my project.
# It was amazing how after only one semester, I went from not understanding the 
# most basic of code to being able to create all that you see within this project
# and more. I wanted to thank the course staff, especially Tyler and Beau, for all 
# their help this semester. The class and I thank you all for everything you did for
# us!

country_data <- read_rds("data/country_data/country_data.rds")
world <- read_rds("data/country_data/world.rds")
country_data_2 <- read_rds("data/country_data/country_data_2.rds")
world_map <- read_rds("data/country_data/world_map.rds")
continent_data <- read_rds("data/continent_data/continent_data.rds")
opinion_data <- read_rds("data/opinion_data/opinion_data.rds")
opinion_USA <- read_rds("data/opinion_USA/opinion_USA.rds")
co2_long_term <- read_rds("data/temp_co2/co2_long_term.rds")
temp_long_term <- read_rds("data/temp_co2/temp_long_term.rds")
correlation <- read_rds("data/temp_co2/correlation.rds")
brazil_ghg <- read_rds("data/model/brazil_ghg.rds")
brazil_land_ghg <- read_rds("data/model/brazil_land_ghg.rds")
brazil <- read_rds("data/model/brazil.rds")
brazil_fit <- read_rds("data/model/brazil_fit.rds")
brazil_land_fit <- read_rds("data/model/brazil_land_fit.rds")
brazil_table <- read_rds("data/model/brazil_table.rds")
brazil_land_table <- read_rds("data/model/brazil_land_table.rds")

# I read in all my rds files so that I could use the objects I created in the app

ui <- navbarPage(
  "The Reality of the Climate Crisis",
  
# Title of Shiny App
  
  theme = shinytheme("sandstone"),

# Background Theme of the Shiny App using the shinythemes package

  tabPanel("Background Information",
           
# Title of 1st Page
           
           fluidPage(
             
             mainPanel(tabsetPanel(type = "tabs",
                                   
# Within the first page, there is a panel with a variety of tabs containing
# different information relevant to the final project
                                   
                                   tabPanel("Climate Change Overview",
                                            includeMarkdown("text/text1.Rmd")),
                                   tabPanel("US Public Opinion",
                                            br(),
                                            plotOutput("opinion_data_plot"),
                                            includeMarkdown("text/text3.Rmd")),
                                   tabPanel("Political Affiliations & Public Opinion",
                                            br(),
                                            plotOutput("opinion_USA_plot"),
                                            includeMarkdown("text/text2.Rmd")),
                                   tabPanel("Global Share of CO2",
                                            br(),
                                            plotlyOutput("worldshare_plot"),
                                            includeMarkdown("text/text4.Rmd")),
                                   tabPanel("Global Share Map",
                                            br(),
                                            imageOutput("worldshare_map_plot"),
                                            includeMarkdown("text/text6.Rmd")))))),

# This is the end of my first page, which includes a variety of text, graphs, and
# images

  tabPanel("CO2 and Temperature Trends",
           
# Title of 2nd Page
           
           fluidPage(
             titlePanel("Global Warming's Greatest Factor: Carbon Emissions"),
             
             mainPanel(splitLayout(
               cellWidths = 400, 
               cellArgs = list(style = "padding: 25px"),
               imageOutput("co2concentration_plot"),
               type = "html",
               loader = "loader2",
               imageOutput("tempanomaly_plot"),
               type = "html",
               loader = "loader2")),
             
             
# The splitLayout function allows me to place the two GIFs I created next to one 
# another
             
             mainPanel(plotOutput("correlation_plot"),
                       width = 700),
             br(),
             includeMarkdown("text/text5.Rmd"))),

  tabPanel("Geographic Data",
           
# Title of 3rd Page

           fluidPage(
             titlePanel("Continent & Country Data Exploration"),
             
             fluidPage(
               br(),
               br(),
               mainPanel(plotOutput("continent_co2_plot"),
                         width = 700),
               br(),
               includeMarkdown("text/text7.Rmd"),
               br(),
               sidebarLayout(
                 sidebarPanel(
                   selectInput("country",
                               "Country",
                               country_data$country,
                               
# The code above creates a panel on the side of the page, where the user can 
# select a country from the list, and information for that country will appear 
# in the three graph outputs 
                               
                               multiple = TRUE,

# Multiple countries can be selected and compared with one another

                               selected = "Afghanistan")),

# When the page opens up, the graphs will automatically have the data appear for
# Afghanistan

                 mainPanel(plotOutput("country_co2_plot"),
                           plotOutput("country_pop_plot"),
                           plotOutput("country_gdp_plot")))))),

tabPanel("Model",
         
# Title of 4th Page
         
         titlePanel("Model"),
         mainPanel(imageOutput("tbl_regression_1")),
         includeMarkdown("text/text8.Rmd"),
         br(),
         mainPanel(imageOutput("tbl_regression_2"))),

tabPanel("About",
         
# Title of 5th Page
         
         titlePanel("About"),
         h3("Project Background and Motivations"),
         includeMarkdown("text/text9.Rmd"),
         mainPanel(imageOutput("my_picture")))

)

# END OF UI CODE

server <- function(input, output) {
  
# Plot 1:
  
  output$opinion_data_plot <- renderPlot({
    
    opinion_data %>%
      ggplot(aes(x = Attitudes,
                 y = Values,
                 fill = Attitudes)) +
      geom_col() +
      
# The graph depicts public opinion numbers with four possible responses, depicting
# peoples' opinions surrounding the severity of climate change
      
      coord_flip() +
      scale_x_discrete(labels = c("A little \n serious",
                                  "Not serious \n at all",
                                  "Somewhat \n serious",
                                  "Very \n serious"),
                       name = "Attitudes") +
      
# Modifies the labels of the y-axis
      
      scale_fill_discrete(name = "Values",
                          type = c("#bdd7e7",
                                   "#eff3ff",
                                   "#6baed6",
                                   "2171b5")) +
      theme_bw() +
      theme(legend.position = "none") +
      
# The theme function removes the legend from showing
      
      labs(title = "United States Public Opinion",
           subtitle = "Public Opinion Regarding the Severity of the Climate Crisis",
           x = "Attitudes",
           y = "Values")
    
# The labs function used to improve the aesthetics of the graph
                       
  })
  
# Plot 2:
  
  output$opinion_USA_plot <- renderPlot({
    
    opinion_USA %>%
      ggplot(aes(x = Political_Affiliation,
                 y = Percent_Believe_In_Climate_Change,
                 fill = Political_Affiliation)) +
      geom_col() +
      
# This graph depicts the correlation between political affiliation in the US 
# and the percentage of individuals that believe in climate change
      
      scale_fill_discrete(labels = c("Center",
                                     "Conservative",
                                     "Liberal"),
                          name = "Political Affiliation",
                          type = c("darkgray",
                                   "darkred",
                                   "darkblue")) +
      
# The function above edits the color that fills the bar in the graph
      
      theme_classic() +
      labs(title = "US Public Opinion",
           subtitle = "Public Opinion in the United States According to One's Political Ideologies",
           x = "Political Affiliation",
           y = "Percent of Individuals Who Believe in Climate Change")
    
# The labs and theme_classic function used to improve the aesthetics of the graph
    
  })
  
# Plot 3:
  
  output$worldshare_plot <- renderPlotly({
    
    country_data <- country_data %>%
      select(country,
             year,
             share_global_co2,
             cumulative_co2, 
             iso_code) %>%
      arrange(desc(cumulative_co2)) %>%
      filter(year == 2018, 
             iso_code != "OWID_WRL") %>%
      slice(1:8) %>%
    
# The code above rearranges the data so that the top 8 producers of carbon emissions
# in 2018 (the most recent year of data) are listed
      
      ggplot(aes(x = share_global_co2,
                 y = cumulative_co2)) +
      geom_text(aes(label = iso_code),
                size = 3,
                angle = 45) +
      
# Although I don't have much experience with geom_text, I really found it interesting
# how I could use the iso_code to display the countries on the graph
      
      labs(title = "Global CO2 Emissions (2018)",
           subtitle = "Measuring National Emissions in 2018", 
           x = "National CO2 Emissions (Share of Global Total)",
           y = "Cumulative CO2 Emissions (Million Tonnes per Year)") +
      theme_bw()
    
# The labs and theme_bw function used to improve the aesthetics of the graph
    
    plotly_build(country_data)
    
# The function plotly_build allows me to create a plotly in the app
    
  })
  
# Plot 4:
  
  output$worldshare_map_plot <- renderImage({
    
    list(src = 'data/country_data/world_map.png',
         contentType = 'image/png',
         width = 700,
         height = 400)
    
# I discovered this code online as a way in which I could place images in my shiny
# app. This particular image depicts a map of the world, with countries filled in
# by a color that is representative of the role they play in global co2 emissions
    
  }, deleteFile = FALSE)
  
# Plot 5: 
  
  output$co2concentration_plot <- renderImage({
    
    list(src = 'data/temp_co2/co2_concentration.gif',
         contentType = 'image/gif',
         height = 400)
    
# I discovered this code online as a way in which I could place images in my shiny
# app. This particular image depicts a GIF, with time increasing from 1850 and showing
# the increase in global co2 emissions over the years
    
  }, deleteFile = FALSE)
  
# Plot 6:
  
  output$tempanomaly_plot <- renderImage({
    
    list(src = 'data/temp_co2/temp_anomaly.gif',
         contentType = 'image/gif',
         height = 400)

# I discovered this code online as a way in which I could place images in my shiny
# app. This particular image depicts a GIF, with time increasing from 1850 and showing
# the increase in temperature anomalies over the years
    
  }, deleteFile = FALSE)
  
# Plot 7:
  
  output$correlation_plot <- renderPlot({
    
    correlation %>%
      ggplot(aes(x = median_temp_anomaly,
                 y = co2_concentrations,
                 color = median_temp_anomaly)) +
      geom_point() +
      geom_smooth(method = "lm",
                  se = FALSE,
                  color = "black",
                  linetype = 2) +
      
# This graph will plot the correlation that exists between increasing temperature
# anomalies and increasing co2 concentrations. It brings together the data from
# the two GIFs created in this project
      
      theme_few() +
      labs(title = "Global Median Temperature Anomalies v. Global CO2 Concentrations",
           subtitle = "Examining the Correlation Between These 2 Variables Over Time",
           x = "Median Temperature Anomaly",
           y = "CO2 Concentration (Parts per Million)") +
      theme(legend.title = element_blank())
    
# The labs and theme_bw function used to improve the aesthetics of the graph
    
  })
  
# Plot 8:
  
  output$continent_co2_plot <- renderPlot({
  
    continent_data %>%
      select(country,
             year,
             co2) %>%
      ggplot(aes(x = year,
                 y = co2,
                 group = country)) +
      facet_wrap(~ country) +
      geom_line() +
    
# The code above will create six different graphs, one for each continent in the 
# data set. Each graph depicts the continent's co2 emission increase over time
    
      theme_bw() +
      labs(title = "Continental Carbon Emissions Over Time",
           x = "Year",
           y = "CO2 Concentrations (Parts per Million)")
  
# The labs and theme_bw function used to improve the aesthetics of the graph
  
  })
  
# Plot 9:
  
  output$country_co2_plot <- renderPlot({
    
    country_data %>%
      select(country,
             year,
             co2) %>%
      filter(country %in% input$country) %>%
      ggplot(aes(x = year,
                 y = co2,
                 group = country,
                 color = country)) +
      geom_smooth(method = "lm",
                  se = FALSE,
                  color = "black",
                  linetype = 2) +
      geom_point() +
      
# The filter function will allow me to create an interactive graph in my shiny app,
# where the user will be able to select from a list of countries the country they 
# seek to learn more about. When they do, this code will produce a graph depicting 
# the country's carbon emissions over time
      
      theme_classic() +
      labs(title = "National CO2 Emissions",
           x = "Year",
           y = "CO2 Concentrations (Parts per Million)") +
      scale_color_discrete(name = "Country")
    
# The labs and theme_classic functions are used to improve the aesthetics of the graph
# and the scale_color_discrete function changes the name of the legend to something more
# visually appealing
    
  })
  
# Plot 10:
  
  output$country_pop_plot <- renderPlot({
    
    country_data %>%
      select(country,
             year,
             population) %>%
      filter(country %in% input$country) %>%
      ggplot(aes(x = year,
                 y = population,
                 group = country,
                 color = country)) +
      geom_smooth(method = "lm",
                  se= FALSE,
                  color = "black",
                  linetype = 2) +
      geom_point() +
      
# The filter function will allow me to create an interactive graph in my shiny app,
# where the user will be able to select from a list of countries the country they 
# seek to learn more about. When they do, this code will produce a graph depicting 
# the country's population over time
      
      theme_classic() +
      labs(title = "National Population Growth",
           x = "Year",
           y = "Population") +
      scale_color_discrete(name = "Country")
    
# The labs and theme_classic functions are used to improve the aesthetics of the graph
# and the scale_color_discrete function changes the name of the legend to something more
# visually appealing
    
  })
  
# Plot 11:
  
  output$country_gdp_plot <- renderPlot({
    
    country_data %>%
      select(country,
             year,
             gdp) %>%
      filter(country %in% input$country) %>%
      ggplot(aes(x = year,
                 y = gdp,
                 group = country,
                 color = country)) +
      geom_smooth(method = "lm",
                  se = FALSE, 
                  color = "black",
                  linetype = 2) +
      geom_point() +
      
# The filter function will allow me to create an interactive graph in my shiny app,
# where the user will be able to select from a list of countries the country they 
# seek to learn more about. When they do, this code will produce a graph depicting 
# the country's gdp over time
      
      theme_classic() +
      labs(title = "National GDP Growth",
           x = "Year",
           y = "GDP") +
      scale_color_discrete(name = "Country")
    
# The labs and theme_classic functions are used to improve the aesthetics of the graph
# and the scale_color_discrete function changes the name of the legend to something more
# visually appealing
    
  })
  
# Plot 12: 
  
  output$tbl_regression_1 <- renderImage({
    
    list(src = 'data/model/brazil_table.png',
         contentType = 'image/png',
         width = 400,
         height = 400)
    
# I discovered this code online as a way in which I could place images in my shiny
# app. This particular image depicts a regression table, containing information such
# as the regression's confidence interval and beta values for each variable
    
  }, deleteFile = FALSE)
  
# Plot 13:
  
  output$tbl_regression_2 <- renderImage({
    
    list(src = 'data/model/brazil_land_table.png',
         contentType = 'image/png',
         width = 400,
         height = 300)
    
# I discovered this code online as a way in which I could place images in my shiny
# app. This particular image depicts a regression table, containing information such
# as the regression's confidence interval and beta values for each variable
    
  }, deleteFile = FALSE)
  
# Plot 14:
  
  output$my_picture <- renderImage({
    
    list(src = 'text/andre_f.png',
         contentType = 'image/png',
         width = 300,
         height = 400)

# This online code to place images in my shiny app allows me to upload a picture 
# of myself for the About Tab
    
  }, deleteFile = FALSE)
  
}

# END OF SERVER CODE

shinyApp(ui = ui, server = server)
