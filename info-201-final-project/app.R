#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)

# Statically load in the datasets
demographics <- read_delim("../data/demographics.csv")
economic <- read_delim("../data/economic.csv")
spending <- read_delim("../data/governmentspending.csv")

# Clean up spending dataset
spending <- spending %>%
  
  # Only keep variables with actual data
  select("Country Name", "Country Code", "2000":"2019") %>%
  
  # Also change names for ease of use
  rename(
    name = "Country Name",
    iso3 = "Country Code"
  ) %>%
  
  # Pivot the data so that each row is an observation
  # This makes it so each row now has the spending for only one year
  # (Makes grouping/filtering easier)
  pivot_longer(
    cols = matches("[0-9]{4}"),
    names_to = "time",
    values_to = "spending"
  ) %>%
  
  # Change "time" variable to double to match other datasets
  mutate(time = as.double(time)) %>%
  
  # Now remove any missing observations
  filter(!is.na(spending))

# Clean up economic dataset
economic <- economic %>%
  
  # Keep only useful variables
  select("Indicator", "LOCATION", "Country", "Time", "Value") %>%
  
  # Rename variables for clarity
  rename(
    indicator = "Indicator",
    iso3 = "LOCATION",
    name = "Country",
    time = "Time",
    value = "Value"
  ) %>%
  
  # Pivot wider so that there aren't as many duplicated observations
  pivot_wider(
    id_cols = c(iso3, name, time),
    names_from = indicator,
    values_from = value
  )

# Finally, join them together to make a combined dataset
combined <- full_join(
  demographics,
  economic,
  c("iso3", "time")
)

combined <- full_join(
  combined,
  spending,
  c("iso3", "time")
)

# Turn double year values into integers
combined <- combined %>%
  mutate(time = as.integer(time))

# Define UI for application
ui <- fluidPage(
  # Break up UI into multiple tabs
  tabsetPanel(
    # First panel, "Overview" section
    tabPanel(
      title = "Overview",
      
      h1("Goals"),
      p("The goals of our application are to provide individuals and 
        policymakers with data that will allow them to make connections 
        between countries' general spending on health, and how it may influence 
        various socioeconomic and demographic indicators. We hope that it will
        be used to make evidence-based policy decisions that improve health outcomes 
        for countries."),
      h1("Data Sources"),
      p("Part of the data comes from UNESCO's demographic and socio-economic 
        indicators dataset, found ",
        a("here", .noWS = "outside", href = "http://data.uis.unesco.org/Index.aspx?DataSetCode=demo_ds#"),
        ". Another part of the data comes from the World Bank's website, which sourced 
        its data from the World Health Organization's Global Health Expenditure database. 
        This data is found ",
        a("here", .noWS = "outside", href = "https://data.worldbank.org/indicator/SH.XPD.GHED.PP.CD"),
        ". Another part of the data comes from the Gapminder dataset, which is a conglomeration 
        of multiple indicators, and is found ",
        a("here", .noWS = "outside", href = "https://www.gapminder.org/data/"),
        "."),
      h1("Dataset Sample"),
      p("Below is a random sample from the cleaned up and processed dataset. 
        Note that each observation is identified by country and year, and the data 
        from the three datasets has been joined on these identifiers in a wide format. 
        Note that there may be missing values for some variables, especially those that 
        are outside of 2000-2019."),
      tableOutput("overview_table")
    ),
    
    # Second panel, interactive page
    tabPanel(
      title = "Interactive 1",
      
      sidebarLayout(
        sidebarPanel(
          sliderInput("gdpvs", "Year: ",
                      min = 2000,
                      max = 2019,
                      value = 2000)
          
        ),
        
        mainPanel(
          plotOutput("scatterplot")
        )
      )
    ),
    
    # Third panel, interactive page
    tabPanel(
      title = "Interactive 2",
      
      sidebarLayout(
        sidebarPanel(
          p("This slider allows you to select the year of the data."),
          sliderInput("yrp2", "Year:",
                      min = 1960,
                      max = 2019,
                      value = 2000)
        ),
        
        mainPanel(
          p("This table shows a comparison of GDP per capita and CO2 
            emissions per capita (in metric tons of CO2). The data is meant 
            to elucidate the relationship between wealth and unsustainability. 
            Over the years, the meaning of wealth in the context of sustainability
            has shifted from having the resources to industrialize, to having 
            the resources to be more sustainable."),
          plotlyOutput("p2plot")
        )
      )
    ),
    
    # Fourth panel, interactive page
    tabPanel(
      title = "Interactive 3",
      
      sidebarLayout(
        sidebarPanel(
          p("This chart attempts to demonstrate whether there are any 
            meaningful correlations between a country's health expenditures 
            and various indicators of wellbeing, and, if so, how these correlations 
            may vary between specific countries or based on other factors."),
          checkboxInput("average3", "Average over year?"),
          sliderInput("year3",
                       "Year:",
                       value = 2000,
                       min = 2000,
                       max = 2019,
                       step = 1,
                       sep = ""
                       ),
          selectInput("indicator3",
                      "Select indicator:",
                      sort(colnames(combined))[-c(32:34, 36:38, 44:48)]
                      )
        ),
        
        mainPanel(
          plotOutput("plot3"),
          textOutput("correlation3")
        )
      )
    ),
    
    # Final panel, conclusions and takeaways
    tabPanel(
      title = "Conclusion",
      
      p("This is a placeholder conclusion page.")
    )
  )
  
)

# Define server logic
server <- function(input, output) {
  
  # Logic for interactive 3
  health_data <- reactive({
    
    result <- combined %>%
      
      # First filter out any observations with no spending data
      filter(!is.na(spending)) %>%
      
      filter(!is.na(get(input$indicator3)))
      
    if (input$average3) {
      result <- result %>% group_by(iso3, region) %>% summarize(spending = mean(spending), blah = mean(get(input$indicator3)))
    } else {
      result <- result %>% filter(time == input$year3)
    }
    
    result
  })
  
  # Plot logic for interactive 3
  output$plot3 <- renderPlot({
    health_data() %>%
      ggplot(aes(x = spending,
                 y = if (!input$average3) get(input$indicator3) else blah,
                 color = region)) +
      geom_point() +
      scale_x_log10() + 
      scale_color_manual(values = c("#E69F00", "#F0E442", "#0072B2", "#D55E00", "#009E73")) +
      labs(x = "Domestic General Government Health Expenditure P.C., PPP (current international $)",
           y = input$indicator3,
           color = "Region")
  })
  
  # Correlation logic for interactive 3
  output$correlation3 <- renderText({
    paste(
      sep = "",
      "The correlation between ",
      input$indicator3,
      " and general domestic health expenditures is ",
      cor(health_data()$spending, if (!input$average3) health_data()[[input$indicator3]] else health_data()$blah),
      "."
    )
  })

  #Code for
  gdpvs <- reactive({
    filter(combined, time %in% input$gdpvs)
  })
  output$scatterplot <- renderPlot({
    ggplot(gdpvs(), aes(x = GDP_PC, y = lifeExpectancy, color = region)) +
      geom_point() +
      labs(title = "GDP per capita vs. life expectancy", x = "GDP per capita (USD)", y = "Life expectancy (Years)")
  })
  # Get random sample of the dataset for the overview page
  output$overview_table <- renderTable({
    combined %>%
      slice_sample(n = 5) %>%
      select("iso3", "name.x", "time", "lifeExpectancy",
             "childMortality", "spending", "GDP growth (annual %)")
  })


  
  p2yrly <- reactive({
    p222 <- combined %>%
      filter(time %in% input$yrp2)  
  })
  output$p2plot <- renderPlotly({
    plot_ly(data = p2yrly(),
            x = ~GDP_PC, y = ~co2_PC, color = ~region,
            marker = list(size = 10),
            type = 'scatter',
            mode = 'markers') %>% 
            layout(
            xaxis = list(title = 'GDP Per Capita (USD)'), 
            yaxis = list(title = 'CO2 Emissions Per Capita (Metric Tons)')
            )  
})
}
# Run the application 
shinyApp(ui = ui, server = server)
