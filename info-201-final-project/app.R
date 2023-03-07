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

# Define UI for application
ui <- fluidPage(
  # Break up UI into multiple tabs
  tabsetPanel(
    # First panel, "Overview" section
    tabPanel(
      title = "Overview",
      
      p("This is a placeholder overview page.")
    ),
    
    # Second panel, interactive page
    tabPanel(
      title = "Interactive 1",
      
      sidebarLayout(
        sidebarPanel(
          p("This is a placeholder sidebar panel.")
        ),
        
        mainPanel(
          p("This is a placeholder main panel.")
        )
      )
    ),
    
    # Third panel, interactive page
    tabPanel(
      title = "Interactive 2",
      
      sidebarLayout(
        sidebarPanel(
          p("This is a placeholder sidebar panel.")
        ),
        
        mainPanel(
          p("This is a placeholder main panel.")
        )
      )
    ),
    
    # Fourth panel, interactive page
    tabPanel(
      title = "Interactive 3",
      
      sidebarLayout(
        sidebarPanel(
          p("This is a placeholder sidebar panel.")
        ),
        
        mainPanel(
          p("This is a placeholder main panel.")
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)
