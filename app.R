#load libraries
library(shiny)
library (leaflet)
library (dplyr)
library (stringr)
library (ggplot2)

neighbourHood <- c('West Hill', 'Long Branch', 'Woodbine Corridor', 'Bathurst Manor', 'Englemount-Lawrence', 'Islington-City Centre West', 'Church-Yonge Corridor', 'Mount Dennis', 'Annex', 'Oakwood Village', 'Newtonbrook East', 'Woburn', 'Scarborough Village', 'Kennedy Park', 'Yonge-Eglinton', 'Newtonbrook West', 'Bay Street Corridor', 'Greenwood-Coxwell', 'West Humber-Clairville', 'Markland Wood', 'York University Heights', 'Bendale', 'Thorncliffe Park', "Tam O'Shanter-Sullivan", 'Bedford Park-Nortown', 'Malvern', 'Mimico', 'Edenbridge-Humber Valley', 'Rouge', 'Eringate-Centennial-West Deane', 'Humber Summit', 'Weston', 'Yorkdale-Glen Park', 'North St.James Town', 'Pelmo Park-Humberlea', 'Clanton Park', 'Junction Area', 'Highland Creek', 'Parkwoods-Donalda', 'Princess-Rosethorn', 'Wexford/Maryvale', 'Lawrence Park North', 'Victoria Village', 'Kingsway South', 'Willowdale West', 'Banbury-Don Mills', 'Lansing-Westgate', 'Humber Heights-Westmount', 'Black Creek', 'East End-Danforth', 'Glenfield-Jane Heights', 'Lawrence Park South', 'Eglinton East', 'Bridle Path-Sunnybrook-York Mills', 'Cliffcrest', 'Kingsview Village-The Westway', 'Lambton Baby Point', 'Dufferin Grove', 'High Park North', 'Leaside-Bennington', 'Agincourt North', 'Stonegate-Queensway', 'Dorset Park', 'Pleasant View', 'Agincourt South-Malvern West', 'Dovercourt-Wallace Emerson-Junction', 'Downsview-Roding-CFB', 'Rexdale-Kipling', 'Beechborough-Greenbrook', 'Keelesdale-Eglinton West', 'Weston-Pellam Park', 'Rosedale-Moore Park', "L'Amoreaux", 'Waterfront Communities-The Island', 'Forest Hill North', 'Steeles', 'Humbermede', 'Little Portugal', 'Caledonia-Fairbank', "O'Connor-Parkview", 'Briar Hill-Belgravia', 'Wychwood', 'Mount Pleasant East', 'Milliken', 'Alderwood', 'Willowridge-Martingrove-Richview', 'Morningside', 'Don Valley Village', 'High Park-Swansea', 'Danforth East York', 'Clairlea-Birchmount', 'Maple Leaf', 'Oakridge', 'Thistletown-Beaumond Heights', 'Bayview Woods-Steeles', 'Flemingdon Park', 'Moss Park', 'Rockcliffe-Smythe', 'Westminster-Branson', 'Birchcliffe-Cliffside', 'Palmerston-Little Italy', 'Trinity-Bellwoods', 'Old East York', 'Mount Olive-Silverstone-Jamestown', 'St.Andrew-Windfields', 'Bayview Village', 'Corso Italia-Davenport', 'Willowdale East', 'Kensington-Chinatown', 'Rustic', 'Brookhaven-Amesbury', 'South Riverdale', 'Henry Farm', 'South Parkdale', 'Humewood-Cedarvale', 'Playter Estates-Danforth', 'The Beaches', 'Hillcrest Village', 'Runnymede-Bloor West Village', 'Niagara', 'Eringate-Centennial_West Deane', 'Danforth', 'Forest Hill South', 'Elms-Old Rexdale', 'Mount Pleasant West', 'Centennial Scarborough', 'Taylor-Massey', 'Ionview', 'Roncesvalles', 'Regent Park', 'Cabbagetown-South St.James Town', 'Etobicoke West Mall', 'Casa Loma', 'University', 'North Riverdale', 'Yonge-St.Clair', 'Broadview North', 'Guildwood', 'New Toronto', 'Woodbine-Lumsden', 'Blake-Jones')
scarborough <- c('Wexford/Maryvale','Clairlea-Birchmount','Centennial Scarborough','Birchcliffe-Cliffside','Agincourt North', 'Agincourt South-Malvern West', 'Kennedy Park', 'The GOlden Mile', 'Dorset Park', 'Wexford','Maryvale', 'Agincourt', 'Armadale', 'Bendale', 'Birch Cliff', 'Birch Cliff Heights', "Brown's Corners", 'Clairlea', 'Cliffside', 'Cliffcrest', 'Dorset Park', 'Eglinton East', 'Golden Mile', 'Guildwood', 'Highland Creek', 'Ionview', "L'Amoreaux", 'Malvern', 'Maryvale', 'Milliken', 'Morningside', 'Morningside Heights', 'Oakridge', 'Port Union', 'Rouge', 'Scarborough City Centre', 'Scarborough Junction', 'Scarborough Village', 'Steeles', "Tam O'Shanter-Sullivan", 'West Hill', 'West Rouge', 'Wexford', 'Woburn')
NorthYork <- c('St.Andrew-Windfields',"Mount Pleasant West","Mount Pleasant East",'Yorkdale-Glen Park','Willowdale West','Willowdale East','Pelmo Park-Humberlea','Parkwoods-Donalda','Glenfield-Jane Heights','Downsview-Roding-CFB',"Clanton Park",'Brookhaven-Amesbury','Banbury-Don Mills', 'Newtonbrook East','Newtonbrook West' ,'Englemount-Lawrence', 'Amesbury', 'Armour Heights', 'Bathurst Manor', 'Bayview Village', 'Bayview Woods-Steeles', 'Bermondsey', 'Black Creek', 'The Bridle Path', 'Clanton Park (Wilson Heights)', 'Don Mills', 'Don Valley Village', 'Downsview', 'Flemingdon Park', 'Glen Park', 'Yorkdale', 'Englemount', 'Marlee Village', 'Henry Farm', 'Hillcrest Village', 'Hoggs Hollow', 'Humber Summit', 'Humbermede', 'Emery', 'Jane and Finch', 'University Heights', 'Elia', 'Lansing', 'Lawrence Heights', 'Lawrence Manor', 'Ledbury Park', 'Maple Leaf', 'Newtonbrook', 'North York City Centre', 'Parkway Forest', 'Parkwoods', 'The Peanut', 'Pelmo Park', 'Humberlea', 'Pleasant View', 'Uptown Toronto', 'Victoria Village', 'Westminster-Branson', 'Willowdale', 'York Mills', 'York University Heights', 'Village at York')
DowntownCoreCentral <- c("Waterfront Communities-The Island",'North St.James Town','Bay Street Corridor','Kensington-Chinatown','Cabbagetown-South St.James Town','Church-Yonge Corridor', 'Annex', 'Alexandra Park', 'The Annex', 'Baldwin Village', 'Cabbagetown', 'CityPlace', 'Chinatown', 'Church and Wellesley', 'Corktown', 'Discovery District', 'Distillery District', 'The Entertainment District', 'East Bayfront', 'Fashion District', 'Financial District', 'Garden District', 'Grange Park', 'Harbord Village', 'Harbourfront', 'Kensington Market', 'Little Japan', 'Moss Park', 'Old Town', 'Quayside', 'Queen Street West', 'Regent Park', 'South Core', 'St. James Town', 'St. Lawrence', 'Toronto Islands', 'Trefann Court', 'University', 'Huron-Sussex', 'Yorkville')
EastEnd <- c('South Riverdale','Playter Estates-Danforth','North Riverdale','East End-Danforth', 'The Beaches', 'The Beach', 'East Chinatown', 'East Danforth', 'Gerrard Street East', 'Gerrard India Bazaar', 'Little India', 'Greektown', 'Danforth', 'Leslieville', 'Main Square', 'Playter Estates', 'Port Lands', 'Villiers Island', 'Riverdale', 'Upper Beaches')
WestEnd <- c('South Parkdale','Runnymede-Bloor West Village','Palmerston-Little Italy','Junction Area','High Park North','High Park-Swansea','Dovercourt-Wallace Emerson-Junction','Corso Italia-Davenport','Beaconsfield Village', 'Bloor West Village', 'Bracondale Hill', 'Brockton Village', 'Carleton Village', 'Corso Italia', 'Davenport', 'Dovercourt Park', 'Dufferin Grove', 'Earlscourt', 'Fort York', 'High Park', 'The Junction', 'West Toronto', 'Little Malta', 'Junction Triangle', 'Koreatown', 'Liberty Village', 'Little Italy', 'Little Portugal', 'Little Tibet', 'Mirvish Village', 'Niagara', 'Palmerston', 'Parkdale', 'Queen Street West', 'Roncesvalles', 'Runnymede', 'Seaton Village', 'Swansea', 'Trinity-Bellwoods', 'Wallace Emerson')
EastYork <- c('Greenwood-Coxwell',"Blake-Jones",'Woodbine-Lumsden','Taylor-Massey','Leaside-Bennington','Danforth East York',"Bridle Path-Sunnybrook-York Mills", 'Woodbine Corridor', 'Old East York', 'Broadview North', 'Crescent Town', 'East Danforth', 'Pape Village', 'Woodbine Heights', 'Suburban East York', 'Bermondsey', "Governor's Bridge", 'Leaside', "O'Connor-Parkview", 'Thorncliffe Park')
Etobicoke <- c("Willowridge-Martingrove-Richvie","West Humber-Clairvill",'Thistletown-Beaumond Height','Rexdale-Kiplin','Princess-Rosethorn',"Mount Olive-Silverstone-Jamestow",'Mimico (includes Humber Bay Shores)','Kingsway South','Kingsview Village-The Westwa','Humber Heights-Westmoun','Elms-Old Rexdal','Eringate-Centennial_West Deane',"Eringate-Centennial-West Deane",'Edenbridge-Humber Valle','Alderwood', 'Centennial Park', 'Clairville', 'Eatonville', 'Etobicoke West Mall', 'The Elms', 'Eringate', 'Humber Bay', 'Humber Heights- Westmount', 'Humber Valley Village', 'Humberwood', 'Islington-City Centre West', 'Kingsview Village', 'The Westway', 'The Kingsway', 'Long Branch', 'Markland Wood', 'Mimico', 'New Toronto', 'Princess Gardens', 'Rexdale', 'Richview', 'Smithfield', 'Stonegate-Queensway', 'Sunnylea', 'Thistletown', 'Thorncrest Village', 'West Humber-Clairville', 'West Deane Park', 'Willowridge')
York <- c('Beechborough-Greenbrook','Rustic','Lambton Baby Point','Keelesdale-Eglinton West','Baby Point', 'Briar Hill-Belgravia', 'Eglinton West', 'Little Jamaica', 'Fairbank', 'Caledonia-Fairbank', 'Humewood-Cedarvale', 'Upper Village', 'Forest Hill', 'Lambton', 'Mount Dennis', 'Oakwood-Vaughan', 'Oakwood Village', 'Five Points', 'Northcliffe', 'Old Mill', 'Rockcliffe-Smythe', 'Silverthorn', 'Keelesdale', 'Tichester', 'Weston')
NorthEnd <- c('Yonge-St.Clair','Wychwood','Weston-Pellam Park','Rosedale-Moore Park','Lawrence Park South','Lawrence Park North','Lansing-Westgate','Forest Hill South','Forest Hill North','Bedford Park-Nortown', 'Bedford Park', 'Casa Loma', 'Chaplin Estates', 'Davisville Village', 'Deer Park', 'Yonge and St. Clair', 'Forest Hill', 'Forest Hill Village', 'Upper Village', 'Lawrence Park', 'Lytton Park', 'Midtown', 'Moore Park', 'North Toronto', 'Rosedale', 'South Hill', 'Rathnelly', 'Summerhill', 'Uptown', 'Wanless Park', 'Wychwood Park', 'Yonge-Eglinton', 'Midtown Toronto')



#import data
data <- read.csv("Auto_Thefts_new.csv", strip.white = TRUE)
data <- data %>% mutate(Area = ifelse(Neighbou %in% scarborough, yes = "Scarborough", 
                                      ifelse (Neighbou %in% NorthYork, yes = "North York", 
                                              ifelse (Neighbou %in% DowntownCoreCentral, yes = "Downtown Core (Central)", 
                                                      ifelse (Neighbou %in% EastEnd, yes = "East End", 
                                                              ifelse (Neighbou %in% WestEnd, yes = "West End", 
                                                                      ifelse(Neighbou %in% Etobicoke, yes = "Etobicoke", 
                                                                             ifelse(Neighbou %in% York, yes = "York",
                                                                                    ifelse(Neighbou %in% EastYork, yes = "East York",
                                                                                           ifelse(Neighbou %in% NorthEnd, yes = "North End", no = "Other"))))))))))

# Define UI ----
ui <- fluidPage(
  titlePanel("Interactive Toronto Auto Theft Visualization"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("checkGroup", h3("Month"), choices = list("January", "February", "March", "April", "May", "June", "July", "August" ,"September", "October", "November", "December"), selected = "Janurary"),
      actionLink("selectall", "Select All"),
      checkboxGroupInput("checkGroup2", h3("Year"), 
                  choices = list(2014, 2015,2016 , 2017, 2018 ), selected = 2018),
      actionLink("Selectall2", "Select All"),
      checkboxGroupInput("checkGroup3", h3("Toronto Areas"), choices = list("Downtown Core (Central)", "East End", "North End", "West End", "East York", "Etobicoke", "North York", "Scarborough", "York"), selected = "York"),
      actionLink("Selectall3", "Select All")
      
    ), 
    mainPanel (leafletOutput("map", "100%", 500), 
               plotOutput("bar"))
  ))


# Define server logic ----
server <- function(input, output, session){
  #select all button for month
  observe({
    if(input$selectall == 0) return(NULL)
       else if(input$selectall%%2==0)
         {
         updateCheckboxGroupInput(session, "checkGroup", choices = list("January", "February", "March", "April", "May", "June", "July", "August" ,"September", "October", "November", "December"))
       }
       else
       {
         updateCheckboxGroupInput(session, "checkGroup", choices = list("January", "February", "March", "April", "May", "June", "July", "August" ,"September", "October", "November", "December"), selected = list("January", "February", "March", "April", "May", "June", "July", "August" ,"September", "October", "November", "December"))
       }
  })
  #select all button for year
  observe({
    
    if(input$Selectall2 == 0) return(NULL)
      else if(input$Selectall2 %%2 == 0)
      {
        updateCheckboxGroupInput(session, "checkGroup2", choices = list(2014, 2015,2016 , 2017, 2018))
      }
        else
        {
          updateCheckboxGroupInput(session, "checkGroup2", choices = list(2014, 2015,2016 , 2017, 2018), selected = list(2014, 2015,2016 , 2017, 2018))
        }
  })
  #select al button for Toronto Neighborhoods
  observe({
    if(input$Selectall3 == 0) return(NULL)
    else if(input$Selectall3 %%2 == 0)
    {
      updateCheckboxGroupInput(session, "checkGroup3", choices = list("Downtown Core (Central)", "East End", "North End", "West End", "East York", "Etobicoke", "North York", "Scarborough", "York"))
    }
    else
    {
      updateCheckboxGroupInput(session, "checkGroup3", choices = list("Downtown Core (Central)", "East End", "North End", "West End", "East York", "Etobicoke", "North York", "Scarborough", "York"), selected = list("Downtown Core (Central)", "East End", "North End", "West End", "East York", "Etobicoke", "North York", "Scarborough", "York"))
    }
  })

    #filtering data for interactive map
    filtered <- reactive({
      if (is.null(input$checkGroup) & is.null(input$checkGroup2) & is.null(input$checkGroup3)){
        return (NULL)
      }
      data %>% filter(occurrencemonth %in% input$checkGroup & occurrenceyear %in% input$checkGroup2 & Area %in% input$checkGroup3)
    })
  
    #filtering data for bar plot
    filteredbar <- reactive({
      if (is.null(input$checkGroup3)){
        return(NULL)
      }
      data %>% filter(Area %in% input$checkGroup3 & occurrenceyear > 2013)
    })

  #printing map output
  output$map <- renderLeaflet({
    leaflet()%>%
      addProviderTiles("CartoDB") %>%
      addCircleMarkers(data = filtered(), radius = 2)
  })
  
  #printing bar plot output
  output$bar <- renderPlot({
    #barplot(filteredbar()[,occurrenceyear], xlab = "Years", ylab = "Number of Auto Thefts")
    ggplot(data = filteredbar(), aes(x = occurrenceyear))+
      geom_bar()+
     xlab("Year of Occurrence")+
      ylab("Number of Occurrences")+
       ggtitle("Number of Auto Thefts Over Time By Neighbourhood")
    })
}
  

# Run the app ----
shinyApp(ui = ui, server = server)

