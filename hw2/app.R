library(shiny)
library(ggvis)
library(reshape2)

# note: i deleted the top four rows and trailing empty columns in csv so i could read it in properly
life <- read.csv('API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv', sep="\t")
fert <- read.csv('API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv', sep="\t")
cont <- read.csv('Metadata_Country_API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv')
pop <- read.csv('API_SP.POP.TOTL_DS2_en_csv_v2.csv')

# unclassified row 
life <- life[-109,-c(2,3,4)]
fert <- fert[-109,c(1,5:59)]
pop <- pop[-109,c(1,5:59)]
colnames(life) <- gsub("^X", "",  colnames(life))
colnames(fert) <- gsub("^X", "",  colnames(fert))
colnames(pop) <- gsub("^X", "",  colnames(pop))
life <- melt(life, id.vars = c("Country.Name"))
names(life) <- c('country', 'year', 'life_expectancy')
fert <- melt(fert, id.vars = c("Country.Name"))
names(fert) <- c('country', 'year', 'fertility_rate')
pop <- melt(pop, id.vars = c("Country.Name"))
names(pop) <- c('country', 'year', 'population')

life$region <- rep(cont$Region, 55)
life$fertility_rate <- fert$fertility_rate
life$population <- pop$population
life <- life[-which(life$region == ""), ]
life <- life[complete.cases(life),]
life$year <- as.numeric(as.character(life$year))
life <- life[order(life$population, decreasing = T),]

ui <- fluidPage(
  headerPanel(title = 'Life Expectancy vs. Fertility Rate Interactive Plot'),
  mainPanel(
    uiOutput("ggvis_ui"),
    ggvisOutput("ggvis"),
    selectInput("continent", label = NULL,
                choices = list("All Regions" = 1, "East Asia & Pacific" = "East Asia & Pacific", 
                               "Europe & Central Asia" = "Europe & Central Asia",
                               "Latin America & Caribbean" = "Latin America & Caribbean", 
                               "Middle East & North Africa" = "Middle East & North Africa",
                               "North America" = "North America", "South Asia" = "South Asia", 
                               "Sub-Saharan Africa" = "Sub-Saharan Africa"), 
                selected = 1),
    sliderInput("size", "Population scaling", 1, 10, 5, ticks = FALSE),
    sliderInput("year", "Year", 1960, 2014, 1983, sep = "", ticks = FALSE, animate = animationOptions(interval = 100))
  )
)

server <- function(input, output) {
  life$id <- 1:nrow(life)
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- life[life$id == x$id, ]
    paste(paste("<b>", row$country, "</b>"),paste("Life Expectancy: ",row$life_expectancy),
          paste("Fertility Rate: ",row$fertility_rate),paste("Population: ",row$population),sep="<br />")
  }
  vis <- reactive({
    life$population <- life$population*input$size
    life <- life[life$year == input$year,]
    if (input$continent != 1) {
      keep <- life[life$region == input$continent,]
      exclude <- life[-(life$region == input$continent),]
      ggvis() %>%
        layer_points(data=keep, ~life_expectancy, ~fertility_rate, size := ~population/1000000, key := ~id,
                     fill = ~factor(region), fillOpacity := 0.7, fillOpacity.hover := 1, stroke := "black")  %>%
        layer_points(data=exclude, ~life_expectancy, ~fertility_rate, size := ~population/1000000, key := ~id,
                     fill = ~factor(region), fillOpacity := 0.1, fillOpacity.hover := 1, stroke := "black")  %>%
        add_axis("x", title = "Life Expectancy") %>%
        add_axis("y", title = "Fertility Rate") %>%
        add_tooltip(all_values, "hover") %>%
        add_legend("fill", title = "Region") %>%
        scale_numeric("x", domain = c(10,90), clamp=TRUE) %>%
        scale_numeric("y", domain = c(0.5,9), clamp=TRUE) %>%
        scale_ordinal("fill", domain=c("East Asia & Pacific", "Europe & Central Asia", "Latin America & Caribbean", "Middle East & North Africa", 
                                       "North America", "South Asia", "Sub-Saharan Africa"), range=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", 
                                                                                                     "#ff7f00", "#ffff33", "#a65628")) %>%
        set_options(width = 800, height = 500)
    }else {
      keep <- life
      ggvis() %>%
        layer_points(data=keep, ~life_expectancy, ~fertility_rate, size := ~population/1000000, key := ~id,
                     fill = ~factor(region), fillOpacity := 0.7, fillOpacity.hover := 1, stroke := "black")  %>%
        add_axis("x", title = "Life Expectancy") %>%
        add_axis("y", title = "Fertility Rate") %>%
        add_tooltip(all_values, "hover") %>%
        add_legend("fill", title = "Region") %>%
        scale_numeric("x", domain = c(10,90), clamp=TRUE) %>%
        scale_numeric("y", domain = c(0.5,9), clamp=TRUE) %>%
        scale_ordinal("fill", range=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", "#a65628")) %>%
        set_options(width = 800, height = 500)
    }
  })
  vis %>% bind_shiny("ggvis", "ggvis_ui")
}

shinyApp(ui = ui, server = server)

