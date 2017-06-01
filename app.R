library(shiny)
library(ggplot2)


load("D:/Duke/Work(Environ)/Programming/public_data-practice/public_data.Rdata")

#dygraphs experimenting
#dygraph(chem.annual, main = "not quite right dygraph", xlab = "possibly row numberings", 
#        ylab = "looks like water.year but also the chemical amounts...")
library(dygraphs)
library(magrittr)
library(xts)
xts(chem.annual$Ca, order.by = chem.annual$datetime) %>%
  dygraph(., main = 'dygraph numba 2')
#make sites columns using dplyr

#library(dygraphs)
#library(magrittr)
#library(xts)
#xts(hydro.annual$P, order.by = hydro.annual$water.year) %>%
#  dygraph(., main = 'dygraph numba 3')


ui <- fluidPage(
  titlePanel(
    ("Hubbard Brook Data")
  ),
  sidebarLayout(
    sidebarPanel(
      p('Welcome to the world of learning... It is a small world after all')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(("chem.annual"), plotOutput("c"), 
                 checkboxGroupInput("chem", "Select a chemical", 
                                    choices = list("Calcium" = "Ca", 
                                            "Magnesium" = "Mg",
                                            "Phosphorus" = "K",
                                            "Sodium" = "Na",
                                            "Aluminum" = "Al",
                                            "NH4" = "NH4",
                                            "SO4" = "SO4",
                                            "NO3" = "NO3",
                                            "Chlorine" = "Cl",
                                            "PO4" = "PO4",
                                            "SiO2" = "siO2",
                                            "Hydrogen" = "H"), selected = "Ca")),
        tabPanel(("hydro.annual"), plotOutput("h")),
        tabPanel(("experimental"), plotOutput("s"))
      )
    )
  ))

server <- function(input, output) {
  output$c <- renderPlot({
      ggplot(chem.annual, aes(water.year)) +
      geom_smooth(aes(y = Ca, col = "Ca"), se = FALSE)
      geom_smooth(aes(y = Mg, col = "Mg"), se = FALSE) +
      geom_smooth(aes(y = K, col = "K"), se = FALSE) +
      geom_smooth(aes(y = Na, col = "Na"), se = FALSE) +
      geom_smooth(aes(y = Al, col = "Al"), se = FALSE) +
      geom_smooth(aes(y = NH4, col = "NH4"), se = FALSE) +
      geom_smooth(aes(y = SO4, col = "SO4"), se = FALSE) +
      geom_smooth(aes(y = NO3, col = "NO3"), se = FALSE) +
      geom_smooth(aes(y = Cl, col = "Cl"), se = FALSE) +
      geom_smooth(aes(y = PO4, col = "PO4"), se = FALSE) +
      geom_smooth(aes(y = SiO2, col = "SiO2"), se = FALSE) +
      geom_smooth(aes(y = H, col = "H"), se = FALSE)
  })
  output$h <- renderPlot({
    ggplot(hydro.annual, aes (water.year)) +
      geom_smooth(aes(y = P, col = "P"), se = FALSE) +
      geom_smooth(aes(y = Q, col = "Q"), se = FALSE) +
      geom_smooth(aes(y = ET, col = "ET"), se = F) +
      geom_point(aes(y = ET, col = "ET"))
  })
  output$s <- renderPlot({
    ggplot(chem.annual, aes(water.year)) +
      geom_point(aes(y = K, col = "K")) +
      geom_point(aes(y = Cl, col = "Cl"))
  })
}

shinyApp(ui = ui, server = server)
