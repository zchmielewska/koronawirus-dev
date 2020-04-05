
# Setup -------------------------------------------------------------------

# packages and scripts
library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(DT)
library(rdrop2)
source("utils/utility-functions.R")

# dropbox setup
token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)

# Polish functions --------------------------------------------------------

# Functions with polish letters (crash otherwise)
loadSettings <- function() {
    settings <- list()
    
    settings$y.vars <- tribble(
        ~ColumnName,   ~FullName,
        "CasesTotal",  "Zakażenia",
        "CasesDelta",  "Przyrost zakażeń",
        "DeathsTotal", "Zgony", 
        "DeathsDelta", "Przyrost zgonów"
    )
    
    settings$x.vars <- tribble(
        ~ColumnName,   ~FullName,
        "EpidemiaDay", "Dzień epidemii",
        "Date",        "Dzień kalendarzowy"
    )
    return(settings)
}
settings <- loadSettings()
getWorldDataTable <- function(world.data, ecdc.date) {
    result <- world.data %>% 
        filter(Date == ecdc.date) %>% 
        filter(complete.cases(.)) %>% 
        arrange(desc(CasesTotal)) %>% 
        mutate(CasesInPop = CasesTotal/popData2018,
               Mortality  = DeathsTotal/CasesTotal) %>% 
        select(Country, CasesTotal, DeathsTotal, popData2018, CasesInPop, Mortality) %>% 
        rename(`Kraj / terytorium` = Country,
               Zakażenia = CasesTotal,
               Zgony = DeathsTotal,
               Populacja = popData2018,
               `Zakażenia w populacji` = CasesInPop,
               `Śmiertelność` = Mortality)
    return(result)
}

# UI ----------------------------------------------------------------------

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Polska", tabName = "poland", icon = icon("flag")),
        menuItem("Świat",  tabName = "world",  icon = icon("globe")),
        menuItem("Dziś",   tabName = "today",  icon = icon("calendar")),
        menuItem("Info",   tabName = "info",   icon = icon("info"))
    )
)

body <- dashboardBody(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(

        # Poland UI ---------------------------------------------------------------
        
        tabItem(
            tabName = "poland",
            fluidRow(
                tableOutput("test")
            ),
            fluidRow(
                valueBoxOutput("epidemiaDayBox"),
                valueBoxOutput("casesBox"),
                valueBoxOutput("deathsBox")
            ),
            fluidRow(
                box(width = 9, solidHeader = TRUE,
                    plotly::plotlyOutput("polandPlot")
                    ),
                box(title = "Ustawienia", width = 3, status = "primary", solidHeader = TRUE,
                    selectInput(
                        inputId = "polandYVar",
                        label = "Zmienna", choices = settings$y.vars$FullName,
                        selected = "Całkowita liczba zakażeń"
                    ),
                    selectInput(
                        inputId = "polandXVar",
                        label = "Przebieg czasu", choices = settings$x.vars$FullName,
                        selected = "Dzień epidemii"
                    ),
                    checkboxInput("polandLogScale", "Skala logarytmiczna", value = FALSE),
                    hr(),
                    textOutput("datestamp")
                )
            )                    
        ),

        # World UI ----------------------------------------------------------------
        
        tabItem(
            tabName = "world",
            fluidRow(
                box(width = 9,
                    plotly::plotlyOutput("worldPlot")
                ),
                box(width = 3,
                    selectInput(
                     inputId = "worldCountry",
                     label = "Wybierz kraj", choices = c("China", "United_States_of_America"),
                     selected = c("China", "United_States_of_America"), multiple = TRUE
                    ),
                    selectInput(
                        inputId = "worldYVar",
                        label = "Zmienna", choices = settings$y.vars$FullName,
                        selected = "Całkowita liczba zakażeń"
                    ),
                    checkboxInput("worldLogScale", "Skala logarytmiczna", value = FALSE)
                )
            )
        ),
        tabItem(
            tabName = "today",
            fluidRow(
                box(width = 9,
                     DTOutput("todayTable")
                )
            )
        ),
        tabItem(
            tabName = "info",
            fluidRow(
                box(title = "Źródło", width = 6, 
                    status = "primary", solidHeader = TRUE,
                    p("Dane zasilające wykresy i tabelę pochodzą ze strony
                      ECDC (European Centre for Disease Prevention and Control).",
                      style="text-align: justify;"),
                    p("Są one publicznie dostępne na stronie:",
                      tags$a(href = "https://www.ecdc.europa.eu/en", "www.ecdc.europa.eu")),
                    p("Warto pamiętać, że liczba zakażeń to jedynie liczba potwierdzonych przypadków.
                      Dane ECDC nie zawierają informacji na temat liczby przeprowadzonych testów 
                      w danym kraju. Im więcej przeprowadzanych testów, tym więcej zidentyfikowanych
                      zakażeń, nawet w przypadkach lekkich objawów choroby. Ma to zatem wpływ na 
                      statystkę śmiertelność, która została wyznaczona jako liczba zgonów
                      do liczby potwierdzonych zakażeń. Faktyczna śmiertelność jest zatem niższa,
                      ponieważ wiele osób przechodzi chorobę bezobjawowo lub nie zgłosiło się
                      faktu zachorowania.", 
                      style="text-align: justify;")
                ),
                box(width = 3,
                    status = "primary", solidHeader = TRUE,
                    p("W przypadku jakichkolwiek problemów, proszę zgłoś",
                      tags$a(href = "https://github.com/zchmielewska/koronawirus/issues", "tutaj"),
                      "swoje uwagi.")
                    )
                )
            )
        )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
    
    # Prepare data ------------------------------------------------------------
    
    # output$test <- renderTable(
    #     poland.data2()
    # )
    
    notify <- function(msg, id = NULL) {
        showNotification(msg, id = id, duration = NULL, closeButton = FALSE)
    }

    ecdc.data <- reactive({
        id <- notify("Ładuję dane...")
        on.exit(removeNotification(id), add = TRUE)
        data <- loadLastFile()
        
        notify("Sprawdzam, czy są dostępne nowe dane...", id = id)
        data <- checkNewData(data)
        
        notify("Wczytuję dane ECDC...", id = id)
        ecdc.data <- prepareECDCdata(data)
    })
    
    ecdc.date <- reactive(
        ecdc.date <- ecdc.data() %>% select(Date) %>% arrange() %>% slice(1) %>% pull()    
    )
    
    poland.data <- reactive({
        id <- notify("Przygotowuję dane dla Polski...")
        on.exit(removeNotification(id), add = TRUE)
        getPolandData(ecdc.data())
       
    }) 
    
    world.data  <- reactive({
        id <- notify("Przygotowuję dane dla Świata...")
        on.exit(removeNotification(id), add = TRUE)
        getWorldData(ecdc.data())
    })
    
    world.data.today <- reactive({
        id <- notify("Przygotowuję dzisiejsze dane...")
        on.exit(removeNotification(id), add = TRUE)
        getWorldDataTable(world.data(), ecdc.date())
    })
    
    # Poland page -------------------------------------------------------------

    output$epidemiaDayBox <- renderValueBox({
        valueBox(
            value = as.double(difftime(lubridate::ymd(ecdc.date()), lubridate::ymd("2020-03-03"), units = "days")),
            subtitle = "Dzień epidemii", icon = icon("first-aid"),
            color = "purple"
        )
    })
    
    output$casesBox <- renderValueBox({
        poland.today <- filter(poland.data(), Date == ecdc.date())
        total        <- poland.today %>% select(CasesTotal) %>% pull()
        delta        <- poland.today %>% select(CasesDelta) %>% pull()
        valueBox(
            paste0(total, " (+", delta, ")"), "Zakażenia", icon = icon("diagnoses"),
            color = "orange"
        )
    })
    
    output$deathsBox <- renderValueBox({
        poland.today <- filter(poland.data(), Date == ecdc.date())
        total        <- poland.today %>% select(DeathsTotal) %>% pull()
        delta        <- poland.today %>% select(DeathsDelta) %>% pull()
        valueBox(
            paste0(total, " (+", delta, ")"), "Zgony", icon = icon("times"),
            color = "navy"
        )
    })
    
    output$polandPlot <- plotly::renderPlotly({
        x.var <- input$polandXVar # x.var <- "Dzień epidemii"
        x     <- settings$x.vars %>% filter(FullName == x.var) %>% select(ColumnName) %>% pull()
        y.var <- input$polandYVar # y.var <- "Całkowita liczba zakażeń"
        y     <- settings$y.vars %>% filter(FullName == y.var) %>% select(ColumnName) %>% pull()
        
        p1 <- ggplot(poland.data(), aes_string(x = x, y = y)) +
            geom_point() +
            geom_line() +
            theme(panel.grid.minor = element_blank()) +
            ggtitle(y.var) +
            xlab(x.var) +
            ylab("")
        
        if(input$polandLogScale) {
            p2 <- p1 + scale_y_continuous(trans='log10')
        } else {
            p2 <- p1
        }
        
        p2
    })
    
    output$datestamp <- renderText(
        paste("Dane z dnia:", ecdc.date())
    )
    
    # World page --------------------------------------------------------------

    output$worldPlot <- plotly::renderPlotly({
        y.var <- input$worldYVar # y.var <- "Całkowita liczba zakażeń"
        y     <- settings$y.vars %>% filter(FullName == y.var) %>% select(ColumnName) %>% pull()
        
        if(length(input$worldCountry) > 0) {
            world.data.plot <- filter(world.data(), Country %in% input$worldCountry)
            
            p1 <- ggplot(world.data.plot, aes_string(x = "Date", y = y, color = "Country")) +
                geom_point() +
                geom_line() +
                ggtitle(y.var) +
                xlab("") +
                ylab("")    
        } else {
            p1 <- ggplot() + ggtitle(y.var)
        }
        
        if(input$worldLogScale) {
            p2 <- p1 + scale_y_continuous(trans='log10')
        } else {
            p2 <- p1
        }
        
        p2
    })
    
    observeEvent(world.data(), {
        choices <- unique(world.data()$Country)
        updateSelectInput(session, "worldCountry", choices = choices, 
                          selected = c("China", "United_States_of_America")) 
    })

    # Today page --------------------------------------------------------------

    output$todayTable <- renderDT(
        datatable(world.data.today(),
        rownames = FALSE,
        selection = "none",
        options = list(lengthMenu = c(10, 50, 100))) %>% 
            formatPercentage(columns = c(5, 6), digits = 2)
    )
}

# Run application ---------------------------------------------------------

ui <- dashboardPage(
    dashboardHeader(title = "Koronawirus"),
    sidebar,
    body   
)

shinyApp(ui = ui, server = server)