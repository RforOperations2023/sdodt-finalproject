## Author: Sebastian Dodt
library(shiny)
library(shinydashboard)
library(dplyr)
library(shinythemes)
library(stringr)
library(vistime)
library(plotly)
library(ggthemes)
library(countrycode)
library(formattable)
library(httr)
library(leaflet)
library(dotenv)


# static variables
background_color <- "white"
load_dot_env(".env")
api_key <- Sys.getenv("api_key")


# loading files
load("data/ship_ids.Rdata")
transshipments <- readRDS("data/dataset.RDS")
port_visits <- readRDS("data/port.RDS")
subtitle_text <- readRDS("data/subtitle.RDS")
flag_choices <- readRDS("data/flags.RDS")
sv <- read.csv("data/current_location.csv")


# The following call has been shortened in loading time
# by pre-filtering the needed boarders in data_processing.R
# all_eez <- sf::st_read("data/World_EEZ_v11_20191118/eez_v11.shp")
us_eez_shp <- readRDS("data/shapefiles_for_us_eez.RDS") 


# pre-processing for home tab
worst_offenders <- transshipments %>%
  group_by(vessel.mmsi) %>%
  summarise(number_of_meetings = n_distinct(id)) %>%
  mutate(percentile = percent_rank(number_of_meetings)) %>%
  select(vessel.mmsi, percentile, number_of_meetings)
suspicion_low <- worst_offenders %>%
  pull(vessel.mmsi)
suspicion_medium <- worst_offenders %>%
  filter(percentile >= 0.5) %>%
  pull(vessel.mmsi)
suspicion_high <- worst_offenders %>%
  filter(percentile >= 0.9) %>%
  pull(vessel.mmsi)

# pre-processing for reefer search tab
recently_updated <- sv %>%
  mutate(last_transmission = as.Date(
    substr(sv$Time.of.Fix..formatted., 2, 11), format = "%Y-%m-%d")
  ) %>%
  filter(last_transmission > as.Date("2023-02-01")) %>%
  pull(MMSI)
ship_mmsi_recent <- ship_mmsi[ship_mmsi %in% recently_updated]
ship_mmsi_recent <- ship_mmsi_recent[ship_mmsi_recent %in% suspicion_high]

## NATO
nato_countries <- read.csv(file = "data/nato_countries.csv")
nato_countries <- nato_countries %>%
  filter(CAT == "NATO") %>%
  pull(CTR) %>%
  unique()
flag_list <- function(selection) {
  if(selection == "U.S.") return(c("USA"))
  if(selection == "NATO") return(nato_countries)
  if(selection == "Five Eyes") return(c("USA", "GBR", "NZA", "AUS", "CAN"))
  else return(flag_choices[!flag_choices == ""])
}

# utility function
get_most_frequent_value <- function(df, column_name) {
  ## helper function to return most frequent value from a column
  return(df %>%
    count(!!as.name(column_name)) %>%
    slice(which.max(n)) %>%
    pull(!!as.name(column_name)))
}

# Header
header <- dashboardHeader(
  title = "Reefer Tracking Portal"
)

# Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",

    # Page Tabs
    menuItem("Home", icon = icon("home"), tabName = "home"),
    menuItem("Ranking", icon = icon("table"), tabName = "ranking"),
    menuItem("Reefer Search", icon = icon("bar-chart"), tabName = "plot"),

    br(),
    hr(),
    br(),
    # filter vessels
    sliderInput(
      "year_range",
      "Years",
      min = 2012,
      max = 2022,
      value = c(2012, 2022),
      sep = "",
      round = TRUE,
      ticks = FALSE
    ),
    sliderInput(
      "top_n",
      "Minimum number of meetings by vessel",
      min = 0,
      max = 500,
      value = 50,
      # sep = "",
      round = FALSE,
      animate = TRUE
    ),
    sliderInput(
      "distance",
      "Distance from shore (nm)",
      min = 0,
      max = 1400,
      value = c(25, 1400),
      # sep = "",
      round = FALSE,
      animate = TRUE
    )
  )
)

# Body
body <- dashboardBody(
  tabItems(

    # TAB 1: HOME
    tabItem(
      "home",
      h3("Introduction"),
      fluidRow(
        column(
          width = 6,
          align = "left",
          p(subtitle_text)
        ),
        column(
          width = 6,
          radioButtons(
            "intro_map_filter",
            "Level of suspicious behaviour (at least)",
            choices = c("Low", "Medium", "High"),
            selected = "Low",
            inline = TRUE
          ),
          leafletOutput("intro_map")
        )
      )
    ),

    # TAB 2: RANKING
    tabItem(
      "ranking",
      h3("Vulnerability to enforcement"),
      radioButtons(
        "enforcer",
        "in jurisdiction of",
        choices = c("U.S.", "Five Eyes", "NATO", "any country"),
        selected = "U.S.",
        inline = TRUE
      ),
      br(),
      fluidRow(
        valueBoxOutput("flag", width = 4),
        valueBoxOutput("eez", width = 4),
        # valueBoxOutput("rfmo", width = 3),
        valueBoxOutput("port", width = 4)
      ),
      fluidRow(
        column(
          width = 4,
          align = "center",
          actionButton("show_flag", label = "show list")
        ),
        column(
          width = 4,
          align = "center",
          actionButton("show_eez", label = "show list")
        ),
        # column(
        #   width = 4,
        #   align = "center",
        #   actionButton("show_rfmo", label = "show list")
        # ),
        column(
          width = 4,
          align = "center",
          actionButton("show_port", label = "show list")
        )
      ),
      hr(),
      textOutput("table_heading"),
      fluidRow(
        column(
          width = 12,
          align = "center",
          DT::dataTableOutput(outputId = "rankingstable")
        )
      )
    ),

    # TAB 3: REEFER SEARCH
    tabItem(
      "plot",
      fluidRow(
        column(
          width = 3,
          selectInput(
            "vessel_mmsi",
            "Support Vessel MMSI number",
            choices = ship_mmsi_recent,
            multiple = FALSE,
            selectize = TRUE,
            selected = 273354740)
        ),
        column(
          width = 6,
          align = "center",
          h1(textOutput("vessel_name"))),
        column(
          width = 3,
          align = "right",
          htmlOutput("vessel_flag"))
      ),
      fluidRow(
        column(
          width = 3,
          downloadButton("download_data", "Download meeting data"),
        ),
        column(
          width = 6,
          uiOutput("description")
        ),
        column(width = 3)
      ),
      hr(),
      h4("Activity"),
      p("This map shows the 90-day history of the selected vessel."),
      p("Zick-zags in the lines indicate illegal meetings with fishing vessels"),
      leafletOutput("history"),
      hr(),
      h4("Distance from shore during meetings"),
      plotlyOutput("distplot"),
      p("The area that lies within 200nm of the shore
          underlies the respective country's jurisdiction."),
      hr(),
      fluidRow(
        column(width = 9,
          h4("Port visits")),
        column(
          width = 3,
          selectInput(
            "city_or_country",
            "Plot by",
            choices = c("Country", "City"),
            multiple = FALSE,
            selectize = TRUE,
            selected = "Country")
        )
      ),
      plotlyOutput("portplotcountry")
    )
  ),
  tags$head(tags$style(HTML('
    /* logo */
    .content-wrapper {
    background-color: #ffffff;
    }')))
)

ui <- dashboardPage(header, sidebar, body, title = "Illegal Fishing")



server <- function(input, output, session) {

  ## TAB 1: HOME

  # Intro Cluster Map of locations
  output$intro_map <- renderLeaflet({
    leaflet() %>%
      addTiles()  %>%
      addMarkers(
        data = vessel_layer_data(),
        ~Longitude,
        ~Latitude,
        popup = ~MMSI,
        clusterOptions = markerClusterOptions(),
        label = ~Label,
        group = "vessels") %>%
      addPolygons(
        data=us_eez_shp,
        fillColor="red",
        stroke=FALSE) %>%
        setView(lng = -98.35, lat = 39.5, zoom = 2)
  })

  # Function to return current position of filtered vessels
  vessel_layer_data <- reactive({
    req(input$intro_map_filter)
    location_data <- sv %>%
      mutate(Destination_filled = if_else(
        is.na(Destination),
        "unknown",
        if_else(Destination == "",
        "unknown",
        Destination))) %>%
      mutate(Label = paste0(
        "Name: ", Name, ", \nFlag: ", Flag, ", \nDestination: ", Destination_filled
      ))
    if(input$intro_map_filter == "Low") {
      return(location_data %>% filter(MMSI %in% suspicion_low))
    } else if(input$intro_map_filter == "Medium") {
      return(location_data %>% filter(MMSI %in% suspicion_medium))
    } else return(location_data %>% filter(MMSI %in% suspicion_high))
   })

  # updating cluster map when level of suspiciousness changes
  observe({
    proxy <- leafletProxy("intro_map", session = session)
    proxy %>% clearGroup("vessels")
    df <- vessel_layer_data()
    proxy %>% addMarkers(
      data = df,
      ~Longitude,
      ~Latitude,
      popup = ~MMSI,
      clusterOptions = markerClusterOptions(),
      label = ~Label,
      group = "vessels")
  })


  ## TAB 2: RANKING

  generate_rankings2 <- reactive({
    all_meetings <- transshipments %>%

    # filter by date when the meeting occured
    mutate(start = as.Date(start)) %>%
    filter(between(
        start,
        as.Date(paste0(input$year_range[1], "-01-01")),
        as.Date(paste0(input$year_range[2], "-12-31"))
    )) %>%

    # filter by the distance from shore where the meeting occured
    filter(between(
        distance_from_shore_m,
        input$distance[1] * 1852,
        input$distance[2] * 1852
    ))

  # filter by the flags of the vessels
  if ("flags" %in% names(input) & !is.null(input$flags)) {
    all_meetings <- all_meetings %>%
      filter(vessel.flag %in% input$flags)
  }


  # generate table columns: name of vessel and flag of vessel
  reefer_info <- all_meetings %>%
    count(vessel.mmsi, vessel.name, vessel.flag, sort = TRUE) %>%
    group_by(vessel.mmsi) %>%
    summarise(Reefer.Name = vessel.name[1], Reefer.Flag = vessel.flag[1])

  # generate table columns: number of tracked and dark meetings,
  # and median distance from shore
  meeting_info <- all_meetings %>%
    mutate(encounter = ifelse(type == "encounter", 1, 0),
      loitering = ifelse(type == "loitering", 1, 0)) %>%
    group_by(vessel.mmsi) %>%
    summarise(
      n_encounter = sum(encounter),
      n_loitering = sum(loitering),
      avg_distance = median(distance_from_shore_m) / 1852,
      authorized = sum(encounter.authorization_status == "authorized",
        na.rm = TRUE)
    ) %>%
    mutate(
      total_meetings = n_encounter + n_loitering,
      tracked = n_encounter / total_meetings,
      authorized = authorized / total_meetings
    )


  table_data <- reefer_info %>%

    # joining data
    left_join(meeting_info, by = "vessel.mmsi") %>%
    left_join(sv, by = c("vessel.mmsi" = "MMSI")) %>%

    # sorting the rows
    arrange(-total_meetings) %>%
    mutate(Reefer.Name = str_to_title(Reefer.Name)) %>%

    # filter by minimum number of meetings
    filter(total_meetings >= input$top_n) %>%
    select(-Status) %>%
    select(-Flag) %>%

    # renaming the rows
    rename(
      "MMSI" = vessel.mmsi,
      "Vessel Name" = Reefer.Name,
      "Flag" = Reefer.Flag,
      "Number of tracked meetings" = n_encounter,
      "Number of dark meetings" = n_loitering,
      "Median distance from shore (nm)" = avg_distance,
      "Meetings" = total_meetings,
      "Status" = Navigation.Status,
      "EEZ" = ISO_TER1
    )
  table_data
  })


  # Reactively highlighting value boxes based on last button click
  rv <- reactiveValues(table_shown = "none")

  # Value Box 1: Flag
  ## data getter
  flag_data <- reactive({
    df <- generate_rankings2() %>%
      select(
        "Flag",
        "Vessel Name",
        "Longitude",
        "Latitude",
        "Meetings",
        "tracked",
        "authorized",
        "Status"
      ) %>%
      filter(Flag %in% flag_list(input$enforcer)) %>%
      mutate(
        tracked = paste0(round(tracked * 100), " %"),
        authorized = paste0(round(authorized * 100), " %"),
        Longitude = round(Longitude, 2),
        Latitude = round(Latitude, 2)
      )
  })

  ## rendering
  output$flag <- renderValueBox({
    num <- flag_data() %>%
      nrow()
    if (rv$table_shown == "flag") {
      valueBox(
        subtitle = paste0("flagged to ", input$enforcer),
        value = num,
        icon = icon("flag"),
        color = "yellow")
    } else {
      valueBox(
        subtitle = paste0("flagged to ", input$enforcer),
        value = num,
        icon = icon("flag"),
        color = "light-blue")
    }
  })

  ## observer
  observeEvent(
    eventExpr = input$show_flag,
    handlerExpr = {
      output$rankingstable <- DT::renderDataTable(
        formattable(
          flag_data(),
          list(
            `Meetings` = color_bar("#ff7f7f")
          )) %>%
          as.datatable(escape = FALSE,
                      options = list(scrollX = TRUE),
                      rownames = FALSE))
      rv$table_shown <- "flag"
    }
  )

  # Value Box 2: EEZ
  ## data getter
  eez_data <- reactive({
    df <- generate_rankings2() %>%
      select(
          "Flag",
          "Vessel Name",
          "Longitude",
          "Latitude",
          "Meetings",
          "tracked",
          "authorized",
          "Status",
          "EEZ"
      ) %>%
      filter(EEZ %in% flag_list(input$enforcer)) %>%
      mutate(
        tracked = paste0(round(tracked * 100), " %"),
        authorized = paste0(round(authorized * 100), " %"),
        Longitude = round(Longitude, 2),
        Latitude = round(Latitude, 2)
      )
  })

  ## rendering
  output$eez <- renderValueBox({
    num <- eez_data() %>%
      nrow()
    eez_subtitle <- function(enforcer) {
      if (enforcer == "any country") {
        return(paste0("in any EEZ"))
      } else {
          return(paste0("in ", input$enforcer, " EEZ"))
      }
    }
    if (rv$table_shown == "eez") {
      valueBox(
        subtitle = eez_subtitle(input$enforcer),
        value = num,
        icon = icon("flag"),
        color = "yellow")
    } else {
      valueBox(
        subtitle = eez_subtitle(input$enforcer),
        value = num,
        icon = icon("flag"),
        color = "light-blue")
    }
  })

  ## observer
  observeEvent(
    eventExpr = input$show_eez,
    handlerExpr = {
      output$rankingstable <- DT::renderDataTable(
        formattable(
          eez_data(),
          list(
            `Meetings` = color_bar("#ff7f7f")
          )) %>%
          as.datatable(escape = FALSE,
                      options = list(scrollX = TRUE),
                      rownames = FALSE))
      rv$table_shown <- "eez"
    }
  )

  # This is a feature that I would like to add later.

  # output$rfmo <- renderValueBox({
  #   rfmo_subtitle <- function(enforcer) {
  #     if (enforcer == "any country") {
  #       return(paste0("in any RFMO"))
  #     } else {
  #       if (enforcer == "U.S.") {
  #         return(paste0("in RFMOs enforceable by the U.S."))
  #       } else {
  #         return(paste0("in RFMOs enforceable by ", input$enforcer))
  #       }
  #     }
  #   }
  #   if (rv$table_shown == "rfmo") {
  #     valueBox(
  #       subtitle = rfmo_subtitle(input$enforcer),
  #       value = 1,
  #       icon = icon("flag"),
  #       color = "yellow")
  #   } else {
  #     valueBox(
  #       subtitle = rfmo_subtitle(input$enforcer),
  #       value = 1,
  #       icon = icon("flag"),
  #       color = "light-blue")
  #   }
  # })

  # observeEvent(
  #   eventExpr = input$show_rfmo,
  #   handlerExpr = {
  #     output$rankingstable <- DT::renderDataTable(
  #       DT::datatable(
  #         data = generate_rankings2(),
  #         options = list(pageLength = 10),
  #         rownames = FALSE)
  #     )
  #     rv$table_shown <- "rfmo"
  #   }
  # )

  # Value Box 4: Port
  ## data getter
  port_data <- reactive({
    generate_rankings2() %>%
      dplyr::select(
        "Flag",
        "Vessel Name",
        "Longitude",
        "Latitude",
        "Meetings",
        "tracked",
        "authorized",
        "Status",
        "EEZ"
      ) %>%
      filter(EEZ %in% flag_list(input$enforcer)) %>%
      filter(Status == "5-Moored") %>%
      rename("Port Country" = EEZ) %>%
      mutate(
        tracked = paste0(round(tracked * 100), " %"),
        authorized = paste0(round(authorized * 100), " %"),
        Longitude = round(Longitude, 2),
        Latitude = round(Latitude, 2)
      )
  })

  ## rendering
  output$port <- renderValueBox({
    num <- port_data() %>%
      nrow()
    port_subtitle <- function(enforcer) {
      if (enforcer == "any country") {
        return(paste0("at any port"))
      } else return(paste0("at ", input$enforcer, " ports"))
    }
    if (rv$table_shown == "port") {
      valueBox(
        subtitle = port_subtitle(input$enforcer),
        value = num,
        icon = icon("flag"),
        color = "yellow")
    } else {
      valueBox(
        subtitle = port_subtitle(input$enforcer),
        value = num,
        icon = icon("flag"),
        color = "light-blue")
    }
  })

  ## observer
  observeEvent(
    eventExpr = input$show_port,
    handlerExpr = {
      output$rankingstable <- DT::renderDataTable(
        formattable(
          port_data(),
          list(
            `Meetings` = color_bar("#ff7f7f")
          )) %>%
          as.datatable(escape = FALSE,
                      options = list(scrollX = TRUE),
                      rownames = FALSE))
      rv$table_shown <- "port"
    }
  )


  ## TAB 3: REEFER SEARCH

  # general function to get meeting data for one vessel
  mmsi_data <- reactive({
    df <- transshipments %>%
      select(
        type,
        vessel.mmsi,
        start,
        end,
        port.name,
        port.country,
        encounter.encountered_vessel.name,
        encounter.encountered_vessel.flag,
        vessel.name,
        vessel.flag,
        vessel.destination_port.name,
        vessel.destination_port.country,
        encounter.encountered_vessel.origin_port.country,
        distance_from_shore_m
      ) %>%
      filter(vessel.mmsi == input$vessel_mmsi) %>%
      mutate(
        start = as.POSIXct(start),
        end = as.POSIXct(end),
        distance_from_shore = distance_from_shore_m / 1852,
        Meeting_Type = ifelse(
          type == "encounter",
          "tracked",
          "dark"),
        port_country = countrycode(
          vessel.destination_port.country,
          "iso3c",
          "country.name"),
        port_country = coalesce(port_country, "unknown"),
        vessel.destination_port.name =
          coalesce(vessel.destination_port.name, "unknown")
      )
  })

  # general function to get meeting and port data for one vessel
  mmsi_with_port <- reactive({
    df <- port_visits %>%
      select(
        type,
        vessel.mmsi,
        start,
        end,
        port.name,
        port.country,
        encounter.encountered_vessel.name,
        encounter.encountered_vessel.flag,
        vessel.name,
        vessel.flag,
        vessel.destination_port.name,
        vessel.destination_port.country,
        encounter.encountered_vessel.origin_port.country,
        distance_from_shore_m
      ) %>%
      filter(vessel.mmsi == input$vessel_mmsi) %>%
      mutate(
        start = as.POSIXct(start),
        end = as.POSIXct(end),
        distance_from_shore = distance_from_shore_m / 1852,
        Meeting_Type = "",
        port_country = ""
      ) %>%
      rbind(mmsi_data()) %>%
      mutate(
        Event = ifelse(
          type == "port", "at port", ifelse(
            type == "loitering", "dark meeting", ifelse(
              type == "encounter", "tracked meeting", type
            )
          )
        ),
        # generating a description for each meeting
        event_description = paste0(
              "<b>", Event, "</b><br>",
              ifelse(
                type == "port",
                paste0(str_to_title(port.name), " in ", port.country),
                ifelse(
                  type == "encounter",
                  paste0(
                    "with ", str_to_title(encounter.encountered_vessel.name),
                    ", flagged to ", encounter.encountered_vessel.flag),
                  "with unknown fishing vessel"
                )
              ),
              "<br>on ", format(start, "%b %d %Y at %H:%M"), " for ",
              ifelse(
                difftime(end, start, units = "hours") > 48,
                paste0(round(difftime(end, start, units = "days")), " days"),
                paste0(round(difftime(end, start, units = "hours")), " hours")
              )
            )
      )
  })

  # generating descriptive values for each vessel
  description_values <- reactive({
    mmsi_meetings <- mmsi_data()
    desc <- list()
    desc$vessel_name <- get_most_frequent_value(
      mmsi_meetings, "vessel.name")
    desc$vessel_flag <- get_most_frequent_value(
      mmsi_meetings, "vessel.flag")
    desc$vessel_country <- countrycode(
      desc$vessel_flag, "iso3c", "country.name")
    desc$vessel_port <- countrycode(
      get_most_frequent_value(
        mmsi_meetings, "vessel.destination_port.country"),
      "iso3c", "country.name")
    desc$vessel_dist <- mmsi_meetings$distance_from_shore %>%
      median()
    desc$vessel_enc_flag <- get_most_frequent_value(
      mmsi_meetings, "encounter.encountered_vessel.flag")
    desc$vessel_enc_port <- get_most_frequent_value(
      mmsi_meetings, "encounter.encountered_vessel.origin_port.country")
    desc
  })

  # generating description
  description_text <- reactive({
    description <- description_values()
    HTML(
        paste0("<br>",
        str_to_title(description$vessel_name),
        " is a reefer vessel flagged to ", description$vessel_country,
        " and most frequently visits ports in ", description$vessel_port,
        ". <br><br>",
        "Its median distance from shore during ",
        "meetings with fishing vessels is ",
        round(description$vessel_dist, 0), " nautical miles.<br><br>"
      ))
    })

  # rendering header
  output$vessel_name <- renderText(
    description_values()$vessel_name
  )

  # rendering flag icon
  output$vessel_flag <- renderUI(
    shinyflags::flag(
      country = countrycode(description_values()$vessel_flag, "iso3c", "iso2c")
    ))

  # rendering description
  output$description <- renderUI({
    description_text()
  })

  # API Call to get 90-day history
  vessel_history <- reactive({
    age <- 90
    vessel_history_url <- paste0('https://api.seavision.volpe.dot.gov/v1/vessels/', input$vessel_mmsi, '/history')
    headers <- c('accept' = 'application/json', 'x-api-key' = api_key)
    params <- list('age' = age)
    response <- httr::GET(url = vessel_history_url, query = params, add_headers(.headers=headers))
    result <- jsonlite::fromJSON(httr::content(response, as = "text"))
    return(result)
  })

  # plot timeline map
  output$history <- renderLeaflet({
  leaflet() %>%
    addTiles() %>%
    addPolylines(data = vessel_history(), lng = ~longitude, lat = ~latitude)
  })

  # plot distribution plot for distance from shore
  output$distplot <- plotly::renderPlotly({
    df <- mmsi_data()
    ggplotly(
      p = ggplot(
        data = df,
        aes(x = distance_from_shore, fill = Meeting_Type)
        ) +
        geom_histogram(
          binwidth = 25,
          boundary = 0,
          position = "stack"
        ) +
        geom_vline(
          aes(xintercept = 200),
          color = "black",
          linetype = "dashed",
          size = 0.5
        ) +
        xlim(0, max(800, max(df$distance_from_shore))) +
        theme_wsj() +
        xlab("Distance from shore during meeting") +
        ylab("Frequency") +
        labs(caption = "The area that lies within 200nm of the shore
          underlies the respective country's jurisdiction.") +
        theme(
          legend.position = "right",
          text = element_text(family = "mono"),
          legend.background = element_rect(fill = background_color),
          plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
        coord_cartesian(xlim = c(0, max(800, max(df$distance_from_shore)))) +
        scale_fill_manual(
          name = "",
          values = c("dark" = "#D95F02", "tracked" = "#7570B3")) +
        scale_x_continuous(
          labels = function(x) {
            suf <- ifelse(x == 0, " nautical miles", " nm")
            return(format(paste0(x, suf)))
          }),
      tooltip = "count"
    )
  })

  # plot bar plot for ports
  port_plot <- reactive({
    df <- mmsi_data()
    if (input$city_or_country == "Country") {
      p <- ggplot(
        data = df,
        aes(
          x = port_country,
          fill = Meeting_Type,
          text = paste0(
            "visited port after ",
            sprintf("%0.0f", ..count..), " meetings")
        )) +
        xlab("Country where vessel headed after meetings")
    } else {
      p <- ggplot(
        data = df,
        aes(
          x = vessel.destination_port.name,
          fill = Meeting_Type,
          text = paste0(
            "visited port after ",
            sprintf("%0.0f", ..count..), " meetings"))
        ) +
        xlab("Port where vessel headed after meetings")
    }
      p = p + geom_histogram(
        boundary = 0,
        position = "stack",
        stat = "count"
      ) +
      ylab("Sum of meetings before heading to the port") +
      theme_wsj() +
      theme(
        axis.text.x = element_text(angle = 90),
        legend.position = "right",
        text = element_text(family = "mono"),
        legend.background = element_rect(fill = background_color),
        plot.background = element_rect(fill = background_color),
        panel.background = element_rect(fill = background_color),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()
      ) +
      scale_fill_manual(
          name = "",
          values = c("dark" = "#D95F02", "tracked" = "#7570B3"))

    ggplotly(
      p = p,
      tooltip = "text"
    )
  })
  output$portplotcountry <- renderPlotly({
    port_plot()
  })

  # download button
  output$download_data <- downloadHandler(
  filename = function() {
    paste("meeting_of_", input$vessel_mmsi, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(mmsi_data(), file)
  }
)
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)
