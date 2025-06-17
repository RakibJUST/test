library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(readr)
library(countrycode)

# --- DATA PREP ---
ai_jobs <- read_csv("ai_job_dataset.csv")
ai_jobs$iso3 <- countrycode(ai_jobs$company_location, "country.name", "iso3c")
exp_map <- c("EN" = "Entry", "MI" = "Mid", "SE" = "Senior", "EX" = "Executive")
ai_jobs$experience_level <- exp_map[ai_jobs$experience_level]

country_summary <- ai_jobs %>%
  group_by(company_location, iso3) %>%
  summarise(
    job_count = n(),
    salary_usd = round(mean(salary_usd, na.rm=TRUE)),
    remote_ratio = round(mean(remote_ratio, na.rm=TRUE)),
    .groups = "drop"
  ) %>%
  filter(!is.na(iso3), job_count > 10)

color_choices <- c(
  "Average Salary (USD)" = "salary_usd",
  "Number of Jobs" = "job_count",
  "Remote Job Ratio (%)" = "remote_ratio"
)

# --- UI ---
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = span("Global AI Job Market 2024",
                 style = "font-family: 'Inter','Arial',sans-serif; font-size: 1.5em; color: #fff; width: 100%; display: inline-block; text-align: center;"
    ),
    titleWidth = "100%"
  ),
  dashboardSidebar(
    width = 150,   # narrower for more plot area
    sidebarMenu(
      id = "sidebarMenu",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Global Map", tabName = "map", icon = icon("globe")),
      menuItem("Country Analysis", tabName = "analysis", icon = icon("flag")),
      menuItem("References", tabName = "references", icon = icon("book"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .skin-blue .main-header .logo {
          width: 100% !important;
          text-align: center;
          float: none;
          background-color: #3c8dbc !important;
        }
        .skin-blue .main-header .navbar {
          margin-left: 0 !important;
        }
        .skin-blue .main-header .logo span {
          width: 100% !important;
          display: block;
          text-align: center;
          color: #fff !important;
        }
        .modern-title {
          font-family: 'Inter','Arial',sans-serif;
          font-size: 2em;
          color: #23242d;
          font-weight: 700;
          letter-spacing: 0.01em;
          text-align: center;
          margin-bottom: 20px;
          margin-top: 14px;
        }
        .welcome-card {
          margin-left: auto; margin-right: auto;
          margin-top: 40px;
          max-width: 700px;
          background: #fff;
          border-radius: 15px;
          box-shadow: 0 8px 32px rgba(50,50,100,0.07);
          padding: 32px 36px 30px 36px;
        }
        body, .content-wrapper, .right-side { background: #F4F6FA !important; }
        .box { background: #fff !important; border-radius: 14px; box-shadow: 0 4px 18px rgba(0,0,0,0.04); }
      "))
    ),
    tabItems(
      # Home Page
      tabItem(tabName = "home",
              tags$div(class = "modern-title", "Welcome to the Global AI Job Market Dashboard"),
              tags$div(class = "welcome-card",
                       tags$p("This dashboard provides an interactive overview of the global Artificial Intelligence (AI) job market, salaries, and experience-level breakdowns using open data from 2024–2025."),
                       tags$ul(
                         tags$li(tags$b("Global Map:"), " Explore the distribution of AI jobs, salaries, and remote work by country. Click a country to see more details."),
                         tags$li(tags$b("Country Analysis:"), " View the breakdown of AI job counts and average salaries by experience level for a selected country."),
                         tags$li(tags$b("References:"), " See the data source and citation."),
                         style = "font-size: 1.1em; margin-bottom:14px;"
                       ),
                       tags$p("Use the sidebar to navigate between views. All data and visualisations are for educational use only."),
                       tags$br(),
                       tags$p(tags$em("Created by: MD RAKIBUL ISLAM, Student ID: S4078778"))
              )
      ),
      # Map Page
      tabItem(tabName = "map",
              tags$div(class = "modern-title", "Global AI Job Market Map Overview"),
              fluidRow(
                box(width = 12, status = "primary", solidHeader = FALSE,
                    tags$div(
                      style = "margin-bottom:15px; margin-top:10px;",
                      tags$label("Color countries by:", style = "font-weight:600; font-size:1.06em;"),
                      selectInput("colorby", NULL,
                                  choices = color_choices, selected = "salary_usd", width = "320px"
                      )
                    ),
                    tags$div(style = "font-size: 0.97em; color: #555; margin-bottom:10px;",
                             "Click a country on the map to see its experience-level breakdown on the next tab."
                    ),
                    plotlyOutput("worldMap", height = "820px")   # Large for 1080p
                )
              )
      ),
      # Country Analysis Page
      tabItem(tabName = "analysis",
              tags$div(class = "modern-title", "Country Analysis"),
              fluidRow(
                box(width = 5, status = "primary", solidHeader = FALSE,
                    selectInput("country_pick", "Choose a Country:",
                                choices = sort(unique(country_summary$company_location)),
                                selected = sort(unique(country_summary$company_location))[1],
                                width = "100%"
                    ),
                    tags$div(style = "margin-top:8px; color:#666; font-size:0.98em;",
                             "Experience-level analysis for selected country."
                    )
                ),
                box(width = 7, status = "primary", solidHeader = FALSE,
                    plotlyOutput("countryBar", height = "700px")   # Tall for HD
                )
              )
      ),
      # References Page
      tabItem(tabName = "references",
              tags$div(class = "modern-title", "References"),
              box(width = 12, status = "primary", solidHeader = TRUE,
                  tags$div(
                    style = "font-size:1.1em; padding:15px;",
                    tags$p("Dataset Citation:"),
                    tags$p(
                      HTML(
                        "Sajjad, B. (2025). <i>Global AI Job Market & Salary Trends 2025.</i> Kaggle.com. ",
                        '<a href="https://www.kaggle.com/datasets/bismasajjad/global-ai-job-market-and-salary-trends-2025" target="_blank">',
                        "https://www.kaggle.com/datasets/bismasajjad/global-ai-job-market-and-salary-trends-2025</a>"
                      )
                    ),
                    tags$hr(),
                    tags$p("This dashboard was created for educational purposes as part of a data visualization assignment. All visualizations use only open data.")
                  )
              )
      )
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  output$worldMap <- renderPlotly({
    plot_ly(
      data = country_summary,
      type = "choropleth",
      locations = ~iso3,
      z = ~get(input$colorby),
      text = ~paste0(
        "<b>", company_location, "</b><br>",
        "Jobs: <b>", job_count, "</b><br>",
        "Avg Salary: <b>$", format(salary_usd, big.mark = ","), "</b><br>",
        "Remote Ratio: <b>", remote_ratio, "%</b>"
      ),
      hoverinfo = "text",
      colorscale = "Viridis",
      marker = list(line = list(width = 0.7, color = "#F4F6FA")),
      colorbar = list(title = names(color_choices)[color_choices == input$colorby]),
      customdata = ~company_location,
      source = "map_click"
    ) %>%
      layout(
        geo = list(
          showland = TRUE,
          landcolor = "#F4F6FA",
          showcountries = TRUE,
          countrycolor = "#aaa",
          showframe = FALSE,
          projection = list(type = 'natural earth'),
          lakecolor = "#D3EAFB"
        ),
        margin = list(t = 10, b = 10)
      )
  })
  
  # For selecting country when clicking the map
  observeEvent(event_data("plotly_click", source = "map_click"), {
    click <- event_data("plotly_click", source = "map_click")
    if (!is.null(click$customdata)) {
      updateSelectInput(session, "country_pick", selected = click$customdata)
      updateTabItems(session, "sidebarMenu", "analysis")
    }
  })
  
  output$countryBar <- renderPlotly({
    req(input$country_pick)
    exp_levels <- c("Entry", "Mid", "Senior", "Executive")
    df <- ai_jobs %>%
      filter(company_location == input$country_pick) %>%
      group_by(experience_level) %>%
      summarise(
        job_count = n(),
        avg_salary = round(mean(salary_usd, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      mutate(
        experience_level = factor(experience_level, levels = exp_levels)
      )
    validate(need(nrow(df) > 0, "No data for this country."))
    plot_ly(
      df,
      x = ~experience_level,
      y = ~job_count,
      type = "bar",
      marker = list(
        color = ~avg_salary,
        colorscale = "OrRd",
        showscale = TRUE,
        colorbar = list(title = "Avg Salary")
      ),
      hovertemplate = paste(
        "<b>%{x}</b><br>",
        "Jobs: %{y}<br>",
        "Avg Salary: $%{customdata:,.0f}",
        "<extra></extra>"
      ),
      customdata = ~avg_salary
    ) %>%
      layout(
        xaxis = list(title = "Experience Level", tickfont = list(size = 15)),
        yaxis = list(title = "Number of Jobs", tickfont = list(size = 13)),
        margin = list(t = 30, b = 20, l = 20, r = 20),
        plot_bgcolor = "#fff",
        paper_bgcolor = "#fff"
      )
  })
  
}

shinyApp(ui, server)
