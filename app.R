library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# ── Data Loading ─────────────────────────────────────────────────
data <- read.csv("data/global_cars_enhanced.csv")

brand_choices <- sort(unique(data$Brand))
body_type_choices <- sort(unique(data$Body_Type))
fuel_type_choices <- sort(unique(data$Fuel_Type))
price_min <- floor(min(data$Price_USD))
price_max <- ceiling(max(data$Price_USD))

# ── Color Palettes ───────────────────────────────────────────────
fuel_colors <- c(
  "Hybrid"   = "#1b6c6e",
  "Petrol"   = "#c0392b",
  "Diesel"   = "#e67e22",
  "Electric" = "#2980b9"
)

group_colors <- c(
  "Hybrid"        = "#1b6c6e",
  "Standard Fuel" = "#c0392b"
)

# ── UI ───────────────────────────────────────────────────────────
ui <- page_sidebar(
  title = "Car Price Analysis Dashboard",
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#16324f"
  ),

  # ── Sidebar with filters ──
  sidebar = sidebar(
    title = "Filters",
    width = 300,

    selectizeInput(
      "input_brand",
      "Brand",
      choices = brand_choices,
      selected = NULL,
      multiple = TRUE,
      options = list(placeholder = "All brands")
    ),

    selectizeInput(
      "input_body_type",
      "Body Type",
      choices = body_type_choices,
      selected = c("Sedan", "SUV", "Hatchback"),
      multiple = TRUE,
      options = list(placeholder = "All body types")
    ),

    sliderInput(
      "input_price_range",
      "Price Range (USD)",
      min = price_min,
      max = price_max,
      value = c(price_min, min(60000, price_max)),
      pre = "$",
      sep = ","
    ),

    selectizeInput(
      "input_fuel_type",
      "Fuel Type",
      choices = fuel_type_choices,
      selected = c("Hybrid", "Petrol", "Diesel"),
      multiple = TRUE,
      options = list(placeholder = "All fuel types")
    ),

    actionButton("reset_btn", "Reset Filters", class = "btn-primary w-100")
  ),

  # ── Main content ──
  layout_columns(
    col_widths = c(4, 4, 4),
    value_box(
      title = "Vehicles in Selection",
      value = textOutput("kpi_count"),
      showcase = icon("car"),
      theme = "primary"
    ),
    value_box(
      title = "Average Price (USD)",
      value = textOutput("kpi_avg_price"),
      showcase = icon("dollar-sign"),
      theme = "success"
    ),
    value_box(
      title = "Avg Efficiency Score",
      value = textOutput("kpi_avg_efficiency"),
      showcase = icon("leaf"),
      theme = "info"
    )
  ),

  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header("Engine Size vs. Performance Efficiency"),
      plotOutput("scatter_engine_efficiency")
    ),
    card(
      card_header("Average Efficiency: Hybrid vs Standard Fuel"),
      plotOutput("bar_fuel_efficiency")
    )
  ),

  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header("Average Price by Fuel Type"),
      plotOutput("bar_fuel_price")
    ),
    card(
      card_header("Horsepower vs Price"),
      plotOutput("scatter_hp_price")
    )
  )
)


# ── Server ───────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Reset filters ──
  observeEvent(input$reset_btn, {
    updateSelectizeInput(session, "input_brand", selected = character(0))
    updateSelectizeInput(session, "input_body_type",
                         selected = c("Sedan", "SUV", "Hatchback"))
    updateSliderInput(session, "input_price_range",
                      value = c(price_min, min(60000, price_max)))
    updateSelectizeInput(session, "input_fuel_type",
                         selected = c("Hybrid", "Petrol", "Diesel"))
  })

  # ── Reactive calc: filtered dataframe ──
  filtered_df <- reactive({
    df <- data

    if (length(input$input_brand) > 0) {
      df <- df %>% filter(Brand %in% input$input_brand)
    }

    if (length(input$input_body_type) > 0) {
      df <- df %>% filter(Body_Type %in% input$input_body_type)
    }

    df <- df %>% filter(
      Price_USD >= input$input_price_range[1],
      Price_USD <= input$input_price_range[2]
    )

    if (length(input$input_fuel_type) > 0) {
      df <- df %>% filter(Fuel_Type %in% input$input_fuel_type)
    }

    df
  })

  # ── KPIs ──
  output$kpi_count <- renderText({
    format(nrow(filtered_df()), big.mark = ",")
  })

  output$kpi_avg_price <- renderText({
    df <- filtered_df()
    if (nrow(df) == 0) return("—")
    paste0("$", format(round(mean(df$Price_USD)), big.mark = ","))
  })

  output$kpi_avg_efficiency <- renderText({
    df <- filtered_df()
    if (nrow(df) == 0) return("—")
    round(mean(df$Efficiency_Score, na.rm = TRUE), 2)
  })

  # ── Scatter: Engine Size vs Efficiency ──
  output$scatter_engine_efficiency <- renderPlot({
    df <- filtered_df()

    if (nrow(df) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No data for current filters.",
                   size = 5, hjust = 0.5) +
          theme_void()
      )
    }

    ggplot(df, aes(x = Engine_CC, y = Efficiency_Score, color = Fuel_Type)) +
      geom_point(alpha = 0.7, size = 3) +
      scale_color_manual(values = fuel_colors, name = "Fuel Type") +
      labs(x = "Engine Size (CC)", y = "Performance Efficiency") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "right")
  })

  # ── Bar: Hybrid vs Standard Fuel Efficiency ──
  output$bar_fuel_efficiency <- renderPlot({
    df <- filtered_df()

    if (nrow(df) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No data for current filters.",
                   size = 5, hjust = 0.5) +
          theme_void()
      )
    }

    df <- df %>%
      mutate(Fuel_Group = ifelse(Fuel_Type == "Hybrid", "Hybrid", "Standard Fuel"))

    agg <- df %>%
      group_by(Fuel_Group) %>%
      summarise(avg_eff = mean(Efficiency_Score, na.rm = TRUE), .groups = "drop") %>%
      mutate(Fuel_Group = factor(Fuel_Group, levels = c("Hybrid", "Standard Fuel")))

    ggplot(agg, aes(x = Fuel_Group, y = avg_eff, fill = Fuel_Group)) +
      geom_col(width = 0.5, color = "white") +
      geom_text(aes(label = sprintf("%.2f", avg_eff)),
                vjust = -0.5, fontface = "bold", size = 5) +
      scale_fill_manual(values = group_colors) +
      labs(x = NULL, y = "Avg Performance Efficiency") +
      ylim(0, 1) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })

  # ── Bar: Average Price by Fuel Type ──
  output$bar_fuel_price <- renderPlot({
    df <- filtered_df()

    if (nrow(df) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No data for current filters.",
                   size = 5, hjust = 0.5) +
          theme_void()
      )
    }

    agg <- df %>%
      group_by(Fuel_Type) %>%
      summarise(avg_price = mean(Price_USD, na.rm = TRUE), .groups = "drop")

    ggplot(agg, aes(x = reorder(Fuel_Type, -avg_price), y = avg_price)) +
      geom_col(fill = "#2a9d8f", color = "white") +
      geom_text(aes(label = paste0("$", format(round(avg_price), big.mark = ","))),
                vjust = -0.5, size = 4) +
      labs(x = "Fuel Type", y = "Average Price (USD)") +
      scale_y_continuous(labels = scales::dollar_format()) +
      theme_minimal(base_size = 14)
  })

  # ── Scatter: Horsepower vs Price ──
  output$scatter_hp_price <- renderPlot({
    df <- filtered_df() %>% filter(!is.na(Horsepower), !is.na(Price_USD))

    if (nrow(df) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No data for current filters.",
                   size = 5, hjust = 0.5) +
          theme_void()
      )
    }

    ggplot(df, aes(x = Horsepower, y = Price_USD, color = Fuel_Type)) +
      geom_point(alpha = 0.7, size = 3) +
      scale_color_manual(values = fuel_colors, name = "Fuel Type") +
      scale_y_continuous(labels = scales::dollar_format()) +
      labs(x = "Horsepower", y = "Price (USD)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "right")
  })
}

shinyApp(ui = ui, server = server)
