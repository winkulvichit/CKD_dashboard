library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(ggalluvial)
library(scales)
library(forcats)
library(readr)
library(stringr)
library(sf)
library(leaflet)
library(htmltools)

DATA_PATH  <- "ALBP_long.csv"
SHAPE_PATH <- "cualbumin_samutsakhon.shp"

# -----------------------------
# Helper functions
# -----------------------------

to_yes_no <- function(x, yes_values = c("1"), no_values = c("0"), na_as_no = TRUE) {
  x_chr <- as.character(x)
  out <- case_when(
    x_chr %in% yes_values ~ "Yes",
    x_chr %in% no_values ~ "No",
    is.na(x_chr) & na_as_no ~ "No",
    TRUE ~ NA_character_
  )
  factor(out, levels = c("No", "Yes"))
}

to_ever_yes_no <- function(x) {
  x_chr <- as.character(x)
  out <- case_when(
    x_chr %in% c("1", "2") ~ "Yes",
    x_chr %in% c("0") ~ "No",
    is.na(x_chr) ~ "No",
    TRUE ~ NA_character_
  )
  factor(out, levels = c("No", "Yes"))
}

filter_binary <- function(data, col_name, input_value) {
  if (identical(input_value, "All")) return(data)
  data %>% filter(.data[[col_name]] == input_value)
}

infer_followup_year <- function(sample_id) {
  case_when(
    str_detect(sample_id, "^F4-") ~ 4L,
    str_detect(sample_id, "^F3-") ~ 3L,
    str_detect(sample_id, "^F2-") ~ 2L,
    TRUE ~ 1L
  )
}

ckd_g_stage <- function(egfr) {
  case_when(
    is.na(egfr) ~ NA_character_,
    egfr >= 90 ~ "G1",
    egfr >= 60 ~ "G2",
    egfr >= 45 ~ "G3a",
    egfr >= 30 ~ "G3b",
    egfr >= 15 ~ "G4",
    egfr < 15 ~ "G5"
  )
}

ckd_a_stage <- function(uacr) {
  case_when(
    is.na(uacr) ~ NA_character_,
    uacr < 30 ~ "A1",
    uacr <= 300 ~ "A2",
    uacr > 300 ~ "A3"
  )
}

ckd_risk_4cat <- function(g_stage, a_stage) {
  case_when(
    is.na(g_stage) | is.na(a_stage) ~ NA_character_,
    g_stage %in% c("G1", "G2") & a_stage == "A1" ~ "Green",
    (g_stage %in% c("G1", "G2") & a_stage == "A2") |
      (g_stage == "G3a" & a_stage == "A1") ~ "Yellow",
    (g_stage %in% c("G1", "G2") & a_stage == "A3") |
      (g_stage == "G3a" & a_stage == "A2") |
      (g_stage == "G3b" & a_stage == "A1") ~ "Orange",
    TRUE ~ "Red"
  )
}

add_overall <- function(tbl, fill_levels = NULL) {
  overall <- tbl %>%
    group_by(category) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    mutate(
      year = "Overall",
      total_n = sum(n),
      pct = if_else(total_n > 0, n / total_n, 0)
    )

  out <- bind_rows(tbl, overall) %>%
    mutate(year = factor(year, levels = c("2022", "2023", "2024", "2025", "Overall")))

  if (!is.null(fill_levels)) {
    out <- out %>% mutate(category = factor(category, levels = fill_levels))
  }

  out
}

build_heatmap_table <- function(data, category_col, levels_vec) {
  data %>%
    filter(!is.na(.data[[category_col]])) %>%
    count(year, category = .data[[category_col]], name = "n") %>%
    complete(year = c("2022", "2023", "2024", "2025"), category = levels_vec, fill = list(n = 0)) %>%
    group_by(year) %>%
    mutate(
      total_n = sum(n),
      pct = if_else(total_n > 0, n / total_n, 0)
    ) %>%
    ungroup() %>%
    add_overall(fill_levels = levels_vec)
}

heatmap_x_labels <- function(tbl) {
  totals <- tbl %>%
    distinct(year, total_n) %>%
    arrange(factor(year, levels = c("2022", "2023", "2024", "2025", "Overall")))

  labels <- paste0(as.character(totals$year), "\nN=", format(totals$total_n, big.mark = ","))
  names(labels) <- as.character(totals$year)
  labels
}

heatmap_plot <- function(df_plot, title_text, x_labels = NULL) {
  if (is.null(x_labels)) {
    x_labels <- levels(df_plot$year)
    names(x_labels) <- levels(df_plot$year)
  }

  ggplot(
    df_plot,
    aes(
      x = year,
      y = fct_rev(category),
      fill = pct,
      text = paste0(
        "Year: ", year,
        "<br>Category: ", category,
        "<br>n: ", n,
        "<br>Total: ", total_n,
        "<br>Percent: ", percent(pct, accuracy = 0.1)
      )
    )
  ) +
    geom_tile(color = "grey85") +
    geom_text(
      aes(label = paste0(percent(pct, accuracy = 0.1), "\n(n=", n, ")")),
      size = 3.6,
      lineheight = 0.95
    ) +
    scale_x_discrete(labels = x_labels, drop = FALSE) +
    scale_fill_gradient(low = "white", high = "steelblue", labels = percent_format(accuracy = 1)) +
    labs(x = NULL, y = NULL, fill = "Percent", title = title_text) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.title = element_text(face = "bold")
    )
}

ordinal_suffix <- function(x) {
  ifelse(x == 1, "st", ifelse(x == 2, "nd", ifelse(x == 3, "rd", "th")))
}

safe_pct <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE) * 100
}

banphaeo_tambons <- c(
  "บ้านแพ้ว", "หลักสาม", "ยกกระบัตร", "โรงเข้", "อำแพง", "คลองตัน",
  "เกษตรพัฒนา", "หนองสองห้อง", "หนองบัว", "เจ็ดริ้ว", "สวนส้ม", "หลักสอง"
)

clean_subdistrict <- function(x) {
  out <- x %>%
    as.character() %>%
    str_trim() %>%
    na_if("") %>%
    str_remove("^ตำบล") %>%
    str_remove("^ต\\.") %>%
    str_remove("^แขวง") %>%
    str_replace_all("\\s+", "") %>%
    str_replace("บ้านคลองตัน", "คลองตัน")

  out <- case_when(
    out %in% c("ไม่ระบุ", "คัดออก", "nan", "NA", "N/A", "") ~ NA_character_,
    TRUE ~ out
  )

  out
}

read_banphaeo_sf <- function(path) {
  sf_obj <- st_read(path, quiet = TRUE, stringsAsFactors = FALSE) %>%
    st_make_valid() %>%
    st_transform(4326)

  if (!all(c("A_NAME_T", "T_NAME_T") %in% names(sf_obj))) {
    stop("Shapefile must contain A_NAME_T and T_NAME_T columns.")
  }

  sf_obj %>%
    filter(A_NAME_T == "บ้านแพ้ว") %>%
    mutate(
      subdistrict_th = T_NAME_T,
      subdistrict_clean = clean_subdistrict(T_NAME_T)
    ) %>%
    select(subdistrict_th, subdistrict_clean, geometry)
}

# -----------------------------
# Read and prepare data
# -----------------------------
raw <- read_csv(DATA_PATH, show_col_types = FALSE) %>%
  mutate(
    sample_id = as.character(sample_id),
    study_id = as.character(study_id),
    age = suppressWarnings(as.numeric(age)),
    egfr = suppressWarnings(as.numeric(egfr)),
    uacr = suppressWarnings(as.numeric(uacr)),
    followup_year = infer_followup_year(sample_id),
    year = as.character(2021 + followup_year),
    dm_f = to_yes_no(dm, yes_values = c("1"), no_values = c("0"), na_as_no = TRUE),
    htn_f = to_yes_no(htn, yes_values = c("1"), no_values = c("0"), na_as_no = TRUE),
    dlp_f = to_yes_no(dlp, yes_values = c("1"), no_values = c("0"), na_as_no = TRUE),
    cvd_f = to_yes_no(cvd, yes_values = c("1"), no_values = c("0"), na_as_no = TRUE),
    smoking_f = to_ever_yes_no(smoking),
    alc_f = to_ever_yes_no(alc),
    herbalmed_f = to_yes_no(herbalmed, yes_values = c("1", "9.96"), no_values = c("0"), na_as_no = TRUE),
    analgesic_f = to_yes_no(analgesic, yes_values = c("1"), no_values = c("0"), na_as_no = TRUE),
    suppl_f = to_yes_no(suppl, yes_values = c("1"), no_values = c("0"), na_as_no = TRUE),
    subdistrict_raw = subdistrict,
    subdistrict_clean = clean_subdistrict(subdistrict),
    ckd_by_egfr = case_when(
      is.na(egfr) ~ NA,
      egfr < 60 ~ TRUE,
      TRUE ~ FALSE
    ),
    ckd_by_uacr = case_when(
      is.na(uacr) ~ NA,
      uacr >= 30 ~ TRUE,
      TRUE ~ FALSE
    ),
    diagnosis_type = case_when(
      ckd_by_egfr %in% TRUE & ckd_by_uacr %in% FALSE ~ "eGFR alone",
      ckd_by_egfr %in% FALSE & ckd_by_uacr %in% TRUE ~ "UACR alone",
      ckd_by_egfr %in% TRUE & ckd_by_uacr %in% TRUE ~ "Both eGFR + UACR",
      ckd_by_egfr %in% FALSE & ckd_by_uacr %in% FALSE ~ "No CKD",
      TRUE ~ NA_character_
    ),
    g_stage = ckd_g_stage(egfr),
    a_stage = ckd_a_stage(uacr),
    risk_4cat = ckd_risk_4cat(g_stage, a_stage)
  )

banphaeo_sf <- tryCatch(
  read_banphaeo_sf(SHAPE_PATH),
  error = function(e) {
    message("Unable to read shapefile: ", e$message)
    NULL
  }
)

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  titlePanel("Ban Phaeo CKD dashboard"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      numericInput("age_lower", "Age lower limit", value = 0, min = 0, step = 1),
      selectInput("dm", "DM", choices = c("All", "Yes", "No"), selected = "All"),
      selectInput("htn", "HTN", choices = c("All", "Yes", "No"), selected = "All"),
      selectInput("dlp", "DLP", choices = c("All", "Yes", "No"), selected = "All"),
      selectInput("cvd", "CVD", choices = c("All", "Yes", "No"), selected = "All"),
      selectInput("smoking", "Smoking", choices = c("All", "Yes", "No"), selected = "All"),
      selectInput("alc", "Alcohol", choices = c("All", "Yes", "No"), selected = "All"),
      selectInput("herbalmed", "Herbal medicine", choices = c("All", "Yes", "No"), selected = "All"),
      selectInput("analgesic", "Analgesic", choices = c("All", "Yes", "No"), selected = "All"),
      selectInput("suppl", "Supplement", choices = c("All", "Yes", "No"), selected = "All"),
      hr(),
      selectInput(
        "map_metric",
        "Geographic map metric",
        choices = c(
          "Patient count" = "n_patients",
          "CKD prevalence (%)" = "ckd_pct",
          "No CKD (%)" = "no_ckd_pct",
          "eGFR alone (%)" = "egfr_alone_pct",
          "UACR alone (%)" = "uacr_alone_pct",
          "Both eGFR + UACR (%)" = "both_pct",
          "Green risk (%)" = "green_pct",
          "Yellow risk (%)" = "yellow_pct",
          "Orange risk (%)" = "orange_pct",
          "Red risk (%)" = "red_pct"
        ),
        selected = "n_patients"
      ),
      helpText("All plots update automatically with the selected filter."),
      helpText("Heatmaps show year-specific N and overall N in the x-axis labels."),
      helpText("Alluvial plots use patients with complete 4-year follow-up after filtering.")
    ),
    mainPanel(
      width = 9,
      fluidRow(
        column(4, wellPanel(h4("Filtered rows"), textOutput("n_rows"))),
        column(4, wellPanel(h4("Unique patients"), textOutput("n_patients"))),
        column(4, wellPanel(h4("Complete 4-year follow-up"), textOutput("n_complete")))
      ),
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Diagnosis heatmap", value = "diag", plotlyOutput("diag_heatmap", height = "520px")),
        tabPanel("G-stage heatmap", value = "g", plotlyOutput("g_heatmap", height = "560px")),
        tabPanel("A-stage heatmap", value = "a", plotlyOutput("a_heatmap", height = "460px")),
        tabPanel("Risk heatmap", value = "risk", plotlyOutput("risk_heatmap", height = "440px")),
        tabPanel("Alluvial: G-stage", value = "g_alluvial", plotOutput("g_alluvial", height = "650px")),
        tabPanel("Alluvial: risk", value = "risk_alluvial", plotOutput("risk_alluvial", height = "650px")),
        tabPanel(
          "Geographic map",
          value = "geo",
          leafletOutput("subdistrict_map", height = "720px"),
          br(),
          htmlOutput("map_note"),
          br(),
          tableOutput("subdistrict_match_preview")
        )
      )
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {

  filtered_data <- reactive({
    dat <- raw %>% filter(is.na(age) | age >= input$age_lower)
    dat <- filter_binary(dat, "dm_f", input$dm)
    dat <- filter_binary(dat, "htn_f", input$htn)
    dat <- filter_binary(dat, "dlp_f", input$dlp)
    dat <- filter_binary(dat, "cvd_f", input$cvd)
    dat <- filter_binary(dat, "smoking_f", input$smoking)
    dat <- filter_binary(dat, "alc_f", input$alc)
    dat <- filter_binary(dat, "herbalmed_f", input$herbalmed)
    dat <- filter_binary(dat, "analgesic_f", input$analgesic)
    dat <- filter_binary(dat, "suppl_f", input$suppl)
    dat
  })

  complete_4y <- reactive({
    filtered_data() %>%
      group_by(study_id) %>%
      filter(n_distinct(followup_year) == 4) %>%
      ungroup()
  })

  output$n_rows <- renderText({ format(nrow(filtered_data()), big.mark = ",") })
  output$n_patients <- renderText({ format(n_distinct(filtered_data()$study_id), big.mark = ",") })
  output$n_complete <- renderText({ format(n_distinct(complete_4y()$study_id), big.mark = ",") })

  diagnosis_tbl <- reactive({
    build_heatmap_table(
      filtered_data(),
      category_col = "diagnosis_type",
      levels_vec = c("No CKD", "eGFR alone", "UACR alone", "Both eGFR + UACR")
    )
  })

  output$diag_heatmap <- renderPlotly({
    tbl <- diagnosis_tbl()
    p <- heatmap_plot(tbl, "CKD diagnosis pattern by calendar year", x_labels = heatmap_x_labels(tbl))
    ggplotly(p, tooltip = "text")
  })

  g_tbl <- reactive({
    build_heatmap_table(
      filtered_data(),
      category_col = "g_stage",
      levels_vec = c("G1", "G2", "G3a", "G3b", "G4", "G5")
    )
  })

  output$g_heatmap <- renderPlotly({
    tbl <- g_tbl()
    p <- heatmap_plot(tbl, "CKD G-stage distribution by calendar year", x_labels = heatmap_x_labels(tbl))
    ggplotly(p, tooltip = "text")
  })

  a_tbl <- reactive({
    build_heatmap_table(
      filtered_data(),
      category_col = "a_stage",
      levels_vec = c("A1", "A2", "A3")
    )
  })

  output$a_heatmap <- renderPlotly({
    tbl <- a_tbl()
    p <- heatmap_plot(tbl, "CKD A-stage distribution by calendar year", x_labels = heatmap_x_labels(tbl))
    ggplotly(p, tooltip = "text")
  })

  risk_tbl <- reactive({
    build_heatmap_table(
      filtered_data(),
      category_col = "risk_4cat",
      levels_vec = c("Green", "Yellow", "Orange", "Red")
    )
  })

  output$risk_heatmap <- renderPlotly({
    tbl <- risk_tbl()
    p <- heatmap_plot(tbl, "KDIGO CKD risk distribution by calendar year", x_labels = heatmap_x_labels(tbl))
    ggplotly(p, tooltip = "text")
  })

  output$g_alluvial <- renderPlot({
    dat <- complete_4y() %>%
      select(study_id, followup_year, g_stage) %>%
      filter(!is.na(g_stage)) %>%
      mutate(g_stage = factor(g_stage, levels = c("G1", "G2", "G3a", "G3b", "G4", "G5"), ordered = TRUE))

    validate(
      need(nrow(dat) > 0, "No complete 4-year follow-up data available after filtering."),
      need(all(1:4 %in% dat$followup_year), "Not all follow-up years are available after filtering.")
    )

    wide <- dat %>%
      mutate(followup_label = paste0(followup_year, ordinal_suffix(followup_year), " year")) %>%
      select(study_id, followup_label, g_stage) %>%
      distinct() %>%
      pivot_wider(names_from = followup_label, values_from = g_stage) %>%
      drop_na()

    validate(need(nrow(wide) > 0, "No patients with complete G-stage data across all 4 years after filtering."))

    flow <- wide %>%
      count(`1st year`, `2nd year`, `3rd year`, `4th year`, name = "n") %>%
      mutate(pct = n / sum(n))

    ggplot(flow, aes(axis1 = `1st year`, axis2 = `2nd year`, axis3 = `3rd year`, axis4 = `4th year`, y = pct)) +
      geom_alluvium(aes(fill = `1st year`), alpha = 0.85, width = 0.2) +
      geom_stratum(width = 0.2, fill = "grey92", color = "grey50") +
      geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
      scale_x_discrete(limits = c("1st year", "2nd year", "3rd year", "4th year"), expand = c(0.08, 0.05)) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      scale_fill_manual(
        values = c(
          G1 = "#AED6F1",
          G2 = "#5DADE2",
          G3a = "#58D68D",
          G3b = "#F4D03F",
          G4 = "#EB984E",
          G5 = "#CD6155"
        ),
        drop = FALSE
      ) +
      labs(
        title = "Alluvial plot of CKD G-stage across follow-up years",
        subtitle = paste0("Complete 4-year follow-up patients: ", nrow(wide)),
        x = NULL,
        y = "Percent of complete cases",
        fill = "1st year G-stage"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"))
  })

  output$risk_alluvial <- renderPlot({
    dat <- complete_4y() %>%
      select(study_id, followup_year, risk_4cat) %>%
      filter(!is.na(risk_4cat)) %>%
      mutate(risk_4cat = factor(risk_4cat, levels = c("Green", "Yellow", "Orange", "Red")))

    validate(
      need(nrow(dat) > 0, "No complete 4-year follow-up data available after filtering."),
      need(all(1:4 %in% dat$followup_year), "Not all follow-up years are available after filtering.")
    )

    wide <- dat %>%
      mutate(followup_label = paste0(followup_year, ordinal_suffix(followup_year), " year")) %>%
      select(study_id, followup_label, risk_4cat) %>%
      distinct() %>%
      pivot_wider(names_from = followup_label, values_from = risk_4cat) %>%
      drop_na()

    validate(need(nrow(wide) > 0, "No patients with complete risk data across all 4 years after filtering."))

    flow <- wide %>%
      count(`1st year`, `2nd year`, `3rd year`, `4th year`, name = "n") %>%
      mutate(pct = n / sum(n))

    ggplot(flow, aes(axis1 = `1st year`, axis2 = `2nd year`, axis3 = `3rd year`, axis4 = `4th year`, y = pct)) +
      geom_alluvium(aes(fill = `1st year`), alpha = 0.75, width = 0.2) +
      geom_stratum(width = 0.2, fill = "grey85", color = "grey50") +
      geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
      scale_x_discrete(limits = c("1st year", "2nd year", "3rd year", "4th year"), expand = c(0.08, 0.05)) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      scale_fill_manual(
        values = c(Green = "#2ECC71", Yellow = "#F1C40F", Orange = "#E67E22", Red = "#E74C3C"),
        drop = FALSE
      ) +
      labs(
        title = "Alluvial plot of KDIGO CKD risk across follow-up years",
        subtitle = paste0("Complete 4-year follow-up patients: ", nrow(wide)),
        x = NULL,
        y = "Percent of complete cases",
        fill = "1st year risk"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"))
  })

  map_data <- reactive({
    req(!is.null(banphaeo_sf))

    area_sum <- filtered_data() %>%
      filter(!is.na(subdistrict_clean), subdistrict_clean != "") %>%
      group_by(subdistrict_clean) %>%
      summarise(
        n_patients = dplyr::n(),
        ckd_pct = safe_pct(diagnosis_type != "No CKD"),
        no_ckd_pct = safe_pct(diagnosis_type == "No CKD"),
        egfr_alone_pct = safe_pct(diagnosis_type == "eGFR alone"),
        uacr_alone_pct = safe_pct(diagnosis_type == "UACR alone"),
        both_pct = safe_pct(diagnosis_type == "Both eGFR + UACR"),
        green_pct = safe_pct(risk_4cat == "Green"),
        yellow_pct = safe_pct(risk_4cat == "Yellow"),
        orange_pct = safe_pct(risk_4cat == "Orange"),
        red_pct = safe_pct(risk_4cat == "Red"),
        .groups = "drop"
      )

    banphaeo_sf %>%
      left_join(area_sum, by = "subdistrict_clean")
  })

  output$subdistrict_map <- renderLeaflet({
    req(input$main_tabs == "geo")
    req(!is.null(banphaeo_sf))
    req(!is.null(input$map_metric), !is.na(input$map_metric))

    metric_labels <- list(
      n_patients = "Patient count",
      ckd_pct = "CKD prevalence (%)",
      no_ckd_pct = "No CKD (%)",
      egfr_alone_pct = "eGFR alone (%)",
      uacr_alone_pct = "UACR alone (%)",
      both_pct = "Both eGFR + UACR (%)",
      green_pct = "Green risk (%)",
      yellow_pct = "Yellow risk (%)",
      orange_pct = "Orange risk (%)",
      red_pct = "Red risk (%)"
    )

    metric <- input$map_metric
    dat <- map_data()

    validate(
      need(metric %in% names(metric_labels), "Invalid map metric."),
      need(nrow(dat) > 0, "No geographic data available after filtering."),
      need(metric %in% names(dat), "Selected metric column not found.")
    )

    dat <- dat %>%
      mutate(
        map_value = .[[metric]],
        map_value = ifelse(is.nan(map_value), NA_real_, map_value),
        ckd_fill = ifelse(is.nan(ckd_pct), NA_real_, ckd_pct)
      )

    values_finite <- dat$map_value[is.finite(dat$map_value)]
    fill_values_finite <- dat$ckd_fill[is.finite(dat$ckd_fill)]
    legend_title <- metric_labels[[metric]]
    fill_legend_title <- "CKD prevalence (%)"

    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = c(0,100),
      na.color = "#f0f0f0"
    )

    if (identical(metric, "n_patients")) {
      label_fmt <- function(x) ifelse(is.na(x), "NA", scales::comma(round(x, 0)))
    } else {
      label_fmt <- function(x) ifelse(is.na(x), "NA", paste0(round(x, 1), "%"))
    }

    bb <- sf::st_bbox(dat)

    leaflet::leaflet(data = dat, options = leaflet::leafletOptions(preferCanvas = TRUE)) %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::fitBounds(
        lng1 = bb[["xmin"]], lat1 = bb[["ymin"]],
        lng2 = bb[["xmax"]], lat2 = bb[["ymax"]]
      ) %>%
      leaflet::addPolygons(
        fillColor = ~pal(ckd_fill),
        fillOpacity = 0.85,
        color = "#444444",
        weight = 1,
        opacity = 1,
        smoothFactor = 0.2,
        popup = ~htmltools::HTML(paste0(
          "<b>ตำบล:</b> ", subdistrict_th, "<br>",
          "<b>N:</b> ", ifelse(is.na(n_patients), 0, scales::comma(round(n_patients, 0))), "<br>",
          "<b>", legend_title, ":</b> ", label_fmt(map_value)
        ))
      ) %>%
      leaflet::addLabelOnlyMarkers(
        data = sf::st_point_on_surface(dat),
        lng = ~sf::st_coordinates(geometry)[, 1],
        lat = ~sf::st_coordinates(geometry)[, 2],
        label = ~paste0(
          subdistrict_th,
          "
N=", ifelse(is.na(n_patients), 0, scales::comma(round(n_patients, 0))),
          if (identical(metric, "n_patients")) {
            paste0("
CKD=", ifelse(is.na(ckd_pct), "NA", paste0(round(ckd_pct, 1), "%")))
          } else {
            paste0("
", ifelse(is.na(map_value), "NA", paste0(round(map_value, 1), "%")))
          }
        ),
        labelOptions = leaflet::labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE,
          style = list(
            "font-size" = "11px",
            "font-weight" = "600",
            "color" = "#1f1f1f",
            "text-shadow" = "0 0 3px white, 0 0 3px white",
            "white-space" = "pre-line",
            "text-align" = "center"
          )
        )
      ) %>%
      leaflet::addLegend(
        position = "bottomright",
        pal = pal,
        values = fill_values_finite,
        title = fill_legend_title,
        opacity = 0.9
      )
  })

  outputOptions(output, "subdistrict_map", suspendWhenHidden = FALSE)

  output$map_note <- renderUI({
    if (is.null(banphaeo_sf)) {
      HTML("<span style='color:#b30000;'>Shapefile could not be read. Make sure cualbumin_samutsakhon.shp/.shx/.dbf/.prj are in the same folder as this app.</span>")
    } else {
      matched_n <- filtered_data() %>%
        filter(!is.na(subdistrict_clean), subdistrict_clean %in% banphaeo_tambons) %>%
        nrow()
      HTML(paste0(
        "Map uses the บ้านแพ้ว subdistrict polygons from the uploaded shapefile. ",
        "Rows in current filtered dataset with cleaned subdistrict matched to บ้านแพ้ว tambons: <b>",
        format(matched_n, big.mark = ","),
        "</b>."
      ))
    }
  })

  output$subdistrict_match_preview <- renderTable({
    metric <- input$map_metric

    map_data() %>%
      sf::st_drop_geometry() %>%
      mutate(
        metric_pct = dplyr::case_when(
          metric == "ckd_pct" ~ ckd_pct,
          metric == "no_ckd_pct" ~ no_ckd_pct,
          metric == "egfr_alone_pct" ~ egfr_alone_pct,
          metric == "uacr_alone_pct" ~ uacr_alone_pct,
          metric == "both_pct" ~ both_pct,
          metric == "green_pct" ~ green_pct,
          metric == "yellow_pct" ~ yellow_pct,
          metric == "orange_pct" ~ orange_pct,
          metric == "red_pct" ~ red_pct,
          TRUE ~ ckd_pct
        )
      ) %>%
      transmute(
        subdistrict = subdistrict_th,
        n_patients = dplyr::coalesce(as.integer(n_patients), 0L),
        percentage = ifelse(is.na(metric_pct), NA_character_, paste0(round(metric_pct, 1), "%"))
      ) %>%
      arrange(desc(n_patients), subdistrict)
  })
}

shinyApp(ui = ui, server = server)
