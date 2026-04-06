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

DATA_PATH  <- "data/ALBP_long.csv"
SHAPE_PATH <- "shapefile/cualbumin_samutsakhon.shp"

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

banphaeo_lookup <- c(
  "BAN PHAEO" = "บ้านแพ้ว",
  "LAK SAM" = "หลักสาม",
  "YOK KRABAT" = "ยกกระบัตร",
  "RONG KHE" = "โรงเข้",
  "AMPHAENG" = "อำแพง",
  "KHLONG TAN" = "คลองตัน",
  "KASET PHATTHANA" = "เกษตรพัฒนา",
  "NONG SONG HONG" = "หนองสองห้อง",
  "NONG BUA" = "หนองบัว",
  "CHET RIO" = "เจ็ดริ้ว",
  "SUAN SOM" = "สวนส้ม",
  "LAK SONG" = "หลักสอง"
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
    st_make_valid()

  if (is.na(st_crs(sf_obj))) {
    st_crs(sf_obj) <- 4326
  } else if (st_crs(sf_obj)$epsg != 4326) {
    sf_obj <- st_transform(sf_obj, 4326)
  }

  required_cols <- c("A_NAME_E", "T_NAME_E")
  missing_cols <- setdiff(required_cols, names(sf_obj))
  if (length(missing_cols) > 0) {
    stop(paste("Shapefile must contain columns:", paste(required_cols, collapse = ", ")))
  }

  sf_obj %>%
    mutate(
      district_en = as.character(A_NAME_E),
      subdistrict_en = as.character(T_NAME_E)
    ) %>%
    filter(district_en == "BAN PHAEO") %>%
    mutate(
      subdistrict_th = dplyr::recode(subdistrict_en, !!!banphaeo_lookup, .default = subdistrict_en),
      subdistrict_clean = clean_subdistrict(subdistrict_th)
    ) %>%
    select(subdistrict_th, subdistrict_en, subdistrict_clean, geometry)
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
    albii_f = case_when(
      as.character(albii) %in% c("Positive", "Negative") ~ as.character(albii),
      is.na(albii) | as.character(albii) == "" ~ NA_character_,
      TRUE ~ as.character(albii)
    ),
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
  tags$div(
    style = "display:flex; align-items:center; gap:12px; margin-bottom:15px;",
    tags$img(
      src = "https://raw.githubusercontent.com/EC-CCN/logo/master/EC-CCN%20logo%204K.png",
      height = "60px"
    ),
    tags$h2("Ban Phaeo CKD dashboard")
  ),
  # titlePanel("Ban Phaeo CKD dashboard"),
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
      selectInput("albii", "Albii", choices = c("All", "Positive", "Negative"), selected = "All"),
      hr(),
      selectInput(
        "map_metric",
        "Geographic map metric",
        choices = c(
          "Participant count" = "n_patients",
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
      selectInput(
        "map_year",
        "Geographic map year",
        choices = c("Overall", "2022", "2023", "2024", "2025"),
        selected = "Overall"
      ),
      helpText("All plots update automatically with the selected filter."),
      helpText("Heatmaps show year-specific N and overall N in the x-axis labels."),
      helpText("Alluvial plots use all participants after filtering; missing later follow-up is shown as Not seen.")
    ),
    mainPanel(
      width = 9,
      fluidRow(
        column(4, wellPanel(h4("Filtered rows"), textOutput("n_rows"))),
        column(4, wellPanel(h4("Unique participants"), textOutput("n_patients"))),
        column(4, wellPanel(h4("Complete 4-year follow-up"), textOutput("n_complete")))
      ),
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          "General info",
          value = "general",
          br(),
          h4("1. New participants and followed-up participants by calendar year"),
          plotlyOutput("yearly_participant_type_plot", height = "380px"),
          tableOutput("yearly_participant_type_table"),
          br(),
          h4("2. Trajectory of individual participants by calendar year"),
          plotOutput("followup_trajectory_plot", height = "650px"),
          br(),
          h4("3. Participants retained through consecutive follow-up years"),
          selectInput(
            "retention_start_year",
            "Starting year",
            choices = c("2022", "2023", "2024", "2025"),
            selected = "2022"
          ),
          plotOutput("consecutive_followup_plot", height = "420px")
        ),
        tabPanel("Diagnosis heatmap", value = "diag", plotlyOutput("diag_heatmap", height = "520px")),
        tabPanel("G-stage heatmap", value = "g", plotlyOutput("g_heatmap", height = "560px")),
        tabPanel("A-stage heatmap", value = "a", plotlyOutput("a_heatmap", height = "460px")),
        tabPanel("Risk heatmap", value = "risk", plotlyOutput("risk_heatmap", height = "440px")),
        tabPanel("Alluvial: G-stage", value = "g_alluvial", plotOutput("g_alluvial", height = "650px")),
        tabPanel("Alluvial: risk", value = "risk_alluvial", plotOutput("risk_alluvial", height = "650px")),
        tabPanel(
          "Geographic map",
          value = "geo",
          plotOutput("subdistrict_map", height = "720px"),
          br(),
          htmlOutput("map_note"),
          br(),
          tableOutput("subdistrict_match_preview")
        )
      )
    )
  ),
  tags$hr(),
  
  tags$div(
    style = "
    text-align:center;
    font-size:12px;
    color:#777;
    margin-top:30px;
    margin-bottom:10px;
  ",
    
    HTML("&copy; 2026 EC-CCN. All rights reserved.")
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
    dat <- filter_binary(dat, "albii_f", input$albii)
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

  yearly_participant_type_tbl <- reactive({
    participant_years <- filtered_data() %>%
      distinct(study_id, year)

    first_year_tbl <- participant_years %>%
      group_by(study_id) %>%
      summarise(first_year = min(year), .groups = "drop")

    participant_years %>%
      left_join(first_year_tbl, by = "study_id") %>%
      mutate(participant_type = if_else(year == first_year, "New", "Followed-up")) %>%
      count(year, participant_type, name = "n") %>%
      complete(
        year = c("2022", "2023", "2024", "2025"),
        participant_type = c("New", "Followed-up"),
        fill = list(n = 0)
      ) %>%
      arrange(factor(year, levels = c("2022", "2023", "2024", "2025")), participant_type)
  })

  output$yearly_participant_type_plot <- renderPlotly({
    tbl <- yearly_participant_type_tbl()

    p <- ggplot(
      tbl,
      aes(
        x = factor(year, levels = c("2022", "2023", "2024", "2025")),
        y = n,
        fill = participant_type,
        text = paste0("Year: ", year, "<br>Type: ", participant_type, "<br>N: ", comma(n))
      )
    ) +
      geom_col(position = "stack") +
      geom_text(
        aes(label = ifelse(n > 0, comma(n), "")),
        position = position_stack(vjust = 0.5),
        color = "black",
        size = 4
      ) +
      scale_fill_manual(values = c("New" = "#5DADE2", "Followed-up" = "#58D68D")) +
      labs(
        title = "Numbers of new and followed-up participants by calendar year",
        x = NULL,
        y = "Number of unique participants",
        fill = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"))

    ggplotly(p, tooltip = "text")
  })

  output$yearly_participant_type_table <- renderTable({
    tbl <- yearly_participant_type_tbl() %>%
      tidyr::pivot_wider(names_from = participant_type, values_from = n) %>%
      mutate(Total = New + `Followed-up`)
    tbl
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  trajectory_wide <- reactive({
    years_int <- 2022:2025

    participant_years <- filtered_data() %>%
      distinct(study_id, year) %>%
      mutate(year = as.integer(year))

    first_years <- participant_years %>%
      group_by(study_id) %>%
      summarise(first_year = min(year), .groups = "drop")

    tidyr::expand_grid(
      study_id = unique(participant_years$study_id),
      year = years_int
    ) %>%
      left_join(first_years, by = "study_id") %>%
      left_join(
        participant_years %>% mutate(present = TRUE),
        by = c("study_id", "year")
      ) %>%
      mutate(
        present = if_else(is.na(present), FALSE, present),
        status = case_when(
          year < first_year ~ "Not yet in cohort",
          present & year == first_year ~ "New",
          present & year > first_year ~ "Followed-up",
          !present & year > first_year ~ "Lost follow-up",
          TRUE ~ NA_character_
        ),
        year_label = as.character(year)
      ) %>%
      select(study_id, year_label, status) %>%
      pivot_wider(names_from = year_label, values_from = status)
  })

  output$followup_trajectory_plot <- renderPlot({
    wide <- trajectory_wide()

    validate(need(nrow(wide) > 0, "No participant trajectory data available after filtering."))

    flow <- wide %>%
      count(`2022`, `2023`, `2024`, `2025`, name = "n") %>%
      mutate(pct = n / sum(n))

    ggplot(
      flow,
      aes(axis1 = `2022`, axis2 = `2023`, axis3 = `2024`, axis4 = `2025`, y = pct)
    ) +
      geom_alluvium(aes(fill = `2022`), alpha = 0.8, width = 0.18, discern = TRUE) +
      geom_stratum(width = 0.18, fill = "grey90", color = "grey50") +
      geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3.8) +
      scale_x_discrete(limits = c("2022", "2023", "2024", "2025"), expand = c(0.06, 0.04)) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      scale_fill_manual(
        values = c(
          "Not yet in cohort" = "#D5D8DC",
          "New" = "#5DADE2",
          "Followed-up" = "#58D68D",
          "Lost follow-up" = "#F5B041"
        ),
        drop = FALSE
      ) +
      labs(
        title = "Trajectory of individual participants by calendar year",
        subtitle = paste0("Unique filtered participants: ", comma(nrow(wide))),
        x = NULL,
        y = "Percent of unique participants",
        fill = "2022 status"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"))
  })

  consecutive_followup_tbl <- reactive({
    start_year <- as.integer(input$retention_start_year)

    year_df <- filtered_data() %>%
      distinct(study_id, year) %>%
      mutate(year = as.integer(year))

    first_year_df <- year_df %>%
      group_by(study_id) %>%
      summarise(first_year = min(year, na.rm = TRUE), .groups = "drop")

    base_ids <- first_year_df %>%
      filter(first_year == start_year) %>%
      distinct(study_id)

    years_seq <- seq(start_year, 2025)
    baseline_n <- nrow(base_ids)

    if (baseline_n == 0) {
      return(tibble(
        year = years_seq,
        stage = seq_along(years_seq),
        label = paste0(years_seq, " (Year ", seq_along(years_seq), ")"),
        n_participants = 0L,
        pct_retained = NA_real_,
        baseline_n = 0L
      ))
    }

    retained_counts <- lapply(years_seq, function(y_end) {
      required_years <- seq(start_year, y_end)

      n_retained <- year_df %>%
        semi_join(base_ids, by = "study_id") %>%
        filter(year %in% required_years) %>%
        distinct(study_id, year) %>%
        count(study_id, name = "n_years_seen") %>%
        filter(n_years_seen == length(required_years)) %>%
        summarise(n_participants = n()) %>%
        pull(n_participants)

      tibble(
        year = y_end,
        n_participants = ifelse(length(n_retained) == 0 || is.na(n_retained), 0L, as.integer(n_retained))
      )
    }) %>%
      bind_rows()

    tibble(year = years_seq) %>%
      left_join(retained_counts, by = "year") %>%
      mutate(
        n_participants = coalesce(n_participants, 0L),
        stage = seq_along(years_seq),
        label = paste0(year, " (Year ", stage, ")"),
        baseline_n = baseline_n,
        pct_retained = ifelse(baseline_n > 0, 100 * n_participants / baseline_n, NA_real_)
      )
  })

  output$consecutive_followup_plot <- renderPlot({
    tbl <- consecutive_followup_tbl()
    start_year <- as.integer(input$retention_start_year)

    ggplot(tbl, aes(x = factor(label, levels = label), y = n_participants)) +
      geom_col(fill = "#5D6D7E", width = 0.7) +
      geom_text(
        aes(label = paste0(comma(n_participants), "
(", ifelse(is.na(pct_retained), "NA", paste0(round(pct_retained, 1), "%")), ")")),
        vjust = -0.3,
        size = 4.1,
        lineheight = 0.9
      ) +
      expand_limits(y = max(tbl$n_participants, 0) * 1.16 + 1) +
      labs(
        title = "Participants retained from selected starting year",
        subtitle = paste0(
          "Baseline = new participants entering in ", start_year,
          "; each later bar shows how many were retained in every consecutive year through that year"
        ),
        x = NULL,
        y = "Number of unique participants"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"))
  })


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
    g_levels <- c("G1", "G2", "G3a", "G3b", "G4", "G5", "Not seen")

    dat <- filtered_data() %>%
      transmute(
        study_id,
        followup_year,
        g_stage = as.character(g_stage)
      ) %>%
      filter(!is.na(study_id), !is.na(followup_year), followup_year %in% 1:4) %>%
      mutate(
        g_stage = ifelse(is.na(g_stage) | g_stage == "", "Not seen", g_stage),
        g_stage = factor(g_stage, levels = g_levels, ordered = TRUE)
      ) %>%
      group_by(study_id, followup_year) %>%
      summarise(
        g_stage = as.character(max(g_stage, na.rm = TRUE)),
        .groups = "drop"
      )

    validate(
      need(nrow(dat) > 0, "No participant data available after filtering.")
    )

    all_ids <- filtered_data() %>%
      distinct(study_id)

    wide <- tidyr::expand_grid(
      study_id = all_ids$study_id,
      followup_year = 1:4
    ) %>%
      left_join(dat, by = c("study_id", "followup_year")) %>%
      mutate(
        followup_label = paste0(followup_year, ordinal_suffix(followup_year), " year"),
        g_stage = ifelse(is.na(g_stage), "Not seen", g_stage),
        g_stage = factor(g_stage, levels = g_levels)
      ) %>%
      select(study_id, followup_label, g_stage) %>%
      pivot_wider(
        names_from = followup_label,
        values_from = g_stage,
        values_fill = factor("Not seen", levels = g_levels)
      ) %>%
      mutate(across(`1st year`:`4th year`, ~ factor(as.character(.x), levels = g_levels)))

    flow <- wide %>%
      count(`1st year`, `2nd year`, `3rd year`, `4th year`, name = "n") %>%
      mutate(pct = n / sum(n))

    ggplot(flow, aes(axis1 = `1st year`, axis2 = `2nd year`, axis3 = `3rd year`, axis4 = `4th year`, y = pct)) +
      geom_alluvium(aes(fill = `1st year`), alpha = 0.85, width = 0.2, na.rm = TRUE) +
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
          G5 = "#CD6155",
          `Not seen` = "#D5D8DC"
        ),
        drop = FALSE
      ) +
      labs(
        title = "Alluvial plot of CKD G-stage across follow-up years",
        subtitle = paste0("All filtered participants: ", nrow(wide)),
        x = NULL,
        y = "Percent of participants",
        fill = "1st year G-stage"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"))
  })

  output$risk_alluvial <- renderPlot({
    risk_levels <- c("Green", "Yellow", "Orange", "Red", "Not seen")

    dat <- filtered_data() %>%
      transmute(
        study_id,
        followup_year,
        risk_4cat = as.character(risk_4cat)
      ) %>%
      filter(!is.na(study_id), !is.na(followup_year), followup_year %in% 1:4) %>%
      mutate(
        risk_4cat = ifelse(is.na(risk_4cat) | risk_4cat == "", "Not seen", risk_4cat),
        risk_4cat = factor(risk_4cat, levels = risk_levels, ordered = TRUE)
      ) %>%
      group_by(study_id, followup_year) %>%
      summarise(
        risk_4cat = as.character(max(risk_4cat, na.rm = TRUE)),
        .groups = "drop"
      )

    validate(
      need(nrow(dat) > 0, "No participant data available after filtering.")
    )

    all_ids <- filtered_data() %>%
      distinct(study_id)

    wide <- tidyr::expand_grid(
      study_id = all_ids$study_id,
      followup_year = 1:4
    ) %>%
      left_join(dat, by = c("study_id", "followup_year")) %>%
      mutate(
        followup_label = paste0(followup_year, ordinal_suffix(followup_year), " year"),
        risk_4cat = ifelse(is.na(risk_4cat), "Not seen", risk_4cat),
        risk_4cat = factor(risk_4cat, levels = risk_levels)
      ) %>%
      select(study_id, followup_label, risk_4cat) %>%
      pivot_wider(
        names_from = followup_label,
        values_from = risk_4cat,
        values_fill = factor("Not seen", levels = risk_levels)
      ) %>%
      mutate(across(`1st year`:`4th year`, ~ factor(as.character(.x), levels = risk_levels)))

    flow <- wide %>%
      count(`1st year`, `2nd year`, `3rd year`, `4th year`, name = "n") %>%
      mutate(pct = n / sum(n))

    ggplot(flow, aes(axis1 = `1st year`, axis2 = `2nd year`, axis3 = `3rd year`, axis4 = `4th year`, y = pct)) +
      geom_alluvium(aes(fill = `1st year`), alpha = 0.75, width = 0.2, na.rm = TRUE) +
      geom_stratum(width = 0.2, fill = "grey85", color = "grey50") +
      geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
      scale_x_discrete(limits = c("1st year", "2nd year", "3rd year", "4th year"), expand = c(0.08, 0.05)) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      scale_fill_manual(
        values = c(Green = "#2ECC71", Yellow = "#F1C40F", Orange = "#E67E22", Red = "#E74C3C", `Not seen` = "#D5D8DC"),
        drop = FALSE
      ) +
      labs(
        title = "Alluvial plot of KDIGO CKD risk across follow-up years",
        subtitle = paste0("All filtered participants: ", nrow(wide)),
        x = NULL,
        y = "Percent of participants",
        fill = "1st year risk"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"))
  })

  map_filtered_data <- reactive({
    dat <- filtered_data()
    if (!is.null(input$map_year) && input$map_year != "Overall") {
      dat <- dat %>% filter(year == input$map_year)
    }
    dat
  })

  map_total_data <- reactive({
    dat <- raw
    if (!is.null(input$map_year) && input$map_year != "Overall") {
      dat <- dat %>% filter(year == input$map_year)
    }
    dat
  })


  map_has_characteristic_filter <- reactive({
    isTRUE(input$age_lower > 0) ||
      any(c(input$dm, input$htn, input$dlp, input$cvd, input$smoking,
            input$alc, input$herbalmed, input$analgesic, input$suppl) != "All")
  })

  map_data <- reactive({
    req(!is.null(banphaeo_sf))

    total_filtered_unique_all <- map_filtered_data() %>%
      summarise(total_filtered_unique_all = dplyr::n_distinct(study_id)) %>%
      pull(total_filtered_unique_all)

    total_area_sum <- map_total_data() %>%
      filter(!is.na(subdistrict_clean), subdistrict_clean != "") %>%
      group_by(subdistrict_clean) %>%
      summarise(
        total_patients_all = dplyr::n_distinct(study_id),
        .groups = "drop"
      )

    area_sum <- map_filtered_data() %>%
      filter(!is.na(subdistrict_clean), subdistrict_clean != "") %>%
      group_by(subdistrict_clean) %>%
      summarise(
        n_patients = dplyr::n_distinct(study_id),
        n_ckd = dplyr::n_distinct(study_id[diagnosis_type != "No CKD" & !is.na(diagnosis_type)]),
        n_no_ckd = dplyr::n_distinct(study_id[diagnosis_type == "No CKD" & !is.na(diagnosis_type)]),
        n_egfr_alone = dplyr::n_distinct(study_id[diagnosis_type == "eGFR alone" & !is.na(diagnosis_type)]),
        n_uacr_alone = dplyr::n_distinct(study_id[diagnosis_type == "UACR alone" & !is.na(diagnosis_type)]),
        n_both = dplyr::n_distinct(study_id[diagnosis_type == "Both eGFR + UACR" & !is.na(diagnosis_type)]),
        n_green = dplyr::n_distinct(study_id[risk_4cat == "Green" & !is.na(risk_4cat)]),
        n_yellow = dplyr::n_distinct(study_id[risk_4cat == "Yellow" & !is.na(risk_4cat)]),
        n_orange = dplyr::n_distinct(study_id[risk_4cat == "Orange" & !is.na(risk_4cat)]),
        n_red = dplyr::n_distinct(study_id[risk_4cat == "Red" & !is.na(risk_4cat)]),
        .groups = "drop"
      )

    out <- banphaeo_sf %>%
      left_join(total_area_sum, by = "subdistrict_clean") %>%
      left_join(area_sum, by = "subdistrict_clean") %>%
      mutate(
        total_filtered_unique_all = total_filtered_unique_all,
        total_patients_all = dplyr::coalesce(total_patients_all, 0L),
        n_patients = dplyr::coalesce(n_patients, 0L),
        n_ckd = dplyr::coalesce(n_ckd, 0L),
        n_no_ckd = dplyr::coalesce(n_no_ckd, 0L),
        n_egfr_alone = dplyr::coalesce(n_egfr_alone, 0L),
        n_uacr_alone = dplyr::coalesce(n_uacr_alone, 0L),
        n_both = dplyr::coalesce(n_both, 0L),
        n_green = dplyr::coalesce(n_green, 0L),
        n_yellow = dplyr::coalesce(n_yellow, 0L),
        n_orange = dplyr::coalesce(n_orange, 0L),
        n_red = dplyr::coalesce(n_red, 0L),
        ckd_pct = ifelse(n_patients > 0, 100 * n_ckd / n_patients, NA_real_),
        no_ckd_pct = ifelse(n_patients > 0, 100 * n_no_ckd / n_patients, NA_real_),
        egfr_alone_pct = ifelse(n_patients > 0, 100 * n_egfr_alone / n_patients, NA_real_),
        uacr_alone_pct = ifelse(n_patients > 0, 100 * n_uacr_alone / n_patients, NA_real_),
        both_pct = ifelse(n_patients > 0, 100 * n_both / n_patients, NA_real_),
        green_pct = ifelse(n_patients > 0, 100 * n_green / n_patients, NA_real_),
        yellow_pct = ifelse(n_patients > 0, 100 * n_yellow / n_patients, NA_real_),
        orange_pct = ifelse(n_patients > 0, 100 * n_orange / n_patients, NA_real_),
        red_pct = ifelse(n_patients > 0, 100 * n_red / n_patients, NA_real_)
      )

    out
  })

  output$subdistrict_map <- renderPlot({
    req(input$main_tabs == "geo")
    req(!is.null(banphaeo_sf))
    req(!is.null(input$map_metric), !is.na(input$map_metric))

    metric <- input$map_metric
    dat <- map_data()

    validate(
      need(nrow(dat) > 0, "No geographic data available after filtering."),
      need(metric %in% c("n_patients", "ckd_pct", "no_ckd_pct", "egfr_alone_pct", "uacr_alone_pct", "both_pct", "green_pct", "yellow_pct", "orange_pct", "red_pct"), "Selected metric column not found.")
    )

    metric_label <- dplyr::case_match(
      metric,
      "n_patients" ~ "Participant count (%)",
      "ckd_pct" ~ "CKD prevalence (%)",
      "no_ckd_pct" ~ "No CKD (%)",
      "egfr_alone_pct" ~ "eGFR alone (%)",
      "uacr_alone_pct" ~ "UACR alone (%)",
      "both_pct" ~ "Both eGFR + UACR (%)",
      "green_pct" ~ "Green risk (%)",
      "yellow_pct" ~ "Yellow risk (%)",
      "orange_pct" ~ "Orange risk (%)",
      "red_pct" ~ "Red risk (%)",
      .default = "Percentage (%)"
    )

    count_uses_subdistrict_total <- metric == "n_patients" && isTRUE(map_has_characteristic_filter())

    if (metric == "n_patients") {
      if (count_uses_subdistrict_total) {
        dat <- dat %>% mutate(n_metric = n_patients, denom_n = total_patients_all)
      } else {
        dat <- dat %>% mutate(n_metric = n_patients, denom_n = total_filtered_unique_all)
      }
    } else if (metric == "ckd_pct") {
      dat <- dat %>% mutate(n_metric = n_ckd, denom_n = n_patients)
    } else if (metric == "no_ckd_pct") {
      dat <- dat %>% mutate(n_metric = n_no_ckd, denom_n = n_patients)
    } else if (metric == "egfr_alone_pct") {
      dat <- dat %>% mutate(n_metric = n_egfr_alone, denom_n = n_patients)
    } else if (metric == "uacr_alone_pct") {
      dat <- dat %>% mutate(n_metric = n_uacr_alone, denom_n = n_patients)
    } else if (metric == "both_pct") {
      dat <- dat %>% mutate(n_metric = n_both, denom_n = n_patients)
    } else if (metric == "green_pct") {
      dat <- dat %>% mutate(n_metric = n_green, denom_n = n_patients)
    } else if (metric == "yellow_pct") {
      dat <- dat %>% mutate(n_metric = n_yellow, denom_n = n_patients)
    } else if (metric == "orange_pct") {
      dat <- dat %>% mutate(n_metric = n_orange, denom_n = n_patients)
    } else if (metric == "red_pct") {
      dat <- dat %>% mutate(n_metric = n_red, denom_n = n_patients)
    } else {
      dat <- dat %>% mutate(n_metric = n_patients, denom_n = total_filtered_unique_all)
    }

    dat <- dat %>%
      mutate(
        display_pct = ifelse(!is.na(denom_n) & denom_n > 0, 100 * n_metric / denom_n, NA_real_),
        fill_pct = display_pct,
        label_text = paste0(
          subdistrict_th,
          "
",
          dplyr::coalesce(as.integer(n_metric), 0L),
          "/",
          dplyr::coalesce(as.integer(denom_n), 0L),
          " (",
          ifelse(is.na(display_pct), "NA", paste0(round(display_pct, 1), "%")),
          ")"
        )
      )

    ggplot(dat) +
      geom_sf(aes(fill = fill_pct), color = "white", linewidth = 0.7) +
      geom_sf_text(
        aes(label = label_text),
        size = 2.45,
        fontface = "bold",
        lineheight = 0.95,
        color = "black",
        check_overlap = TRUE,
        na.rm = TRUE
      ) +
      scale_fill_gradient(
        low = "#fff7bc",
        high = "#d95f0e",
        limits = c(0, 100),
        na.value = "grey90",
        name = metric_label,
        labels = function(x) paste0(x, "%")
      ) +
      labs(
        title = "Geographic distribution by subdistrict",
        subtitle = paste0("Polygon fill shows ", metric_label, " with a fixed 0-100 scale")
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 10),
        plot.subtitle = element_text(size = 8),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 8)
      )
  }, res = 170)

  output$map_note <- renderUI({
    if (is.null(banphaeo_sf)) {
      HTML("<span style='color:#b30000;'>Shapefile could not be read. Make sure cualbumin_samutsakhon.shp/.shx/.dbf/.prj are in the same folder as this app.</span>")
    } else {
      filtered_df <- map_filtered_data()
      matched_n <- filtered_df %>%
        dplyr::filter(!is.na(subdistrict_clean), subdistrict_clean %in% banphaeo_tambons) %>%
        dplyr::summarise(n = dplyr::n_distinct(.data$study_id)) %>%
        dplyr::pull(n)
      total_filtered_n <- filtered_df %>%
        dplyr::summarise(n = dplyr::n_distinct(.data$study_id)) %>%
        dplyr::pull(n)
      year_note <- if (!is.null(input$map_year) && input$map_year != "Overall") paste0(" for year <b>", input$map_year, "</b>") else " across all years"
      count_note <- if (isTRUE(map_has_characteristic_filter())) {
        " For <b>Participant count</b>, because participant characteristics are filtered, the numerator is the filtered participant N in each subdistrict and the denominator is the <b>unfiltered total participant N in that subdistrict</b>."
      } else {
        " For <b>Participant count</b>, because participant characteristics are not filtered, the numerator is the participant N in each subdistrict and the denominator is the <b>total unique participants in the selected map cohort</b>."
      }
      HTML(paste0(
        "Map fill is based on the <b>selected geographic metric percentage</b> with a fixed 0-100 scale.",
        count_note,
        " For all other metrics, the numerator is the filtered participants in that diagnosis/risk category and the denominator is the <b>filtered participant N in that subdistrict</b>.",
        " Current matched unique participants", year_note, ": <b>",
        format(matched_n, big.mark = ","),
        "</b> of total filtered unique participants: <b>",
        format(total_filtered_n, big.mark = ","),
        "</b>."
      ))
    }
  })

  output$subdistrict_match_preview <- renderTable({
    metric <- input$map_metric

    dat <- map_data() %>%
      sf::st_drop_geometry()

    count_uses_subdistrict_total <- metric == "n_patients" && isTRUE(map_has_characteristic_filter())

    if (metric == "n_patients") {
      if (count_uses_subdistrict_total) {
        dat <- dat %>% mutate(n_metric = n_patients, denom_n = total_patients_all)
      } else {
        dat <- dat %>% mutate(n_metric = n_patients, denom_n = total_filtered_unique_all)
      }
    } else if (metric == "ckd_pct") {
      dat <- dat %>% mutate(n_metric = n_ckd, denom_n = n_patients)
    } else if (metric == "no_ckd_pct") {
      dat <- dat %>% mutate(n_metric = n_no_ckd, denom_n = n_patients)
    } else if (metric == "egfr_alone_pct") {
      dat <- dat %>% mutate(n_metric = n_egfr_alone, denom_n = n_patients)
    } else if (metric == "uacr_alone_pct") {
      dat <- dat %>% mutate(n_metric = n_uacr_alone, denom_n = n_patients)
    } else if (metric == "both_pct") {
      dat <- dat %>% mutate(n_metric = n_both, denom_n = n_patients)
    } else if (metric == "green_pct") {
      dat <- dat %>% mutate(n_metric = n_green, denom_n = n_patients)
    } else if (metric == "yellow_pct") {
      dat <- dat %>% mutate(n_metric = n_yellow, denom_n = n_patients)
    } else if (metric == "orange_pct") {
      dat <- dat %>% mutate(n_metric = n_orange, denom_n = n_patients)
    } else if (metric == "red_pct") {
      dat <- dat %>% mutate(n_metric = n_red, denom_n = n_patients)
    } else {
      dat <- dat %>% mutate(n_metric = n_patients, denom_n = total_filtered_unique_all)
    }

    dat %>%
      mutate(
        metric_pct = ifelse(!is.na(denom_n) & denom_n > 0, 100 * n_metric / denom_n, NA_real_)
      ) %>%
      transmute(
        `ตำบล` = subdistrict_th,
        `N (%)` = paste0(
          dplyr::coalesce(as.integer(n_metric), 0L),
          "/",
          dplyr::coalesce(as.integer(denom_n), 0L),
          " (",
          ifelse(is.na(metric_pct), "NA", paste0(round(metric_pct, 1), "%")),
          ")"
        ),
        `Percentage` = ifelse(is.na(metric_pct), NA_character_, paste0(round(metric_pct, 1), "%"))
      ) %>%
      arrange(`ตำบล`)
  })


}

shinyApp(ui = ui, server = server)
