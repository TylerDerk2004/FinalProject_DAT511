# ============================================================
# NOAA Storm Events Database Analysis (2025)
# Final Course Project
# ============================================================


# --- 1. Load Libraries ---------------------------------------
library(dplyr)
library(readr)
library(ggplot2)
library(forcats)
library(lubridate)
library(tidyr)
library(scales)
library(knitr)

# --- 2. Set File Paths ---------------------------------------
folder_path <- "/Users/tylerderkovitz/Downloads/storm_data"

details_file   <- file.path(folder_path, "StormEvents_details-ftp_v1.0_d2025_c20260323.csv.gz")
fatalities_file <- file.path(folder_path, "StormEvents_fatalities-ftp_v1.0_d2025_c20260323.csv.gz")
locations_file  <- file.path(folder_path, "StormEvents_locations-ftp_v1.0_d2025_c20260323.csv.gz")

# --- 3. Load CSV Files ---------------------------------------
details    <- read_csv(details_file,    show_col_types = FALSE)
fatalities <- read_csv(fatalities_file, show_col_types = FALSE)
locations  <- read_csv(locations_file,  show_col_types = FALSE)

# --- 4. Join Files by EVENT_ID --------------------------------
joined_data <- details %>%
  left_join(locations,  by = "EVENT_ID") %>%
  left_join(fatalities, by = "EVENT_ID")

# Save joined dataset
output_file <- file.path(folder_path, "StormEvents_joined_data.csv")
write_csv(joined_data, output_file)
message("Joined data saved to: ", output_file)
print(head(joined_data))

# --- 5. Data Cleaning ----------------------------------------
names(joined_data) <- toupper(names(joined_data))

joined_data <- joined_data %>%
  mutate(
    MONTH_NUM = as.integer(BEGIN_YEARMONTH) %% 100L,
    MONTH     = factor(MONTH_NUM,
                       levels = 1:12,
                       labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                  "Jul","Aug","Sep","Oct","Nov","Dec"))
  )

parse_damage <- function(x) {
  x <- toupper(trimws(as.character(x)))
  multiplier <- case_when(
    grepl("K$", x) ~ 1e3,
    grepl("M$", x) ~ 1e6,
    grepl("B$", x) ~ 1e9,
    TRUE            ~ 1
  )
  numeric_part <- as.numeric(gsub("[^0-9.]", "", x))
  ifelse(is.na(numeric_part), 0, numeric_part * multiplier)
}

joined_data <- joined_data %>%
  mutate(
    DAMAGE_PROPERTY_NUM = parse_damage(DAMAGE_PROPERTY),
    DAMAGE_CROPS_NUM    = parse_damage(DAMAGE_CROPS),
    TOTAL_DAMAGE        = DAMAGE_PROPERTY_NUM + DAMAGE_CROPS_NUM,
    INJURIES_DIRECT     = replace_na(INJURIES_DIRECT, 0),
    INJURIES_INDIRECT   = replace_na(INJURIES_INDIRECT, 0),
    DEATHS_DIRECT       = replace_na(DEATHS_DIRECT, 0),
    DEATHS_INDIRECT     = replace_na(DEATHS_INDIRECT, 0),
    TOTAL_INJURIES      = INJURIES_DIRECT + INJURIES_INDIRECT,
    TOTAL_DEATHS        = DEATHS_DIRECT + DEATHS_INDIRECT
  )

# --- 6. Question 1: Most Harmful Event Types (Health) --------
health_summary <- joined_data %>%
  group_by(EVENT_TYPE) %>%
  summarise(
    Total_Deaths   = sum(TOTAL_DEATHS,   na.rm = TRUE),
    Total_Injuries = sum(TOTAL_INJURIES, na.rm = TRUE),
    Total_Impact   = Total_Deaths + Total_Injuries,
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Impact)) %>%
  slice_head(n = 15) %>%
  pivot_longer(cols = c(Total_Deaths, Total_Injuries),
               names_to = "Impact_Type", values_to = "Count") %>%
  mutate(EVENT_TYPE = fct_reorder(EVENT_TYPE, Count, .fun = sum))

p1 <- ggplot(health_summary, aes(x = Count, y = EVENT_TYPE, fill = Impact_Type)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("Total_Deaths" = "#c0392b", "Total_Injuries" = "#e67e22"),
                    labels = c("Fatalities", "Injuries")) +
  scale_x_continuous(labels = comma) +
  labs(title = "Top 15 Most Harmful Event Types: Health Impact (2025)",
       x = "Total Count", y = "Event Type", fill = "Impact Type",
       caption = "Source: NOAA Storm Events Database, 2025") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
print(p1)

# --- 7. Question 2: Events by State --------------------------
state_totals <- joined_data %>%
  filter(!is.na(STATE)) %>%
  count(STATE, name = "Event_Count") %>%
  arrange(desc(Event_Count)) %>%
  slice_head(n = 15) %>%
  mutate(STATE = fct_reorder(STATE, Event_Count))

p2 <- ggplot(state_totals, aes(x = Event_Count, y = STATE, fill = Event_Count)) +
  geom_col() +
  scale_fill_gradient(low = "#aed6f1", high = "#1a5276") +
  scale_x_continuous(labels = comma) +
  labs(title = "Top 15 States by Total Storm Events (2025)",
       x = "Number of Events", y = "State",
       caption = "Source: NOAA Storm Events Database, 2025") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))
print(p2)

top_states <- state_totals$STATE
state_event_type <- joined_data %>%
  filter(STATE %in% levels(top_states)) %>%
  count(STATE, EVENT_TYPE) %>%
  group_by(STATE) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(STATE = factor(STATE, levels = levels(top_states)))

p3 <- ggplot(state_event_type, aes(x = n, y = STATE, fill = EVENT_TYPE)) +
  geom_col() +
  labs(title = "Most Frequent Event Type per State (Top 15 States, 2025)",
       x = "Occurrences", y = "State", fill = "Event Type",
       caption = "Source: NOAA Storm Events Database, 2025") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold")) +
  guides(fill = guide_legend(nrow = 3))
print(p3)

# --- 8. Question 3: Events by Month -------------------------
top_events <- joined_data %>%
  count(EVENT_TYPE, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(EVENT_TYPE)

monthly_dist <- joined_data %>%
  filter(EVENT_TYPE %in% top_events, !is.na(MONTH)) %>%
  count(EVENT_TYPE, MONTH, MONTH_NUM) %>%
  mutate(EVENT_TYPE = factor(EVENT_TYPE, levels = top_events))

p4 <- ggplot(monthly_dist, aes(x = MONTH, y = n, fill = EVENT_TYPE)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ EVENT_TYPE, scales = "free_y", ncol = 2) +
  labs(title = "Monthly Distribution of Top 10 Event Types (2025)",
       x = "Month", y = "Number of Events",
       caption = "Source: NOAA Storm Events Database, 2025") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold"))
print(p4)

# --- 9. Question 4: Economic Damage by State -----------------
damage_by_state <- joined_data %>%
  filter(!is.na(STATE)) %>%
  group_by(STATE) %>%
  summarise(
    Total_Property_Damage = sum(DAMAGE_PROPERTY_NUM, na.rm = TRUE),
    Total_Crop_Damage     = sum(DAMAGE_CROPS_NUM,    na.rm = TRUE),
    Total_Damage          = Total_Property_Damage + Total_Crop_Damage,
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Damage)) %>%
  slice_head(n = 15) %>%
  pivot_longer(cols = c(Total_Property_Damage, Total_Crop_Damage),
               names_to = "Damage_Type", values_to = "Amount") %>%
  mutate(STATE = fct_reorder(STATE, Amount, .fun = sum))

p5 <- ggplot(damage_by_state, aes(x = Amount, y = STATE, fill = Damage_Type)) +
  geom_col(position = "stack") +
  scale_x_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
  scale_fill_manual(values = c("Total_Property_Damage" = "#2980b9",
                                "Total_Crop_Damage"     = "#27ae60"),
                    labels = c("Property Damage", "Crop Damage")) +
  labs(title = "Top 15 States by Economic Damage from Storm Events (2025)",
       x = "Total Damage (Millions USD)", y = "State", fill = "Damage Type",
       caption = "Source: NOAA Storm Events Database, 2025") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
print(p5)

message("Analysis complete. All 5 figures generated.")
