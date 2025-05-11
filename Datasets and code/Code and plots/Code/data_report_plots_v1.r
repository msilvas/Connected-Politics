# Importing tidyverse  
library(tidyverse)

# Set working directory

setwd("/Users/mass/Documents/Masters/Courses/Connected Politics/Github repository/Content/Datasets and code")

#Loading base dataset


df <- read.csv("Datasets/Final merged dataset/df_merged.csv")

library(dplyr)
df <- select(df, -sdg_index_score, -goal1, -goal2, -goal3, -goal4, -goal5, -goal6,
             -goal7, -goal8, -goal9, -goal10, -goal11, -goal12,
             -goal13, -goal14, -goal15, -goal16, -goal17)

sdg_index <- read.csv("sdg_index_2015_2023.csv")
sdg_index <- sdg_index |>
    select(-Country, -id, - SDG.Index.Score)

df <- left_join(df, sdg_index, by = "year")

library(dplyr)

sdg_index <- sdg_index |>
   rename(country_abb = id)

   

# Transform all column names of df to lowercase
colnames(df) <- tolower(colnames(df))

# Merging the dataset with the subregions
subregions <- read.csv("Datasets/Aux datasets/region_dataset.csv")
head(subregions)


# Multiply each sdg column to make it a percentage over total mentions in a speech
df <- df |>
    mutate(across(starts_with("sdg_"), ~ . * 100))


df <- df |>
  left_join(
    subregions |>
      select(speech_id, sub_region, inter_region, continent),
    by = c("id" = "speech_id")
  )

#### Create a column with the max sdg mentioned
df <- df |>
    mutate(max_sdg_mentioned = pmax(sdg_0, sdg_1, sdg_2, sdg_3, sdg_4, sdg_5, sdg_6, sdg_7, sdg_8, sdg_9, sdg_10, sdg_11, sdg_12, sdg_13, sdg_14, sdg_15, sdg_16, na.rm = TRUE))

    # Create a column with the column name of the max value
    df <- df |>
        mutate(max_sdg_column = case_when(
            max_sdg_mentioned == sdg_0 ~ "sdg_0",
            max_sdg_mentioned == sdg_1 ~ "sdg_1",
            max_sdg_mentioned == sdg_2 ~ "sdg_2",
            max_sdg_mentioned == sdg_3 ~ "sdg_3",
            max_sdg_mentioned == sdg_4 ~ "sdg_4",
            max_sdg_mentioned == sdg_5 ~ "sdg_5",
            max_sdg_mentioned == sdg_6 ~ "sdg_6",
            max_sdg_mentioned == sdg_7 ~ "sdg_7",
            max_sdg_mentioned == sdg_8 ~ "sdg_8",
            max_sdg_mentioned == sdg_9 ~ "sdg_9",
            max_sdg_mentioned == sdg_10 ~ "sdg_10",
            max_sdg_mentioned == sdg_11 ~ "sdg_11",
            max_sdg_mentioned == sdg_12 ~ "sdg_12",
            max_sdg_mentioned == sdg_13 ~ "sdg_13",
            max_sdg_mentioned == sdg_14 ~ "sdg_14",
            max_sdg_mentioned == sdg_15 ~ "sdg_15",
            max_sdg_mentioned == sdg_16 ~ "sdg_16",
            TRUE ~ NA_character_
        ))

#### Random sample for validation
set.seed(139390)  # Ensure reproducibility
sampled_speeches <- df |>
    group_by(max_sdg_column) |>
    sample_frac(size = 150 / nrow(df)) |>
    ungroup() |>
    select(id, max_sdg_column)

#### 
# Assign random values to the validation column
sampled_speeches <- sampled_speeches |>
    mutate(validation = sample(c("Alp", "Mo", "Kun"), n(), replace = TRUE, prob = c(0.4, 0.2, 0.4)))

sampled_speeches

# Save the sampled_speeches as a CSV file
write_csv(sampled_speeches, "/Users/mass/Documents/Masters/Courses/Connected Politics/Github repository/Content/Datasets and code/Datasets/speech_validation_sample.csv")

##### Importing and merging additional information

# GDP per capita dataset - Wold Bank
ppp = read_csv("Datasets/Aux datasets/gpd_pc_0823.csv")
head(ppp)

# From wide to long

ppp_long <- ppp |>
    select(-`Series Name`, -`Series Code`, -"country_name") |>
    pivot_longer(cols = -"country_abb", names_to = "year", values_to = "PPP") |>
    mutate(year = substr(year, 1, 4))

# Ffill missing values
library(zoo)

ppp_long <- ppp_long |>
  group_by(country_abb) |>
  mutate(PPP = ifelse(PPP == "..", NA, as.numeric(PPP))) |>
  mutate(PPP = zoo::na.locf(PPP, na.rm = FALSE, fromLast = TRUE)) |>
  mutate(PPP = zoo::na.locf(PPP, na.rm = FALSE)) |>
  ungroup() |>
  mutate(PPP = round(PPP, 2, na.rm = TRUE))

# Create a column named country_abb with the first three characters of Id
    df <- df |>
        mutate(country_abb = substr(id, 1, 3))

# Auxiliar id to merge datasets and add PPP per year/country to the main dataset
ppp_long$aux_id <- paste(ppp_long$country_abb, ppp_long$year, sep = "_")

# Auxiliar id to merge datasets and add PPP per year/country to the main dataset
df$aux_id <- paste(df$country_abb, df$year, sep = "_")

# Left join with df
df <- df |>
    left_join(ppp_long |> select (aux_id, PPP), by = "aux_id")


# Importing the dataset with the SDG index

sdg_index <- read_csv("/Users/mass/Documents/Masters/Courses/Connected Politics/Github repository/Content/Datasets and code/Datasets/Aux datasets/sdg_index_2000_2023.csv")
head(sdg_index)


# Replace NA values with nulls and ensure all numeric columns are numeric
sdg_index <- sdg_index |>
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), NA, .))) |>
    mutate(across(where(is.character), ~ ifelse(is.na(.), NA, .)))

# Auxiliar id to merge datasets and add SDG scores per year/country to the main dataset
sdg_index$aux_id <- paste(sdg_index$id, sdg_index$year, sep = "_")

# Left join with df
df <- df |>
    left_join(sdg_index |> select (-id, -country, -year), by = "aux_id")


# Rows without SDG index score 
na_rows <- df |>
    filter(across(goal1:goal17, ~ is.na(.))) |>
    select(id, goal1:goal17)

na_rows

# Replace all non numeric values in numeric columns with NA

df <- df |>
    mutate(across(where(is.numeric), ~ ifelse(!is.na(as.numeric(.)), as.numeric(.), NA)))
    numeric_columns <- c("sdg_0", "sdg_1", "sdg_2", "sdg_3", "sdg_4", "sdg_5", "sdg_6", "sdg_7", "sdg_8", "sdg_9", "sdg_10", "sdg_11", "sdg_12",
     "sdg_13", "sdg_14", "sdg_15", "sdg_16", "ppp", "sdg_index_score", "goal1", "goal2", "goal3", "goal4", "goal5", "goal6",
     "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal14", "goal15", "goal16", "goal17")

colnames(df) <- tolower(colnames(df))


df <- df |>
    mutate(across(all_of(numeric_columns), ~ ifelse(!is.na(as.numeric(.)), as.numeric(.), NA))) |>
    select(-aux_id)

##### Creation of plots
library(dplyr)

## SDG Mentions before and after 2015
# Grouping the SDGs
df <- df |>
    mutate(
        no_sdg_related = sdg_0,
        reducing_ineq = sdg_1 + sdg_2 + sdg_10,
        acc_safe_cond = sdg_3 + sdg_6 + sdg_7,
        sust_growth = sdg_8 + sdg_9 + sdg_16,
        education = sdg_4 + sdg_5,
        sust_partner = sdg_11 + sdg_12,
        climate_action = sdg_13 + sdg_14 + sdg_15
    )

# Calculate the average of the specified columns by year
 
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(colorblindr)

avg_by_year <- df |>
    select(year, no_sdg_related, reducing_ineq, acc_safe_cond, sust_growth, education, sust_partner, climate_action) |>
    group_by(year) |>
    summarise(across(everything(), mean, na.rm = TRUE))

# Pivot longer for plotting

avg_by_year_long <- avg_by_year |>
    pivot_longer(cols = -year, names_to = "category", values_to = "average")

# Reorder the SDG category

avg_by_year_long$category <- factor(avg_by_year_long$category,
    levels = c("no_sdg_related", "sust_growth", "climate_action", "reducing_ineq", "education", 
               "acc_safe_cond", "sust_partner"
               ))


#### Creating the linepolot

plot1 <- ggplot(avg_by_year_long, aes(x = year, y = average, color = category, shape = category)) +
    geom_line(size = 1.4) +  # Increased line size for accessibility
    geom_point(size = 3) +   # Increased point size for accessibility
    geom_vline(xintercept = 2015, linetype = "dashed", color = "black") +
    annotate("text", x = 2016.2, y = max(avg_by_year_long$average) / 3,
             label = "SDG implementation (2015)", vjust = -1, color = "black", fontface = "bold") +
    labs(
        title = "Figure 1. Trends in SDG Topic Mentions in UN General Debate Speeches between 2008 and 2023\n",
        x = "Year\n",
        y = "Prevalence of SDG Topic (%)\n",  # ✅ Teacher suggestion
        color = "SDG Category",
        shape = "SDG Category"
    ) +
    scale_color_manual(
        values = c( 
            "no_sdg_related" = "#000000",
            "acc_safe_cond" = "#0072B2",
            "reducing_ineq" = "#D55E00",
            "climate_action" = "#009E73",
            "education" = "#CC79A7",
            "sust_partner" = "#56B4E9",
            "sust_growth" = "#E69F00"
        ),
        labels = c(
            "no_sdg_related" = "No SDG\nRelated Topics",

            "acc_safe_cond" = paste0(
                "Access to Safe\nConditions\n",
                "(SDG 3: Good Health & Well-being,\n",
                " SDG 6: Clean Water & Sanitation,\n",
                " SDG 7: Affordable & Clean Energy)"
            ),

            "reducing_ineq" = paste0(
                "Reducing Overall\nInequality\n",
                "(SDG 1: No Poverty,\n",
                " SDG 2: Zero Hunger,\n",
                " SDG 10: Reduced Inequalities)"
            ),

            "climate_action" = paste0(
                "Holistic Climate\nAction\n",
                "(SDG 13: Climate Action,\n",
                " SDG 14: Life Below Water,\n",
                " SDG 15: Life on Land)"
            ),

            "education" = paste0(
                "Equality Through\nEducation\n",
                "(SDG 4: Quality Education,\n",
                " SDG 5: Gender Equality)"
            ),

            "sust_partner" = paste0(
                "Sustainable\nPartnerships\n",
                "(SDG 11: Sustainable Cities & Communities,\n",
                " SDG 12: Responsible Consumption & Production)"
            ),

            "sust_growth" = paste0(
                "Sustainable\nGrowth\n",
                "(SDG 8: Decent Work & Economic Growth,\n",
                " SDG 9: Industry, Innovation and Infrastructure,\n",
                " SDG 16: Peace, Justice & Strong Institutions)"
            )
        )
    ) +
    scale_shape_manual(
        values = c(
            "no_sdg_related" = 15,  # square
            "acc_safe_cond" = 16,   # circle
            "reducing_ineq" = 17,   # triangle
            "climate_action" = 18,  # diamond
            "education" = 3,        # plus
            "sust_partner" = 7,     # cross
            "sust_growth" = 8       # asterisk
        ),
        labels = c(
            "no_sdg_related" = "No SDG\nRelated Topics",

            "acc_safe_cond" = paste0(
                "Access to Safe\nConditions\n",
                "(SDG 3: Good Health & Well-being,\n",
                " SDG 6: Clean Water & Sanitation,\n",
                " SDG 7: Affordable & Clean Energy)"
            ),

            "reducing_ineq" = paste0(
                "Reducing Overall\nInequality\n",
                "(SDG 1: No Poverty,\n",
                " SDG 2: Zero Hunger,\n",
                " SDG 10: Reduced Inequalities)"
            ),

            "climate_action" = paste0(
                "Holistic Climate\nAction\n",
                "(SDG 13: Climate Action,\n",
                " SDG 14: Life Below Water,\n",
                " SDG 15: Life on Land)"
            ),

            "education" = paste0(
                "Equality Through\nEducation\n",
                "(SDG 4: Quality Education,\n",
                " SDG 5: Gender Equality)"
            ),

            "sust_partner" = paste0(
                "Sustainable\nPartnerships\n",
                "(SDG 11: Sustainable Cities & Communities,\n",
                " SDG 12: Responsible Consumption & Production)"
            ),

            "sust_growth" = paste0(
                "Sustainable\nGrowth\n",
                "(SDG 8: Decent Work & Economic Growth,\n",
                " SDG 9: Industry, Innovation and Infrastructure,\n",
                " SDG 16: Peace, Justice & Strong Institutions)"
            )
        )
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        axis.title = element_text(face = "bold", size = 18),
        axis.text = element_text(size = 16),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        aspect.ratio = 0.6
    )


plot1

cvd_grid(plot1) # Test for Color Vision Deficiency

# Saving the plot: 

ggsave("Figure1.png", plot = plot1, , dpi = 600, width = 19, height = 13, bg = "white")
ggsave("Figure1.pdf", plot = plot1, , dpi = 600, width = 19, height = 13, bg = "white")



## Average by category and subregion since 2015

# Calculate the average mention after 2015 by subregion and category

df_after_2015_long <- df |>
    filter(year >= 2015) |>
    select(sub_region, year, reducing_ineq, acc_safe_cond, -sust_growth, education, sust_partner, climate_action) |>
    pivot_longer(cols = -c(sub_region, year), names_to = "category", values_to = "value")

avg_mentions_percent <- df_after_2015_long |>
    group_by(sub_region, category) |>
    summarise(average_value = mean(value, na.rm = TRUE)) |>
    ungroup() |>
    group_by(sub_region) |>
    mutate(percent = average_value / sum(average_value) * 100) |>
    ungroup()


# plot2 (barplot): Mention Topic over time by sub-regions

library(ggplot2)

plot2 <- avg_mentions_percent |>
  filter(!is.na(sub_region)) |>
  ggplot(aes(x = sub_region, y = percent, fill = category)) +
  
  # Stacked bars
  geom_bar(stat = "identity", position = "fill") +
  
  # Percentage labels
  geom_text(
    aes(label = sprintf("%.0f%%", percent)),
    position = position_fill(vjust = 0.5),
    size = 5,
    color = "white",
    fontface = "bold"
  ) +
  
  # Titles and subtitle
  labs(
    title = "Average SDG Category Mentions by Subregion",
    subtitle = "UNGD Speeches (2015 - 2023)\nNote: Sustainable Growth and Non-SDG Topics Excluded",
    x = "",
    y = "Percentage of Topic Mentions",
    fill = "SDG Category"
  ) +
  
  # Custom fill colors and legend labels
  scale_fill_manual(
    values = c( 
      "acc_safe_cond"   = "#0072B2",
      "reducing_ineq"   = "#D55E00",
      "climate_action"  = "#009E73",
      "education"       = "#CC79A7",
      "sust_partner"    = "#56B4E9"
    ),
    labels = c(    
      "acc_safe_cond" = paste0(
        "Access to Safe\nConditions\n",
        "(SDG 3: Good Health & Well-being,\n",
        " SDG 6: Clean Water & Sanitation,\n",
        " SDG 7: Affordable & Clean Energy)"
      ),
      "reducing_ineq" = paste0(
        "Reducing Overall\nInequality\n",
        "(SDG 1: No Poverty,\n",
        " SDG 2: Zero Hunger,\n",
        " SDG 10: Reduced Inequalities)"
      ),
      "climate_action" = paste0(
        "Holistic Climate\nAction\n",
        "(SDG 13: Climate Action,\n",
        " SDG 14: Life Below Water,\n",
        " SDG 15: Life on Land)"
      ),
      "education" = paste0(
        "Equality Through\nEducation\n",
        "(SDG 4: Quality Education,\n",
        " SDG 5: Gender Equality)"
      ),
      "sust_partner" = paste0(
        "Sustainable\nPartnerships\n",
        "(SDG 11: Sustainable Cities & Communities,\n",
        " SDG 12: Responsible Consumption & Production)"
      )
    )
  ) +

  # Percent axis
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  
  # Flip coordinates
  coord_flip(clip = "off") +
  
  # Theme customization
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(
      size = 26, face = "bold", hjust = 0.5, margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = 18, hjust = 0.5, margin = margin(b = 15)
    ),
    axis.title.y = element_text(
      size = 20, face = "bold", margin = margin(r = 15)
    ),
    axis.title.x = element_text(
      size = 20, face = "bold", margin = margin(t = 10)
    ),
    axis.text = element_text(size = 16),
    axis.text.y = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    legend.key.width = unit(2.5, "cm"),
    legend.key.height = unit(0.8, "cm"),
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.spacing.x = unit(1.5, "cm"),
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  
  # Spread legend across multiple rows
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE,
      title.position = "top",
      label.position = "right"
    )
  )

plot2

cvd_grid(plot2) # Test for Color Vision Deficiency

# Saving the plot

ggsave("plot2_barplot.pdf", plot = plot2, device = "pdf", dpi = 600, width = 25, height = 17, bg="white")
ggsave("plot2_barplot.png", plot = plot2, device = "png", dpi = 600, width = 25, height = 17, bg="white")



# plot2 (geom_line): Mention Topic over time by sub-regions

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# Prepare the data (starting from df)

df_after_2015_long <- df |>
  filter(year >= 2015) |>
  select(sub_region, year, reducing_ineq, acc_safe_cond, education, sust_partner, climate_action) |>
  pivot_longer(cols = -c(sub_region, year), names_to = "category", values_to = "value") |>
  filter(!is.na(sub_region))

# Calculate percentage mentions per year and subregion
percent_mentions <- df_after_2015_long |>
  group_by(sub_region, year, category) |>
  summarise(avg_value = mean(value, na.rm = TRUE), .groups = "drop_last") |>
  mutate(percent = avg_value / sum(avg_value, na.rm = TRUE) * 100) |>
  ungroup()

# Create line + point plot with facet_wrap by subregion

plot2_geom_line <- percent_mentions |>
  ggplot(aes(x = year, y = percent, color = category)) +
  
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  
  facet_wrap(~ sub_region, ncol = 4, scales = "free_y") +
  
  # Titles and labels
  labs(
    title = "Average SDG Category Mentions by Subregion",
    subtitle = "UNGD Speeches (2015 - 2023)\nNote: Sustainable Growth and Non-SDG Topics Excluded",
    x = "",
    y = "Percentage of Topic Mentions",
    color = "SDG Category"
  ) +

  # Correct color scale for line plot
  scale_color_manual(
    values = c( 
      "acc_safe_cond"   = "#0072B2",
      "reducing_ineq"   = "#D55E00",
      "climate_action"  = "#009E73",
      "education"       = "#CC79A7",
      "sust_partner"    = "#56B4E9"
    ),
    labels = c(    
      "acc_safe_cond" = paste0(
        "Access to Safe\nConditions\n",
        "(SDG 3: Good Health & Well-being,\n",
        " SDG 6: Clean Water & Sanitation,\n",
        " SDG 7: Affordable & Clean Energy)"
      ),
      "reducing_ineq" = paste0(
        "Reducing Overall\nInequality\n",
        "(SDG 1: No Poverty,\n",
        " SDG 2: Zero Hunger,\n",
        " SDG 10: Reduced Inequalities)"
      ),
      "climate_action" = paste0(
        "Holistic Climate\nAction\n",
        "(SDG 13: Climate Action,\n",
        " SDG 14: Life Below Water,\n",
        " SDG 15: Life on Land)"
      ),
      "education" = paste0(
        "Equality Through\nEducation\n",
        "(SDG 4: Quality Education,\n",
        " SDG 5: Gender Equality)"
      ),
      "sust_partner" = paste0(
        "Sustainable\nPartnerships\n",
        "(SDG 11: Sustainable Cities & Communities,\n",
        " SDG 12: Responsible Consumption & Production)"
      )
    )
  ) +

  # Theme styling
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(size = 26, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 18, hjust = 0.5, margin = margin(b = 15)),
    axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 15)),
    axis.text = element_text(size = 13),
    strip.text = element_text(size = 15, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    legend.key.width = unit(2.5, "cm"),
    legend.key.height = unit(0.8, "cm"),
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.spacing.x = unit(1.5, "cm"),
    panel.spacing = unit(1.5, "lines"),
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  
guides(
  color = guide_legend(
    nrow = 2,
    byrow = TRUE,
    title.position = "top",
    label.position = "right",
    label.hjust = 0,         # 👈 Align labels to the left
    title.hjust = 0          # 👈 Align title to the left too
  )
)


# Show the plot
plot2_geom_line

cvd_grid(plot2_geom_line) # Test for Color Vision Deficiency


# Saving the plot:
ggsave("plot2.pdf", plot = plot2_geom_line, dpi = 600, width = 25, height = 15, bg = "white")
ggsave("plot2.png", plot = plot2_geom_line, dpi = 600, width = 25, height = 15, bg = "white")

# Plot3: GDP VS Topic Mentios in 2023

library(dplyr)


df_2023 <- df |>
  filter(year == 2023, !is.na(sub_region)) |>
  mutate(grouped_sdg = case_when(
    max_sdg_column %in% c("sdg_8", "sdg_9", "sdg_16") ~ "Sustainable Growth",
    max_sdg_column %in% c("sdg_1", "sdg_2", "sdg_10") ~ "Reducing Overall Inequality",
    max_sdg_column %in% c("sdg_3", "sdg_6", "sdg_7") ~ "Access to Safe Conditions",
    max_sdg_column %in% c("sdg_4", "sdg_5") ~ "Equality Through Education",
    max_sdg_column %in% c("sdg_11", "sdg_12") ~ "Sustainable Partnerships",
    max_sdg_column %in% c("sdg_13", "sdg_14", "sdg_15") ~ "Holistic Climate Action",
    TRUE ~ NA_character_
  )) |>
  filter(!is.na(grouped_sdg)) # remove ungrouped SDGs if any


df_2023$grouped_sdg <- factor(df_2023$grouped_sdg,
  levels = c(
    "Sustainable Growth",
    "Holistic Climate Action",
    "Reducing Overall Inequality",
    "Equality Through Education",
    "Access to Safe Conditions",
    "Sustainable Partnerships"
  )
)

grouped_sdg_labels <- c(
  "Sustainable Growth\n (SDG 8, 9 & 16)",
  "Holistic Climate Action\n (SDG 13, 14 & 15)",
  "Reducing Overall Inequality\n (SDG 1, 2 & 10)",
  "Equality Through Education\n (SDG 4 & 5)",
  "Access to Safe Conditions\n (SDG 3, 6 & 7)",
  "Sustainable Partnerships\n (SDG 11 & 12)"
)

df_2023$grouped_sdg <- factor(df_2023$grouped_sdg,
  levels = c(
    "Sustainable Growth",
    "Holistic Climate Action",
    "Reducing Overall Inequality",
    "Equality Through Education",
    "Access to Safe Conditions",
    "Sustainable Partnerships"
  ),
  labels = grouped_sdg_labels  # 👈 label them with SDG numbers inside
)

# Import ggrepel
library(ggrepel)
library(ggplot2)


plot3 <- ggplot(df_2023, aes(x = ppp, y = grouped_sdg, fill = continent, shape = continent)) +
  geom_jitter(
    width = 0, height = 0.2, size = 3,
    color = "black", stroke = 0.4, alpha = 0.9
  ) +
  
  scale_x_log10(labels = scales::comma) +
  
  # Set continent colors
  scale_fill_manual(values = c(
    "Africa" = "#4361EE",
    "Americas" = "#FF9F1C",
    "Asia" = "#F95700",
    "Europe" = "#5E60CE",
    "Oceania" = "#D00060"
  )) +

  # Set custom shapes per continent
  scale_shape_manual(values = c(
    "Africa" = 21, "Americas" = 22, "Asia" = 23,
    "Europe" = 24, "Oceania" = 25
  )) +
  labs(
    title = "Most Mentioned SDG Topic vs GDP per capita (PPP) in 2023",
    subtitle = "Note: PPP is displayed on a log10 scale",
    x = "GDP per capita (Purchase Power Parity)",
    y = "Most Mentioned SDG Topic",
    fill = "Continent",
    shape = "Continent",
  ) +
theme_minimal() +
theme(
  axis.text = element_text(size = 16),
  axis.text.y = element_text(size = 16, margin = margin(r = 10)),   # Reduced margin
  axis.title = element_text(size = 16, face = "bold"),
  axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 15)), # Less margin
  
  legend.text = element_text(size = 16),
  legend.title = element_text(size = 16),
  panel.grid.major.y = element_line(color = "grey", size = 0.4),
  
  plot.title = element_text(
    hjust = 0.5, face = "bold", size = 18,
    margin = margin(b = 10)  # 👈 Adds space *below* title
  ),
  
  plot.subtitle = element_text(
    hjust = 0.5, size = 16,
    margin = margin(b = 15)  # 👈 Adds more space *below* subtitle
  ),
  
  legend.position = "bottom",
  legend.direction = "horizontal",
  
  plot.margin = margin(20, 20, 20, 20)  # 👈 Balanced margins around the whole plot
)

plot3

cvd_grid(plot3) # Test for Color Vision Deficiency

ggsave("plot3.pdf", plot = plot3, dpi = 600, width = 15, height = 10, bg ="white")
ggsave("plot3.png", plot = plot3, dpi = 600, width = 15, height = 10, bg= "white")

# Plot 4: SDG Index score vs PPP in 2023

# Filter the data for the year 2023 and remove NA sub_region
df_2023 <- df |>
    filter(year == 2023, !is.na(sub_region))

# Select only 3 highest and 3 lowest countries by SDG Index Score
label_data <- bind_rows(
  df_2023 |> arrange(desc(sdg_index_score)) |> slice_head(n = 3),
  df_2023 |> arrange(sdg_index_score) |> slice_head(n = 3)
) |> distinct(country_abb, .keep_all = TRUE)

library(ggrepel)

plot4 <- ggplot(df_2023,
              aes(x = ppp, y = sdg_index_score, color = continent, shape = continent)) +
                geom_point(size = 4, alpha = 0.9, stroke = 1.5) +

  # Horizontal reference line at 75
  geom_hline(yintercept = 75, linetype = "dashed", color = "black", size = 1) +

  # ✨ LOESS trend line with confidence interval
  geom_smooth(aes(group = 1), method = "loess", se = TRUE, 
              color = "black", fill = "gray70", size = 1.2, linetype = "solid", alpha = 0.3) +

  # Labels for selected countries
geom_label_repel(
    data = label_data,
    aes(label = country_abb),
    size = 5,
    box.padding = 0.3,
    point.padding = 0.4,
    color = "black"
  ) +

  labs(
    title = "Figure 4. GDP per capita PPP and SDG Index Score in 2023",
    x = "\nGDP per capita (log scale, PPP)\n",
    y = "\nSDG Index Score (0 - 100)\n",
    color = "Continent",
    shape = "Continent") + 

  # Custom color palette
  scale_color_manual(values = c(
    "Africa" = "#648FFF",
    "Americas" = "#FE6100",
    "Asia" = "#785EF0",
    "Europe" = "#FFB000",
    "Oceania" = "#DC267F"
  )) +

  # Logarithmic x-axis
  scale_x_log10(
    labels = comma_format(),
    breaks = c(1000, 3000, 10000, 30000, 100000)
  ) +

  # Theme tweaks
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 12),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.text = element_text(size = 12, margin = margin(r = 12)),
    legend.title = element_text(size = 14),
    legend.spacing.y = unit(0.5, 'cm'),
    legend.key.width = unit(1, "cm"),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    aspect.ratio = 0.7
  )

plot4

cvd_grid(plot4) # Test for Color Vision Deficiency

ggsave("plot4.pdf", plot = plot4,
       device = "pdf", width = 15, height = 10, dpi = 300, bg= "white")
ggsave("plot4.png", plot = plot4,
       device = "png", width = 15, height = 10, dpi = 300, bg = "white")


# Gap Analysis 

# Load necessary libraries

library(dplyr)
library(tidyr)
library(ggplot2)

# === Step 1: Prepare the mentions and implementation columns ===
mentions_cols <- paste0("sdg_", 1:16)
implementation_cols <- paste0("goal", 1:16)

# === Step 2: Filter data for 2015-2023 ===
df_filtered <- df %>%
  filter(year >= 2015 & year <= 2023)

# === Step 3: Standardize (z-score) mentions and implementation scores ===
mentions_scaled <- as.data.frame(scale(df_filtered[, mentions_cols]))
implementation_scaled <- as.data.frame(scale(df_filtered[, implementation_cols]))

# Make sure implementation columns match mentions for subtraction
colnames(implementation_scaled) <- mentions_cols

# === Step 4: Calculate the gap ===
gap_df <- mentions_scaled - implementation_scaled
gap_df$year <- df_filtered$year

# === Step 5: Reshape to long format ===
gap_long <- gap_df %>%
  pivot_longer(cols = starts_with("sdg_"), names_to = "SDG", values_to = "gap")

# === Step 6: Map SDGs to Topics ===
sdg_to_topic <- c(
  "sdg_3" = "acc_safe_cond", "sdg_6" = "acc_safe_cond", "sdg_7" = "acc_safe_cond",
  "sdg_1" = "reducing_ineq", "sdg_2" = "reducing_ineq", "sdg_10" = "reducing_ineq",
  "sdg_13" = "climate_action", "sdg_14" = "climate_action", "sdg_15" = "climate_action",
  "sdg_4" = "education", "sdg_5" = "education",
  "sdg_11" = "sust_partner", "sdg_12" = "sust_partner",
  "sdg_8" = "sust_growth", "sdg_9" = "sust_growth", "sdg_16" = "sust_growth"
)

gap_long <- gap_long %>%
  mutate(topic = recode(SDG, !!!sdg_to_topic))

# === Step 7: Summarize gap per topic (mean only) ===
gap_summary <- gap_long %>%
  group_by(topic) %>%
  summarize(mean_gap = mean(gap, na.rm = TRUE), .groups = "drop")

# === Step 8: Create readable topic labels ===
topic_labels_short <- c(
  "acc_safe_cond" = "Access to Safe Condition\n(SDG 3, 6 & 7)",
  "reducing_ineq" = "Reducing Inequality\n(SDG 1, 2 & 10)",
  "climate_action" = "Holistic Climate Action\n(SDG 13, 14 & 15)",
  "education" = "Equality Through Education\n(SDG 4 & 5)",
  "sust_partner" = "Sustainable Partnerships\n(SDG 11 & 12)",
  "sust_growth" = "Sustainable Growth\n(SDG 8, 9 & 16)"
)

gap_summary <- gap_summary %>%
  mutate(topic_label = recode(topic, !!!topic_labels_short))

# === Step 9: Order topics by mean gap ===
gap_summary <- gap_summary %>%
  arrange(mean_gap) %>%
  mutate(topic_label = factor(topic_label, levels = topic_label))

# === Step 10: Build simple red/blue barplot ===

max_gap <- max(abs(gap_summary$mean_gap)) * 1.2

gap_barplot <- ggplot(gap_summary, aes(x = mean_gap, y = topic_label, fill = mean_gap > 0)) +
  geom_col(width = 0.7, color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
  scale_fill_manual(
    values = c("TRUE" = "#EF5350", "FALSE" = "#42A5F5"),
    labels = c("FALSE" = "More Action", "TRUE" = "More Talk"),
    name = "Gap Direction"
  ) +
  scale_x_continuous(
    limits = c(-max_gap, max_gap),
    expand = c(0, 0)
  ) +
  labs(
    title = "Figure 5. Gap Between SDG Mentions and Implementation by Topic (2015 - 2023)",
    x = "Gap (Mentions - Implementation (Using Z-Score))",
    y = "SDG Topic"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(size = 19, face = "bold", hjust = 0.5, margin = margin(b = 5)),
    axis.title.x = element_text(size = 19, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 19, face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(color = "black", size = 16),
    axis.text.y = element_text(color = "black", size = 16),
    panel.grid.major.x = element_line(color = "lightgrey"),  
    panel.grid.major.y = element_line(color = "lightgrey"),  
    panel.grid.minor = element_blank(),                   
    plot.background = element_rect(fill = "white", color = NA),  
    panel.background = element_rect(fill = "white", color = NA), 
    legend.position = "bottom",
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    plot.margin = margin(20, 40, 20, 40)
  ) +
  coord_cartesian(clip = "off")

# Show and Save
print(gap_barplot)

ggsave("plot5_slides.pdf", plot = gap_barplot, width = 14, height = 8, bg = "white")
ggsave("plot5_slides.png", plot = gap_barplot, width = 14, height = 8, bg = "white")

# Panel Regression Analysis using lag: 

library(dplyr)

df_mentions <- df %>% 
  filter(year >= 2016 & year <= 2023)

df_implementation <- df %>% 
  filter(year >= 2015 & year <= 2022)

# Step 1: Shift the implementation data + rename columns before merging
df_implementation_shifted <- df_implementation %>%
  mutate(year = year + 1) %>%
  rename_with(~ paste0(., "_lagged"), -c(country_abb, year))
  # Add '_lagged' to all columns except country_abb and year

# Step 2: Clean merge
df_merged <- df_mentions %>%
  left_join(df_implementation_shifted, by = c("country_abb", "year"))

df_merged <- df_merged %>%
  mutate(
    reducing_ineq_lagged_mentions = sdg_1_lagged + sdg_2_lagged + sdg_10_lagged,
    acc_safe_cond_lagged_mentions = sdg_3_lagged + sdg_6_lagged + sdg_7_lagged,
    sust_growth_lagged_mentions = sdg_8_lagged + sdg_9_lagged + sdg_16_lagged,
    education_lagged_mentions = sdg_4_lagged + sdg_5_lagged,
    sust_partner_lagged_mentions = sdg_11_lagged + sdg_12_lagged,
    climate_action_lagged_mentions = sdg_13_lagged + sdg_14_lagged + sdg_15_lagged
  )

df_merged <- df_merged %>%
  mutate(
    reducing_ineq_lagged = goal1_lagged + goal2_lagged + goal10_lagged,
    acc_safe_cond_lagged = goal3_lagged + goal6_lagged + goal7_lagged,
    sust_growth_lagged = goal8_lagged + goal9_lagged + goal16_lagged,
    education_lagged = goal4_lagged + goal5_lagged,
    sust_partner_lagged = goal11_lagged + goal12_lagged,
    climate_action_lagged = goal13_lagged + goal14_lagged + goal15_lagged
  )
names(df_merged)

library(plm)
library(broom)
library(dplyr)

# Define topic names
topics <- c("reducing_ineq", "acc_safe_cond", "sust_growth", "education", "sust_partner", "climate_action")

# Convert to panel data
pdata <- pdata.frame(df_merged, index = c("country_abb", "year"))

# Initialize empty list to store results
results_list <- list()

# Loop over each topic
for (topic in topics) {
  
  # Build formula dynamically
  formula <- as.formula(paste0(topic, "_lagged ~ ", topic, "_lagged_mentions"))
  
  # Run fixed-effects model
  model <- plm(formula, data = pdata, model = "within")
  
  # Tidy the model output
  model_result <- tidy(model) %>%
    mutate(topic = topic)
  
  # Save
  results_list[[topic]] <- model_result
}


# Combine all results into a dataframe
final_results <- bind_rows(results_list)

# Create a summary table for all models
library(stargazer)

# Extract models for stargazer
models <- lapply(results_list, function(x) {
  plm(as.formula(paste0(x$topic, "_lagged ~ ", x$topic, "_lagged_mentions")), data = pdata, model = "within")
})

# Generate a stargazer table
stargazer(models,
          type = "text",
          title = "Panel Regression Results: Effect of SDG Mentions on Implementation",
          dep.var.labels = c("Implementation Scores"),
          covariate.labels = c("Mentions (Lagged)"),
          column.labels = topics,
          align = TRUE,
          no.space = TRUE,
          digits = 3,
          out = "model_summary_table.txt")


# Combine all results into a dataframe
final_results <- bind_rows(results_list)

# Show only the main effects (the lagged_mentions variables)
final_results_main <- final_results %>%
  filter(term != "(Intercept)") %>%
  arrange(desc(estimate))

# Print results
print(final_results_main)

# Step 1: Add significance column
final_results_main <- final_results_main %>%
  mutate(significance = ifelse(p.value < 0.05, "Significant", "Not Significant"))

# Step 2: Short labels for Y-axis
topic_labels_short <- c(
  "acc_safe_cond" = "Access to Safe Condition\n (SDG 3, 6 & 7)",
  "reducing_ineq" = "Reducing Inequality\n (SDG 1, 2 & 10)",
  "climate_action" = "Holistic Climate Action\n (SDG 13, 14 & 15)",
  "education" = "Equality Through Education\n (SDG 4 & 5)",
  "sust_partner" = "Sustainable Partnerships\n (SDG 11 & 12)",
  "sust_growth" = "Sustainable Growth\n (SDG 8, 9 & 16)"
)


# Step 4: Update the dataset with a new short name column
final_results_main <- final_results_main %>%
  mutate(topic_short = recode(topic, !!!topic_labels_short))

# Step 5: Build the plot
coefficient_plot <- ggplot(final_results_main, aes(x = estimate, y = reorder(topic_labels_short, estimate), color = significance)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = estimate - 1.96 * std.error,
                     xmax = estimate + 1.96 * std.error), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  
  scale_color_manual(
    values = c("Significant" = "red", "Not Significant" = "black"),
    labels = c("Significant" = "Significant", "Not Significant" = "Not Significant")
  ) +
  
  scale_y_discrete(labels = topic_labels_short) +  # Short names on Y-axis
  
  labs(
    title = "Figure 6. Effect of SDG Topic Mentions on Implementation Scores",
    subtitle = "Effects Panel Regression (Mentions Lagged by 1 Year, 2015-2023)",
    x = "Coefficient Estimate (with 95% Confidence Interval)",
    y = "SDG Topics",
    color = "Statistical Significance"
  ) + 
theme_minimal(base_size = 18) +
theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 18, hjust = 0.5, margin = margin(b = 15)),
    axis.title.x = element_text(color = "black", size = 18, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text (color = "black", size = 18, hjust = 0.5, face = "bold", margin = margin(r = 15)),
    axis.text.y = element_text(color = "black", size = 16),
    axis.text.x = element_text(color = "black", size = 16),
    strip.text = element_text(size = 15, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    legend.key.width = unit(2.5, "cm"),
    legend.key.height = unit(0.8, "cm"),
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.spacing.x = unit(1.5, "cm"),
    panel.spacing = unit(1.5, "lines"),
    plot.margin = margin(10, 30, 10, 30),
    aspect.ratio = 0.5    # ✅ Only one aspect ratio set!
)



coefficient_plot

ggsave("plot6.pdf", plot = coefficient_plot, width = 15, height = 10, bg= "white")
ggsave("plot6.png", plot = coefficient_plot, width = 15, height = 10, bg ="white")




#### Creatin Plot1 without SDG16: 

## SDG Mentions before and after 2015
# Grouping the SDGs
df <- df |>
    mutate(
        no_sdg_related = sdg_0,
        reducing_ineq = sdg_1 + sdg_2 + sdg_10,
        acc_safe_cond = sdg_3 + sdg_6 + sdg_7,
        sust_growth = sdg_8 + sdg_9,
        education = sdg_4 + sdg_5,
        sust_partner = sdg_11 + sdg_12,
        climate_action = sdg_13 + sdg_14 + sdg_15
    )

# Calculate the average of the specified columns by year
 
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

avg_by_year <- df |>
    select(year, no_sdg_related, reducing_ineq, acc_safe_cond, sust_growth, education, sust_partner, climate_action) |>
    group_by(year) |>
    summarise(across(everything(), mean, na.rm = TRUE))

# Pivot longer for plotting

avg_by_year_long <- avg_by_year |>
    pivot_longer(cols = -year, names_to = "category", values_to = "average")

# Reorder the SDG category

avg_by_year_long$category <- factor(avg_by_year_long$category,
    levels = c("no_sdg_related", "sust_growth", "climate_action", "reducing_ineq", "education", 
               "acc_safe_cond", "sust_partner"
               ))


#### Creating the linepolot
plot1_updated <- ggplot(avg_by_year_long, aes(x = year, y = average, color = category, shape = category)) +
    geom_line(size = 1.4) +  # Increased line size for accessibility
    geom_point(size = 3) +   # Increased point size for accessibility
    geom_vline(xintercept = 2015, linetype = "dashed", color = "black") +
    annotate("text", x = 2016.2, y = max(avg_by_year_long$average) + 5,
             label = "SDG implementation (2015)", vjust = -1, color = "black", fontface = "bold") +
    labs(
        title = "Trends in SDG Topic Mentions in UN General Debate Speeches between 2008 and 2023 (After Excluding SDG 16)",
        x = "Year\n",
        y = "Prevalence of SDG Topic (%)\n",  # ✅ Teacher suggestion
        color = "SDG Category",
        shape = "SDG Category"
    ) +
    scale_color_manual(
        values = c( 
            "no_sdg_related" = "#000000",
            "acc_safe_cond" = "#0072B2",
            "reducing_ineq" = "#D55E00",
            "climate_action" = "#009E73",
            "education" = "#CC79A7",
            "sust_partner" = "#56B4E9",
            "sust_growth" = "#E69F00"
        ),
        labels = c(
            "no_sdg_related" = "No SDG\nRelated Topics",

            "acc_safe_cond" = paste0(
                "Access to Safe\nConditions\n",
                "(SDG 3: Good Health & Well-being,\n",
                " SDG 6: Clean Water & Sanitation,\n",
                " SDG 7: Affordable & Clean Energy)"
            ),

            "reducing_ineq" = paste0(
                "Reducing Overall\nInequality\n",
                "(SDG 1: No Poverty,\n",
                " SDG 2: Zero Hunger,\n",
                " SDG 10: Reduced Inequalities)"
            ),

            "climate_action" = paste0(
                "Holistic Climate\nAction\n",
                "(SDG 13: Climate Action,\n",
                " SDG 14: Life Below Water,\n",
                " SDG 15: Life on Land)"
            ),

            "education" = paste0(
                "Equality Through\nEducation\n",
                "(SDG 4: Quality Education,\n",
                " SDG 5: Gender Equality)"
            ),

            "sust_partner" = paste0(
                "Sustainable\nPartnerships\n",
                "(SDG 11: Sustainable Cities & Communities,\n",
                " SDG 12: Responsible Consumption & Production)"
            ),

            "sust_growth" = paste0(
                "Sustainable\nGrowth\n",
                "(SDG 8: Decent Work & Economic Growth,\n",
                " SDG 9: Industry, Innovation and Infrastructure,\n",
                " SDG 16: Peace, Justice & Strong Institutions)"
            )
        )
    ) +
    scale_shape_manual(
        values = c(
            "no_sdg_related" = 15,  # square
            "acc_safe_cond" = 16,   # circle
            "reducing_ineq" = 17,   # triangle
            "climate_action" = 18,  # diamond
            "education" = 3,        # plus
            "sust_partner" = 7,     # cross
            "sust_growth" = 8       # asterisk
        ),
        labels = c(
            "no_sdg_related" = "No SDG\nRelated Topics",

            "acc_safe_cond" = paste0(
                "Access to Safe\nConditions\n",
                "(SDG 3: Good Health & Well-being,\n",
                " SDG 6: Clean Water & Sanitation,\n",
                " SDG 7: Affordable & Clean Energy)"
            ),

            "reducing_ineq" = paste0(
                "Reducing Overall\nInequality\n",
                "(SDG 1: No Poverty,\n",
                " SDG 2: Zero Hunger,\n",
                " SDG 10: Reduced Inequalities)"
            ),

            "climate_action" = paste0(
                "Holistic Climate\nAction\n",
                "(SDG 13: Climate Action,\n",
                " SDG 14: Life Below Water,\n",
                " SDG 15: Life on Land)"
            ),

            "education" = paste0(
                "Equality Through\nEducation\n",
                "(SDG 4: Quality Education,\n",
                " SDG 5: Gender Equality)"
            ),

            "sust_partner" = paste0(
                "Sustainable\nPartnerships\n",
                "(SDG 11: Sustainable Cities & Communities,\n",
                " SDG 12: Responsible Consumption & Production)"
            ),

            "sust_growth" = paste0(
                "Sustainable\nGrowth\n",
                "(SDG 8: Decent Work & Economic Growth,\n",
                " SDG 9: Industry, Innovation and Infrastructure,\n",
                " SDG 16: Peace, Justice & Strong Institutions)"
            )
        )
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        axis.title = element_text(face = "bold", size = 18),
        axis.text = element_text(size = 16),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        aspect.ratio = 0.6
    )
plot1_updated

ggsave("Figure1_witout16.png", plot = plot1_updated, , dpi = 600, width = 19, height = 13, bg = "white")
ggsave("Figure1_without16.pdf", plot = plot1_updated, , dpi = 600, width = 19, height = 13, bg = "white")




