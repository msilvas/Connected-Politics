# Importing tidyverse  
library(tidyverse)

#Loading base dataset
df = read_csv("/Users/mass/Documents/Masters/Courses/Connected Politics/Github repository/Content/Datasets and code/Datasets/Aux datasets/sdg_analysis_results_v2.csv")
head(df)


# Transform all column names of df to lowercase
colnames(df) <- tolower(colnames(df))

# Merging the dataset with the subregions
subregions = read_csv("/Users/mass/Documents/Masters/Courses/Connected Politics/Github repository/Content/Datasets and code/Datasets/Aux datasets/region_dataset.csv")
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
ppp = read_csv("/Users/mass/Documents/Masters/Courses/Connected Politics/Github repository/Content/Datasets and code/Datasets/Aux datasets/gpd_pc_0823.csv")
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
avg_by_year <- df |>
    select(year, no_sdg_related, reducing_ineq, acc_safe_cond, sust_growth, education, sust_partner, climate_action) |>
    group_by(year) |>
    summarise(across(everything(), mean, na.rm = TRUE))

# Pivot longer for plotting
avg_by_year_long <- avg_by_year |>
    pivot_longer(cols = -year, names_to = "category", values_to = "average")

#### Creating the linepolot
plot1 <- ggplot(avg_by_year_long, aes(x = year, y = average, color = category)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    geom_vline(xintercept = 2015, linetype = "dashed", color = "black") +
    annotate("text", x = 2017, y = max(avg_by_year_long$average) / 3,
                     label = "SDG implementation (2015)", vjust = -1, color = "black", fontface = "bold") +
    labs(
        title = "Average mention percentage of SDG topics in UN General Assembly Speeches by year\n",
        x = "Year\n",
        y = "Topic presence percentage\n",
        color = "SDG Category"
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
            "acc_safe_cond" = "Access to Safe\nConditions\n(SDG: 3, 6, 7)",
            "reducing_ineq" = "Reducing Overall\nInequality\n(SDG: 1, 2, 10)",
            "climate_action" = "Holistic Climate\nAction\n(SDG: 13, 14, 15)",
            "education" = "Equality Through\nEducation\n(SDG: 4, 5)",
            "sust_partner" = "Sustainable\nPartnerships\n(SDG: 11, 12)",
            "sust_growth" = "Sustainable\nGrowth\n(SDG: 8, 16)"
        )
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 17),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.text = element_text(size = 8)
    ) +
    theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
    theme(aspect.ratio = 0.6)

plot1



#Checking for color blindness
cvd_grid(plot1)


# Save the plot with high resolution
ggsave("plot1_avg_yearly_presence_un_speeches.pdf", plot = plot1,
    path = "/Users/mass/Documents/Masters/Courses/Connected Politics/Github repository/Content/Datasets and code/Code and plots/Plots/",
    device = "pdf", dpi = 600, width = 12, height = 8)

ggsave("plot1_avg_yearly_presence_un_speeches.png", plot = plot1,
    path = "/Users/mass/Documents/Masters/Courses/Connected Politics/Github repository/Content/Datasets and code/Code and plots/Plots/",
    device = "png", dpi = 600, width = 12, height = 8)


## Average by category and subregion since 2015
# Calculate the average mention after 2015 by subregion and category

df_after_2015_long <- df |>
    filter(year > 2015) |>
    select(sub_region, year, reducing_ineq, acc_safe_cond, -sust_growth, education, sust_partner, climate_action) |>
    pivot_longer(cols = -c(sub_region, year), names_to = "category", values_to = "value")

avg_mentions_percent <- df_after_2015_long |>
    group_by(sub_region, category) |>
    summarise(average_value = mean(value, na.rm = TRUE)) |>
    ungroup() |>
    group_by(sub_region) |>
    mutate(percent = average_value / sum(average_value) * 100) |>
    ungroup()

    # Plot the percent stacked barplot with percentage labels and inverted axes
    plot2 <- avg_mentions_percent |>
        filter(!is.na(sub_region)) |>
        ggplot(aes(x = sub_region, y = percent, fill = category)) +
        geom_bar(stat = "identity", position = "fill") +
        geom_text(aes(label = sprintf("%.0f%%", percent)), 
                  position = position_fill(vjust = 0.5), size = 4, color = "black",   
                  stroke = 0.2, stroke.color = "white", fontface = "bold") +
        labs(title = "Average percentage of SDG Category Mentions \n in UN General Assembly Speeches by Subregion (Since 2015)",
             subtitle = "After excluding Sustainable growth\n",
             x = "",
             y = "\nPercentage of topic mentions",
             fill = "SDG Category  ") +
        scale_fill_manual(
            values = c( 
                "acc_safe_cond" = "#0072B2", "reducing_ineq" = "#D55E00",
                "climate_action" = "#009E73","education" = "#CC79A7",
                "sust_partner" = "#56B4E9", "sust_growth" = "#E69F00"),
            labels = c(
                "acc_safe_cond" = "Access to Safe Conditions (SDG 3, SDG 6, SDG 7)",
                "reducing_ineq" = "Reducing Overall Inequality (SDG 1, SDG 2, SDG 10)",
                "climate_action" = "Holistic Climate Action (SDG 13, SDG 14, SDG 15)",
                "education" = "Equality Through Education (SDG 4, SDG 5)",
                "sust_partner" = "Sustainable Partnerships (SDG 11, SDG 12)",
                "sust_growth" = "Sustainable Growth (SDG 8, SDG 9, SDG 16)"
            )
        ) +
        scale_y_continuous(labels = scales::percent_format(scale = 100)) +
        theme_minimal() +
        theme(
            plot.title = element_text(vjust = 1, hjust = 0.65, face = "bold", size = 22),
            plot.subtitle = element_text(hjust = 0.5, size = 16),  # Added subtitle styling
            axis.title = element_text(face = "bold", size = 18),
            axis.text.y = element_text(size = 14, face = "bold"),  # Increased y-axis text size and bolded
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.box = "horizontal",
            legend.text = element_text(size = 14, margin = margin(r = 12)),  # Increased space between legend elements
            legend.title = element_text(size = 16),  # Increased legend title size
            legend.key.width = unit(1.5, "cm")  # Increased the width of the legend keys
        ) +
        guides(fill = guide_legend(nrow = 2)) +
        theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
        theme(aspect.ratio = 0.7) +
        coord_flip()

    # Save the plot with increased width
    plot2

    cvd_grid(plot2)

    ggsave("plot2_pct_sdg_cat_subregion.pdf", plot = plot2,
        path = "/Users/mass/Documents/Masters/Courses/Connected Politics/Github repository/Content/Datasets and code/Code and plots/Plots/",
        device = "pdf", dpi = 600, width = 25, height = 15)

    ggsave("plot2_pct_sdg_cat_subregion.png", plot = plot2,
        path = "/Users/mass/Documents/Masters/Courses/Connected Politics/Github repository/Content/Datasets and code/Code and plots/Plots/",
        device = "png", dpi = 600, width = 25, height = 15)

###### SDG Index score vs PPP

# Filter the data for the year 2023 and remove NA sub_region
df_2023 <- df |>
    filter(year == 2023, !is.na(sub_region))

# Import ggrepel

library(ggrepel)

# Create the scatterplot with a distinct color palette
plot3 <- ggplot(df_2023,
                aes(x = ppp, y = sdg_index_score, color = continent, shape = continent)) +
  geom_point(size = 4, alpha = 0.9, stroke = 1.5) +
  geom_hline(yintercept = 75, linetype = "dashed", color = "black", size = 1) +
  geom_label_repel(
    data = filter(df_2023, sdg_index_score > 85 | sdg_index_score < 50 | 
                  rank(-ppp) <= 10),
    aes(label = country_abb), size = 5, box.padding = 0.35, point.padding = 0.5,
    color = "black"
  ) +
  labs(title = "\nGDP per capita PPP and SDG Index Score in 2023",
       x = "\nGDP per capita (Purchase Power Parity)\n",
       y = "\nSDG Index Score\n",
       color = "Continent",
       shape = "Continent") +
  scale_color_manual(values = c("#648FFF", "#FE6100", "#785EF0", 
                               "#FFB000", "#DC267F")) +
  scale_x_continuous(labels = scales::comma) +  # Change x axis scale to avoid scientific notation
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 12),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.text = element_text(size = 12, margin = margin(r = 12)),  # Added margin to increase space between labels
    legend.title = element_text(size = 14),  # Increased legend title size
    legend.spacing.y = unit(0.5, 'cm'),  # Added spacing between legend items
    legend.key.width = unit(1, "cm")  # Increased the width of the legend keys
  ) +
  theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
  theme(aspect.ratio = 0.7)

plot3

cvd_grid(plot3)

# Save the plot with high resolution and increased width
ggsave("plot3_gdp_v_ppp.pdf", plot = plot3,
    path = "/Users/mass/Documents/Masters/Courses/Connected Politics/Github repository/Content/Datasets and code/Code and plots/Plots/",
    device = "pdf", dpi = 600, width = 20, height = 15)

ggsave("plot3_gdp_v_ppp.png", plot = plot3,
    path = "/Users/mass/Documents/Masters/Courses/Connected Politics/Github repository/Content/Datasets and code/Code and plots/Plots/",
    device = "png", dpi = 600, width = 20, height = 15)


#### Non sdg related speeches
# Filter the top 10 speeches with the highest sdg_0
top_10_speeches <- df |>
    arrange(desc(sdg_0)) |>
    slice(1:10)

# Create the bar plot
plot_top_10 <- ggplot(top_10_speeches, aes(x = reorder(id, sdg_0), y = sdg_0)) +
    geom_bar(stat = "identity", fill = "#0072B2") +
    labs(
        title = "Top 10 Speeches with Highest SDG_0 Mentions",
        x = "Speech ID",
        y = "SDG_0 Mentions"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)
    )

plot_top_10

# Save the plot with high resolution
ggsave("plot_top_10_speeches_sdg_0.pdf", plot = plot_top_10,
    path = "/Users/mass/Documents/Masters/Courses/Connected Politics/Github repository/Content/Datasets and code/Code and plots/Plots/",
    device = "pdf", dpi = 600, width = 12, height = 8)

ggsave("plot_top_10_speeches_sdg_0.png", plot = plot_top_10,
    path = "/Users/mass/Documents/Masters/Courses/Connected Politics/Github repository/Content/Datasets and code/Code and plots/Plots/",
    device = "png", dpi = 600, width = 12, height = 8)

#### Ridge plot for implementation distribution


df <- df |>
  mutate(
    reducing_ineq_eval = rowMeans(across(c(goal1, goal2, goal10)), na.rm = TRUE),
    acc_safe_cond_eval = rowMeans(across(c(goal3, goal6, goal7)), na.rm = TRUE),
    sust_growth_eval = rowMeans(across(c(goal8, goal9, goal16)), na.rm = TRUE),
    education_eval = rowMeans(across(c(goal4, goal5)), na.rm = TRUE),
    sust_partner_eval = rowMeans(across(c(goal11, goal12, goal17)), na.rm = TRUE),
    climate_action_eval = rowMeans(across(c(goal13, goal14, goal15)), na.rm = TRUE)
  )

# Pivot longer for plotting
df_long_eval <- df |>
    filter(year == 2023) |>
    select(sub_region, reducing_ineq_eval, acc_safe_cond_eval, sust_growth_eval, education_eval, sust_partner_eval, climate_action_eval) |>
    pivot_longer(cols = -sub_region, names_to = "category", values_to = "evaluation") |>
    filter(!is.na(sub_region) & !sub_region %in% c("Micronesia", "Polynesia"))



# Import necessary library
library(ggridges)
plot4 <- df_long_eval |>
    ggplot(aes(x = evaluation, y = category, fill = category)) +
    geom_density_ridges(alpha = 0.6, scale = 1) +
    geom_vline(xintercept = 70, linetype = "solid", color = "black", size = 1) +
    annotate("text", x = 82, y = 0.5, label = "70 score mark", color = "black", size = 4, vjust = -1) +
    labs(title = "SDG Evaluation Scores Distribution by Category in 2023 \n",
         x = "\nEvaluation Score\n",
         y = "") +
    scale_fill_manual(values = c(
        "acc_safe_cond_eval" = "#0072B2",
        "reducing_ineq_eval" = "#D55E00",
        "climate_action_eval" = "#009E73",
        "education_eval" = "#CC79A7",
        "sust_partner_eval" = "#56B4E9",
        "sust_growth_eval" = "#E69F00"
    )) +
    scale_y_discrete(labels = c(
        "acc_safe_cond_eval" = "Access to Safe Conditions\n(SDG 3, SDG 6, SDG 7)",
        "reducing_ineq_eval" = "Reducing Overall Inequality\n(SDG 1, SDG 2, SDG 10)",
        "climate_action_eval" = "Holistic Climate Action\n(SDG 13, SDG 14, SDG 15)",
        "education_eval" = "Equality Through Education\n(SDG 4, SDG 5)",
        "sust_partner_eval" = "Sustainable Partnerships\n(SDG 11, SDG 12)",
        "sust_growth_eval" = "Sustainable Growth\n(SDG 8, SDG 9, SDG 16)"
    )) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        axis.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 14),
        legend.position = "none",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.text = element_text(size = 12)
    ) +
    theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
    theme(aspect.ratio = 0.75)

plot4

cvd_grid(plot4)


ggsave("plot4_sdg_eval_dist.pdf", plot = plot4,
    path = "/Users/mass/Documents/Masters/Courses/Connected Politics/Github repository/Content/Datasets and code/Code and plots/Plots/",
    device = "pdf", dpi = 600, width = 12, height = 8)

ggsave("plot4_sdg_eval_dist.png", plot = plot4,
    path = "/Users/mass/Documents/Masters/Courses/Connected Politics/Github repository/Content/Datasets and code/Code and plots/Plots/",
    device = "png", dpi = 600, width = 12, height = 8)


#### Boxplot of implementation 


# Plot the boxplot faceted by subregion
plot5 <- ggplot(df_long_eval, aes(x = category, y = evaluation, fill = category)) +
    geom_boxplot() +
    geom_hline(yintercept = 75, linetype = "dashed", color = "black", size = 1, 
                         alpha = 0.7) +
    facet_wrap(~ sub_region, scales = "fixed") +
    labs(title = "Average SDG Evaluations Index by category and Subregion in 2023 \n",
             x = "",
             y = "Average Evaluation Score",
             fill = "SDG Category") +
    scale_fill_manual(values = c(
        "acc_safe_cond_eval" = "#0072B2",
        "reducing_ineq_eval" = "#D55E00",
        "climate_action_eval" = "#009E73",
        "education_eval" = "#CC79A7",
        "sust_partner_eval" = "#56B4E9",
        "sust_growth_eval" = "#E69F00"
    ),
    labels = c(
        "acc_safe_cond_eval" = "Access to Safe Conditions\n (SDG 3, SDG 6, SDG 7)",
        "reducing_ineq_eval" = "Reducing Overall Inequality\n (SDG 1, SDG 2, SDG 10)",
        "climate_action_eval" = "Holistic Climate Action\n (SDG 13, SDG 14, SDG 15)",
        "education_eval" = "Equality Through Education\n (SDG 4, SDG 5)",
        "sust_partner_eval" = "Sustainable Partnerships\n (SDG 11, SDG 12)",
        "sust_growth_eval" = "Sustainable Growth\n (SDG 8, SDG 9, SDG 16)"
    )) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 26),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_blank(),
        strip.text = element_text(size = 15, face = "bold"),  # Increased sub_region label size
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.text = element_text(size = 15, margin = margin(r = 50)),  # Increased legend text size
        legend.title = element_text(size = 18)  # Increased legend title size
    ) +
    guides(fill = guide_legend(nrow = 2)) +  # Set the number of rows in the legend to 2
    theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
    theme(aspect.ratio = 0.75)

plot5

cvd_grid(plot5)

ggsave("plot5_boxplot_sdg_eval.pdf", plot = plot5,
       path = "/Users/mass/Documents/Masters/Courses/Connected Politics/Github repository/Content/Datasets and code/Code and plots/Plots/",
       device = "pdf", dpi = 600, width = 20, height = 15)

ggsave("plot5_boxplot_sdg_eval.png", plot = plot5,
       path = "/Users/mass/Documents/Masters/Courses/Connected Politics/Github repository/Content/Datasets and code/Code and plots/Plots/",
       device = "png", dpi = 600, width = 20, height = 15)


# Save final dataset
write_csv(df, "//Users/mass/Documents/Masters/Courses/Connected Politics/Github repository/Content/Datasets and code/Datasets/Final merged dataset/df_merged.csv")


# Case selection 

# Group df by country_abb and calculate the average climate_action_eval
avg_climate_action <- df |>
    group_by(country_abb) |>
    summarise(climate_action_eval = mean(climate_action_eval, na.rm = TRUE)) |>
    ungroup()

# Select the top 10 and bottom 10 values based on climate_action_eval
top_10_climate_action <- avg_climate_action |>
    arrange(desc(climate_action_eval)) |>
    slice(1:10)

bottom_10_climate_action <- avg_climate_action |>
    arrange(climate_action_eval) |>
    slice(1:10)

# Combine the top and bottom 10 into one dataframe
selected_countries <- bind_rows(top_10_climate_action, bottom_10_climate_action)

selected_countries

# Save the selected countries as a subset

subset_df <- df |>
    filter(country_abb %in% selected_countries$country_abb)

write_csv(subset_df, "/Users/mass/Documents/Masters/Courses/Connected Politics/Github repository/Content/Datasets and code/Datasets/Final merged dataset/top_and_bottom10_climate_action_countries.csv")