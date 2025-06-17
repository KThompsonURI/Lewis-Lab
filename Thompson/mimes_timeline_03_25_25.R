#KT - Lewis Lab URI

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(scales)

mimes_data <- read_csv("mimes_timeline_data.csv")

dataset_order <- c(
  "NAICS",
  "Census Geographies",
  "Census Demographics",
  "North Atlantic Right Whale Number",
  "Initial Stocks for Biomass of Species",
  "Consumption Rates for Species",
  "USACE Districts",
  "US Coast Guard Districts",
  "Stellwagen Bank NMS",
  "Restoration Project areas",
  "Protected Areas",
  "National Parks",
  "NERRs",
  "MPAs",
  "Federal Sand and Gravel Borrow Areas",
  "Federal Consistency Location Descriptions",
  "EPA Regional Boundaries",
  "Beach Nourishment",
  "280 fathom contour"
)

plot_data <- mimes_data
plot_data$Dataset <- factor(plot_data$Dataset, levels = dataset_order)
plot_data <- plot_data %>% arrange(Dataset)

year_mapping <- data.frame(
  year = c(seq(1910, 2000, by = 10), 2000:2025),
  position = 1:length(c(seq(1910, 2000, by = 10), 2000:2025))
)

pos_2000 <- year_mapping$position[year_mapping$year == 2000]
pos_2000 <- pos_2000[1]

segments_data <- plot_data %>%
  select(Dataset, Category, `Type (Geometry)`, start_year, end_year) %>%
  left_join(year_mapping, by = c("start_year" = "year")) %>%
  rename(start_pos = position) %>%
  left_join(year_mapping, by = c("end_year" = "year")) %>%
  rename(end_pos = position)

markers_data <- plot_data %>%
  select(Dataset, Category, `Type (Geometry)`, start_year, end_year) %>%
  pivot_longer(cols = c(start_year, end_year), 
               names_to = "point_type", 
               values_to = "year") %>%
  distinct() %>%
  left_join(year_mapping, by = "year") %>%
  rename(year_pos = position)

pre_2000_bg <- plot_data %>%
  select(Dataset, Category) %>%
  mutate(
    xmin = min(year_mapping$position[year_mapping$year < 2000]),
    xmax = pos_2000,
    ymin = as.numeric(Dataset) - 0.5,
    ymax = as.numeric(Dataset) + 0.5
  )

post_2000_bg <- plot_data %>%
  select(Dataset, Category) %>%
  mutate(
    xmin = pos_2000,
    xmax = max(year_mapping$position),
    ymin = as.numeric(Dataset) - 0.5,
    ymax = as.numeric(Dataset) + 0.5
  )

category_colors_pre_2000 <- c(
  "Biophysical" = "#FFCCCB",
  "Boundaries" = "#EBF2F8",
  "Ecological" = "#D8F0D8",
  "Social" = "#F4EDF6"
)

category_colors_post_2000 <- c(
  "Biophysical" = "#FFCCCB",
  "Boundaries" = "#D6E8F8",
  "Ecological" = "#D8F0D8",
  "Social" = "#F2D9F0"
)

line_colors <- c(
  "Biophysical" = "#E41A1C",
  "Boundaries" = "#377EB8",
  "Ecological" = "#4DAF4A",
  "Social" = "#984EA3"
)

regular_markers <- markers_data %>%
  filter(`Type (Geometry)` != "Database")

database_markers <- markers_data %>%
  filter(`Type (Geometry)` == "Database")

geometry_shapes <- c(
  "Point" = 21,
  "Polygon" = 23,
  "Polyline" = 24,
  "Table" = 22,
  "Counties" = 25
)

database_shape <- c(
  "Database" = 22
)

marker_years <- unique(c(plot_data$start_year, plot_data$end_year))
marker_positions <- year_mapping$position[year_mapping$year %in% marker_years]

category_boundaries <- plot_data %>%
  arrange(Category, Dataset) %>%
  group_by(Category) %>%
  summarize(
    min_pos = min(as.numeric(Dataset)) - 0.5,
    max_pos = max(as.numeric(Dataset)) + 0.5
  )

p <- ggplot() +
  theme_minimal() +
  geom_hline(yintercept = 1:length(dataset_order), color = "lightgray", size = 0.3) +
  geom_vline(xintercept = marker_positions, color = "lightgray", alpha = 0.5, size = 0.5) +
  geom_rect(data = pre_2000_bg,
            aes(xmin = xmin, xmax = xmax, 
                ymin = ymin, ymax = ymax,
                fill = Category),
            alpha = 0.7) +
  geom_rect(data = post_2000_bg,
            aes(xmin = xmin, xmax = xmax, 
                ymin = ymin, ymax = ymax,
                fill = paste0(Category, "_post")),
            alpha = 0.7) +
  geom_hline(data = category_boundaries, 
             aes(yintercept = max_pos), 
             color = "darkgray", size = 0.5) +
  geom_segment(data = segments_data,
               aes(x = start_pos, xend = end_pos,
                   y = Dataset, yend = Dataset,
                   color = Category),
               size = 1) +
  geom_vline(xintercept = pos_2000, 
             linetype = "dashed", color = "black", alpha = 0.7) +
  geom_point(data = regular_markers,
             aes(x = year_pos, y = Dataset, 
                 shape = `Type (Geometry)`),
             color = "black",
             fill = regular_markers$Category %>%
               recode(
                 "Biophysical" = "#E41A1C", 
                 "Boundaries" = "#377EB8", 
                 "Ecological" = "#4DAF4A", 
                 "Social" = "#984EA3"
               ),
             size = 3, stroke = 1) +
  geom_point(data = database_markers,
             aes(x = year_pos, y = Dataset, 
                 shape = `Type (Geometry)`),
             color = "black",
             fill = database_markers$Category %>%
               recode(
                 "Biophysical" = "#E41A1C", 
                 "Boundaries" = "#377EB8", 
                 "Ecological" = "#4DAF4A", 
                 "Social" = "#984EA3"
               ),
             size = 2, stroke = 1) +
  scale_fill_manual(values = c(
    category_colors_pre_2000,
    setNames(category_colors_post_2000, paste0(names(category_colors_post_2000), "_post"))
  ), guide = "none") +
  scale_color_manual(values = line_colors) +
  scale_shape_manual(values = c(geometry_shapes, database_shape)) +
  scale_x_continuous(
    breaks = year_mapping$position,
    labels = year_mapping$year,
    expand = c(0.01, 0.01)
  ) +
  theme(
    plot.title = element_text(hjust = 0, face = "bold", size = 16),
    axis.text.x = element_text(
      angle = 90, 
      vjust = 0.5, 
      hjust = 1, 
      size = 8,
      color = ifelse(year_mapping$year %in% marker_years, "black", "gray"),
      face = ifelse(year_mapping$year %in% marker_years, "bold", "plain")
    ),
    axis.text.y = element_text(size = 10, face = "bold", color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    legend.box.just = "left",
    plot.margin = margin(10, 100, 10, 10)
  ) +
  labs(
    title = "MIMES Model Data Time Ranges",
    x = "Year",
    y = NULL,
    color = "Category",
    shape = "Data Type"
  )

p <- p + 
  guides(
    color = guide_legend(title = "Category"),
    shape = guide_legend(title = "Data Type")
  )

p