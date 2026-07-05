
# S2 Fig: Division-wise prevalence of different types of bullying (a. Physical bullying; b. Cyberbullying ; c. Verbal bullying; d. Ever bullied).



library(readr)
library(sf)
library(ggplot2)
library(dplyr)
bangladesh_map <- st_read("C:/Users/HP/Desktop/map/bgd_adm_bbs_20201113_SHP/bgd_admbnda_adm1_bbs_20201113.shp")
bullying_data <- read_csv("C:/Users/HP/Desktop/map/percentage.csv")

bangladesh_map <- bangladesh_map %>%
  left_join(bullying_data, by = c("ADM1_EN" = "DIVISION"))

bangladesh_map <- bangladesh_map %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(x = st_coordinates(centroid)[,1], 
         y = st_coordinates(centroid)[,2])
bullying_map <- ggplot(bangladesh_map) +
  geom_sf(aes(fill = physical_bulling), color = "black") +  
  scale_fill_gradient(low = "lightblue", high = "red", na.value = "gray") +  
  geom_text(aes(x = x, y = y, label = paste0(ADM1_EN, "\n", round(physical_bulling, 1), "%")),
            size = 4, color = "black", fontface = "bold") +  
  labs(title = "Physical Bullying",
       fill = "Percentage %") +
  theme_void() + theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))

print(bullying_map)

ggsave("C:/Users/HP/Desktop/phy_bd_map.jpg",
       plot = bullying_map, width = 10, height = 7, dpi = 300)

//cyber bulling//
 
bullying_map1 <- ggplot(bangladesh_map) +
  geom_sf(aes(fill = cyber_bulling), color = "black") +  
  scale_fill_gradient(low = "lightblue", high = "red", na.value = "gray") +  
  geom_text(aes(x = x, y = y, label = paste0(ADM1_EN, "\n", round(cyber_bulling, 1), "%")),
            size = 4, color = "black", fontface = "bold") +  
  labs(title = "Cyber Bullying",
       fill = "Percentage %") +
  theme_void() + theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))


print(bullying_map1)

ggsave("C:/Users/HP/Desktop/cyb_bd_map.jpg",
       plot = bullying_map1, width = 10, height = 7, dpi = 300) 

//verbal bulling//
  bullying_map2 <- ggplot(bangladesh_map) +
  geom_sf(aes(fill = verbal_bulling), color = "black") +  
  scale_fill_gradient(low = "lightblue", high = "red", na.value = "gray") +  
  geom_text(aes(x = x, y = y, label = paste0(ADM1_EN, "\n", round(verbal_bulling, 1), "%")),
            size = 4, color = "black", fontface = "bold") +  
  labs(title = "Verbal Bullying",
       fill = "Percentage %") +
  theme_void() + theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))


print(bullying_map2)

ggsave("C:/Users/HP/Desktop/ver_bd_map.jpg",
       plot = bullying_map2, width = 10, height = 7, dpi = 300)
  
  

//ever bulling//
  
  bullying_map3 <- ggplot(bangladesh_map) +
  geom_sf(aes(fill = ever_bulling), color = "black") +  
  scale_fill_gradient(low = "lightblue", high = "red", na.value = "gray") +  
  geom_text(aes(x = x, y = y, label = paste0(ADM1_EN, "\n", round(ever_bulling, 1), "%")),
            size = 4, color = "black", fontface = "bold") +  
  labs(title = "Ever Bullied",
       fill = "Percentage %") +
  theme_void() + theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))


print(bullying_map3)

ggsave("C:/Users/HP/Desktop/ever_bd_map.jpg",
       plot = bullying_map3, width = 10, height = 7, dpi = 300)
  
  
  
 # Fig 1: Forest Plot of bullying effect on adolescents’ mental wellbeing
library(haven)
library(dplyr)
library(ggplot2)
library(broom)

# Read data
data <- read_dta("C:/Users/HP/Desktop/New folder/full dataset.dta")
data$composite_bul <- ifelse(
  data$physical + data$ver_bul + data$cyb_bul > 0,
  1, 0
)

get_coef <- function(df, bully_var, group_name){
  
  fit <- lm(
    as.formula(paste("Depression ~", bully_var)),
    data = df
  )
  
  est <- tidy(fit, conf.int = TRUE)
  
  est %>%
    filter(term == bully_var) %>%
    mutate(
      Group = group_name,
      Bullying = case_when(
        bully_var == "physical" ~ "Physical Bullying",
        bully_var == "ver_bul" ~ "Verbal Bullying",
        bully_var == "cyb_bul" ~ "Cyber Bullying",
        bully_var == "composite_bul" ~ "Composite Bullying"
      )
    ) %>%
    select(Group, Bullying, estimate, conf.low, conf.high)
}

# Married Female
mf <- data %>% filter(QRTYPE == "Married Female")

# Unmarried Female
uf <- data %>% filter(QRTYPE == "Unarried Female")

# Unmarried Male
um <- data %>% filter(QRTYPE == "Unmarried male")






results <- bind_rows(
  
  get_coef(mf, "physical", "Married Female"),
  get_coef(mf, "ver_bul", "Married Female"),
  get_coef(mf, "cyb_bul", "Married Female"),
  get_coef(mf, "composite_bul", "Married Female"),
  
  get_coef(uf, "physical", "Unmarried Female"),
  get_coef(uf, "ver_bul", "Unmarried Female"),
  get_coef(uf, "cyb_bul", "Unmarried Female"),
  get_coef(uf, "composite_bul", "Unmarried Female"),
  
  get_coef(um, "physical", "Unmarried Male"),
  get_coef(um, "ver_bul", "Unmarried Male"),
  get_coef(um, "cyb_bul", "Unmarried Male"),
  get_coef(um, "composite_bul", "Unmarried Male")
)


library(ggplot2)

results$Group <- factor(
  results$Group,
  levels = c(
    "Married Female",
    "Unmarried Female",
    "Unmarried Male"
  )
)

results$Bullying <- factor(
  results$Bullying,
  levels = c(
    "Verbal Bullying",
    "Physical Bullying",
    "Cyber Bullying",
    "Composite Bullying"
  )
)





results$Group <- factor(
  results$Group,
  levels = c(
    "Married Female",
    "Unmarried Female",
    "Unmarried Male"
  )
)

results$Bullying <- factor(
  results$Bullying,
  levels = c(
    "Composite Bullying",
    "Cyber Bullying",
    "Verbal Bullying",
    "Physical Bullying"
  )
)

plot6 <- ggplot(
  results,
  aes(
    x = estimate,
    y = Bullying,
    xmin = conf.low,
    xmax = conf.high,
    colour = Bullying
  )
) +
  
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "grey50"
  ) +
  
  geom_errorbarh(
    height = 0.15,
    linewidth = 1.2
  ) +
  
  geom_point(
    size = 3.5
  ) +
  
  facet_grid(
    Group ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  
  scale_colour_manual(
    values = c(
      "Verbal Bullying"    = "#B22222" , # Deep Red (Firebrick)
      "Physical Bullying"  = "#0072B2" , # Navy Blue
      "Cyber Bullying"     = "#FF8C00" , # Deep Green
      "Composite Bullying" = "#000000"  # Black
    )
  ) +
  
  labs(
    x = "Coefficient (95% CI)",
    y = NULL,
    colour = NULL
  ) +
  
  theme_bw(base_size = 15) +
  
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(
      angle = 0,
      face = "bold",
      size = 13
    ),
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "cm")
  )

plot6
