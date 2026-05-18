library(ggplot2)
library(dplyr)
library(lubridate)
library(pracma)
library(future)
library(RTransferEntropy)
library(zoo)

# Loading the data that Casper made, it's in the WhatsApp group.
df <- read.csv("traffic_sitelevel_march_2026_sites_100_161_162_163_164_clean.csv")

df$timestamp <- as.POSIXct(
  df$timestamp,
  format = "%Y-%m-%d %H:%M:%S",
  tz = "Australia/Melbourne"
)

df$volume <- as.numeric(df$site_total_volume)

# Sort chronologically
df <- df %>% arrange(timestamp, site_no)


# Extract sites 161 and 164
site161 <- df %>%
  filter(site_no == 161) %>%
  select(timestamp, volume) %>%
  rename(vol161 = volume)

site164 <- df %>%
  filter(site_no == 164) %>%
  select(timestamp, volume) %>%
  rename(vol164 = volume)

# Merge by timestamp so that our datasets are aligned
te_data <- inner_join(site161, site164, by = "timestamp")

# Calculating the Transfer Entropy between sites 164 and 161. Site 164 is a major junction while site 161 is a through-road right next to it.
# So we expect a high transfer entropy from site 161 to 164 
# (since about half the traffic at 161 flows into (and the other half away from) 164 
# but from 164 they can go in 8 directions (back and forth in all 4 junction directions))
# In case you haven't read the slides yet:
# Transfer entropy captures the relationship between two variables at time lags

te_161_to_164 <- transfer_entropy(
  te_data$vol161,
  te_data$vol164,
  lx = 1,
  ly = 1,
  q = 0.1,
  entropy = "Shannon",
  shuffles = 100
)

print(te_161_to_164)

# Functions to calculate Shannon Entropy and Sample Entropy so that we can calculate the windows easier down below
# In case you haven't read the slides yet: 
# Shannon entropy quantifies uncertainty (higher entropy is higher uncertainty)
# Sample Entropy quantifies system complexity (when higher = more complex) or temporal regularity (lower SampEn)
# For Sample Entropy we are using the standard parameters of edim = 2 and r = 0.2  * std. You can play around with these if you want and report on it.

# The sample entropy hyperparameters:
edim = 2
r = 0.2

shannon_entropy <- function(x, num_bins = 20) {
  
  h <- hist(x, breaks = num_bins, plot = FALSE)
  
  p <- h$counts / sum(h$counts)
  
  p <- p[p > 0]
  
  -sum(p * log(p))
}

# Apply Shannon Entropy to all sites individually over the whole dataset
shannon_results <- df %>%
  group_by(site_no) %>%
  summarise(
    shannon_H = shannon_entropy(volume)
  )

print(shannon_results)

sample_entropy_func <- function(x) {
  
  if(sd(x) == 0) {
    return(NA)
  }
  
  sample_entropy(
    x,
    edim = edim,
    r = r * sd(x),
    tau = 1
  )
}

# Apply Sample Entropy to all sites individually over the whole dataset
sample_results <- df %>%
  group_by(site_no) %>%
  summarise(
    sampen = sample_entropy_func(volume)
  )

print(sample_results)

# Are there changes over time in the entropy estimates? What might these changes be indicative of?
# We set a rolling window and calculate Shannon and Sample Entropy over these windows, then plot them
# Right now the window in 96 * 0.25h = 24 hours

window_size <- 96

# This code takes a while to run, so be mindful of that
rolling_entropy <- df %>%
  
  group_by(site_no) %>%
  
  arrange(timestamp) %>%
  
  mutate(
    
    rolling_shannon =
      rollapply(
        volume,
        width = window_size,
        FUN = shannon_entropy,
        align = "right",
        fill = NA
      ),
    
    rolling_sampen =
      rollapply(
        volume,
        width = window_size,
        FUN = sample_entropy_func,
        align = "right",
        fill = NA
      )
  )

# Plotting the Shannon Entropy

ggplot(
  rolling_entropy,
  aes(timestamp, rolling_shannon, color = factor(site_no))
) +
  
  geom_line(linewidth = 0.8) +
  
  labs(
    title = "Rolling Shannon Entropy Through Time",
    x = "Time",
    y = "Shannon Entropy",
    color = "Site"
  ) +
  
  theme_minimal()

# Plotting the Sample Entropy

ggplot(
  rolling_entropy,
  aes(timestamp, rolling_sampen, color = factor(site_no))
) +
  
  geom_line(linewidth = 0.8) +
  
  labs(
    title = "Rolling Sample Entropy Through Time",
    x = "Time",
    y = "Sample Entropy",
    color = "Site"
  ) +
  
  theme_minimal()