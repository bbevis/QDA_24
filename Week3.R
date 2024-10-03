
###############################################
#
# Week 3 - Plotting
# Imai & Williams 3.3 - 3.9
#
###############################################

library(tidyverse)
library(ggplot2)
library(RColorBrewer)
install.packages("viridis")
library(viridis)

afghan = read.csv('afghan.csv')

# Survey data on civilian victimisation where people were asked:
# Over the past year, have you or anyone in your family suffered harm due to actions
# of the Foreign Forces or the Taliban?

head(afghan)
glimpse(afghan)
colnames(afghan)

############################
# Data exploration
############################

# harmed by ISAF, harmed by Taliban (1 = yes, 0 = no)
afghan %>%
  select(violent.exp.ISAF, violent.exp.taliban) %>%
  head()

# How many people were harmed by either forces?
# Were any harmed by both?
afghan %>%
  filter(!is.na(violent.exp.ISAF)) %>%
  filter(!is.na(violent.exp.taliban)) %>%
  group_by(violent.exp.ISAF, violent.exp.taliban) %>%
  count()

afghan %>%
  filter(!is.na(violent.exp.ISAF)) %>%
  filter(!is.na(violent.exp.taliban)) %>%
  group_by(violent.exp.ISAF, violent.exp.taliban) %>%
  count()

# perhaps we can make this a bit neater
afghan %>%
  filter(!is.na(violent.exp.ISAF)) %>%
  filter(!is.na(violent.exp.taliban)) %>%
  group_by(violent.exp.ISAF, violent.exp.taliban) %>%
  count() %>%
  mutate(harmed_by = case_when(violent.exp.ISAF == 0 & violent.exp.taliban == 1 ~ "Taliban",
                               violent.exp.ISAF == 1 & violent.exp.taliban == 0 ~ "ISAF",
                               violent.exp.ISAF == 0 & violent.exp.taliban == 0 ~ "Neither",
                               violent.exp.ISAF == 1 & violent.exp.taliban == 1 ~ "Both"))


############################
# Bar plots
############################

# check out https://ggplot2.tidyverse.org/

# Basic charts...
afghan %>%
  filter(!is.na(violent.exp.ISAF)) %>%
  filter(!is.na(violent.exp.taliban)) %>%
  group_by(violent.exp.ISAF, violent.exp.taliban) %>%
  count() %>%
  mutate(harmed_by = case_when(violent.exp.ISAF == 0 & violent.exp.taliban == 1 ~ "Taliban",
                               violent.exp.ISAF == 1 & violent.exp.taliban == 0 ~ "ISAF",
                               violent.exp.ISAF == 0 & violent.exp.taliban == 0 ~ "Neither",
                               violent.exp.ISAF == 1 & violent.exp.taliban == 1 ~ "Both")) %>%
  ggplot(aes(x = harmed_by, y = n)) +
  geom_bar(stat = 'identity')

# lets make this prettier
afghan %>%
  filter(!is.na(violent.exp.ISAF)) %>%
  filter(!is.na(violent.exp.taliban)) %>%
  group_by(violent.exp.ISAF, violent.exp.taliban) %>%
  count() %>%
  mutate(harmed_by = case_when(violent.exp.ISAF == 0 & violent.exp.taliban == 1 ~ "Taliban", # create a single categorical variable
                               violent.exp.ISAF == 1 & violent.exp.taliban == 0 ~ "ISAF",
                               violent.exp.ISAF == 0 & violent.exp.taliban == 0 ~ "Neither",
                               violent.exp.ISAF == 1 & violent.exp.taliban == 1 ~ "Both")) %>%
  ggplot(aes(x = reorder(harmed_by, n), y = n, fill = harmed_by)) + # initial parameters for any ggplot must include variables to plot
  geom_bar(stat = 'identity') + # for bar plots, include stat = 'identity'. Not required for line or points plot
  theme_classic() + # creates a minimal look
  scale_fill_manual(values = c( "#F8766D", "grey", '#00BFC4', 'grey')) + # manually specify colours if you want
  coord_flip() + # swap y and x axis. Do this to make names easier to read
  theme(legend.position = 'none', # in this case, we don't need a legend
        axis.text=element_text(size=20), # Make sure texts and titles are legible
        axis.title=element_text(size=20, face="bold")) +
  labs(x="",y="Counts of harm") # Give the axes meaningful names

ggsave("./afghan_barplot.png",dpi=300,height=7,width=12) # Save the file to specific

# Multiple ways to plot data
afghan %>%
  pivot_longer(violent.exp.ISAF:violent.exp.taliban, # reshape the data
               names_to = "harming_group",
               values_to = "harm") %>%
  mutate(harm = as.factor(harm)) %>%
  group_by(harming_group, harm) %>%
  count() %>%
  mutate(harming_group = case_when(harming_group == 'violent.exp.ISAF'  ~ "ISAF", # create a single categorical variable
                                   harming_group == 'violent.exp.taliban' ~ "Taliban")) %>%
  ggplot(aes(x = harm, y = n, fill = harming_group)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_minimal() + # minimal theme
  scale_x_discrete(labels = c('No harm', 'Harm', 'No response')) +
  scale_fill_manual(values = c( "#85c1e9", '#1a5276')) + # google hex colours
  theme(legend.position = 'top',
        legend.title = element_blank(), # legend titles are not always needed
        legend.text = element_text(size=20),
        axis.text=element_text(size=20), # Make sure texts and titles are legible
        panel.grid.minor = element_blank(), # remove specific grid lines
        panel.grid.major.x = element_blank(),
        axis.title=element_text(size=20, face="bold")) +
    labs(x="",y="Counts of harm")

ggsave("./afghan_barplot_2.png",dpi=300,height=7,width=12) # Save the file to specific


# Now you know how to make charts pretty, lets see what other TYPES of charts we can make

############################
# Histograms - exploring distributions
############################

afghan %>%
  ggplot(aes(x = age)) +
  geom_histogram(binwidth = .5) +
  scale_x_continuous(breaks = seq(20, 80, by = 10)) +
  theme_minimal()

############################
# Boxplots - showing distributions with means
############################

afghan %>%
  ggplot(aes(y = educ.years, x = province)) +
  geom_boxplot(aes(fill = province)) +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE, option = "B")
  # scale_fill_brewer(palette = "Dark2")

# For palette options, see: https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/

afghan %>%
  ggplot(aes(y = age)) +
  geom_boxplot(aes(color = province)) +
  theme_minimal() +
  facet_wrap(.~province) +
  scale_x_discrete(expand = c(0, 0.5)) +
  ylim(0, 100) +
  theme(legend.position = c(.8,.2),
        axis.text.x = element_blank(),
        panel.grid.minor = element_blank())

############################
# point plots - a cleaner way of showing distribution and means
############################

afghan %>%
  group_by(province) %>%
  summarise(m = mean(age), # sumarising data first to get means and standard errors
            n = n(),
            se = sd(age) / sqrt(n)) %>%
  ggplot(aes(y = m, ymin = m - se, ymax = m + se, x = reorder(province, desc(m)), color = province)) +
  geom_errorbar(width = .2) +
  geom_point(aes(size = 3)) +
  theme_minimal() +
  coord_flip() +
  theme(legend.position = 'none') +
  scale_color_brewer(palette = "Dark2")

############################
# Scatter + line of best fit
############################

#simulate data
x <- c(1, 2, 3, 4, 5, 6, 7, 8)
y <- c(2, 5, 6, 7, 9, 12, 16, 19)

data = data_frame(x, y)

#create scatter plot of x vs. y
data %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(color = 'darkgrey') +
  geom_smooth(stat = "smooth", method = "lm", color = 'darkgreen',fill="#69b3a2") +
  theme_minimal()

