#load packages --------

library(tidyverse)
library(here)

#read in data --------

plotstudents <- read.csv(here("data", "AcademicSuccess.csv"))

#custom palette for plot--------

custom_palette <- c("#FF0000", "#008000", "#0000FF", "#FFA500", "#800080",
                    "#00FFFF", "#FF00FF", "#00FF00", "#FFC0CB", "#008080",
                    "#E6E6FA", "#808000", "#800000", "#000080", "#7FFFD4",
                    "#FF7F50", "#DAA520", "#006400")

#plot data using geom_freqpoly by Target(Dropout and Graduate)--------

plot_freqpoly <- function(data, x_var, bin_width) {
  x_var <- enquo(x_var)  # Capture the variable for later evaluation
  
  filtered_data <- data %>%
    filter(Target %in% c("Dropout", "Graduate"))
  
  filtered_data %>%
    ggplot(aes(x = !!x_var, colour = Target)) +
    geom_freqpoly(binwidth = bin_width) +
    scale_x_continuous(breaks = unique(filtered_data[[quo_name(x_var)]])) +
    theme_minimal()
}

plot_freqpoly(plotstudents, Marital.status, 0.5)
plot_freqpoly(plotstudents, Application.mode, 0.5)
plot_freqpoly(plotstudents, Application.order, 0.5)#
plot_freqpoly(plotstudents, Course, 0.5)
plot_freqpoly(plotstudents, Daytime.evening.attendance, 0.5)
plot_freqpoly(plotstudents, Previous.qualification, 0.5)
plot_freqpoly(plotstudents, Nacionality, 0.5)#
plot_freqpoly(plotstudents, Mother.s.qualification, 0.5)
plot_freqpoly(plotstudents, Father.s.qualification, 0.5)
plot_freqpoly(plotstudents, Mother.s.occupation, 0.5)
plot_freqpoly(plotstudents, Father.s.occupation, 0.5)
plot_freqpoly(plotstudents, Displaced, 0.5)
plot_freqpoly(plotstudents, Educational.special.needs, 0.5)#
plot_freqpoly(plotstudents, Debtor, 0.5)
plot_freqpoly(plotstudents, Tuition.fees.up.to.date, 0.5)
plot_freqpoly(plotstudents, Gender, 0.5)
plot_freqpoly(plotstudents, Scholarship.holder, 0.5)
plot_freqpoly(plotstudents, Age.at.enrollment, 0.5)
plot_freqpoly(plotstudents, International, 0.5)#
plot_freqpoly(plotstudents, Curricular.units.1st.sem..credited., 0.5)#
plot_freqpoly(plotstudents, Curricular.units.1st.sem..enrolled., 0.5)
plot_freqpoly(plotstudents, Curricular.units.1st.sem..evaluations., 0.5)
plot_freqpoly(plotstudents, Curricular.units.1st.sem..approved., 0.5)
plot_freqpoly(plotstudents, Curricular.units.1st.sem..grade., 0.3)
plot_freqpoly(plotstudents, Curricular.units.1st.sem..without.evaluations., 0.5)
plot_freqpoly(plotstudents, Curricular.units.2nd.sem..credited., 0.5)#
plot_freqpoly(plotstudents, Curricular.units.2nd.sem..enrolled., 0.5)
plot_freqpoly(plotstudents, Curricular.units.2nd.sem..evaluations., 0.5)
plot_freqpoly(plotstudents, Curricular.units.2nd.sem..approved., 0.5)
plot_freqpoly(plotstudents, Curricular.units.2nd.sem..grade., 0.5)
plot_freqpoly(plotstudents, Curricular.units.2nd.sem..without.evaluations., 0.5)
plot_freqpoly(plotstudents, Unemployment.rate, 0.5)#
plot_freqpoly(plotstudents, Inflation.rate, 0.5)#
plot_freqpoly(plotstudents, GDP, 0.5)#

plotstudents %>%
  filter(Target %in% c("Dropout", "Graduate")) %>%
  ggplot(aes(x = Target, y = Age.at.enrollment, fill = Target)) +
  geom_violin(trim = FALSE) + 
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")

#plot data using geom_point by Target(Dropout and Graduate)--------

plotstudents %>%
  filter(Target %in% c("Dropout", "Graduate")) %>%
  ggplot(aes(x = Marital.status, color = Target))+
  geom_jitter()+
  theme_minimal()

names(plotstudents)






