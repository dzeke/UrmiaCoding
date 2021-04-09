### pie plot

# Load ggplot2
library(ggplot2)
library(dplyr)

# Create Data
data <- data.frame(
  group=c("Causeway", "Other"),
  value=c(8, 451-8),
  xpos = c(2,"")
)

data$grouplabel <- paste0(data$group, " (",data$value,")","")

# Basic piechart
data <- data %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# Basic piechart
ggplot(data, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1.5, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  
  #scale_y_continuous(breaks=cumsum(data$value) - data$value / 2, labels= data$grouplabel)
  
  geom_text(aes(x=xpos, y = ypos, label = grouplabel, color = xpos), size=6) +
  scale_color_manual(values=c("black","red")) +
  scale_fill_brewer(palette="Set1")