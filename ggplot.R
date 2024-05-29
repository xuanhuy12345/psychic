# Load necessary library
library(ggplot2)

# Example data
sampleData <- read_excel("MAP2024/PsychicApp Sample1 Data.xlsx")

# Create histogram
ggplot(sampleData, aes(x = Successes)) + 
  geom_histogram(bins = 12, color = "white") +
  geom_histogram(data = sampleData[sampleData$Successes >= 6,], bins = 12, color = "white", fill = "red") +
  geom_vline(xintercept=6, color="red", linetype="dashed", size=1) +
  annotate("text", x=6.3, y = 15, label="Num of Success ≥ 6", color="red", vjust=-1)
