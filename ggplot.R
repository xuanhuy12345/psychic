# Load necessary library
library(ggplot2)

# Example data
sampleData <- read_excel("MAP2024/PsychicApp Sample1 Data.xlsx")

psychic1 = sampleData[sampleData$NumCard == 2, ]
psychic2 = sampleData[sampleData$NumCard == 5, ]

# Create histogram for psychic1 using numbers
ggplot(psychic1, aes(x = Successes)) + 
  geom_histogram(bins = 8, color = "white") +
  geom_histogram(data = psychic1[psychic1$Successes >= 4,], bins = 8, color = "white", fill = "light blue") +
  geom_vline(xintercept=4, color="blue", linetype="dashed", size=1) +
  annotate("text", x=4.1, y = 11, label="As extreme as (≥) 4", color="black", vjust=-1)


# Create histogram for psychic2 using numbers
ggplot(psychic2, aes(x = Successes)) + 
  geom_histogram(bins = 8, color = "white") +
  geom_histogram(data = psychic2[psychic2$Successes >= 4,], bins = 8, color = "white", fill = "light blue") +
  geom_vline(xintercept=4, color="blue", linetype="dashed", size=1) +
  annotate("text", x=4.1, y = 11, label="As extreme as (≥) 4", color="black", vjust=-1)
