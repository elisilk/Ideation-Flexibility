setwd("/Users/Amy/Documents")
read_Data <- read.csv("Framing-Outcomes-Individual-Shifts.csv")

yr1 <- subset(read_Data, Year == 1)
yr2 <- subset(read_Data, Year == 2)
yr3 <- subset(read_Data, Year == 3)
yr4 <- subset(read_Data, Year == 4)


df1 <- data.frame(AverageN = yr1$NeutralAverage, ContextN = yr1$NeutralContext, AverageF = yr1$FramedAverage, ContextF = yr1$FramedContext, Framing = yr1$FramingType, Shift = yr1$Shift.N.F.)
df2 <- data.frame(AverageN = yr2$NeutralAverage, ContextN = yr2$NeutralContext, AverageF = yr2$FramedAverage, ContextF = yr2$FramedContext, Framing = yr2$FramingType, Shift = yr2$Shift.N.F)
df3 <- data.frame(AverageN = yr3$NeutralAverage, ContextN = yr3$NeutralContext, AverageF = yr3$FramedAverage, Framing = yr3$FramingType, Shift = yr3$Shift.N.F.)
df4 <- data.frame(AverageN = yr4$NeutralAverage, ContextN = yr4$NeutralContext, AverageF = yr4$FramedAverage, Framing = yr4$FramingType, Shift = yr4$Shift.N.F.)

model1 <- lm(Shift ~ AverageN + ContextN + ContextF + Framing, data=df1)
summary(model1)
model2 <- lm(Shift ~ AverageN + ContextN + ContextF + Framing, data=df2)
summary(model2)
model3 <- lm(Shift ~ AverageN + ContextN + Framing, data=df3)
summary(model3)
model4 <- lm(Shift ~ AverageN + ContextN + Framing, data=df4)
summary(model4)



## Reorganizing the data frames so that each row of the data frame corresponds to an ideation session.
## In this organization, each participant will have multiple sessions (Neutral + Framed).
## This allows us to do a regression that predicts the Average PR score for a session as a function
## of the Context + Framing. Framing can be either Neutral, Incremental, or Radical, and so we can
## use the Neutral as the baseline and then compare Incremental to Neutral and Radical to Neutral.

# names(yr4) # Exploring the data frame

# Selecting out only the rows of interest for each ideation session
yr4.neutral <- yr4[,1:20]
yr4.neutral$Framing <- "Neutral"
yr4.framed <- yr4[,c(1:2,21,23:39,22)]

# Renaming the columns to the column names are consistent for both ideation session data frames
colnames(yr4.neutral) <- c("Year","ParticipantID", "Context", paste("Idea",1:16,sep=""), "Average", "Framing")
colnames(yr4.framed) <- c("Year","ParticipantID", "Context", paste("Idea",1:16,sep=""), "Average", "Framing")

# colnames(yr4.neutral) # Verifying the column names
# colnames(yr4.framed) # Verifying the column names

# Combining the neutral and framed sessions into one larger, reorganized data frame
yr4.reorg <- rbind(yr4.neutral, yr4.framed)

# Convert the Framing column to a factor with "Neutral" as the baseline
yr4.reorg$Framing <- factor(yr4.reorg$Framing, levels=c("Neutral","Incremental","Radical"))

# summary(yr4.reorg) # Verifying the reorganized data frame

# A new regression model predicting the Average PR score of the session as function of the problem Context and the Framing
model4 <- lm(Average ~ Context + Framing, data=yr4.reorg)
summary(model4)
