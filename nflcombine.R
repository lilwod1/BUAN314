
library(sqldf)
library(ggplot2)
library(tidyverse)

nfl_combine <- read.csv("nfl_combine_2010_to_2023.csv", header = TRUE)

#######################
#####DATA CLEANING#####
#######################
#rename Broad.Jump column
names(nfl_combine)[names(nfl_combine)=='Broad.Jump'] <- 'BroadJump'

#create a data frame without ANY null values in any column (ONLY players who were drafted)
nfl_A<-na.omit(nfl_combine)
#create a data frame with ONLY players who were NOT drafted
nfl_B<-sqldf("SELECT * 
             FROM nfl_combine
             WHERE Drafted = 'False'")
#delete Round & Pick columns from nfl_A and nfl_B
nfl_A$Round <- NULL
nfl_A$Pick <- NULL
nfl_B$Round <- NULL
nfl_B$Pick <- NULL
#remove null values
nfl_A<-na.omit(nfl_A)
nfl_B<-na.omit(nfl_B)
#convert height to inches for a numeric variable
nfl_A <- nfl_A %>%
  separate(Height, into = c("feet", "inches"), sep = "-", convert = TRUE) %>%
  mutate(height_total_in = feet*12 + inches)

nfl_B <- nfl_B %>%
  separate(Height, into = c("feet", "inches"), sep = "-", convert = TRUE) %>%
  mutate(height_total_in = feet*12 + inches)

#correlation matrix for both nfl_A and nfl_B
cor_A <- cor(nfl_A[,c(7,8,9,10,11,12,13,15)]) 
pairs(nfl_A[,c(7,8,9,10,11,12,13,15)])

cor_B <- cor(nfl_B[,c(7,8,9,10,11,12,13,15)]) 
pairs(nfl_B[,c(7,8,9,10,11,12,13,15)])

#FEATURE ENGINEERING
#group positions
nfl_combine <- nfl_combine %>%
  mutate(
    PositionGroup = case_when(
      Pos %in% c("DE", "DL", "DT", "EDGE") ~ "DLINE",
      Pos %in% c("LB", "ILB", "OLB") ~ "LB",
      Pos %in% c("CB", "S", "DB") ~ "DB",
      Pos %in% c("RB", "FB") ~ "RB",
      Pos %in% c("OL", "OG","OT","C") ~ "OLINE",
      TRUE ~ Pos     # ← Keep original values
    )
  )
nfl_A <- nfl_A %>%
  mutate(
    PositionGroup = case_when(
      Pos %in% c("DE", "DL", "DT", "EDGE") ~ "DLINE",
      Pos %in% c("LB", "ILB", "OLB") ~ "LB",
      Pos %in% c("CB", "S", "DB") ~ "DB",
      Pos %in% c("RB", "FB") ~ "RB",
      Pos %in% c("OL", "OG","OT","C") ~ "OLINE",
      TRUE ~ Pos     # ← Keep original values
    )
  )
nfl_B <- nfl_B %>%
  mutate(
    PositionGroup = case_when(
      Pos %in% c("DE", "DL", "DT", "EDGE") ~ "DLINE",
      Pos %in% c("LB", "ILB", "OLB") ~ "LB",
      Pos %in% c("CB", "S", "DB") ~ "DB",
      Pos %in% c("RB", "FB") ~ "RB",
      Pos %in% c("OL", "OG","OT","C") ~ "OLINE",
      TRUE ~ Pos     # ← Keep original values
    )
  )
#join nfl_A and nfl_B to create a clean data frame with both drafted and undrafted players
nfl_C <- sqldf('
      SELECT *
      FROM nfl_A
      UNION 
      SELECT *
      FROM nfl_B
      ')

#change the Drafted column-- instead of using TRUE/FALSE, 0 and 1
nfl_C$Drafted[nfl_C$Drafted=='True']<-1
nfl_C$Drafted[nfl_C$Drafted=='False']<-0

##############################
#####DESCRIPTIVE ANALYSIS#####
##############################
#run queries to learn more about data structure
sqldf("SELECT PositionGroup, COUNT(*) AS Count
       FROM nfl_combine
       GROUP BY PositionGroup;")

sqldf("SELECT PositionGroup, COUNT(*) AS Count
       FROM nfl_C
       GROUP BY PositionGroup;")

#summary statistics query
summary_stats<-sqldf("
  SELECT 
    MIN(X40yd), 
    MAX(X40yd),
    AVG(X40yd),
    MIN(Vertical),
    MAX(Vertical),
    AVG(Vertical),
    MIN(X3Cone),
    MAX(X3Cone),
    AVG(X3Cone)
  FROM nfl_C;
")

#compare average strength across position groups - query
sqldf("
  SELECT PositionGroup, AVG(Bench) AS AvgBench
  FROM nfl_C
  GROUP BY PositionGroup
  ORDER BY AvgBench DESC;
")

sqldf("
  SELECT PositionGroup, AVG(Bench) AS AvgBench
  FROM nfl_A
  GROUP BY PositionGroup
  ORDER BY AvgBench DESC;
")

#compare average metrics between WR, DB, RB
sqldf("
  SELECT PositionGroup, 
         AVG(X40yd) AS Avg40,
         AVG(Vertical) AS AvgVertical,
         AVG(Bench) AS AvgBench,
         AVG(Shuttle) AS AvgShuttle,
         AVG(X3Cone) AS AvgThreeCone
  FROM nfl_C
  WHERE PositionGroup IN ('RB', 'WR', 'DB')
  GROUP BY PositionGroup;
")

#list the fastest OLINE/DLINE players, showing if they were drafted or not
sqldf("
      SELECT PositionGroup, Drafted,X40yd
      FROM nfl_C
      WHERE PositionGroup IN ('OLINE','DLINE')
      ORDER BY PositionGroup DESC
      LIMIT 15
")

#Which position group is the overall most explosive?
sqldf("
  SELECT PositionGroup,
         AVG(Vertical) AS AvgVertical,
         AVG(BroadJump) AS AvgBroad
  FROM nfl_A
  GROUP BY PositionGroup
  ORDER BY AvgVertical DESC;
")

#athletic score 2023
sqldf("
  SELECT Player, PositionGroup,Drafted,Year,
         ( (1/X40yd) * 100 
           + Vertical 
           + BroadJump 
           + (25 - X3Cone) 
         ) AS AthleticScore
  FROM nfl_combine
  WHERE Year=2023
  ORDER BY AthleticScore DESC
  LIMIT 15;
")

#explosiveness score - based on vertical and broad jump
sqldf("
  SELECT Player, PositionGroup,Drafted,
         (Vertical) + BroadJump AS ExplosivenessScore
  FROM nfl_C
  ORDER BY ExplosivenessScore DESC
  LIMIT 20;
")

#create scatter plots comparing player metrics
ggplot(nfl_C, aes(x=Weight, y=X40yd,color=Drafted))+
  geom_jitter()+
  geom_smooth(aes())

ggplot(nfl_C, aes(x=height_total_in, y=Vertical,color=Drafted))+
  geom_jitter()+
  geom_smooth()

ggplot(nfl_C, aes(x=Weight, y=Broad.Jump,color=Drafted))+
  geom_jitter()+
  geom_smooth()

#create boxplots comparing 40yd times of players who were drafted or not
boxplot(nfl_A$X40yd, #only players who were drafted
        main="Average Drafted 40yd Dash",
        xlab="Time",
        ylab="",
        col="purple",
        horizontal = TRUE
)
summary(nfl_A$X40yd)

boxplot(nfl_B$X40yd, #only players who were drafted
        main="Average Undrafted 40yd Dash",
        xlab="Time",
        ylab="",
        col="purple",
        horizontal = TRUE
)
summary(nfl_B$X40yd)

#comparing 40-yard times across position groups -- only looking at Drafted
ggplot(nfl_A, aes(PositionGroup, X40yd)) +
  geom_boxplot() +
  labs(title = "40-Yard Dash by Position Group")

#comparing 3-cone drill times across position groups -- only looking at Drafted
ggplot(nfl_A, aes(PositionGroup, X3Cone)) +
  geom_boxplot() +
  labs(title = "3-Cone Drill Times by Position Group")

#delete inches, feet, and Year columns from nfl_C
nfl_C$inches <- NULL
nfl_C$feet <- NULL
nfl_C$Year <- NULL
nfl_C<-na.omit(nfl_C)
nfl_C<-na.omit(nfl_C)

# 1. Keep only numeric variables (adjust if you want a subset)
combine_num <- nfl_C %>%
  select(where(is.numeric))   # or: select(Forty, Vertical, BroadJump, Bench, Shuttle, ThreeCone)

# 2. Compute correlation matrix
cor_mat <- cor(combine_num, use = "pairwise.complete.obs")
#convert correlation matrix
cor_df <- cor_mat %>%
  as.data.frame() %>%
  rownames_to_column(var = "Var1") %>%
  pivot_longer(
    cols = -Var1,
    names_to = "Var2",
    values_to = "Correlation"
  )
#correlation heatmap
ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(
    limits = c(-1, 1),
    midpoint = 0,
    low = "blue",
    mid = "white",
    high = "red"
  ) +
  coord_fixed() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  labs(
    title = "Correlation Heatmap of NFL Combine Metrics",
    x = NULL,
    y = NULL
  )

#build a density plot comparing 40-yard times across positions
library(sqldf)

nfl_subset1 <- sqldf("
  SELECT *
  FROM nfl_C
  WHERE PositionGroup IN ('RB', 'WR', 'DB')
")

ggplot(nfl_subset1, aes(Vertical, fill = PositionGroup)) +
  geom_density(alpha = 0.4)


##################
#####MODELING#####
##################

model <- lm(X3Cone ~ X40yd + Shuttle, data=nfl_C)
summary(model)

#actual vs. predicted plot - speed drills
ggplot(data = data.frame(fitted = model$fitted.values,
                         residuals = nfl_C$X3Cone))+
  geom_point(aes(x=fitted, y=residuals))+
  labs(x="Predicted",y="Actual",title="Actual vs. Predicted Plot")

#use a query to join nfl_C with nfl_combine Round and Pick
nfl_drafted <- sqldf("
  SELECT a.*, nc.Round, nc.Pick
  FROM nfl_A a
  LEFT JOIN (SELECT Pos, Player, Round, Pick 
            FROM nfl_combine) nc
  ON nc.Pos = a.Pos AND nc.Player = a.Player;
")

#select only WRs (for analysis later)
nfl_drafted_wr <- sqldf("
  SELECT *
  FROM nfl_drafted
  WHERE Pos='WR'")

#WR draft model - what pick overall would they be based off their metrics
draft_model <- lm(Pick ~ Weight+X40yd+Vertical+Bench+BroadJump+X3Cone+Shuttle, data=nfl_drafted_wr)

summary(draft_model)

#actual vs. Predicted Plot
ggplot(data = data.frame(fitted = draft_model$fitted.values,
                         residuals = nfl_drafted_wr$Pick)) +
  geom_point(aes(x = fitted, y = residuals)) +
  labs(x = "Predicted", 
       y = "Actual",
       title= "Actual vs. Predicted Plot")

#ONLY LOOKING AT 40YD
draft_model40 <- lm(Pick ~ X40yd, data=nfl_drafted_wr)
summary(draft_model40)

#actual vs. Predicted Plot
ggplot(data = data.frame(fitted = draft_model40$fitted.values,
                         residuals = nfl_drafted_wr$Pick)) +
  geom_point(aes(x = fitted, y = residuals)) +
  labs(x = "Predicted", 
       y = "Actual",
       title= "Actual vs. Predicted Plot")

#save nfl_C as a tidy csv 
write.csv(nfl_C,"tidy_data.csv",row.names = FALSE)
