
source("./starter.R")

## Load Data
# Assuming the data is in the same directory with the script.

allData <- read_csv("./replicationData.csv")

## Summary Table 

SumTable <- rbind(summaryTable(allData$anycoupatt_fin),
                  summaryTable(allData$rechangeatt_fin),
                  summaryTable(allData$reshuffleatt_fin),
                  summaryTable(allData$gwf_legislature),
                  summaryTable(allData$personalism),
                  summaryTable(allData$gwf_leadermil),
                  summaryTable(allData$ll_rgdpe_PW),
                  summaryTable(allData$lag_growth_rgdpe_PW),
                  summaryTable(allData$ll_pop_PW),
                  summaryTable(allData$postcold))

## row and column names for the summary table.

colnames(SumTable) <- c("Obs.","Mean","Std.Dev.","Min","Max","Median")
rownames(SumTable) <- c("Any coup attempt",
                        "Reshuffling coup attempt",
                        "Regime-change coup attempt",
                        "Legislature",
                        "Personalism",
                        "Military leader",
                        "ln(GDP/capita)",
                        "Economic Growth",
                        "ln(Population)",
                        "Post-Cold War")


## You can find the summary table in the Appendix Table 1 
# or comment out the name below to see it in R.

## Show Table
SumTable

## Appendix A1
## For Latex
## kable(SumTable,digits = 3, caption = "Summary Statistics", booktabs = T, format = "latex")

### Personalism Through Years, Figure 1 Main Article. 

fig1Data <- ddply(allData, .(region,year),getMean)

colnames(fig1Data) <- c("Regions","Years","Personalism")

ggplot(data=fig1Data, aes(x=Years, y=Personalism, color=Regions)) +
  geom_line() + labs(title = 'Mean Personalism Levels Through Years') + xlab("Years") +
  ylab("Mean Level of Personalism") + theme_minimal()  + theme(legend.position="bottom")

ggsave("./Figure2(Personalism).png", width = 20, height = 10, units = "cm")

### Reshuffle and Regime-Change Coups Through Years


fig2Data <- allData[which(allData$reshuffleatt_fin >0),c("year","personalism","reshuffleatt_fin","gwf_legislature")]

fig2Data$Legislature <- as.factor(fig2Data$gwf_legislature)


Shuffle <- ggplot(data=fig2Data, aes(x=year, y=personalism, color=Legislature)) +
  geom_point(shape=15) + geom_hline(yintercept=0.42, linetype = "dashed") + theme_minimal() + 
  ylab("Level of Personalism") +
  scale_colour_manual(labels = c("No Legislature", "Legislature"), values = c("#F8766D","#619CFF")) +
  ggtitle("Reshuffling Coups Through Years") +
  theme(legend.position="bottom")

##ggsave("./Figure2(Reshuffle-Coups).png", width = 20, height = 10, units = "cm")



## Regime-Change Coups Through Years
fig3Data <- allData[which(allData$rechangeatt_fin >0),c("year","personalism","rechangeatt_fin","gwf_legislature")]

fig3Data$Legislature <- as.factor(fig3Data$gwf_legislature)

ReChange <- ggplot(data=fig3Data, aes(x=year, y=personalism, color=Legislature)) +
  geom_point(shape=17) + geom_hline(yintercept=0.42, linetype = "dashed") + theme_minimal() + 
  ylab("Level of Personalism") +
  scale_colour_manual(labels = c("No Legislature", "Legislature"), values = c("#F8766D","#619CFF")) +
  ggtitle("Regime-Change Coups Through Years") +
  theme(legend.position="bottom")

##ggsave("./Figure3(Regime-ChangeCoups).png", width = 20, height = 10, units = "cm")

pdf("./Figure1(Coups).pdf",width=8, height=5)
grid.arrange(Shuffle,ReChange, nrow = 2)
dev.off()


## Some Percentages

# Percentage of Regime Change and Reshuffling coups against all coups
nrow(fig2Data[,"reshuffleatt_fin"])/(nrow(fig2Data[,"reshuffleatt_fin"])+nrow(fig3Data[,"rechangeatt_fin"]))*100
nrow(fig3Data[,"rechangeatt_fin"])/(nrow(fig2Data[,"reshuffleatt_fin"])+nrow(fig3Data[,"rechangeatt_fin"]))*100

# Percentage of country years with Legislative
nrow(allData[which(allData$gwf_legislature == 1),"gwf_legislature"])/nrow(allData[,"gwf_legislature"])*100

# Percentage of Reshuffling Coups that occur in a country year that has lower than median personalism level
nrow(fig2Data[which(fig2Data$personalism <= 0.42),
              "reshuffleatt_fin"])/nrow(fig2Data[,"reshuffleatt_fin"])*100

# Percentage of Reshuffling Coups that occur in a country year that has a Legislature
nrow(fig2Data[which(fig2Data$gwf_legislature == 1),
              "reshuffleatt_fin"])/nrow(fig2Data[,"reshuffleatt_fin"])*100

# Share of Regime Change Coups that occur in a country year that has lower than median personalism level
nrow(fig3Data[which(fig3Data$personalism <= 0.42),
              "rechangeatt_fin"])/nrow(fig3Data[,"rechangeatt_fin"])*100

# Share of Regime Coups that occur in a country year that has lower than median personalism level 
# and has a Legislature
nrow(fig3Data[which(fig3Data$personalism <= 0.42 & fig3Data$gwf_legislature == 1),
              "rechangeatt_fin"])/nrow(fig3Data[which(fig3Data$personalism <= 0.42),
                                                "rechangeatt_fin"])*100
