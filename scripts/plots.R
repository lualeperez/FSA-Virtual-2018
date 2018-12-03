### Script with static summary plots (i.e. not map-based)

cat("\n\n")
cat(paste("Start static plots script","\n",sep=""))

# load labelled data
load(image_analysis)
options(warn=-1) # to stop receiving warnings from coercion and from reseting x and y axis during plotting

# output directory where the plots are saved
out.dir <- file.path(plots_output_dir)

# Removal of News as a datasource from our data
labelled.df <- subset(labelled.df, source != "News")

library(tidyr)
library(magrittr)
library(dplyr)

# Long Dataframe needed for certain types of plots.
labelled.df.long <- gather(labelled.df, Allergen, "Mentions", c(fourteen.allergen.names,other.allergen.names), factor_key = TRUE)

# data.frame with the number of mentions grouped by source and Allergen
allergen.bysource.df <- labelled.df.long %>%
  group_by(source, Allergen) %>%
  summarise(count=sum(Mentions))

library(ggplot2)
library(forcats)

# Plot with the 14 allergen mentions by source
#  Use of reorder() to order allergens by count DESC
#  Use of gsub() to replace "_" by spaces in the axis labels
fourteen.bysource <- ggplot(subset(allergen.bysource.df, Allergen %in% fourteen.allergen.names),
                                 aes(x = reorder(gsub("_"," ",Allergen), count), y = count, fill = source)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_brewer(palette="Spectral") +
  xlab("Allergen")+
  ylab("Mentions") +
  ggtitle("Mentions of the 14 Allergens by Source")+
  coord_flip()
fourteen.bysource

ggsave(paste("14_allergens_bysource.",output_format,sep = ""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)


# Plot with the other allergen mentions by source
other.bysource <- ggplot(subset(allergen.bysource.df,
                                Allergen %in% other.allergen.names),
                                aes(x = reorder(gsub("_"," ", Allergen),count), y = count, fill = source)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_brewer(palette="Spectral") +
  xlab("Allergen") +
  ylab("Mentions") +
  ggtitle("Mentions of Other Allergens by Source") +
  coord_flip()
other.bysource

ggsave(paste("other_allergens_bysource.",output_format,sep = ""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)



#### TOP 10 ALL Allergens Histograme
# Get the top-10 allergens mentioned in our data
total_count_per_allergen <- labelled.df %>% subset(select = c(fourteen.allergen.names,other.allergen.names)) %>% colSums(na.rm=T) %>% sort(decreasing=T)
top_10_names <- names(total_count_per_allergen)[1:10] # top-10 name list
top_4_names  <- top_10_names[1:4]                     # top-4  name list

# top-10 allergens mentions by source
top10.bysource <- ggplot(subset(allergen.bysource.df, Allergen %in% top_10_names),
                            aes(x = reorder(gsub("_"," ",Allergen), count), y = count, fill = source)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_brewer(palette="Spectral") +
  xlab("Allergen")+
  ylab("Mentions") +
  ggtitle("TOP 10 Allergens by Source")+
  coord_flip()
top10.bysource

ggsave(paste("top10_allergens.",output_format,sep = ""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)



library(scales)
library(ggrepel)

# get the 14 allergens mentions only for Twitter
all_allergens.norm.df.t14 <- subset(labelled.df.long,
                                    source == "Twitter" &
                                    Allergen %in% fourteen.allergen.names)
# Remove "_" from allergens name
all_allergens.norm.df.t14$Allergen <- gsub("_"," ",all_allergens.norm.df.t14$Allergen)

# Plot with the 14 allergens mentions on Twitter over time (month)
by.month.twitter.14 <- ggplot(all_allergens.norm.df.t14,
                              aes(x = Month, y = Mentions, colour = Allergen),
                              group = Allergen) +
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "line") +
  theme_minimal()+
  theme(legend.position="bottom")+
  facet_grid(sentiment_class~.)
by.month.twitter.14

ggsave(paste("14_allergens_bymonth.",output_format,sep =""), plot = last_plot(), device = NULL, path = out.dir,
       width = 30, height = 20, units = "cm",
       dpi = 300)

# Plot with the 14 allergens mentions on Twitter over time (week)
by.week.twitter.14 <- ggplot(all_allergens.norm.df.t14,
                             aes(x = Week, y = Mentions, colour = Allergen),
                             group = Allergen) +
  stat_summary(fun.y = sum, # adds up all observations for the week
               geom = "line") +
  theme_minimal() +
  theme(legend.position="bottom") +
  facet_grid(sentiment_class~.) +
  ggtitle("14 Allergen Mentions over Time (Twitter Only)")+
  theme(strip.text.y = element_text(angle = 0))
by.week.twitter.14

ggsave(paste("14_allergens_byweek.",output_format,sep =""), plot = last_plot(), device = NULL, path = out.dir,
       width = 30, height = 20, units = "cm",
       dpi = 300)

# get the other allergens mentions only for Twitter
all_allergens.norm.df.t.other <- subset(labelled.df.long, source == "Twitter" & Allergen %in% other.allergen.names)
# Remove "_" from allergens name
all_allergens.norm.df.t.other$Allergen <- gsub("_"," ",all_allergens.norm.df.t.other$Allergen)

# Plot with the other allergens mentions on Twitter over time (month)
by.month.twitter.other <- ggplot(all_allergens.norm.df.t.other, aes(x = Month, y = Mentions, colour = Allergen), group = Allergen)+
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "line") +
  theme_minimal()+
  theme(legend.position="bottom") +
  facet_grid(sentiment_class~.) +
  theme(strip.text.y = element_text(angle = 0))
by.month.twitter.other

ggsave(paste("other_allergens_bymonth.", output_format, sep = ""), plot = last_plot(), device = NULL, path = out.dir,
       width = 30, height = 20, units = "cm",
       dpi = 300)

# Plot with the other allergens mentions on Twitter over time (week)
by.week.twitter.other <- ggplot(all_allergens.norm.df.t.other, aes(x = Week, y = Mentions, colour = Allergen), group = Allergen)+
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "line") +
  theme_minimal()+
  theme(legend.position="bottom") +
  facet_grid(sentiment_class~.) +
  ggtitle("Other Allergen Mentions over Time (Twitter Only)")+
  theme(strip.text.y = element_text(angle = 0))
by.week.twitter.other

ggsave(paste("other_allergens_byweek.", output_format, sep = ""), plot = last_plot(), device = NULL, path = out.dir,
       width = 30, height = 20, units = "cm",
       dpi = 300)


#################################################################################
## Intersections of the common allergens labels and contextual issues labelling #
#################################################################################

# Plot with the allergen enquiries separated by source and sentiment
enquiries_source_react.bar <- ggplot(labelled.df %>%
                                     group_by(source, sentiment_class) %>%
                                     summarise(count = sum(allergy_enquiries)),
                                     aes(x = source, y = count, fill = sentiment_class)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x= "Source", y="Allergen Enquiries", fill = "Sentiment Class") +
  scale_fill_manual(values = c("#DF3309","grey80","#0D83E6"))+
  ggtitle("Allergen Enquiries by Source and Sentiment Class")
enquiries_source_react.bar

ggsave(paste("food_enquiries_bysource.",output_format,sep = ""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Plot with the food labelling separated by source and sentiment
labelling_source_react.bar <- ggplot(labelled.df %>%
                                     group_by(sentiment_class, source) %>%
                                     summarise(count = sum(food_labelling)),
                                     aes(x = source, y = count, fill = sentiment_class))+
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x= "Source", y="Mentions Flagged for Food Labelling", fill = "Sentiment Class") +
  scale_fill_manual(values = c("#DF3309","grey80","#0D83E6"))+
  ggtitle("Food Labelling Mentions by Source and Sentiment Class")
labelling_source_react.bar

ggsave(paste("labelling_by_source_and_reaction.", output_format, sep = ""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)



### Contextual meaning issues combined in a single plot by sentiment class
stream1.issues.names   <- c("allergy_enquiries","food_labelling","mild_reaction","severe_reaction")
stream1.issues.df      <- subset(labelled.df, select = c(stream1.issues.names,"sentiment_class"))
stream1.issues.df.long <- gather(stream1.issues.df, Issue, "Mentions", stream1.issues.names, factor_key = TRUE)

# Contextual meaning issues grouped by Issue and sentiment_class
stream1.issues.sentiment.groupedby <-stream1.issues.df.long %>%
  group_by(Issue, sentiment_class) %>%
  summarise(counts = sum(Mentions))

# Plot with the contextual issues separated by sentiment
stream1.issues.bar <- ggplot(stream1.issues.sentiment.groupedby,
                             aes(x = gsub("_", " ", Issue), y = counts, fill = sentiment_class)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x= "Issues", y="Mentions", fill = "Sentiment Class") +
  scale_fill_manual(values = c("#DF3309","grey80","#0D83E6"))+
  theme(legend.position="bottom") +
  ggtitle("Sentiment Analysis by Context")
stream1.issues.bar

ggsave(paste("Summary_of_Contextual_info_bar.", output_format, sep = ""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)


### Percentage of 14 allergens mentions for which we have a mild/severe reaction
fourteen.allergen.mild   <- colSums(labelled.df[labelled.df$mild_reaction == 1,   fourteen.allergen.names])
fourteen.allergen.severe <- colSums(labelled.df[labelled.df$severe_reaction == 1, fourteen.allergen.names])
fourteen.allergen.total  <- colSums(labelled.df[,fourteen.allergen.names])

fourteen.allergens.total.df <- data.frame(fourteen.allergen.mild,fourteen.allergen.severe,fourteen.allergen.total)

colnames(fourteen.allergens.total.df) <- c("mild","severe","total")

fourteen.allergens.total.df$perc_mild   <- round((fourteen.allergens.total.df$mild/fourteen.allergens.total.df$total)*100,1)
fourteen.allergens.total.df$perc_severe <- round((fourteen.allergens.total.df$severe/fourteen.allergens.total.df$total)*100,1)
fourteen.allergens.total.df$allergen    <- rownames(fourteen.allergens.total.df)

fourteen.allergen.total.long <- gather(fourteen.allergens.total.df, severity, "percentage", c("perc_mild","perc_severe"), factor_key = TRUE)

fourteen.allergen.mild   <- colSums(labelled.df[labelled.df$mild_reaction == 1,   fourteen.allergen.names])





# Plot with the fraction of mild/severe adverse reactions for each of the official 14 list allergens 
fourteen.allergen.mentions <- ggplot(fourteen.allergen.total.long,
                                     aes(x = reorder(gsub("_"," ", allergen), total),
                                         y = percentage, fill = severity)) +
  geom_bar(width = 0.4 ,position = "dodge", stat="identity") +
  theme_minimal() +
  scale_fill_manual(values=c("#ffa64d", "#cc0000"),
                    breaks=c("perc_mild", "perc_severe"),
                    labels=c("% Mild", "% Severe")) +
  labs(x= "Allergens", y="Percentage", fill = "Severity") +
  ggtitle("Percentage of Mild/Severe reactions over 14 allergens mentions") +
  coord_flip()
fourteen.allergen.total.long

ggsave(paste("Percentage_14_allergens_mentions_mild_severe.", output_format, sep = ""), plot = last_plot(), device = NULL, path = out.dir,
       width = 20, height = 15, units = "cm",
       dpi = 300)



fourteen.allergen.mentions


# Plot with the fraction of mild/severe adverse reactions for each of the other list allergens

other.allergen.mild   <- colSums(labelled.df[labelled.df$mild_reaction == 1,   other.allergen.names])
other.allergen.severe <- colSums(labelled.df[labelled.df$severe_reaction == 1, other.allergen.names])
other.allergen.total  <- colSums(labelled.df[,other.allergen.names])

other.allergens.total.df <- data.frame(other.allergen.mild,other.allergen.severe,other.allergen.total)


colnames(other.allergens.total.df) <- c("mild","severe","total")

other.allergens.total.df$perc_mild   <- round((other.allergens.total.df$mild/other.allergens.total.df$total)*100,1)
other.allergens.total.df$perc_severe <- round((other.allergens.total.df$severe/other.allergens.total.df$total)*100,1)
other.allergens.total.df$allergen    <- rownames(other.allergens.total.df)

other.allergen.total.long <- gather(other.allergens.total.df, severity, "percentage", c("perc_mild","perc_severe"), factor_key = TRUE)

other.allergen.mild   <- colSums(labelled.df[labelled.df$mild_reaction == 1,   other.allergen.names])

#choose top 5 other allergens 
other.allergen.total.long_subset <- subset(other.allergen.total.long, allergen=="coconut" | allergen=="fungus" | allergen=="banana" 
                                           | allergen=="seeds" | allergen=="vegetable")


# Plot with the fraction of mild/severe adverse reactions for each of the other allergens
other.allergen.mentions <- ggplot(other.allergen.total.long_subset,
                                  aes(x = reorder(gsub("_"," ", allergen), total),
                                      y = percentage, fill = severity)) +
  geom_bar(width = 0.4 ,position = "dodge", stat="identity") +
  theme_minimal() +
  scale_fill_manual(values=c("#ffa64d", "#cc0000"),
                    breaks=c("perc_mild", "perc_severe"),
                    labels=c("% Mild", "% Severe")) +
  labs(x= "Other Allergens", y="Percentage", fill = "Severity") +
  ggtitle("Percentage of Mild/Severe reactions over other allergens") +
  coord_flip() + theme_bw()

other.allergen.mentions <-other.allergen.mentions + theme(axis.text.x=element_text(size=14)  ,axis.text=element_text(size=14),
                                                          axis.title=element_text(size=16,face="bold"), title = element_text(size=14), axis.text.y =element_text(size=18),
                                                          axis.title.x =element_text(size=16,face="bold"), axis.title.y.right =element_text(size=16,face="bold"),
                                                          axis.title.x.bottom =element_text(size=16,face="bold")) + guides(color=guide_legend("Severity"), element_text(size=16)) 

other.allergen.mentions

ggsave(paste("Percentage_other_allergens_mentions_mild_severe.", output_format, sep = ""), plot = last_plot(), device = NULL, path = out.dir,
       width = 20, height = 15, units = "cm",
       dpi = 300)

#######
## Subsetting to remove "_" from the Allergy names
int_14allergen_react.df          <- subset(labelled.df.long, Mentions > 0 & Allergen %in% fourteen.allergen.names)
int_14allergen_react.df$Allergen <- gsub("_"," ", int_14allergen_react.df$Allergen)

# Plot with the Adverse reactions reports over time for each of the 14 allergens list in "bar format"
int_14allergen_react <- ggplot(int_14allergen_react.df,
                               aes(x = Week, y = reactions_report, fill = reactions_report))+
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar") +
  scale_fill_manual(values = c("grey90","yellow","red"))+
  labs(x="Month", y="Mentions", fill="Class of Reported Reaction")+
  scale_x_date(breaks = "month")+
  ggtitle("Reactions to 14 Allergen Mentions Over Time (All Sources)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_blank())+
  theme(panel.grid.minor = element_blank())+
  theme(strip.text.y = element_text(angle = 0))+
  theme(legend.position="bottom")+
  facet_grid(Allergen~.)
int_14allergen_react

ggsave(paste("14_allergens_reactions.",output_format, sep = ""), plot = last_plot(), device = NULL, path = out.dir,
       width = 30, height = 20, units = "cm",
       dpi = 300)


# Plot with the Adverse reactions reports over time for each of the 14 allergens list in "bubble format"
allergen.react.df <- subset(labelled.df.long, Allergen %in% fourteen.allergen.names) %>%
  group_by(Allergen, Month, sentiment_class, reactions_report) %>%
  summarise(Count= sum(Mentions))

#Remove "_" from allergens names
allergen.react.df$Allergen <- gsub("_"," ",allergen.react.df$Allergen)
allergen.react.bubble <- ggplot(allergen.react.df, aes(x = Month, y = fct_reorder(Allergen, Count),
                                                size = ifelse(Count == 0, NA, Count),
                                                colour = reactions_report))+
  geom_point()+
  scale_size_area(max_size = 10)+
  scale_colour_manual(values = c("grey90","yellow","red"))+
  labs(x="Month", y="Allergen", size="Number of Mentions",
       col="Class of Reported Reaction")+
  scale_x_date(breaks = "month")+
  ggtitle("Reactions to 14 Allergen Mentions Over Time (All Sources)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))+
  theme(panel.grid.minor = element_blank())+
  theme(strip.text.y = element_text(angle = 0))+
  facet_grid(sentiment_class~.)
allergen.react.bubble

ggsave(paste("14_allergens_reactions_bubble.", output_format, sep = ""), plot = last_plot(), device = NULL, path = out.dir,
       width = 30, height = 25, units = "cm",
       dpi = 300)

# Plot with the food labelling issues over time for each of the 14 allergens list in "bubble format"
allergen.react.labelling.df <- subset(labelled.df.long, Allergen %in% fourteen.allergen.names & food_labelling > 0) %>%
  group_by(Allergen, Month, sentiment_class, reactions_report) %>%
  summarise(Count= sum(Mentions))

#Remove "_" from allergen names
allergen.react.labelling.df$Allergen <- gsub("_", " ", allergen.react.labelling.df$Allergen)

allergen.react.labelling.bubble <- ggplot(allergen.react.labelling.df, aes(x = Month, y = fct_reorder(Allergen, Count),
                                                       size = ifelse(Count == 0, NA, Count),
                                                       colour = reactions_report))+
  geom_point()+
  scale_size_area(max_size = 10)+
  scale_colour_manual(values = c("grey90","yellow","red"))+
  labs(x="Month", y="Allergen", size="Number of Mentions",
       col="Class of Reported Reaction")+
  scale_x_date(breaks = "month")+
  ggtitle("Reactions to 14 Allergen Mentions Over Time (All Sources, Flagged under Food Labelling)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))+
  theme(panel.grid.minor = element_blank())+
  theme(strip.text.y = element_text(angle = 0))+
  facet_grid(sentiment_class~.)
allergen.react.labelling.bubble

ggsave(paste("14_allergens_labelling_bubble.", output_format, sep = ""), plot = last_plot(), device = NULL, path = out.dir,
       width = 30, height = 25, units = "cm",
       dpi = 300)

# Plot with the allergen enquiries over time for each of the 14 allergens list in "bar format"
allergen.react.enquiries.df <- subset(labelled.df.long, Allergen %in% fourteen.allergen.names & allergy_enquiries > 0) %>%
  group_by(Allergen, Month, sentiment_class, reactions_report) %>%
  summarise(Count= sum(Mentions))

allergen.react.enquiries.bubble <- ggplot(allergen.react.enquiries.df, aes(x = Month, y = fct_reorder(Allergen, Count),
                                                                           size = ifelse(Count == 0, NA, Count),
                                                                           colour = reactions_report))+
  geom_point()+
  scale_size_area(max_size = 10)+
  scale_colour_manual(values = c("grey90","yellow","red"))+
  labs(x="Month", y="Allergen", size="Number of Mentions",
       col="Class of Reported Reaction")+
  scale_x_date(breaks = "month")+
  ggtitle("Reactions to 14 Allergen Mentions Over Time (All Sources, Flagged under Allergy Enquiries)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))+
  theme(strip.text.y = element_text(angle = 0))+
  theme(panel.grid.minor = element_blank())+
  facet_grid(sentiment_class~.)
allergen.react.enquiries.bubble

ggsave(paste("14_allergens_enquiries_bubble.", output_format, sep = ""), plot = last_plot(), device = NULL, path = out.dir,
       width = 30, height = 25, units = "cm",
       dpi = 300)


#####
# Plot with the Adverse reactions reports over time for each of the other allergens list in "bar format"
# Subseting to remove "_" from allergen names
int_otherallergen_react.df <- subset(labelled.df.long, Mentions > 0 & Allergen %in% other.allergen.names)
int_otherallergen_react.df$Allergen <- gsub("_", " ", int_otherallergen_react.df$Allergen)
int_otherallergen_react <- ggplot(int_otherallergen_react.df,
                               aes(x = Week, y = reactions_report, fill = reactions_report))+
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar") +
  theme_minimal() +
  labs(x="Month", y="Mentions", fill="Class of Reported Reaction")+
  scale_fill_manual(values = c("grey90","yellow","red"))+
  ggtitle("Reported Reactions to Other Allergens Over Time (All Sources)")+
  scale_x_date(breaks = "month")+
  facet_grid(Allergen~.)+
  theme(strip.text.y = element_text(angle = 0))+
  theme(axis.text.y = element_blank())+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                 size = 12, hjust = 1))+
  theme(legend.position="bottom")+
  theme(panel.grid.minor = element_blank())
int_otherallergen_react

ggsave(paste("other_allergens_reactions.", output_format, sep = ""), plot = last_plot(), device = NULL, path = out.dir,
       width = 30, height = 30, units = "cm",
       dpi = 300)

# Plot with the allergen enquiries over time for each of the other allergens list in "bar format"
Int_enquiry_reaction <- ggplot(subset(labelled.df, allergy_enquiries > 0 ), aes(x = Week, y = allergy_enquiries, fill = reactions_report))+
  stat_summary(fun.y = sum, # adds up all observations for the week
               geom = "bar") +
  theme_minimal()+
  labs(x="Month", y="Allergy Enquiries", fill="Class of Reported Reaction")+
  theme(legend.position="bottom")+
  scale_x_date(breaks = "month")+
  scale_fill_manual(values = c("grey90","yellow","red"))+
  ggtitle("Allergen Enquiries Over Time by Sentiment Class (All Sources)")+
  facet_grid(sentiment_class~.)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))+
  theme(strip.text.y = element_text(angle = 0))+
  theme(panel.grid.minor = element_blank())
Int_enquiry_reaction

ggsave(paste("enquiries_reactions.", output_format, sep = ""), plot = last_plot(), device = NULL, path = out.dir,
       width = 30, height = 15, units = "cm",
       dpi = 300)

# Plot with the food labelling issues over time for each of the other allergens list in "bar format"
labelling_reaction <- ggplot(subset(labelled.df, food_labelling > 0 ), aes(x = Week, y = food_labelling, fill = reactions_report))+
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar") +
  theme_minimal()+
  labs(x="Month", y="Mentions Flagged for Food Labelling", fill="Class of Reported Reaction")+
  theme(legend.position="bottom")+
  scale_x_date(breaks = "month")+
  scale_fill_manual(values = c("grey90","yellow","red"))+
  ggtitle("Food Labelling Over Time by Sentiment Class (All Sources)")+
  facet_grid(sentiment_class~.)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))+
  theme(strip.text.y = element_text(angle = 0))+
  theme(panel.grid.minor = element_blank())
labelling_reaction

ggsave(paste("labelling_reactions.", output_format, sep = ""), plot = last_plot(), device = NULL, path = out.dir,
      width = 30, height = 15, units = "cm",
       dpi = 300)

#######################################

#Co-occurrence Heatmap among Allergens

#Creating Percentage Heatmap for Allergens with Co-occurances

# to create subset for 14 allergens and other allergens
fourteen_allergens.df.norm.subset <- labelled.df[,fourteen.allergen.names]
other_allergens.df.norm.subset <- labelled.df[,other.allergen.names]


# creating percentage matrix for heatmap for fourteen allergens

column_names = colnames(fourteen_allergens.df.norm.subset)
number_of_rows = nrow(fourteen_allergens.df.norm.subset)
number_of_cols = ncol(fourteen_allergens.df.norm.subset)
mat = matrix(list(), nrow=number_of_cols, ncol=number_of_cols)

for (i in 1:(number_of_cols)){
  for (j in 1:number_of_cols){
    temp = table(fourteen_allergens.df.norm.subset[,i] + fourteen_allergens.df.norm.subset[,j])
    count_one = table(fourteen_allergens.df.norm.subset[,i])
    number_of_positive = as.vector(count_one[names(count_one) ==1])
    count_of_both_present = as.vector(temp[names(temp) ==2]) # 1,1 - are cases where both are present
    if (length(count_of_both_present) == 0){
      count_of_both_present = 0
    }
    percentage_of_2 = 100*count_of_both_present/number_of_positive
    #print(c(column_names[i],column_names[j], percentage_of_2))
    mat[i, j] = percentage_of_2
  }
}

percentage_mat = data.frame(mat)
colnames(percentage_mat) = column_names
rownames(percentage_mat) = column_names
percentage_mat <- data.matrix(percentage_mat, rownames.force = NA)

library(reshape2)
library(ggplot2)
library(stringi)
melted_percentage_mat = melt(percentage_mat)


melted_percentage_mat$Var1 <- stri_replace_all_fixed(melted_percentage_mat$Var1, "_", " ")
melted_percentage_mat$Var2 <- stri_replace_all_fixed(melted_percentage_mat$Var2, "_", " ")


g2  <- ggplot(data = melted_percentage_mat, aes(Var1, Var2, fill = value))+  
  geom_tile(color = "white", aes(fill = value))+
  geom_text(aes(label = round(value, 1)))+
  scale_fill_gradient2(low = "white", high = "red", mid = "white",
                       midpoint = 0, limit = c(0,60), space = "Lab",
                       name="Percentage") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))+
  coord_fixed()

g2 <-g2 + ggtitle("Co-occurrence Heatmap for 14 Allergens") +
  xlab("Allergen") + ylab("Co-occurrence with")

g2 <-g2 + theme(axis.text.x=element_text(size=14),
                axis.title=element_text(size=16,face="bold"), title = element_text(size=16))

g2
# plot(p2)

ggsave(paste("percentage_14_allergens.", output_format, sep = ""), plot = last_plot(), device = NULL, path = out.dir,
       width = 30, height = 30, units = "cm",
       dpi = 500)


# creating percentage matrix for heatmap for other allergens

drops <- c("kidney_beans")
other_allergens.df.norm.subset <- other_allergens.df.norm.subset[ , !(names(other_allergens.df.norm.subset) %in% drops)]

column_names = colnames(other_allergens.df.norm.subset)
number_of_rows = nrow(other_allergens.df.norm.subset)
number_of_cols = ncol(other_allergens.df.norm.subset)
mat = matrix(list(), nrow=number_of_cols, ncol=number_of_cols)

for (i in 1:(number_of_cols)){
  for (j in 1:number_of_cols){
    temp = table(other_allergens.df.norm.subset[,i] + other_allergens.df.norm.subset[,j])
    count_one = table(other_allergens.df.norm.subset[,i])
    number_of_positive = as.vector(count_one[names(count_one) ==1])
    count_of_both_present = as.vector(temp[names(temp) ==2]) # 1,1 - are cases where both are present
    if (length(count_of_both_present) == 0){
      count_of_both_present = 0
    }
    percentage_of_2 = 100*count_of_both_present/number_of_positive
    #print(c(column_names[j],column_names[i], percentage_of_2))
    mat[i, j] = percentage_of_2
  }
}

percentage_mat = data.frame(mat)
colnames(percentage_mat) = column_names
rownames(percentage_mat) = column_names
percentage_mat <- data.matrix(percentage_mat, rownames.force = NA)
melted_percentage_mat_other = melt(percentage_mat)

melted_percentage_mat_other$Var1 <- stri_replace_all_fixed(melted_percentage_mat_other$Var1, "_", " ")
melted_percentage_mat_other$Var2 <- stri_replace_all_fixed(melted_percentage_mat_other$Var2, "_", " ")


# plot heatmap for other allergens

g2  <- ggplot(data = melted_percentage_mat_other, aes(Var1, Var2, fill = value))+  # plot heatmap 2 for 14 Allergens
  geom_tile(color = "white", aes(fill = value))+
  geom_text(aes(label = round(value, 1)))+
  scale_fill_gradient2(low = "white", high = "red", mid = "white",
                       midpoint = 0, limit = c(0,60), space = "Lab",
                       name="Percentage") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))+
  coord_fixed()

g2 <-g2 + ggtitle("Co-occurrence Heatmap for Other Allergens") +
  xlab("Allergens") + ylab("Co-occurrence with")

g2 <-g2 + theme(axis.text.x=element_text(size=14)  ,axis.text=element_text(size=14),
                axis.title=element_text(size=16,face="bold"), title = element_text(size=16))

g2

# plot(p2)

ggsave(paste("percentage_other_allergens.", output_format, sep = ""), plot = last_plot(), device = NULL, path = out.dir,
       width = 40, height = 40, units = "cm",
       dpi = 300)


cat(paste("Finished static plots script","\n",sep=""))
cat("\n\n")

#


