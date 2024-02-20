#load libraries
library(dplyr) #find alternatives
library(tidyverse) #find alternatives
library(SingleCaseES) #find alternatives ?
library(metafor)

#list WDs
wd_data <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Summarise results'

#load data
setwd(wd_data)
load('Clean_MeData_dd_lis.Rdata')
load('pairs.Rdata')

# unite the dark diversity data with the pairs classification (with dplyr)
log_data <- left_join(dd_data, pairs) 

# unite the dark diversity data with the pairs classification (without dplyr)
log_data2 <- merge(dd_data, pairs, all.x = T, sort = F,
                  by = c('data.origin', 'country', 'Site', 'enforcement'))

# remove the data that dont belong to a MPA-control pair (with tidyr)
log_data <- log_data %>% drop_na(pair)

# remove the data that dont belong to a MPA-control pair (without tidyr)
log_data2 <- log_data2[complete.cases(log_data2$pair),]

# fix capital letter in pairs (with stringr)
log_data$pair <- str_to_title(log_data$pair)

# fix capital letter in pairs (without stringr)
log_data2$pair <- paste0(toupper(substr(log_data2$pair, 1, 1)), 
                          substr(log_data2$pair, 2, nchar(log_data2$pair)))

# convert pair to factor
log_data$pair <- as.factor(log_data$pair)
log_data2$pair <- as.factor(log_data2$pair)

# remove unnecessary columns (with dplyr)
log_data <- log_data %>% select(pair,protection,enforcement,dark,observed_richness,total) 

# remove unnecessary columns (without dplyr)
log_data2 <- data.frame(pair = log_data2$pair, protection = log_data2$protection,
                        enforcement =  log_data2$enforcement, 
                        dark =  log_data2$dark,
                        observed_richness =  log_data2$observed_richness,
                        total =  log_data2$total)


# calculate completeness (with dplyr)
log_data <- log_data %>% mutate("completeness" = log(observed_richness/dark))

# calculate completeness (without dplyr)
log_data2$completeness <- log(log_data2$observed_richness / log_data2$dark)

# convert to long format (with tidyr)
log_data <- gather(log_data, "index", "richness", 4:7)

# convert to long format (without tidyr)
log_data2_A <- log_data2[,c(1:3)]
log_data2_A$index <- 'dark'
log_data2_A$richness <- log_data2$dark

log_data2_B <- log_data2[,c(1:3)]
log_data2_B$index <- 'observed_richness'
log_data2_B$richness <- log_data2$observed_richness

log_data2_C <- log_data2[,c(1:3)]
log_data2_C$index <- 'total'
log_data2_C$richness <- log_data2$total

log_data2_D <- log_data2[,c(1:3)]
log_data2_D$index <- 'completeness'
log_data2_D$richness <- log_data2$completeness

log_data2 <- rbind(log_data2_A, log_data2_B, log_data2_C, log_data2_D)

# create list of all fully protected MPAs (with dplyr)
fully_protected_pairs <- log_data %>% filter(enforcement>2) %>% distinct(pair)

# create list of all fully protected MPAs (without dplyr)
fully_protected_pairs2 <- as.character(unique(log_data2$pair[log_data2$enforcement > 2]))

# Change protected/no protected to MPA vs Control
log_data$protection <- ifelse(log_data$protection == "Protected", "MPA", "Control")
log_data2$protection <- ifelse(log_data2$protection == "Protected", "MPA", "Control")

# keep the dark diversity index only (with dplyr)
d_ratio <- log_data %>% filter(index == "dark") 

# keep the dark diversity index only (without dplyr)
d_ratio2 <- log_data2[log_data2$index == 'dark',]


######## keep going just with the code ##########

# apply the `batch_calc_ES` to recive log-ratios
d_ratio <-  batch_calc_ES(dat = d_ratio, 
                          grouping =  pair, 
                          condition = protection, 
                          outcome = richness,
                          scale = "count", 
                          ES = "LRRi",
                          baseline_phase  = "Control")


# arrange the order of pairs by the effect size

d_ratio <- d_ratio %>% mutate(pair = fct_reorder(pair,Est))

d_ratio$Index = "Dark diversity"

# keep only observed richness

o_ratio <- log_data %>% filter(index == "observed_richness")

# apply the `batch_calc_ES` to get log-ratios

o_ratio <-  batch_calc_ES(dat = o_ratio,
                          grouping = pair,
                          condition = protection,
                          outcome = richness,
                          scale = "count",
                          ES = "LRRi",
                          baseline_phase = "Control")

o_ratio$Index<-"Observed richness"

# keep only species pool (total)

t_ratio <- log_data %>% filter(index == "total")

# apply the `batch_calc_ES` to get log-ratios

t_ratio <-  batch_calc_ES(dat = t_ratio,
                          grouping = pair,
                          condition = protection,
                          outcome = richness,
                          scale = "count",
                          ES = "LRRi",
                          baseline_phase = "Control")

t_ratio$Index <- "Species pool"


###### PLOT THIS IN BASE

plot_log_ratio<- ggplot(data = d_ratio,
                aes(x=Est,y=pair,color = Index))+
  geom_vline(xintercept = 0,linetype = "dashed",size = 1)+
  geom_point(size = 5)+
  geom_point(data = o_ratio,aes(x=Est,y= pair,color=Index),size = 5)+
  geom_point(data = t_ratio,aes(x=Est,y= pair,color=Index),size =5)+
  theme_classic()+
  ylab("MPA")+
  xlab("Log ratio")+
  geom_errorbar(data = d_ratio,aes(xmin= Est-SE, xmax=Est+SE), width=.2)+
  geom_errorbar(data = o_ratio ,aes(xmin= Est-SE, xmax=Est+SE), width=.2)+
  geom_errorbar(data = t_ratio ,aes(xmin= Est-SE, xmax=Est+SE), width=.2)+
  scale_color_manual(values=c('#7DCDE4',"#F2E96C","#ACD672"))+
  theme_classic()+
  theme(legend.position = 'none')

#get the things I need to plot and put them in normal formats

d_ratio <- as.data.frame(d_ratio)
d_ratio <- d_ratio[order(d_ratio$Est),] #order for the plot

o_ratio <- as.data.frame(o_ratio)

t_ratio <- as.data.frame(t_ratio)

#calculate the extremes for the plots
ext_d <- d_ratio

#get the xlim for the plot
x_lim <- range(c(d_ratio$Est, o_ratio$Est, t_ratio$Est))

#make an empty plot
plot(1, type = "n", xlab = "", 
     ylab = "", xlim = x_lim,  
     ylim = c(0, nrow(d_ratio))) 





metafor_dark <- log_data %>%
  filter(index =="dark") %>%
  group_by(pair,protection) %>% 
  summarise("mean"= mean(richness),
            "sd"=sd(richness),
            "n" = n()) %>% 
  pivot_wider(names_from=protection, 
              values_from=c(mean, 
                            sd,
                            n)) 




models_effect_size = ggplot(effects, aes(x = factor(Predictor,  level = level_order),y = Estimate)) +
  geom_point(size=4, aes(col = Trait_category)) +
  coord_flip() +
  facet_grid(~stage)+
  geom_errorbar(aes(ymin=Q2.5,ymax=Q97.5, col = Trait_category),size = 1,width=0.3) +
  scale_color_manual(values = c("black","#23D19D","#E0CDE0"),labels = c("Abundance","Adult","Planktonic"))+
  geom_hline(yintercept = 0)+
  theme_bw() +
  theme(strip.text.x = element_text(size = 16)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16)) +
  theme(axis.line = element_line(colour = "gray48"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(x = "Predcitor", colour = "Trait category")
