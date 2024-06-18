#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/ggpubr")

library(ggplot2)
setwd("~/Desktop/Kramer Lab/R")
#ddcq calculated with average of the three technical duplicate cq values per primer

deltadeltaCRC2 <- read.csv("MqPCR_CRC2.csv")
deltadeltaCRC2 <- deltadeltaCRC2[c(1:29),c(1,5,10,12,13,14,15,16)]
colnames(deltadeltaCRC2) <- c("Sample","Target","ddCq", "plus","minus","species","region","number")
deltadeltaCRC2$number <- factor(deltadeltaCRC2$number)
show(deltadeltaCRC2)
# deltadeltaCRC2$plus <- log2(deltadeltaCRC2$plus)
# deltadeltaCRC2$minus <- log2(deltadeltaCRC2$minus)
# deltadeltaCRC2$ddCq <- log2(deltadeltaCRC2$ddCq)
# deltadeltaCRC2$line <- as.factor(substring(deltadeltaCRC2$sample, 2,2))
# deltadeltaCRC2$Tissue <- as.factor(substring(deltadeltaCRC2$sample, 0, 1))
# deltadeltaCRC2$seedling <- as.factor(substring(deltadeltaCRC2$seedling, 6, 15))
# levels(deltadeltaCRC2$line) <- c("M","E")
# deltadeltaCRC2$new = factor(deltadeltaCRC2$Target, levels=c("HXK1-1","HXK1-2"), labels=c("HXK1 5'","HXK1 3'"))
# strength <- c("+","+","-","-","+","+","-","-","-","-","+","+")
# deltadeltaCRC2$strength <- as.factor(strength)

#GGplot for Mc and Ml
ggplot(subset(deltadeltaCRC2, deltadeltaCRC2$species == "Mc" | deltadeltaCRC2$species == "Ml"),
       aes(x=number, y=ddCq, fill=region))+
  geom_col(width = 0.75, position = "dodge")+
  geom_errorbar(aes(x=number, ymin=minus, ymax=plus),
                position = position_dodge(width = 0.75), width=0.25,color="black")+
  theme_classic()+
  # theme_presentation()+
  xlab("Sample")+ylab("CRC2 transcript\nrelative fold change")+
  scale_y_continuous(expand = c(0,0), limits = c(0,2500))+
  # scale_fill_manual(values=c("#F3756C","#009ADE","#39B600"),
  #                   labels=c("Nectary","Middle","Blade"))+
  facet_grid(.~species, scales = "free")+
  # scale_fill_manual(values=c("#F3756C","#009ADE"))+
  theme(
    legend.title = element_text(colour="black",size=19),
    legend.text = element_text(colour="black",size=16),
    axis.title = element_text(colour="black",size=22),
    axis.text = element_text(colour="black",size=16),
    strip.text = element_text(colour="black",size=16),
    # legend.position="none",
    # axis.title.x=element_blank(),
    # axis.text.x = element_text(angle = 50, hjust = 1, vjust=1)
  )+
  labs(fill="Region")

#GGplot for Mp and Mv
ggplot(subset(deltadeltaCRC2, deltadeltaCRC2$species == "Mp" | deltadeltaCRC2$species == "Mv"),
       aes(x=number, y=ddCq, fill=region))+
  geom_col(width = 0.75, position = "dodge")+
  geom_errorbar(aes(x=number, ymin=minus, ymax=plus),
                position = position_dodge(width = 0.75), width=0.25,color="black")+
  theme_classic()+
  # theme_presentation()+
  xlab("Sample")+ylab("CRC2 transcript\nrelative fold change")+
  scale_y_continuous(expand = c(0,0), limits = c(0,500))+
  # scale_fill_manual(values=c("#F3756C","#009ADE","#39B600"),
  #                   labels=c("Nectary","Middle","Blade"))+
  facet_grid(.~species, scales = "free")+
  # scale_fill_manual(values=c("#F3756C","#009ADE"))+
  theme(
    legend.title = element_text(colour="black",size=19),
    legend.text = element_text(colour="black",size=16),
    axis.title = element_text(colour="black",size=22),
    axis.text = element_text(colour="black",size=16),
    strip.text = element_text(colour="black",size=16),
    # legend.position="none",
    # axis.title.x=element_blank(),
    # axis.text.x = element_text(angle = 50, hjust = 1, vjust=1)
  )+
  labs(fill="Region")

ggsave(filename = "CWIN.pdf", height = 5.625, width = 12)



























#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/ggpubr")

library(ggplot2)
setwd("~/Desktop/Kramer Lab/R")
#ddcq calculated with average of the three technical duplicate cq values per primer

deltadeltaCRC2 <- read.csv("MqPCR_CRC2.csv")
deltadeltaCRC2 <- deltadeltaCRC2[c(1:29),c(1,5,10,12,13,14,15,16)]
colnames(deltadeltaCRC2) <- c("Sample","Target","ddCq", "plus","minus","species","region","number")
deltadeltaCRC2$number <- factor(deltadeltaCRC2$number)
show(deltadeltaCRC2)
# deltadeltaCRC2$plus <- log2(deltadeltaCRC2$plus)
# deltadeltaCRC2$minus <- log2(deltadeltaCRC2$minus)
# deltadeltaCRC2$ddCq <- log2(deltadeltaCRC2$ddCq)
# deltadeltaCRC2$line <- as.factor(substring(deltadeltaCRC2$sample, 2,2))
# deltadeltaCRC2$Tissue <- as.factor(substring(deltadeltaCRC2$sample, 0, 1))
# deltadeltaCRC2$seedling <- as.factor(substring(deltadeltaCRC2$seedling, 6, 15))
# levels(deltadeltaCRC2$line) <- c("M","E")
# deltadeltaCRC2$new = factor(deltadeltaCRC2$Target, levels=c("HXK1-1","HXK1-2"), labels=c("HXK1 5'","HXK1 3'"))
# strength <- c("+","+","-","-","+","+","-","-","-","-","+","+")
# deltadeltaCRC2$strength <- as.factor(strength)

#GGplot for Mc and Ml
ggplotMcMl <- ggplot(subset(deltadeltaCRC2, deltadeltaCRC2$species == "Mc" | deltadeltaCRC2$species == "Ml"),
                     aes(x=number, y=ddCq, fill=region))+
  geom_col(width = 0.75, position = "dodge")+
  geom_errorbar(aes(x=number, ymin=minus, ymax=plus),
                position = position_dodge(width = 0.75), width=0.25,color="black")+
  theme_classic()+
  # theme_presentation()+
  xlab("Sample")+ylab("CRC2 transcript\nrelative fold change")+
  scale_y_continuous(expand = c(0,0), limits = c(0,1270))+
  # scale_fill_manual(values=c("#F3756C","#009ADE","#39B600"),
  #                   labels=c("Nectary","Middle","Blade"))+
  facet_grid(.~species, scales = "free")+
  # scale_fill_manual(values=c("#F3756C","#009ADE"))+
  theme(
    legend.title = element_text(colour="black",size=19),
    legend.text = element_text(colour="black",size=16),
    axis.title = element_text(colour="black",size=22),
    axis.text = element_text(colour="black",size=16),
    strip.text = element_text(colour="black",size=16),
    # legend.position="none",
    # axis.title.x=element_blank(),
    # axis.text.x = element_text(angle = 50, hjust = 1, vjust=1)
  )+
  labs(fill="Region")

#GGplot for Mp and Mv
ggplotMpMv <- ggplot(subset(deltadeltaCRC2, deltadeltaCRC2$species == "Mp" | deltadeltaCRC2$species == "Mv"),
                     aes(x=number, y=ddCq, fill=region))+
  geom_col(width = 0.75, position = "dodge")+
  geom_errorbar(aes(x=number, ymin=minus, ymax=plus),
                position = position_dodge(width = 0.75), width=0.25,color="black")+
  theme_classic()+
  # theme_presentation()+
  xlab("Sample")+ylab("CRC2 transcript\nrelative fold change")+
  scale_y_continuous(expand = c(0,0), limits = c(0,600))+
  # scale_fill_manual(values=c("#F3756C","#009ADE","#39B600"),
  #                   labels=c("Nectary","Middle","Blade"))+
  facet_grid(.~species, scales = "free")+
  # scale_fill_manual(values=c("#F3756C","#009ADE"))+
  theme(
    legend.title = element_text(colour="black",size=19),
    legend.text = element_text(colour="black",size=16),
    axis.title = element_text(colour="black",size=22),
    axis.text = element_text(colour="black",size=16),
    strip.text = element_text(colour="black",size=16),
    # legend.position="none",
    # axis.title.x=element_blank(),
    # axis.text.x = element_text(angle = 50, hjust = 1, vjust=1)
  )+
  labs(fill="Region")

show(ggplotMpMv)
show(ggplotMcMl)

install.packages("ggpubr")
library(ggpubr)
ggarrange(ggplotMcMl,ggplotMpMv)

ggsave(filename = "CRC2.pdf", height = 5.625, width = 12)


