#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/ggpubr")


library(ggplot2)
setwd("~/Desktop/Kramer Lab/R")
#ddcq calculated with average of the three technical duplicate cq values per primer

deltadeltaSWEET9 <- read.csv("MqPCR_SWEET9.csv")
deltadeltaSWEET9 <- deltadeltaSWEET9[c(1:29),c(1,5,11,13,14,15,16,17)]
colnames(deltadeltaSWEET9) <- c("Sample","Target","ddCq", "plus","minus","species","region","number")
deltadeltaSWEET9$number <- factor(deltadeltaSWEET9$number)
show(deltadeltaSWEET9)
# deltadeltaSWEET9$plus <- log2(deltadeltaSWEET9$plus)
# deltadeltaSWEET9$minus <- log2(deltadeltaSWEET9$minus)
# deltadeltaSWEET9$ddCq <- log2(deltadeltaSWEET9$ddCq)
# deltadeltaSWEET9$line <- as.factor(substring(deltadeltaSWEET9$sample, 2,2))
# deltadeltaSWEET9$Tissue <- as.factor(substring(deltadeltaSWEET9$sample, 0, 1))
# deltadeltaSWEET9$seedling <- as.factor(substring(deltadeltaSWEET9$seedling, 6, 15))
# levels(deltadeltaSWEET9$line) <- c("M","E")
# deltadeltaSWEET9$new = factor(deltadeltaSWEET9$Target, levels=c("HXK1-1","HXK1-2"), labels=c("HXK1 5'","HXK1 3'"))
# strength <- c("+","+","-","-","+","+","-","-","-","-","+","+")
# deltadeltaSWEET9$strength <- as.factor(strength)

#GGplot for Mc
ggplotMc <- ggplot(subset(deltadeltaSWEET9, deltadeltaSWEET9$species == "Mc"),
                     aes(x=number, y=ddCq, fill=region))+
  geom_col(width = 0.75, position = "dodge")+
  geom_errorbar(aes(x=number, ymin=minus, ymax=plus),
                position = position_dodge(width = 0.75), width=0.25,color="black")+
  theme_classic()+
  # theme_presentation()+
  xlab("Sample")+ylab("SWEET9 transcript\nrelative fold change")+
  scale_y_continuous(expand = c(0,0), limits = c(0,250))+
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

#GGplot for Ml
ggplotMl <- ggplot(subset(deltadeltaSWEET9, deltadeltaSWEET9$species == "Ml"),
                     aes(x=number, y=ddCq, fill=region))+
  geom_col(width = 0.75, position = "dodge")+
  geom_errorbar(aes(x=number, ymin=minus, ymax=plus),
                position = position_dodge(width = 0.75), width=0.25,color="black")+
  theme_classic()+
  # theme_presentation()+
  xlab("Sample")+ylab("SWEET9 transcript\nrelative fold change")+
  scale_y_continuous(expand = c(0,0), limits = c(0,50000))+
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



#GGplot for Mp 
ggplotMp <- ggplot(subset(deltadeltaSWEET9, deltadeltaSWEET9$species == "Mp"),
                     aes(x=number, y=ddCq, fill=region))+
  geom_col(width = 0.75, position = "dodge")+
  geom_errorbar(aes(x=number, ymin=minus, ymax=plus),
                position = position_dodge(width = 0.75), width=0.25,color="black")+
  theme_classic()+
  # theme_presentation()+
  xlab("Sample")+ylab("SWEET9 transcript\nrelative fold change")+
  scale_y_continuous(expand = c(0,0), limits = c(0,7500))+
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

#GGplot for Mv 
ggplotMv <- ggplot(subset(deltadeltaSWEET9, deltadeltaSWEET9$species == "Mv"),
                     aes(x=number, y=ddCq, fill=region))+
  geom_col(width = 0.75, position = "dodge")+
  geom_errorbar(aes(x=number, ymin=minus, ymax=plus),
                position = position_dodge(width = 0.75), width=0.25,color="black")+
  theme_classic()+
  # theme_presentation()+
  xlab("Sample")+ylab("SWEET9 transcript\nrelative fold change")+
  scale_y_continuous(expand = c(0,0), limits = c(0,2100))+
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

show(ggplotMc)
show(ggplotMl)
show(ggplotMp)
show(ggplotMv)


install.packages("ggpubr")
library(ggpubr)
ggarrange(ggplotMc,ggplotMl,ggplotMp,ggplotMv)

ggsave(filename = "SWEET9.pdf", height = 5.625, width = 12)

