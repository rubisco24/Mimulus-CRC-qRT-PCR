library(ggplot2)
setwd("~/Desktop/Kramer Lab/R")
#ddcq calculated with average of the three seedling cq values per primer

deltadelta <- read.csv("YanSampleData.csv")
deltadelta <- deltadelta[c(1:10),c(1,2,5,7,8,9,10)]
deltadelta$Sample <- factor(deltadelta$Sample)
colnames(deltadelta) <- c("sample","Target","ddCq", "plus","minus","Stage","Tissue")
# deltadelta$plus <- log2(deltadelta$plus)
# deltadelta$minus <- log2(deltadelta$minus)
# deltadelta$ddCq <- log2(deltadelta$ddCq)
# deltadelta$line <- as.factor(substring(deltadelta$sample, 2,2))
# deltadelta$Tissue <- as.factor(substring(deltadelta$sample, 0, 1))
# deltadelta$seedling <- as.factor(substring(deltadelta$seedling, 6, 15))
# levels(deltadelta$line) <- c("M","E")
# deltadelta$new = factor(deltadelta$Target, levels=c("HXK1-1","HXK1-2"), labels=c("HXK1 5'","HXK1 3'"))
# strength <- c("+","+","-","-","+","+","-","-","-","-","+","+")
# deltadelta$strength <- as.factor(strength)


ggplot(deltadelta,
             aes(x=sample, y=ddCq, fill=Tissue))+
  geom_col(width = 0.75, position = "dodge")+
  geom_errorbar(aes(x=sample, ymin=minus, ymax=plus),
                position = position_dodge(width = 0.75), width=0.25,color="black")+
  theme_classic()+
  # theme_presentation()+
  xlab("Sample")+ylab("CWIN transcript\nrelative fold change")+
  scale_y_continuous(expand = c(0,0), limits = c(0,30))+
  # scale_fill_manual(values=c("#F3756C","#009ADE","#39B600"),
  #                   labels=c("Nectary","Middle","Blade"))+
  facet_grid(.~Stage, scales = "free")+
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
  labs(fill="Tissue")


ggsave(filename = "CWIN.pdf", height = 5.625, width = 12)

