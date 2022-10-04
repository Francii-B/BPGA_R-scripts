## ---------------------------
##
## Script name: COG Distribution plot
##
## Purpose of script:
##    This code provides an alternative and a 'ggplot2'-based version  
##     of the COG-Distribution produced by BPGA (DOI: 10.1038/srep24373).
##
## Author: Francesca Brunetti
##
## Date Created: 2022-10-03
##
## Copyright (c) Francesca Brunetti, 2022
## Email: brunetti.1768585@studenti.uniroma1.it
##
## ---------------------------
## R version: 3.6 / 4.1
##
## Notes:
##  The input file (Cog_Category1.txt) is expected either to be 
##    produced by BPGA or to be in a BPGA-like format.
##  The script generates three plots regarding the COG-categories   
##    distribution in the Core-, Accessory- and Unique-genome. The
##    code in divided in 2 blocks, i.e. 2 alternative ways to produce
##    the same plot, specifying the COG-categories as:
##      1.SINGLE-LETTER version
##      2.FULLNAME version
##
## ---------------------------

## ---------------------------

## load up the packages we will need: 

require(ggplot2)
require(gridExtra)
#install.packages("ggplot2")
#install.packages("gridExtra")

## ---------------------------

## LOADING DATA
##  the tsv regarding the COG distribution 
cog_classes= read.table("Supporting_files/Cog_Category1.txt", 
                        header=T, sep="\t")


## SPLITTING DATA
##  splitting the first column in two parts: 
##    A)single-letter COG category  (colname: X)
##    B)full-name COG category      (colname: fullname)

cog_classes$fullname=gsub("\\[.\\] ", "", cog_classes$X)
cog_classes$X=gsub("\\] .*", "", cog_classes$X)
cog_classes$X=gsub("\\[", "", cog_classes$X)



##--------1. SINGLE-LETTER version -----------

## BARPLOT
##  plotting data, showing the single-letter coded COG categories. Three
##   different plots can be generated, depending on the input ('type'):
##    'CORE'      = Core genome
##    'ACCESSORY' = Accessory genome
##    'UNIQUE'    = Unique genome

cog_plot <- function(type){
  #plot
  c_plot=ggplot(cog_classes, aes(x=X, y=get(type), fill=X)) +
    geom_bar(stat="identity", color="black", position=position_dodge())+
    theme(axis.text.x = element_text(angle = 35, hjust=1), 
          axis.title.x = element_blank(),
          axis.title.y = element_text(hjust=-.3),
          legend.position="none",
          text = element_text(size = 20, color="black"),
          axis.title = element_text(face = "bold"),
          axis.text = element_text(color="#1B1212"),
          title = element_text(face= "bold"))
  
  #addictional parameters according to the "type"
  if (type == "CORE"){             #core-genome
    c_plot= c_plot+
      labs(title="A", y=" ")
  } else if (type == "ACCESSORY"){ #accessory-genome
    c_plot= c_plot+
      labs(title="B", y="Relative abundance (%)")
  } else {                         #unique-genome
    c_plot= c_plot+
      labs(title="C", y=" ")
  }
  return(c_plot)
}

## CALL & COMBINE
##  generating the three plots and combining them together
bp_c=cog_plot("CORE")       #A
bp_a=cog_plot("ACCESSORY")  #B
bp_u=cog_plot("UNIQUE")     #C

combine_bp=grid.arrange(bp_c, bp_a, bp_u)

#ggsave(combine_bp, file="COG_distribution.png", height = 10, width = 9)




##--------2. FULLNAME version -----------
##  Same plot, different x-axis text (and order). 

cog_plot_f <- function(type){
  #plot
  c_plot=ggplot(cog_classes, aes(x=fullname, y=get(type), fill=fullname)) +
    geom_bar(stat="identity", color="black", position=position_dodge())+
    theme(axis.title.x = element_blank(),
          legend.position="none",
          plot.margin = unit(c(0.2,0,0,4.5), "cm"),
          text = element_text(size = 25, color="black"),
          axis.title = element_text(face = "bold"),
          axis.text = element_text(color="#1B1212"),
          axis.text.y = element_text(size=30), 
          title = element_text(face= "bold", size=30))
  
  #addictional parameters
  if (type == "CORE"){             #core-genome
    c_plot= c_plot+
      labs(title="A", y=" ")+
      theme(axis.text.x = element_blank())
  } else if (type == "ACCESSORY"){ #accessory-genome
    c_plot= c_plot+
      labs(title="B", y="Relative abundance (%)")+
      theme(axis.text.x = element_blank())
  } else {                         #unique-genome
    c_plot= c_plot+
      labs(title="C", y=" ")+
      theme(axis.text.x = element_text(angle = 45, hjust=1))
  }
  return(c_plot)
}

bp_c=cog_plot_f("CORE")       #A
bp_a=cog_plot_f("ACCESSORY")  #B
bp_u=cog_plot_f("UNIQUE")     #C

combine_bp=grid.arrange(bp_c, bp_a, bp_u,   heights =  c(2,2,4))

#ggsave(combine_bp, file="COG_distribution_fullname.png", height = 21, width = 19)




