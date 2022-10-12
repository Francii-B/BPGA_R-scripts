## ---------------------------
##
## Script name: Pan/Core plot
##
## Purpose of script: 
##    This code provides a ggplot2-version of the pan/core-plot produced
##     by BPGA (DOI: 10.1038/srep24373).
##
## Author: Francesca Brunetti
##
## Date Created: 2022-09-29
##
## Copyright (c) Francesca Brunetti, 2022
## Email: francesca.brunetti@uniroma1.it
##
## ---------------------------
## R version: 3.6.3 or 4.1.2
##
## Notes:
##  The input files (pan_genomes.txt and core_genomes.txt) are expected either to be 
##      produced by BPGA or to be in a BPGA-like format.
##  The script is divided in 2 blocks:
##      1.FUNCTIONS: custom functions to perfom each step
##      2.MAIN: pipeline to make the plot. Parameters and filenames can be changed.
##
## ---------------------------



##------------- 1.FUNCTIONS -------------

## PACKAGE CHECKING 
##   Checking wether ggplot2 or other packages were already installed
check_package <- function(pack_name="ggplot2"){
  pbool=pack_name %in% rownames(installed.packages())
  if (!pbool){
    cat("+++ ERROR +++\n\tThe required package '", pack_name, 
        "' was not found.\n\tInstall it before running the code.\n", 
        sep="" )
  }
  return (pbool)
}


## READING THE DATAFRAMES
##   file: path+filename to be read (expected in a BPGA-format).
##   type: specify wether the file is associated to the Core- or to the Pan-genome

##---- NOTE ----
##  Each line of the BPGA-files "pan_genome.txt" and "core_genome.txt"
##    ends with "\t#". From both of the files, remove only the last line if they are empty 
##    or they just contain "\t#".
read_m <- function(file, type){
  if (file_test("-f",file)){
    tab=read.table(file, sep="\t", header = T, row.names = NULL, comment.char="#")[,c(1,2)]
    colnames(tab)= c("Genome", "Size")
    tab$Group=type
    return(tab)
  }
  stop(cat("+++ ERROR +++\n\tThe input file '", file, 
           "' was not found.\n", sep="" ))
  geterrmessage()
}



## CREATING THE PLOT
##  input: core/pan merged matrices
##  exp_base, exp_power: parameters of the exponential function
##  pf_base, pf_power: parameters of the power fit function
##  formula: TRUE if the curve functions (exponential and power-fit) have to be displayed
##           (default: FALSE)
##  exp_x, exp_y, pf_x, pf_y: if formula==T, specify the coordinates on the plot. Else,
##                            default coordinates are provided.
##  output: path+filename of the output file.
pplot <- function(input, exp_base, exp_power, pf_base, pf_power, formula=F,
                  exp_x=length(levels(input$Genome))-7, exp_y=max(input$Size), 
                  pf_x=length(levels(input$Genome))-7, pf_y=min(input$Size), output="panplot"){
  #plot
  panplot=ggplot(data = input, aes(x=Genome, y=Size, fill=Group)) + 
    #boxplot features
    geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0))+
    labs(x="Number of genomes", y="Number of Gene Families", colour="Curve", fill="Boxplot")+
    #drawing the curves
    geom_function(aes(colour= "Core-genome exponential curve"),
                  fun = function(x) exp_base*exp(exp_power*x))+
    geom_function(aes(colour="Pan-genome power fit curve"),
                  fun = function(x) pf_base*x^(pf_power))+
    scale_colour_manual(values = c( "#e03333", "#338fe0"))+ #color of the curves
    #text and legend
    theme(text = element_text(size = 20, color="black"),
          axis.title = element_text(face = "bold"),
          axis.text = element_text(color="#1B1212"),
          legend.title = element_blank(), legend.position = "bottom")
  
  #adding the curve formula on the plot
  if (formula){
    formula_exp = paste("f[1](x) == ",exp_base, "*e^{",exp_power, "*x}", sep="")
    formula_pf = paste("f(x) == ",pf_base, "*x^{",pf_power, "}", sep="")
    
    panplot= panplot +
      annotate("text", x=exp_x, y=exp_y, size=7, color="#00003f",
               label = as.character(bquote(.(formula_exp))), parse=T)+
      annotate("text", x=pf_x, y=pf_y, size=7, color="#8b0000",
               label = as.character(bquote(.(formula_pf))), parse = T)
  }
  
  panplot #preview
 ggsave(panplot, file=paste(output, ".png", sep=""), #saving the plot
        width=13, height = 8) #image size
}

## ---------------------------


##------------- 2.MAIN -------------
#install.packages("ggplot2")
  
if (check_package()){
  library(ggplot2)
  
  #input files
  pan=read_m("Supporting_files/pan_genome.txt", "Pan-genome")
  core=read_m("Supporting_files/core_genome.txt", "Core-genome")
  
  #merging the df
  whole=rbind(pan, core)
  whole$Genome=factor(whole$Genome, levels= unique(core$Genome))
  
  #pan/core-plot
    #DO NOT modify first parameter ('whole')
  pplot(whole, exp_base=2207.87, exp_power=-0.0017,  
               pf_base=2555.05, pf_power=0.13,
               formula=T,
               exp_x=33, exp_y=4200,
               pf_x=33, pf_y=2300)
}

## ---------------------------
               