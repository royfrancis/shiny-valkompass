# shiny-certificate
# functions

library(ggplot2)
library(shiny)
library(shinythemes)
library(showtext)
library(ggh4x)
library(dplyr)

# variables --------------------------------------------------------------------

if (!"gfont" %in% sysfonts::font_families()) font_add_google("Barlow", "gfont")
showtext_opts(dpi = 300)

theme_report <- function (basesize=14,font=NA) {
  theme_bw(base_size=basesize, base_family=font) %+replace% 
    theme(
      panel.border=element_blank(),
      panel.grid.minor=element_blank(),
      panel.grid.major=element_blank(),
      legend.position="top",
      legend.direction="horizontal",
      legend.justification="center",
      strip.background=element_blank(),
      plot.title=element_text(hjust=0.5),
      axis.text=element_blank(),
      axis.title=element_blank(),
      axis.ticks=element_blank()
    )
}

cols <- c("#e02e3d","#7dbee1","#ffc346","#32a532","#911414","#331d79","#1e69aa","#82c882")
names(cols) <- c("S","M","SD","C","V","KD","L","MP")

p2018 <- readRDS("pca-2018.Rds")
p2022 <- readRDS("pca-2022.Rds")

# functions --------------------------------------------------------------------

fn_dir <- function(session) {
  
  wd <- file.path(tempdir(check = TRUE), session$token)
  if (!dir.exists(wd)) dir.create(wd)
  cat(paste0("Working directory: ", wd, "\n"))
  return(wd)
}

# fn_version
fn_version <- function() {
  return("v1.0.0")
}

# validation
fn_validate <- function(input, message1, message2, message3) {
  
  if (missing(message1)) message1 <- "Input is missing."
  gcheck <- length(grep("Argument \\'\\w+\\' missing", message1))
  if (gcheck == 1) {
    m1 <- sub("Argument ", "", message1)
    m1 <- sub(" missing.", "", m1)
  }

  if (all(is.null(input))) {
    if (missing(message1)) message1 <- "Input is missing."
    print(message1)
  } else if (is.numeric(input) | is.list(input)) {
    if (all(is.na(input))) {
      if (missing(message2)) {
        if (gcheck == 1) message2 <- paste0("Argument ", m1, " is NA.", sep = "")
        if (gcheck != 1) message2 <- "Input is NA."
      }
      print(message2)
    }
  } else if (is.character(input)) {
    if (all(nchar(input) == 0)) {
      if (missing(message3)) {
        if (gcheck == 1) message3 <- paste0("Argument ", m1, " is empty.", sep = "")
        if (gcheck != 1) message3 <- "Input is empty."
      }
      print(message3)
    }
  } else {
    NULL
  }
}

# validate numeric
fn_validate_numeric <- function(input) {
  if (is.na(as.numeric(input))) print("Input is not a numeric.")
}

# validate image
fn_validate_im <- function(x) {
  if (!is.null(x)) {
    y <- tolower(sub("^.+[.]", "", basename(x$datapath)))
    if (!y %in% c("jpg", "png", "jpeg", "gif")) {
      return("Image must be one of JPG/JPEG, PNG or GIF formats.")
    }
    if ((x$size / 1024 / 1024) > 1) {
      return("Image must be less than 1MB in size.")
    }
  }
}

#' @title Add bootstrap row+column
#' @description Wrapper function to add bootstrap row and column
#' @param ... UI builder elements
#'
rc <- function(...) {
  fluidRow(column(12, ...))
}

#dfr <- read.csv("data.csv")
#dfr1 <- as.data.frame(t(select(filter(dfr,year==2018),S,M,SD,C,V,KD,L,MP)))
#dfr1 <- dfr1[,apply(dfr1,2,var)>0]

#dfr2 <- as.data.frame(t(cbind(select(filter(dfr,year==2022),S,M,SD,C,V,KD,L,MP))))
#dfr2 <- dfr2[,apply(dfr2,2,var)>0]

# pca
#p1 <- prcomp(dfr1, center = TRUE,scale. = TRUE)
#p2 <- prcomp(dfr2, center = TRUE,scale. = TRUE)
#saveRDS(p1,"pca-2018.Rds")
#saveRDS(p2,"pca-2022.Rds")

plot_pca <- function(dfr,path){
  plt <- ggplot(dfr)+
    geom_hline(aes(yintercept=0),color="grey90",size=0.4,alpha=0.5)+
    geom_vline(aes(xintercept=0),color="grey90",size=0.4,alpha=0.5)+
    geom_point(aes(PC1,PC2,fill=id,color=year),size=6,shape=21,stroke=1)+
    geom_pointpath(aes(x=PC1,y=PC2,group=id),size = 0, linetype=1, color="grey60", alpha=0.6, arrow = arrow(length=unit(0.10,"cm"), ends="last", type = "closed"), mult=1)+
    geom_text(aes(PC1,PC2,label=id),size=2.5,color="grey90",family="Barlow",fontface="bold")+
    coord_fixed()+
    scale_fill_manual(values=cols,guide="none")+
    scale_colour_manual(values=c("#115E59","#14B8A6"))+
    labs(x="",y="",caption=paste0(format(Sys.Date(),format="%d-%b-%Y"),"  •  PCA on 62 likert scale questions (32 from 2018 and 30 from 2022) from SVT valkompass."))+
    theme_report(font = "Barlow", basesize = 6)+
    theme(plot.caption=element_text(hjust=0.5,size=5.5),
          legend.title = element_text(size=6))
  
  ggsave(file.path(path,"valkompass-pca.png"),plt,height=4,width=5.5)
}


#
