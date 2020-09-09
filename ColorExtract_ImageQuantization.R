library(tidyverse) ## I love ggplot and tidy data.... so this is a must for anything. 
library(magick) ## Hello magick!!! 
library(scales) ## I find rescale function so useful!  and i love show_col function :)
library(imager) ## i don't know how else to convert image to data frame at the moment. 
#install.packages("treemap")
library(treemap)
library(aws.s3)

## using image_read function in magick I can read image as below. 
#im <- image_read("https://lvlcreatives.s3.amazonaws.com/sd_cn.jpg")
im <- image_read("https://lvlcreatives.s3.amazonaws.com/sr_cn.jpg")


## now display image with 500px wide
im %>% image_resize("500")
## Reduce the colour used in image with image_quantize.  For example, let's say I want to reduce to 24 colours.
im %>%
  image_resize("500") %>%
  image_quantize(max=24)


get_colorPal <- function(im, n=8, cs="RGB"){
  print(cs) 
  tmp <-im %>% image_resize("100") %>% 
    image_quantize(max=n, colorspace=cs) %>%  ## reducing colours! different colorspace gives you different result
    magick2cimg() %>%  ## I'm converting, becauase I want to use as.data.frame function in imager package.
    RGBtoHSV() %>% ## i like sorting colour by hue rather than RGB (red green blue)
    as.data.frame(wide="c") %>%  #3 making it wide makes it easier to output hex colour
    mutate(hex=hsv(rescale(c.1, from=c(0,360)),c.2,c.3),
           hue = c.1,
           sat = c.2,
           value = c.3) %>%
    count(hex,hue,sat,value) %>% 
    mutate(colorspace = cs)
  
  return(tmp %>% select(colorspace,hex,hue,sat,value,n)) ## I want data frame as a result.
  
}


get_colorPal(im)

#if you just want list of colour values...
get_colorPal(im) %>% pull(hex)


params <- list(im=list(im), 
               n=10, ## number of colour you want 
               cs="RGB") ## gray fails so I've removed it...

my_colors <- pmap_df(params,get_colorPal)

## Let's see what got spitted out as results for different colourspace specifiction in image_quantize function.

## I want to view reduced colours by different colourspaces all at once! 
my_colors %>%  
  group_by(colorspace) %>%
  mutate(ypos=row_number(value)) %>%  ## I decided to stack colours by value. 
  ggplot(aes(x=fct_infreq(colorspace),y=ypos, fill=hex)) +  
  geom_tile() +
  geom_text(aes(label=hex), color="#ffffffbe", 
            size=4, family="Roboto Condensed") +
  scale_fill_identity() +
  scale_y_continuous(breaks=NULL) +
  #theme_void(base_family="Roboto Condensed") +
  coord_flip(ylim=c(1,12)) +
  #theme(axis.text = element_text(color = "black", family="Roboto Condensed", hjust=1)) +
  labs(caption="Using different colourspce to reduce the colour used in images")




my_colors %>% treemap(
  index = "hex",
  type = "color",
  vSize = "n",
  vColor = "hex",
  algorithm = "squarified",
  # fontfamily.labels="Arial",
  # fontfamily.title="Arial",
  border.col = c("#ffffff", "#ffffff50"),
  #fontsize.labels=c(24,0),
  #sortID = "-size",
  aspRatio = 16 / 9,
  title = "Clustering with RGB"
)

treemap(my_colors,
  index = "hex",
  type = "color",
  vSize = "n",
  vColor = "hex",
  algorithm = "squarified",
  # fontfamily.labels="Arial",
  # fontfamily.title="Arial",
  border.col = c("#ffffff", "#ffffff50"),
  #fontsize.labels=c(24,0),
  #sortID = "-size",
  aspRatio = 16 / 9,
  title = "Clustering with RGB"
) 









polar1 <-im
# 
polar2 <-my_colors %>%  
  group_by(colorspace) %>%
  mutate(ypos=row_number(value)) %>%  ## I decided to stack colours by value. 
  ggplot(aes(x=fct_infreq(colorspace),y=ypos, fill=hex)) +  
  geom_tile() +
  geom_text(aes(label=hex), color="#ffffffbe", 
            size=4, family="Roboto Condensed") +
  scale_fill_identity() +
  scale_y_continuous(breaks=NULL) +
  #theme_void(base_family="Roboto Condensed") +
  coord_flip(ylim=c(1,12)) +
  #theme(axis.text = element_text(color = "black", family="Roboto Condensed", hjust=1)) +
  labs(caption="Using different colourspce to reduce the colour used in images")


polar3 <-my_colors %>% treemap(
index = "hex",
type = "color",
vSize = "n",
vColor = "hex",
algorithm = "squarified",
fontfamily.labels="Arial",
fontfamily.title="Arial",
border.col = c("#ffffff", "#ffffff50"),
#fontsize.labels=c(24,0),
#sortID = "-size",
aspRatio = 16 / 9,
title = "Clustering with RGB"
)

## I think there's better way to write....
fig_polar <- image_graph(width=600, height=600)
polar1
polar2
polar3
dev.off()
## quartz_off_screen
##                 2
 image_append(fig_polar) ## by default it appends to the side. 

 
 png(filename="creative_tree.png")
 my_colors %>% treemap(
   index = "hex",
   type = "color",
   vSize = "n",
   vColor = "hex",
   algorithm = "squarified",
   # fontfamily.labels="Arial",
   # fontfamily.title="Arial",
   border.col = c("#ffffff", "#ffffff50"),
   #fontsize.labels=c(24,0),
   #sortID = "-size",
   #aspRatio = 16 / 9,
   title = "Clustering with RGB"
 )
 dev.off()
 
 s3BucketName <- "colortreemaps"
 Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAYJXD5GB6OZ5WQ3VL",
            "AWS_SECRET_ACCESS_KEY" = "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",
            "AWS_DEFAULT_REGION" = "us-east-1")

 get_bucket(bucket = "colortreemaps",
            check_region = FALSE,
            verbose = TRUE)
# getwd()
 plot<- load.image("creative_tree.png")
 #plot(plot)
 
in_mem_obj <- rawConnection(raw(0), "w")

put_object(rawConnectionValue(in_mem_obj),
           object = "creative_tree.png",
           bucket = s3BucketName,
           check_region = FALSE)

 