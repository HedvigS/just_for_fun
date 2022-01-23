#script written by hedders

message("THIS SCRIPT WANTS TO INSTALL PACKAGES!")

selection <- menu(c("Yes", "No"), title="Do you want this?")

#if you're confused about this menu thing and just wanna go ahead and run the script: uncomment the following line:
#selection = 1L

if(selection == 1){
#if you selected that installing pkgs is ok, i'll do:
if (!suppressPackageStartupMessages(require("pacman"))) { install.packages("pacman") }

pacman::p_load(tidyverse, 
               reshape2, 
               wesanderson) } else{
                 #if you said no thanks to installing pkgs, I'll just try and load 'em as best I can
                 library(tidyverse)
                 library(reshape2)
                 library(wesanderson)
               }

#set up an output folder
output_dir <- "plots"
if (!dir.exists(output_dir)) { dir.create(output_dir) }

##reading and wrangling data

#emoji data. To make life easier, i've copy-pasted in the values I got from Daniel so that you don't have to download and read anything in.
emoji_raw <- tibble(emoji_n = c(3490774767, 1806111372, 1457562312 ,1235094942 , 971366413  ,852139920 , 775655707,  583962585 , 570768125 , 569129410 ,552759779,    526265249,  495439928,  462784958,  411698154  ,411072600 , 408299154 , 406206089 , 399366302 , 396612298))

emoji <- emoji_raw %>% 
  arrange(desc(emoji_n)) %>% 
  mutate(emoji_n = emoji_n/ max(emoji_n))  #relativise the freq to make comparable

words_raw <- tibble(words_n = c( 53.10, 30.97, 22.63 ,19.35, 16.89, 15.31, 8.38 , 8.00,  6.55,  5.74  ,5.70, 5.50,  5.18 , 4.82,  4.70,  4.59 , 4.52 , 4.11 , 3.88,  3.83))

words <- words_raw %>% 
  arrange(desc(words_n)) %>% 
  mutate(words_n = words_n/ max(words_n))  

#joined emoji and word data and generate a perfect zipfian series between 1 and 0 (note: this isn't a simulated random distribtion around a zipfian curve but a perfect zipfian line)
joined <- cbind(emoji, words) %>% 
  rownames_to_column("rank") %>% 
  mutate(rank = as.numeric(rank)) %>% 
  mutate(Zipf = 1 / rank)  


#plotting
joined %>% 
  melt(id.vars = "rank") %>% 
  ggplot() +
  geom_point(aes(x = rank, y = value, color = variable)) +
  theme_classic(  ) +
  theme(legend.title = element_blank()) +
  ylab("Relative frequencies") +
  scale_color_manual(values = wes_palette(3, name = "FantasticFox1", type = "discrete"))

ggsave(file.path(output_dir, "relative_freq_words_emojis_zipg.png"))

png(file.path(output_dir, "splom_emoji_freq.png"))
joined %>%
  dplyr::select(-rank) %>%
  psych::pairs.panels(       
    method = "pearson", # correlation method
   hist.col = "#a3afd1",# a color I like
   density = TRUE,  # show density plots
   ellipses = F, # show correlation ellipses
   cex.labels= 1,
   cor=T,
   lm=T,
   ci = T, cex.cor = 0.9,stars = T)
dev.off()