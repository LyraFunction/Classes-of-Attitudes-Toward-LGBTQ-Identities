################################################################################
################# LCA Model Setup for Need for Dominance #######################
################################################################################

## Load libraries for project
library(tidyverse)
library(tidyplots)
library(patchwork)
library(poLCA)
library(reshape2)
#libary(poLCAParallel)

## Load files
setwd("~/Desktop/Projects/Github-Repos/Need-for-Dominance/scripts/r")
Study1 <- as_tibble(read_csv("~/Desktop/Projects/Github-Repos/Need-for-Dominance/data_raw/NFC Study Data.csv"))
Study2 <- as_tibble(read_csv("~/Desktop/Projects/Github-Repos/Need-for-Dominance/data_raw/NFC Study 2 Data.csv"))

################################################################################
########################## Initial dataframe prep ##############################
################################################################################
Study1 <- Study1 |> add_column(Study = 1, REALtgm = NA, REALtgw = NA, 
                               REALpan = NA, REALace = NA, EVALtgm = NA, 
                               EVALtgw = NA, EVALpan = NA, EVALace = NA, 
                               STAB1tgm = NA, STAB1tgw = NA, STAB1pan = NA, 
                               STAB1ace = NA, STAB2tgm = NA, STAB2tgw = NA, 
                               STAB2pan = NA, STAB2ace = NA)
Study2 <- Study2 |> add_column(Study = 2)
selection <- c(
  Study = "Study",
  NFC1 = "NFC Items_1",
  NFC2 = "NFC Items_2",
  NFC3 = "NFC Items_3",
  NFC4 = "NFC Items_4",
  NFC5 = "NFC Items_5",
  NFC6 = "NFC Items_6",
  NFC7 = "NFC Items_7",
  NFC8 = "NFC Items_8",
  NFC9 = "NFC Items_9",
  NFC10 = "NFC Items_10",
  NFC11 = "NFC Items_11",
  NFC12 = "NFC Items_12",
  NFC13 = "NFC Items_13",
  NFC14 = "NFC Items_14",
  NFC15 = "NFC Items_15",
  SDO1 = "SDO Items_1",
  SDO2 = "SDO Items_2",
  SDO3 = "SDO Items_3",
  SDO4 = "SDO Items_4",
  SDO5 = "SDO Items_5",
  SDO6 = "SDO Items_6",
  SDO7 = "SDO Items_7",
  SDO8 = "SDO Items_8",
  SDO9 = "SDO Items_9",
  SDO10 = "SDO Items_10",
  SDO11 = "SDO Items_11",
  SDO12 = "SDO Items_12",
  SDO13 = "SDO Items_13",
  SDO14 = "SDO Items_14",
  SDO15 = "SDO Items_15",
  SDO16 = "SDO Items_16",
  REALcgm = "Q8_1",
  REALcgw = "Q8_2",
  REALtgm = "REALtgm",
  REALtgw = "REALtgw",
  REALnb = "Q8_3",
  REALhet = "Q8_6",
  REALgl = "Q8_5",
  REALbi = "Q8_4",
  REALpan = "REALpan",
  REALace = "REALace",
  EVALcgm = "Group Evaluations_1",
  EVALcgw = "Group Evaluations_2",
  EVALtgm = "EVALtgm",
  EVALtgw = "EVALtgw",
  EVALnb = "Group Evaluations_3",
  EVALhet = "Group Evaluations_6",
  EVALgl = "Group Evaluations_5",
  EVALbi = "Group Evaluations_4",
  EVALpan = "EVALpan",
  EVALace = "EVALace",
  STAB1cgm = "Stability Items 1_1",
  STAB1cgw = "Stability Items 1_2",
  STAB1tgm = "STAB1tgm",
  STAB1tgw = "STAB1tgw",
  STAB1nb = "Stability Items 1_3",
  STAB1het = "Stability Items 1_6",
  STAB1gl = "Stability Items 1_5",
  STAB1bi = "Stability Items 1_4",
  STAB1pan = "STAB1pan",
  STAB1ace = "STAB1ace",
  STAB2cgm = "Stability Items 2_1",
  STAB2cgw = "Stability Items 2_2",
  STAB2tgm = "STAB2tgm",
  STAB2tgw = "STAB2tgw",
  STAB2nb = "Stability Items 2_3",
  STAB2het = "Stability Items 2_6",
  STAB2gl = "Stability Items 2_5",
  STAB2bi = "Stability Items 2_4",
  STAB2pan = "STAB2pan",
  STAB2ace = "STAB2ace",
  Lib_Con = "Liberal_Conservative",
  Religiousness = "Religiousness",
  Education = "Education",
  Age = "Age")
Study1 <- Study1 |> dplyr::select(all_of(selection))
selection <- c(
  Study = "Study",
  NFC1 = "NFC_1",
  NFC2 = "NFC_2",
  NFC3 = "NFC_3",
  NFC4 = "NFC_4",
  NFC5 = "NFC_5",
  NFC6 = "NFC_6",
  NFC7 = "NFC_7",
  NFC8 = "NFC_8",
  NFC9 = "NFC_9",
  NFC10 = "NFC_10",
  NFC11 = "NFC_11",
  NFC12 = "NFC_12",
  NFC13 = "NFC_13",
  NFC14 = "NFC_14",
  NFC15 = "NFC_15",
  SDO1 = "SDO_1",
  SDO2 = "SDO_2",
  SDO3 = "SDO_3",
  SDO4 = "SDO_4",
  SDO5 = "SDO_5",
  SDO6 = "SDO_6",
  SDO7 = "SDO_7",
  SDO8 = "SDO_8",
  SDO9 = "SDO_9",
  SDO10 = "SDO_10",
  SDO11 = "SDO_11",
  SDO12 = "SDO_12",
  SDO13 = "SDO_13",
  SDO14 = "SDO_14",
  SDO15 = "SDO_15",
  SDO16 = "SDO_16",
  REALcgm = "Real_Men",
  REALcgw = "Real_Women",
  REALtgm = "Real_TM",
  REALtgw = "Real_TW",
  REALnb = "Real_NB",
  REALhet = "Real_Straight",
  REALgl = "Real_GL",
  REALbi = "Real_Bi",
  REALpan = "Real_Pan",
  REALace = "Real_Ace",
  EVALcgm = "Eval_1",
  EVALcgw = "Eval_2",
  EVALtgm = "Eval_3",
  EVALtgw = "Eval_4",
  EVALnb = "Eval_5",
  EVALhet = "Eval_6",
  EVALgl = "Eval_7",
  EVALbi = "Eval_8",
  EVALpan = "Eval_9",
  EVALace = "Eval_10",
  STAB1cgm = "SB_1_Men",
  STAB1cgw = "SB_1_Women",
  STAB1tgm = "SB_1_TM",
  STAB1tgw = "SB_1_TW",
  STAB1nb = "SB_1_NB",
  STAB1het = "SB_1_Straight",
  STAB1gl = "SB_1_GL",
  STAB1bi = "SB_1_Bi",
  STAB1pan = "SB_1_Pan",
  STAB1ace = "SB_1_Ace",
  STAB2cgm = "SB_2_Men",
  STAB2cgw = "SB_2_Women",
  STAB2tgm = "SB_2_TM",
  STAB2tgw = "SB_2_TW",
  STAB2nb = "SB_2_NB",
  STAB2het = "SB_2_Straight",
  STAB2gl = "SB_2_GL",
  STAB2bi = "SB_2_Bi",
  STAB2pan = "SB_2_Pan",
  STAB2ace = "SB_2_Ace",
  Lib_Con = "Self_Lib_Conserv",
  Religiousness = "Religiosity_1",
  Education = "Education",
  Age = "Age")
Study2 <- Study2 |> dplyr::select(all_of(selection))
rm(selection)
data <- bind_rows(Study1, Study2, .id = "Study")
rm(Study1)
rm(Study2)
data$Study <- as_factor(data$Study)
data$Lib_Con <- as_factor(data$Lib_Con)
data$Education <- as.integer(data$Education)
# Remove obvious mistakes
data <- data[-1, ] # Invalid case
data <- data[-133, ] # Aged 2001
# Set eval to minimum 1 instead of 0
#data <- data %>%
#mutate(across(43:52, ~ if_else(is.na(.), NA_integer_, . + 1)))
cols <- c("EVALcgm","EVALcgw","EVALtgm","EVALtgw","EVALnb","EVALhet","EVALgl",
          "EVALbi","EVALpan","EVALace") 
data <- data |>
  mutate(across(all_of(cols), ~ pmax(1, ceiling(.x/15))))
# Scoring
data$STAB1cgm <- 8 - data$STAB1cgm
data$STAB1cgw <- 8 - data$STAB1cgw
data$STAB1tgm <- 8 - data$STAB1tgm
data$STAB1tgw <- 8 - data$STAB1tgw
data$STAB1nb <- 8 - data$STAB1nb
data$STAB1het <- 8 - data$STAB1het
data$STAB1gl <- 8 - data$STAB1gl
data$STAB1bi <- 8 - data$STAB1bi
data$STAB1pan <- 8 - data$STAB1pan
data$STAB1ace <- 8 - data$STAB1ace
data$NFC_Total <- rowMeans(data[ , 2:16])
data$SDO_Total <- with(data, rowMeans(cbind(SDO1, SDO2, SDO3, SDO4, 8 - SDO5, 
                                            8 - SDO6, 8 - SDO7, 8 - SDO8, SDO9, 
                                            SDO10, SDO11, SDO12, 8 - SDO13, 
                                            8 - SDO14, 8 - SDO15, 8 - SDO16)))
# Integer conversion
data <- data %>%
  mutate(across(33:72, ~ as.integer(as.character(.))))
# Create data1 variable
data1 <- data |>
  filter(Study == 1)
data2 <- data |>
  filter(Study == 2)
rm(cols)

################################################################################
############################# Study 1 model ###################################
################################################################################
Study1AIC <- data.frame()
Study1BIC <- data.frame()
max_II <- -100000
min_bic <- 100000
f <- cbind(REALnb,EVALnb,STAB1nb,
           REALgl,EVALgl,STAB1gl,
           REALbi,EVALbi,STAB1bi)~1
for(i in 2:10){
  lc <- poLCA(f, data1, nclass=i, maxiter=5000,
              tol=1e-5, na.rm=FALSE,
              nrep=10, verbose=TRUE, calc.se=FALSE)
  Study1AIC <- rbind(Study1AIC, cbind(i, (lc$aic)))
  Study1BIC <-rbind(Study1BIC, cbind(i, (lc$bic)))
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    m1 <- lc
    }
} 
f <- cbind(REALnb,EVALnb,STAB1nb,
           REALgl,EVALgl,STAB1gl,
           REALbi,EVALbi,STAB1bi)~NFC_Total+SDO_Total + Age + Religiousness + Lib_Con
m1c <- poLCA(f, data1, nclass=4, maxiter=5000,
            tol=1e-5, na.rm=FALSE,
            nrep=10, verbose=TRUE, calc.se=TRUE)
probs.start.new <-poLCA.reorder(m1c$probs.start,order(m1c$P,decreasing=TRUE))
m1c <- poLCA(f, data1, nclass=4, maxiter=5000,
             tol=1e-5, na.rm=FALSE,
             nrep=1, verbose=TRUE, calc.se=TRUE, probs.start = probs.start.new)

################################################################################
############################# Study 2  Model ##################################
################################################################################
Study2AIC <- data.frame()
Study2BIC <- data.frame()
max_II <- -100000
min_bic <- 100000
f <- cbind(REALtgw,EVALtgw,STAB1tgw,
           REALtgm,EVALtgm,STAB1tgm,
           REALnb,EVALnb,STAB1nb,
           REALgl,EVALgl,STAB1gl,
           REALbi,EVALbi,STAB1bi)~1
for(i in 2:10){
  lc <- poLCA(f, data2, nclass=i, maxiter=5000,
              tol=1e-5, na.rm=FALSE,
              nrep=10, verbose=TRUE, calc.se=FALSE)
  Study2AIC <- rbind(Study2AIC, cbind(i, (lc$aic)))
  Study2BIC <-rbind(Study2BIC, cbind(i, (lc$bic)))
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    m2 <- lc
    }
} 

f <- cbind(REALtgw,EVALtgw,STAB1tgw,
           REALtgm,EVALtgm,STAB1tgm,
           REALnb,EVALnb,STAB1nb,
           REALgl,EVALgl,STAB1gl,
           REALbi,EVALbi,STAB1bi)~NFC_Total+SDO_Total + Age + Religiousness + Lib_Con
m2c <- poLCA(f, data2, nclass=5, maxiter=5000,
            tol=1e-5, na.rm=FALSE,
            nrep=10, verbose=TRUE, calc.se=TRUE)
probs.start.new  <-poLCA.reorder(m2c$probs.start,order(m2c$P,decreasing=TRUE))
m2c <- poLCA(f, data2, nclass=5, maxiter=5000,
             tol=1e-5, na.rm=FALSE,
             nrep=1, verbose=TRUE, calc.se=TRUE, probs.start = probs.start.new)
rm(f, i, max_II, min_bic, lc, m1, m2)

################################################################################
############################## Data Visualization ##############################
################################################################################

######################## NFC and SDO density across study ######################
NFC_Density <- data |>
  tidyplot(x = NFC_Total, color = Study) |>
  add_histogram(bins = 20) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |> 
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 16, family = "Helvetica Neue") |>
  adjust_x_axis(title = "NFC Score") |>
  adjust_y_axis_title("Count") |>
  adjust_legend_position(position = "none") |>
  adjust_title(title = "Need for Closure Density")
SDO_Density <- data |>
  tidyplot(x = SDO_Total, color = Study) |>
  add_histogram(bins = 20) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |> 
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 16, family = "Helvetica Neue") |>
  adjust_x_axis(title = "SDO Score") |>
  adjust_y_axis_title("Count") |>
  adjust_legend_position(position = "left") |>
  adjust_title(title = "Social Dominance Orientation Density")
NFC_Density + SDO_Density
rm(NFC_Density, SDO_Density)

#################### Realness of identities across studies #####################

Real_Trans_women_Density <- data |>
  tidyplot(x = REALtgw, color = Study) |>
  add_histogram(bins = 7) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |> 
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 16, family = "Helvetica Neue") |>
  adjust_x_axis(title = "Realness Score") |>
  adjust_y_axis_title("Count") |>
  adjust_legend_position(position = "none") |>
  adjust_title(title = "Perceived Trans Woman Identity Realness")
Real_Trans_men_Density <- data |>
  tidyplot(x = REALtgm, color = Study) |>
  add_histogram(bins = 7) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |> 
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 16, family = "Helvetica Neue") |>
  adjust_x_axis(title = "Realness Score") |>
  adjust_y_axis_title("Count") |>
  adjust_legend_position(position = "none") |>
  adjust_title(title = "Perceived Trans Man Identity Realness")
Real_Nonbinary_Density <- data |>
  tidyplot(x = REALnb, color = Study) |>
  add_histogram(bins = 7) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |> 
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 16, family = "Helvetica Neue") |>
  adjust_x_axis(title = "Realness Score") |>
  adjust_y_axis_title("Count") |>
  adjust_legend_position(position = "top") |>
  adjust_title(title = "Perceived Nonbinary Identity Realness")
Real_Gay_Lesbian_Density <- data |>
  tidyplot(x = REALgl, color = Study) |>
  add_histogram(bins = 7) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |> 
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 16, family = "Helvetica Neue") |>
  adjust_x_axis(title = "Realness Score") |>
  adjust_y_axis_title("Count") |>
  adjust_legend_position(position = "none") |>
  adjust_title(title = "Perceived Gay and Lesbian Identity Realness")
Real_Bisexual_Density <- data |>
  tidyplot(x = REALbi, color = Study) |>
  add_histogram(bins = 7) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |> 
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 16, family = "Helvetica Neue") |>
  adjust_x_axis(title = "Realness Score") |>
  adjust_y_axis_title("Count") |>
  adjust_legend_position(position = "none") |>
  adjust_title(title = "Perceived Bisexual Identity Realness")

Real_Trans_women_Density + Real_Trans_men_Density + Real_Nonbinary_Density + Real_Gay_Lesbian_Density + Real_Bisexual_Density + plot_layout(ncol = 5, nrow = 1)
rm(Real_Trans_women_Density,Real_Trans_men_Density,Real_Nonbinary_Density,Real_Gay_Lesbian_Density,Real_Bisexual_Density)

################## Evaluations of identities across studies ####################

EVAL_Trans_women_Density <- data |>
  tidyplot(x = EVALtgw, color = Study) |>
  add_histogram(bins = 7) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |> 
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 16, family = "Helvetica Neue") |>
  adjust_x_axis(title = "Evaluation") |>
  adjust_y_axis_title("Count") |>
  adjust_legend_position(position = "none") |>
  adjust_title(title = "Evaluations of Trans Woman Identity")
EVAL_Trans_men_Density <- data |>
  tidyplot(x = EVALtgm, color = Study) |>
  add_histogram(bins = 7) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |> 
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 16, family = "Helvetica Neue") |>
  adjust_x_axis(title = "Evaluation") |>
  adjust_y_axis_title("Count") |>
  adjust_legend_position(position = "none") |>
  adjust_title(title = "Evaluations of Trans Man Identity")
EVAL_Nonbinary_Density <- data |>
  tidyplot(x = EVALnb, color = Study) |>
  add_histogram(bins = 7) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |> 
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 16, family = "Helvetica Neue") |>
  adjust_x_axis(title = "Evaluation") |>
  adjust_y_axis_title("Count") |>
  adjust_legend_position(position = "top") |>
  adjust_title(title = "Evaluations of Nonbinary Identity")
EVAL_Gay_Lesbian_Density <- data |>
  tidyplot(x = EVALgl, color = Study) |>
  add_histogram(bins = 7) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |> 
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 16, family = "Helvetica Neue") |>
  adjust_x_axis(title = "Evaluation") |>
  adjust_y_axis_title("Count") |>
  adjust_legend_position(position = "none") |>
  adjust_title(title = "Evaluations of Gay and Lesbian Identity")
EVAL_Bisexual_Density <- data |>
  tidyplot(x = EVALbi, color = Study) |>
  add_histogram(bins = 7) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |> 
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 16, family = "Helvetica Neue") |>
  adjust_x_axis(title = "Evaluation") |>
  adjust_y_axis_title("Count") |>
  adjust_legend_position(position = "none") |>
  adjust_title(title = "Evaluations of Bisexual Identity")

EVAL_Trans_women_Density + EVAL_Trans_men_Density + EVAL_Nonbinary_Density + EVAL_Gay_Lesbian_Density + EVAL_Bisexual_Density + plot_layout(ncol = 5, nrow = 1)
rm(EVAL_Trans_women_Density,EVAL_Trans_men_Density,EVAL_Nonbinary_Density,EVAL_Gay_Lesbian_Density,EVAL_Bisexual_Density)

################### Stability of identities across studies #####################

STAB_Trans_women_Density <- data |>
  tidyplot(x = STAB1tgw, color = Study) |>
  add_histogram(bins = 7) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |> 
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 16, family = "Helvetica Neue") |>
  adjust_x_axis(title = "Stability") |>
  adjust_y_axis_title("Count") |>
  adjust_legend_position(position = "none") |>
  adjust_title(title = "Perceived Identity Stability of Trans Women")
STAB_Trans_men_Density <- data |>
  tidyplot(x = STAB1tgm, color = Study) |>
  add_histogram(bins = 7) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |> 
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 16, family = "Helvetica Neue") |>
  adjust_x_axis(title = "Stability") |>
  adjust_y_axis_title("Count") |>
  adjust_legend_position(position = "none") |>
  adjust_title(title = "Perceived Identity Stability of Trans Men")
STAB_Nonbinary_Density <- data |>
  tidyplot(x = STAB1nb, color = Study) |>
  add_histogram(bins = 7) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |> 
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 16, family = "Helvetica Neue") |>
  adjust_x_axis(title = "Stability") |>
  adjust_y_axis_title("Count") |>
  adjust_legend_position(position = "top") |>
  adjust_title(title = "Perceived Identity Stability of Nonbinary People")
STAB_Gay_Lesbian_Density <- data |>
  tidyplot(x = STAB1gl, color = Study) |>
  add_histogram(bins = 7) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |> 
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 16, family = "Helvetica Neue") |>
  adjust_x_axis(title = "Stability") |>
  adjust_y_axis_title("Count") |>
  adjust_legend_position(position = "none") |>
  adjust_title(title = "Perceived Identity Stability of Gay and Lesbian People")
STAB_Bisexual_Density <- data |>
  tidyplot(x = STAB1bi, color = Study) |>
  add_histogram(bins = 7) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |> 
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 16, family = "Helvetica Neue") |>
  adjust_x_axis(title = "Stability") |>
  adjust_y_axis_title("Count") |>
  adjust_legend_position(position = "none") |>
  adjust_title(title = "Perceived Identity Stability of Bisexual People")

STAB_Trans_women_Density + STAB_Trans_men_Density + STAB_Nonbinary_Density + STAB_Gay_Lesbian_Density + STAB_Bisexual_Density + plot_layout(ncol = 5, nrow = 1)
rm(STAB_Trans_women_Density,STAB_Trans_men_Density,STAB_Nonbinary_Density,STAB_Gay_Lesbian_Density,STAB_Bisexual_Density)

################################################################################
################################## Fit Plots ###################################
################################################################################
# Setup
ClassComparison1 <- cbind(Study1AIC,Study1BIC$V2)
names(ClassComparison1)<- c("Classes","AIC","BIC");ClassComparison1<-data.frame(ClassComparison1);(ClassComparison1)
ClassComparison1 <- ClassComparison1 %>% 
  pivot_longer(
    cols = c("AIC",
             "BIC"), 
    names_to = "Fit_Indicator",
    values_to = "Value"
  )
ClassComparison1$Study <- "Study 1"

ClassComparison2 <- cbind(Study2AIC,Study2BIC$V2)
names(ClassComparison2)<- c("Classes","AIC","BIC");ClassComparison2<-data.frame(ClassComparison2);(ClassComparison2)
ClassComparison2 <- ClassComparison2 %>% 
  pivot_longer(
    cols = c("AIC",
             "BIC"), 
    names_to = "Fit_Indicator",
    values_to = "Value"
  )
ClassComparison2$Study <- "Study 2"
FitComparison <- bind_rows(ClassComparison1, ClassComparison2, .id = "Study")
rm(Study1AIC, Study1BIC, Study2AIC, Study2BIC, ClassComparison1, ClassComparison2)
# Plots
FitPlot <- FitComparison |>
  tidyplot(x = Classes, y = Value, color = Fit_Indicator) |>
  add_data_points(size = 4) |>
  add_line(linewidth = 1.5) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |>
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 36, family = "Helvetica Neue", face = "plain") |>
  adjust_title("AIC & BIC Fit by Study") |>
  adjust_x_axis_title("Class Count") |>
  adjust_legend_title("Fit Indicator") |>
  adjust_legend_position(position = "top") |>
  split_plot(by = Study, axes = "all_x", axis.titles = "margins", labeller = label_both)
FitPlot

rm(FitComparison, FitPlot)
################################################################################
################################### LCA Plots ################################## 
################################################################################

poLCA.entropy(m1c)
poLCA.entropy(m2c)

m1probability <- melt(m1c$probs)
m1probabilityplot <- m1probability %>%
  mutate(
    L1 = factor(L1, levels = unique(L1), labels = c("Realness of Nonbinary Identity", "Evaluation of Nonbinary Identity", "Stability of Nonbinary Identity","Realness of Gay and Lesbian Identity","Evaluation of Gay and Lesbian Identity", "Stability of Gay and Lesbian Identity", "Realness of Bisexual Identity", "Evaluation of Bisexual Identity", "Stability of Bisexual Identity")),
    Var1 = factor(Var1),                    
    Var2 = factor(Var2)                      
  )
Class1Plot <- m1probabilityplot |>
  tidyplot(x = Var2, y = value, color = Var1) |>
  add_areastack_relative(alpha = .75) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |>
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 72, family = "Helvetica Neue", face = "plain") |>
  adjust_title("Study 1 Class Plot") |>
  adjust_legend_title("Class") |>
  adjust_legend_position(position = "top") |>
  rename_x_axis_levels(new_names = c(
    "Pr(1)" = "1",
    "Pr(2)" = "2",
    "Pr(3)" = "3",
    "Pr(4)" = "4",
    "Pr(5)" = "5",
    "Pr(6)" = "6",
    "Pr(7)" = "7")) |>
  adjust_x_axis_title("Rating") |>
  adjust_y_axis_title("Rating Probability") |>
  rename_color_levels(new_names = c(
    "class 1: " = "Class 1",
    "class 2: " = "Class 2",
    "class 3: " = "Class 3",
    "class 4: " = "Class 4"
  )) |>
  split_plot(by = L1, axes = "all_x", axis.titles = "margins", scales = "fixed", ncol = 3, nrow = 3)


m2probability <- melt(m2c$probs)
m2probability <- m2probability %>%
  mutate(
    L1 = factor(L1, levels = unique(L1), labels = c("Realness of Transgender Identity (Women)", "Evaluation of Transgender Identity (Women)", "Stability of Transgender Identity (Women)","Realness of Transgender Identity (Men)", "Evaluation of Transgender Identity (Men)", "Stability of Transgender Identity (Men)","Realness of Nonbinary Identity", "Evaluation of Nonbinary Identity", "Stability of Nonbinary Identity","Realness of Gay and Lesbian Identity","Evaluation of Gay and Lesbian Identity", "Stability of Gay and Lesbian Identity", "Realness of Bisexual Identity", "Evaluation of Bisexual Identity", "Stability of Bisexual Identity")),
    Var1 = factor(Var1),                    
    Var2 = factor(Var2)                      
  )
Class2Plot <- m2probability |>
  tidyplot(x = Var2, y = value, color = Var1) |>
  add_areastack_relative(alpha = .75) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |>
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 72, family = "Helvetica Neue", face = "plain") |>
  adjust_title("Study 2 Class Plot") |>
  adjust_legend_title("Class") |>
  adjust_legend_position(position = "top") |>
  rename_x_axis_levels(new_names = c(
    "Pr(1)" = "1",
    "Pr(2)" = "2",
    "Pr(3)" = "3",
    "Pr(4)" = "4",
    "Pr(5)" = "5",
    "Pr(6)" = "6",
    "Pr(7)" = "7")) |>
  adjust_x_axis_title("Rating") |>
  adjust_y_axis_title("Rating Probability") |>
  rename_color_levels(new_names = c(
    "class 1: " = "Class 1",
    "class 2: " = "Class 2",
    "class 3: " = "Class 3",
    "class 4: " = "Class 4",
    "class 5: " = "Class 5"
  )) |>
  split_plot(by = L1, axes = "all_x", axis.titles = "margins", scales = "fixed", ncol = 3, nrow = 5)

################################################################################
################################ Remove Objects ################################
################################################################################

rm(f, i, data, data1, data2)

