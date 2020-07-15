##### TITLE: Install and load libaries #########################################

# Filename: eventExtract_libraries.R
# Type: Script
# Title: Install and load libaries for running
# Version: DRAFT 1.0
# Date: 2018-11-14
# Author: JAshe
# Maintainer: j.ashe@exeter.ac.uk
# Description: Install and load libaries
# License: GPL

# Project: eventExtract

##### CODE: ####################################################################

#---- A. Packages in use -------------------------------------------------------

# installing the packages and loading libraries:

if(!require(dplyr)) {
  install.packages("dplyr"); require(dplyr)}  #load / install+load dplyr
if(!require(psych)) {
  install.packages("psych"); require(psych)}  #load / install+load psych
if(!require(tidyr)) {
  install.packages("tidyr"); require(tidyr)}  #load / install+load tidyr
if(!require(zoo)) {
  install.packages("zoo"); require(zoo)}  #load / install+load zoo
if(!require(lubridate)) {
  install.packages("lubridate"); require(lubridate)}  #load / install+load lubridate
# if(!require(rollRegres)) {
#   install.packages("rollRegres"); require(rollRegres)}  #load / install+load rollRegres

#---- B. Packages in use (PLOTS) -----------------------------------------------

if(!require(ggplot2)) {
  install.packages("ggplot2"); require(ggplot2)}  #load / install+load ggplot2
if(!require(gtable)) {
  install.packages("gtable"); require(gtable)}  #load / install+load gtable
if(!require(cowplot)) {
  install.packages("cowplot"); require(cowplot)}  #load / install+load cowplot
if(!require(viridis)) {
  install.packages("viridis"); require(viridis)}  #load / install+load viridis


#---- B. Packages not in use ---------------------------------------------------

# if(!require(gridExtra)) {
# install.packages("gridExtra"); require(gridExtra)}  #load / install+load gridExtra
# 
# if(!require(grid)) {
#   install.packages("grid"); require(grid)}  #load / install+load grid
# 
# if(!require(lattice)) {
#   install.packages("lattice"); require(lattice)}  #load / install+load lattice
# 
# if(!require(reshape2)) {
#   install.packages("reshape2"); require(reshape2)}  #load / install+load reshape2
# 
# if(!require(car)) {
#   install.packages("car"); require(car)}  #load / install+load car


##### END ######################################################################
