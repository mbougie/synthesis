library(ggplot2)
library(maps)
library(rgdal)# R wrapper around GDAL/OGR
library(sp)
require("RPostgreSQL")
library(postGIStools)
library(plyr)
# library(dplyr)
library(viridis)
library(scales)
library(rjson)
# library(jsonlite)
require(RColorBrewer)
library(glue)
# library(ggpubr)
library(cowplot)


library(dplyr)



library(gridBase)
library(grid)
library(gridExtra) #load Grid

rootpath = 'C:\\Users\\Bougie\\Desktop\\Gibbs\\scripts\\projects\\synthesis\\intensification\\shemas\\seth'

##### link to scripts #####################################################
source(paste(rootpath, 'r_code\\intensification_maps_seth_v2.R', sep='\\'))
source(paste(rootpath, 'r_code\\graphics_dummy_legend.R', sep='\\'))

###### link to json files #################################################
json_file = 'C:\\Users\\Bougie\\Desktop\\Gibbs\\scripts\\projects\\synthesis\\intensification\\shemas\\seth\\json\\synthesis_master.json'
jsondata <- fromJSON(file=json_file)

json_legend_file = 'C:\\Users\\Bougie\\Desktop\\Gibbs\\scripts\\projects\\synthesis\\intensification\\shemas\\seth\\json\\dummy_legends.json'
json_dummy <- fromJSON(file=json_legend_file)



user <- "mbougie"
host <- '144.92.235.105'
port <- '5432'
password <- 'Mend0ta!'

### Make the connection to database ######################################################################
con_synthesis <- dbConnect(PostgreSQL(), dbname = 'synthesis', user = user, host = host, port=port, password = password)


rm(jsondata)
jsondata <- fromJSON(file=json_file)


###########################################################################################
#####get the dataframes###################################################################
###########################################################################################



### Main query that all the datasets will reference #####################################################
getquery <- function(parent){
  if(parent == 'intensification'){
    query_ext <- 'SELECT
    "dataset".fips,
    "dataset".mean,
    ("dataset".mean * conversion_table.conv_factor)  as current_field,
    \'lookup\' as dataset,
    geom
    FROM
    intensification_11_20_2019."dataset"
    INNER JOIN spatial.counties
    ON "dataset".fips = counties.fips
    INNER JOIN misc.conversion_table ON \'lookup\' = conversion_table.intensification'
    
    
    print(query_ext)
    return(query_ext)
  }else if(parent == 'extensification'){
    query_ext <- 'SELECT
    "dataset".fips,
    "dataset".mean,
    ("dataset".mean * conversion_table.conv_factor)  as current_field,
    \'lookup\' as dataset,
    geom
    FROM
    intensification_11_20_2019."dataset"
    INNER JOIN spatial.counties
    ON "dataset".fips = counties.fips
    INNER JOIN misc.conversion_table ON \'lookup\' = conversion_table.intensification'
    
    
    print(query_ext)
    return(query_ext)
  }
}








# 
# ### Main query that all the datasets will reference #####################################################
# query_ext <- 'SELECT
#               "dataset".fips,
#               "dataset".mean,
#               ("dataset".mean * conversion_table.conv_factor)  as current_field,
#               \'lookup\' as dataset,
#               geom
#               FROM
#               intensification_11_20_2019."dataset"
#               INNER JOIN spatial.counties
#               ON "dataset".fips = counties.fips
#               INNER JOIN misc.conversion_table ON \'lookup\' = conversion_table.intensification'
# 
# 
# print(query_ext)
# 


###########################################################################################
######--------cc---------- ###########################################################################################################
###########################################################################################

parent = 'intensification'
child = 'cc'
grandchild = 'phos'


# ############# CC #############################################
# ### ext #####
# query_specific <- gsub("dataset",jsondata$cc$phos$dataset,query_ext)
# query_specific <- gsub("lookup",jsondata$cc$phos$lookup,query_specific)
# 
# print(query_specific)
# 
# jsondata$cc$phos$df <- get_postgis_query(con_synthesis,
#                                                   query_specific,
#                                                   geom_name = "geom")



query_specific <- gsub("dataset",jsondata[[parent]][[child]][[grandchild]]$dataset,getquery(parent))
query_specific <- gsub("lookup",jsondata[[parent]][[child]][[grandchild]]$lookup,query_specific)

print(query_specific)

jsondata[[parent]][[child]][[grandchild]]$df <- get_postgis_query(con_synthesis,
                                                                  query_specific,
                                                                  geom_name = "geom")################################################################






###########################################################################################
######--------oo---------- ###########################################################################################################
###########################################################################################
parent = 'intensification'
child = 'oo'
grandchild = 'phos'


# ### ext #####
# query_specific <- gsub("dataset",jsondata$oo$phos$dataset,query_ext)
# query_specific <- gsub("lookup",jsondata$oo$phos$lookup,query_specific)
# 
# print(query_specific)
# 
# jsondata$oo$phos$df <- get_postgis_query(con_synthesis,
#                                                   query_specific,
#                                                   geom_name = "geom")


query_specific <- gsub("dataset",jsondata[[parent]][[child]][[grandchild]]$dataset,getquery(parent))
query_specific <- gsub("lookup",jsondata[[parent]][[child]][[grandchild]]$lookup,query_specific)

print(query_specific)

jsondata[[parent]][[child]][[grandchild]]$df <- get_postgis_query(con_synthesis,
                                                                  query_specific,
                                                                  geom_name = "geom")
################################################################



###########################################################################################
######--------oo---------- ###########################################################################################################
###########################################################################################
parent = 'intensification'
child = 'co'
grandchild = 'phos'

# ### ext #####
# query_specific <- gsub("dataset",jsondata$co$phos$dataset,query_ext)
# query_specific <- gsub("lookup",jsondata$co$phos$lookup,query_specific)
# 
# print(query_specific)
# 
# jsondata$co$phos$df <- get_postgis_query(con_synthesis,
#                                                   query_specific,
#                                                   geom_name = "geom")


query_specific <- gsub("dataset",jsondata[[parent]][[child]][[grandchild]]$dataset,getquery(parent))
query_specific <- gsub("lookup",jsondata[[parent]][[child]][[grandchild]]$lookup,query_specific)

print(query_specific)

jsondata[[parent]][[child]][[grandchild]]$df <- get_postgis_query(con_synthesis,
                                                                  query_specific,
                                                                  geom_name = "geom")






###########################################################################################
######--------net---------- ###########################################################################################################
###########################################################################################
parent = 'intensification'
child = 'net'
grandchild = 'phos_grouped_legend'

### ext #####
# query_specific <- gsub("dataset",jsondata$net$phos_grouped_legend$dataset,query_ext)
# query_specific <- gsub("lookup",jsondata$net$phos_grouped_legend$lookup,query_specific)
# 
# print(query_specific)
# 
# jsondata$net$phos_grouped_legend$df <- get_postgis_query(con_synthesis,
#                                                   query_specific,
#                                                   geom_name = "geom")

query_specific <- gsub("dataset",jsondata[[parent]][[child]][[grandchild]]$dataset,getquery(parent))
query_specific <- gsub("lookup",jsondata[[parent]][[child]][[grandchild]]$lookup,query_specific)

print(query_specific)

jsondata[[parent]][[child]][[grandchild]]$df <- get_postgis_query(con_synthesis,
                                                                  query_specific,
                                                                  geom_name = "geom")






##############nlch########################################################################



###########################################################################################
######--------cc---------- ###########################################################################################################
###########################################################################################

parent = 'intensification'
child = 'cc'
grandchild = 'nlch'


# ############# CC #############################################
# ### ext #####
# query_specific <- gsub("dataset",jsondata$cc$phos$dataset,query_ext)
# query_specific <- gsub("lookup",jsondata$cc$phos$lookup,query_specific)
# 
# print(query_specific)
# 
# jsondata$cc$phos$df <- get_postgis_query(con_synthesis,
#                                                   query_specific,
#                                                   geom_name = "geom")



query_specific <- gsub("dataset",jsondata[[parent]][[child]][[grandchild]]$dataset,getquery(parent))
query_specific <- gsub("lookup",jsondata[[parent]][[child]][[grandchild]]$lookup,query_specific)

print(query_specific)

jsondata[[parent]][[child]][[grandchild]]$df <- get_postgis_query(con_synthesis,
                                                                  query_specific,
                                                                  geom_name = "geom")################################################################






###########################################################################################
######--------oo---------- ###########################################################################################################
###########################################################################################
parent = 'intensification'
child = 'oo'
grandchild = 'nlch'


# ### ext #####
# query_specific <- gsub("dataset",jsondata$oo$phos$dataset,query_ext)
# query_specific <- gsub("lookup",jsondata$oo$phos$lookup,query_specific)
# 
# print(query_specific)
# 
# jsondata$oo$phos$df <- get_postgis_query(con_synthesis,
#                                                   query_specific,
#                                                   geom_name = "geom")


query_specific <- gsub("dataset",jsondata[[parent]][[child]][[grandchild]]$dataset,getquery(parent))
query_specific <- gsub("lookup",jsondata[[parent]][[child]][[grandchild]]$lookup,query_specific)

print(query_specific)

jsondata[[parent]][[child]][[grandchild]]$df <- get_postgis_query(con_synthesis,
                                                                  query_specific,
                                                                  geom_name = "geom")
################################################################



###########################################################################################
######--------oo---------- ###########################################################################################################
###########################################################################################
parent = 'intensification'
child = 'co'
grandchild = 'nlch'

# ### ext #####
# query_specific <- gsub("dataset",jsondata$co$phos$dataset,query_ext)
# query_specific <- gsub("lookup",jsondata$co$phos$lookup,query_specific)
# 
# print(query_specific)
# 
# jsondata$co$phos$df <- get_postgis_query(con_synthesis,
#                                                   query_specific,
#                                                   geom_name = "geom")


query_specific <- gsub("dataset",jsondata[[parent]][[child]][[grandchild]]$dataset,getquery(parent))
query_specific <- gsub("lookup",jsondata[[parent]][[child]][[grandchild]]$lookup,query_specific)

print(query_specific)

jsondata[[parent]][[child]][[grandchild]]$df <- get_postgis_query(con_synthesis,
                                                                  query_specific,
                                                                  geom_name = "geom")






###########################################################################################
######--------net---------- ###########################################################################################################
###########################################################################################
parent = 'intensification'
child = 'net'
grandchild = 'nlch_grouped_legend'

### ext #####
# query_specific <- gsub("dataset",jsondata$net$phos_grouped_legend$dataset,query_ext)
# query_specific <- gsub("lookup",jsondata$net$phos_grouped_legend$lookup,query_specific)
# 
# print(query_specific)
# 
# jsondata$net$phos_grouped_legend$df <- get_postgis_query(con_synthesis,
#                                                   query_specific,
#                                                   geom_name = "geom")

query_specific <- gsub("dataset",jsondata[[parent]][[child]][[grandchild]]$dataset,getquery(parent))
query_specific <- gsub("lookup",jsondata[[parent]][[child]][[grandchild]]$lookup,query_specific)

print(query_specific)

jsondata[[parent]][[child]][[grandchild]]$df <- get_postgis_query(con_synthesis,
                                                                  query_specific,
                                                                  geom_name = "geom")


######################################################################################################
############  get ggplot objects #####################################################################
######################################################################################################

getggplotObject <- function(obj_vector){
  
  ###declare the empty list that will hold all the ggplot objects
  ggplot_object_list <- list()
  
  for (obj in obj_vector){
    ggplot_object = createMap(obj)
    ggplot_object_list <- append(ggplot_object_list, list(ggplot_object))
  }
  return(ggplot_object_list)
  
}









######################################################################################################
############  get group legend #####################################################################
######################################################################################################

getggplotObject_map <- function(obj_vector){
  
  ###declare the empty list that will hold all the ggplot objects
  ggplot_object_list <- list()
  
  for (obj in obj_vector){
    ggplot_object = createDummy(obj)
    ggplot_object_list <- append(ggplot_object_list, list(ggplot_object))
  }
  return(ggplot_object_list)
  
}
 


############################################################################################
# # panel test ################################################################################
###########################################################################################
rm(runMain)
########### main code ########################################################
runMain <- function(arg1, arg2){
  
  rm(p1_1, p1_2, p1_3, p1_4, p2_1, p2_2, p2_3, p2_4)
  source(paste(rootpath, 'r_code\\intensification_maps_seth_v2.R', sep='\\'))
  
  p1_1 = createMap(jsondata$intensification$cc[[arg1]])
  p1_2 = createMap(jsondata$intensification$oo[[arg1]])
  p1_3 = createMap(jsondata$intensification$co[[arg1]])
  p1_4 = createMap(jsondata$intensification$net[[paste(arg1, "grouped_legend", sep="_")]])
  
  
  p2_1 = createMap(jsondata$intensification$cc[[arg2]])
  p2_2 = createMap(jsondata$intensification$oo[[arg2]])
  p2_3 = createMap(jsondata$intensification$co[[arg2]])
  p2_4 = createMap(jsondata$intensification$net[[paste(arg2, "grouped_legend", sep="_")]])
  
  
  rm(dummy_legend1, dummy_legend2)
  source(paste(rootpath, 'r_code\\graphics_dummy_legend.R', sep='\\'))
  
  ####create legend object
  dummy_legend1 <- createDummy(json_dummy$intensification[[arg1]])
  dummy_legend2 <- createDummy(json_dummy$intensification[[arg2]])
  
  
  lay <- rbind(c(1,1,1,1,1,1,1),
               c(1,1,1,1,1,1,1),
               c(2,2,2,2,2,2,2),
               c(2,2,2,2,2,2,2),
               c(3,3,3,3,3,3,3),
               c(3,3,3,3,3,3,3),
               c(4,4,4,4,4,4,4),
               c(4,4,4,4,4,4,4),
               c(5,5,5,5,5,5,5))
  
  
  rm(col1, col2)
  col1 <- arrangeGrob(p1_1, p1_2, p1_3, p1_4, dummy_legend1, layout_matrix = lay)
  col2 <- arrangeGrob(p2_1, p2_2, p2_3, p2_4, dummy_legend2, layout_matrix = lay)
  
  plot_grid(col1,col2, ncol = 2, rel_heights = c(1, .1))
  fileout = 'H:\\new_data_8_18_19\\d_drive\\synthesis\\s35\\intensification\\schemas\\seth\\graphics\\test.png'
  ggsave(fileout, width = 34, height = 38, dpi = 500)
}
#####################################################################################



runMain('phos', 'nlch')























































############# junk drawer code #####################################################
###############################################################################

# #### test maps ######################################################################
# runTest_maps <- function(){
#   rm(phos_1, nlch_1)
#   source(paste(rootpath, 'r_code\\intensification_maps_seth_v2.R', sep='\\'))
#   
#   phos_1 = createMap(jsondata$intensification$cc$phos)
#   nlch_1 = createMap(jsondata$intensification$cc$nlch)
#   
#   lay <- rbind(c(1,1,1,1,1,1,1),
#                c(1,1,1,1,1,1,1),
#                c(NA,NA,NA,NA,NA,NA,NA),
#                c(NA,NA,NA,NA,NA,NA,NA),
#                c(NA,NA,NA,NA,NA,NA,NA),
#                c(NA,NA,NA,NA,NA,NA,NA),
#                c(NA,NA,NA,NA,NA,NA,NA),
#                c(NA,NA,NA,NA,NA,NA,NA),
#                c(NA,NA,NA,NA,NA,NA,NA))
#   
#   col1 <- arrangeGrob(phos_1, layout_matrix = lay)
#   col2 <- arrangeGrob(nlch_1, layout_matrix = lay)
#   
#   plot_grid(col1,col2, ncol = 2, rel_heights = c(1, .1))
#   fileout = 'H:\\new_data_8_18_19\\d_drive\\synthesis\\s35\\intensification\\schemas\\seth\\graphics\\test.png'
#   ggsave(fileout, width = 34, height = 38, dpi = 500)
# }






### get the grouped legend ##################################
####NOTE: before to remove the legend object and refresh the function if changes are made to it!!!!!!!!
# rm(dummy_legend)
# source(paste(rootpath, 'r_code\\graphics_dummy_legend.R', sep='\\'))
# 
# ####create legend object
# dummy_legend1 <- createDummy(json_dummy$intensification$phos)
# dummy_legend2 <- createDummy(json_dummy$intensification$nlch)

# ###create a matrix that will be filled with the plots above
# lay <- rbind(c(1,1,1,1,1,1,1),
#              c(1,1,1,1,1,1,1))


# lay <- rbind(c(1,1,1,1,1,1,1),
#              c(1,1,1,1,1,1,1),
#              c(2,2,2,2,2,2,2),
#              c(2,2,2,2,2,2,2),
#              c(3,3,3,3,3,3,3),
#              c(3,3,3,3,3,3,3),
#              c(4,4,4,4,4,4,4),
#              c(4,4,4,4,4,4,4),
#              c(5,5,5,5,5,5,5))


# lay <- rbind(c(NA,NA,NA,NA,NA,NA,NA),
#              c(NA,NA,NA,NA,NA,NA,NA),
#              c(NA,NA,NA,NA,NA,NA,NA),
#              c(NA,NA,NA,NA,NA,NA,NA),
#              c(NA,NA,NA,NA,NA,NA,NA),
#              c(NA,NA,NA,NA,NA,NA,NA),
#              c(NA,NA,NA,NA,NA,NA,NA),
#              c(NA,NA,NA,NA,NA,NA,NA),
#              c(5,5,5,5,5,5,5))












#merge all three plots within one grid (and visualize this)
# g <- arrangeGrob(dummy_legend, layout_matrix = lay)


# col1 <- arrangeGrob(dummy_legend1, layout_matrix = lay)
# col2 <- arrangeGrob(dummy_legend2, layout_matrix = lay)
# col1 <- arrangeGrob(phos_1, phos_2, phos_3, phos_4, dummy_legend1, layout_matrix = lay)
# col2 <- arrangeGrob(nlch_1, nlch_2, nlch_3, nlch_4, dummy_legend2, layout_matrix = lay)
# 
# 
# 
# 
# plot_grid(col1,col2, ncol = 2, rel_heights = c(1, .1))
# fileout = 'H:\\new_data_8_18_19\\d_drive\\synthesis\\s35\\intensification\\schemas\\seth\\graphics\\test.png'
# ggsave(fileout, width = 34, height = 38, dpi = 500)

























# ############################################################################################
# # # panel 1 ################################################################################
# ###########################################################################################
# 
# 
# #### Get and store ggplot objects (i.e. the d objects created in the map script)
# list_exp <- getggplotObject(list(jsondata$agroibis$AET_ext, jsondata$agroibis$Irrig_ext, jsondata$carbon$carbon_ext))
# list_aban <- getggplotObject(list(jsondata$agroibis$AET_abd, jsondata$agroibis$Irrig_abd, jsondata$carbon$carbon_abd))
# 
# ###create panel image ######################
# dir = "H:\\new_data_8_18_19\\d_drive\\synthesis\\s35\\intensification\\graphics\\"
# fileout=paste(dir,"intensification_panel_1",".png", sep="")
# 
# col1 = plot_grid(plotlist = list_exp, ncol = 1, nrow = 3, align = 'vh')
# col2 = plot_grid(plotlist = list_aban, ncol = 1, nrow = 3, align = 'vh')
# 
# plot_grid(col1,col2, ncol = 2, rel_heights = c(1, .1))
# ####the key to not getting a bit error is to change the dpi  ---- no difference between 500dpi and 300dpi
# ggsave(fileout, width = 34, height = 38, dpi = 500)
# 
# 
# 
# 
# ############################################################################################
# # # panel 2 ################################################################################
# ###########################################################################################
# 
# 
# #### Get and store ggplot objects (i.e. the d objects created in the map script)
# list_exp <- getggplotObject(list(jsondata$agroibis$TPrunoff_ext, jsondata$agroibis$NLeach_ext, jsondata$agroibis$SEDrunoff_ext))
# list_aban <- getggplotObject(list(jsondata$agroibis$TPrunoff_abd, jsondata$agroibis$NLeach_abd, jsondata$agroibis$SEDrunoff_abd))
# 
# ###create panel image ######################
# dir = "H:\\new_data_8_18_19\\d_drive\\synthesis\\s35\\intensification\\graphics\\"
# fileout=paste(dir,"intensification_panel_2",".png", sep="")
# 
# col1 = plot_grid(plotlist = list_exp, ncol = 1, nrow = 3, align = 'vh')
# col2 = plot_grid(plotlist = list_aban, ncol = 1, nrow = 3, align = 'vh')
# 
# plot_grid(col1,col2, ncol = 2, rel_heights = c(1, .1))
# ####the key to not getting a bit error is to change the dpi  ---- no difference between 500dpi and 300dpi
# ggsave(fileout, width = 34, height = 38, dpi = 500)
# 
# 
# 
# 
# 
# ############################################################################################
# # # panel 3 ################################################################################
# ###########################################################################################
# 
# 
# #### Get and store ggplot objects (i.e. the d objects created in the map script)
# list_exp <- getggplotObject(list(jsondata$napp$napp_ext, jsondata$n2o$n2o_ext))
# list_aban <- getggplotObject(list(jsondata$napp$napp_abd, jsondata$n2o$n2o_abd))
# 
# ###create panel image ######################
# dir = "H:\\new_data_8_18_19\\d_drive\\synthesis\\s35\\intensification\\graphics\\"
# fileout=paste(dir,"intensification_panel_3",".png", sep="")
# 
# col1 = plot_grid(plotlist = list_exp, ncol = 1, nrow = 2, align = 'vh')
# col2 = plot_grid(plotlist = list_aban, ncol = 1, nrow = 2, align = 'vh')
# 
# plot_grid(col1,col2, ncol = 2, rel_heights = c(1, .1))
# ####the key to not getting a bit error is to change the dpi  ---- no difference between 500dpi and 300dpi
# ggsave(fileout, width = 34, height = 38, dpi = 500)
# 

