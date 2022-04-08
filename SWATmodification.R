
#Sys.unsetenv("http_proxy"); Sys.unsetenv("https_proxy")
#--------------------install and load SWATmodel from EcoHydRology repo
install.packages("EcoHydRology", repos="http://R-Forge.R-project.org")
library(EcoHydRology)
dir.create("~/src")
setwd("~/src")
system("svn checkout svn://scm.r-forge.r-project.org/svnroot/ecohydrology/")
install.packages("~/src/ecohydrology/pkg/SWATmodel/",repos = NULL)
library(SWATmodel)
#-----SET your working directory to your ArcSWAT init folder dir
setwd("~/SWAT_AliNakhli/TXTINOUT/")
source("https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/SWATmodel/R/readSWAT.R?root=ecohydrology")
save(readSWAT,file="readSWAT.R")
source("https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/EcoHydRology/R/setup_swatcal.R?root=ecohydrology")
source("https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/EcoHydRology/R/swat_objective_function_rch.R?root=ecohydrology")
file.remove(list.files(pattern="output."))
runSWAT2012()
#---------using readSWAT function to read output files "output.sub","output.rch","output.hru"
outdata_rch = readSWAT("rch",".")
outdata_sub= readSWAT("sub",".")
#-----------for reading output.hru you might get some error because of headers so we will fix it here
###remember to check the columns merge two heders such as output in TxtInOutLOC
pathtofile="."
cfilename_hruoutA=paste0(pathtofile,"/output.hru")
cfilename_hruout=readLines(paste0(pathtofile,"/output.hru"))
textreplace=gsub(pattern = "WTAB CLIm WTAB SOLm", replace="WTABclimwtabsolm",x=cfilename_hruout)
writeLines(textreplace, con=cfilename_hruoutA)
outdata_hru=readSWAT("hru",".") 
##-------So now we have all swat initialization and output files from standard SWAT, let's do some management stuff------
##we know that using standard SWAT initialization we can get area of each HRUs
#########################################################################
###geting Date from SWAT initialization file.cio
###set directory to the swat initialization folder########################
pathtofile="."
cfilename=paste0(pathtofile,"/file.cio")
SWATnbyr = read.fortran(textConnection(readLines(cfilename)[8]), "f20")[1,]
SWATiyr = read.fortran(textConnection(readLines(cfilename)[9]), "f20")[1,]
SWATidaf = read.fortran(textConnection(readLines(cfilename)[10]), "f20")[1,]
SWATidal = read.fortran(textConnection(readLines(cfilename)[11]), "f20")[1,]
startdate=as_date(paste0(SWATiyr,"-01-01")) + SWATidaf -1
enddate=as_date(paste0(SWATiyr+SWATnbyr -1,"-01-01")) + SWATidal -1
AllDays=data.frame(date=seq(startdate, by = "day", length.out = enddate-startdate+1))
#####################################################################################################
####################################################################
#####here we need to generate the data table that have user info: nrows=number of HRUs , ncol= 13 ### MGT_DATAFRAME
#The info that user needs to fill out
#1-Barn_number =B01, B02, B03, B04,...B99 etc.
#2-HRU_number for the specified Barn = 000010111, 000010112, 000010113, 000010114 for B01; AND 000020121, 000020122, 000020123 for B02
###3-AREA_ha area of eaxh HRU from outdata_hru$AREAkm2[outdata_hru_STAN$GIS==i][1]*100 ) 
#4-year of fertilizer application----year_app
#5-month of fertilizer application---month_app
#6-day of fertilizer application---day_app
###7-Date of fertilizer application
#8-MGT-OP ##mgt op number, 3 for fert application
###9-FERT_NAME #this is fert name in fert.dat file ##BbbYYJJJ USE THis format for fert_name
#10-FRT_KG #the amount of fertilizer applied to HRU (kg/ha) for HRU
###11-fert_applied # area_hru*frt_kg (kg)
#12-FRT_SURFACE # 0.00
#13-FERT_ID #fertilizer ID in fert.dat file 
#################################################################
##how many lines are in fert.dat?
######################################################
pathtofile="."
f=file(paste0(pathtofile,"/fert.dat"), open="rb")
nlines=0L
while (length(chunk <- readBin(f, "raw", 65536)) > 0) {
  nlines <- nlines + sum(chunk == as.raw(10L))
}
row_fert_dat=print(nlines)+1
close(f)
##########################
totalnumber_HRU=7 # this is the total number of hrus that we want to do mgt application and equals to MGT_DATAFRAME nrow 
Make_MGT_DATAFRAME= function(totalnumber_HRU){
  if(missing(totalnumber_HRU)){
    totalnumber_HRU=0
  }
  MGT_DATAFRAME=data.frame(
    Barn_number=character(),
    HRU_number=numeric(),
    #Area_ha=integer(),
    year_app=integer(),
    month_app=integer(),
    day_app=integer(),
    MGT_OP=numeric(),
    #FERT_NAME=character(),
    FRT_KG=integer()
    #FERT_APPLIED=numeric(),
    #FRT_SURFACE= numeric(),
    #FERT_ID=numeric()
  )
  MGT_DATAFRAME[1:totalnumber_HRU,] = NA 
  return(MGT_DATAFRAME)
}

mgt_datafram= Make_MGT_DATAFRAME(totalnumber_HRU)
################################################here we try to open the data fram in google sheet so that the user can modify it manually
###user just need to fill Barn_number, HRU_number,year of fertilizer application----year_app,
#####-month of fertilizer application---month_app, day of fertilizer application---day_app,-MGT-OP ##mgt op number, 3 for fert application,
####FRT_KG,FRT_SURFACE # 0.00
#devtools::install_github("tidyverse/googlesheets4")
pacman::p_load(googlesheets4)
gsheet = gs4_create(
  "mgt_dataframe",
  sheets=mgt_datafram
)
mgt_datafram=read_sheet(unclass(gsheet))
mgt_datafram$Area_ha=NA
mgt_datafram$Date_app=as.Date(NA)
mgt_datafram$FERT_NAME=NA
mgt_datafram$FRT_SURFACE=NA
mgt_datafram$FERT_ID=NA
#######################################################
for (i in 1:nrow(mgt_datafram)){
  mgt_datafram$Date_app[i]=as.Date(paste0(mgt_datafram$day_app[i]," ",month.name[mgt_datafram$month_app[i]],",",mgt_datafram$year_app[i]), format = "%d %B, %Y")
  mgt_datafram$Area_ha[i]=outdata_hru$AREAkm2[outdata_hru$GIS==mgt_datafram$HRU_number[i]][1]*100
  mgt_datafram$FERT_NAME[i]=paste0(mgt_datafram$Barn_number[i],mgt_datafram$year_app[i] %% 100,yday(mgt_datafram$Date_app[i]))
}
mgt_datafram$FERT_ID=seq(row_fert_dat+1,(row_fert_dat+nrow(mgt_datafram)),by=1)
mgt_datafram$FERT_APPLIED=mgt_datafram$Area_ha*mgt_datafram$FRT_KG
mgt_datafram$FRT_SURFACE=format(round(0.00, 2), nsmall = 2)


#############################################################################
####Now that we have all inputs for *.mgt files we will modify them for selected HRUs
#############################################################################
pathtofile="."
for (i in mgt_datafram$HRU_number){
  #i=10111
  b=formatC(i, width = 9, format = "d", flag = "0")
  assign(paste0("cfilename_mgtA",i,sep=""), paste0(pathtofile,"/",b,".mgt"))
  assign(paste0("cfilename_mgt",i,sep=""), readLines(paste0(pathtofile,"/",b,".mgt")))
  c=formatC(mgt_datafram$month_app[mgt_datafram$HRU_number==i], width = 2, format = "d", flag = "")
  d=formatC(mgt_datafram$day_app[mgt_datafram$HRU_number==i], width = 2, format = "d", flag = "")
  e=formatC(mgt_datafram$FERT_ID[mgt_datafram$HRU_number==i], width = 4, format = "d", flag = "")
  f=formatC(format(round(mgt_datafram$FRT_KG[mgt_datafram$HRU_number==i],5),nsmall=5), width = 12, format = "d", flag = "")
  g=formatC(mgt_datafram$FRT_SURFACE[mgt_datafram$HRU_number==i], width = 6, format = "d", flag = "")
  assign(paste0("mgt_fert",i,sep=""), paste0(" ",c," ",d,"           ",mgt_datafram$MGT_OP[mgt_datafram$HRU_number==i]," ",e,"        ",f," ",g))
  textreplace=gsub(pattern = readLines(get(paste0("cfilename_mgtA",i,sep="")))[31],replace=get(paste0("mgt_fert",i,sep="")),x=get(paste0("cfilename_mgt",i,sep=""))) 
  writeLines(textreplace, con=get(paste0("cfilename_mgtA",i,sep="")))
}
