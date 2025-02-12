## ---------------------------
## Script name: piotr_parallel.R
## Description: Wrapper for Piotr Software to utilize multi-core processing. Piotr software was written by Roman Geisthövel (c) 2019 accompanying PhD thesis Automatic Swiss style rock depiction. See (http://motlimot.net/software.html)
## Author: BrankeJ
## Date Created/last updated: 2025-02-07
## License: License: GNU GPL V.3 or later
## Copyright (c) BrankeJ, 2025
## ---------------------------
##  ## Notes:
##   
##
## ---------------------------

################ LIBRARIES #####################
library(terra)
library(dplyr)
library(foreach)
library(doParallel)
library(doSNOW)
library(tcltk)


################ FUNCTIONS ################


source(file.path(r"(C:\Users\dnb\Desktop\wrapping-piotr)","piotr_functions.R"))

################ INIT #####################


rockmask = F

# initial paths
DGM_path = r"(G:\TIROL_DGM\DGM_Tirol_5m_epsg31254.tif)" # path to DGM
piotr_exe = r"(G:\piotr_tests\PiotrWin64\Piotr\piotr.exe)" #path to piotr.exe
AOI_path = r"(G:\piotr_tests\PiotrWin64\Piotr\data\AOI_Venediger_31254.shp)" #path to AOI shp
rockmask_path = r"(G:\piotr_tests\PiotrWin64\Piotr\data\FELS_Polygone_31254.shp)" #path to rockmask shp (optional if wanted => rockmask = T)

# piotr params
l = 15
overlap = 50
n_size  = 400

################ BEGIN #####################

# create tmp and out folders if not exist
if (isTRUE(file.path(dirname(piotr_exe), "tmp"))) {
  cat("already exists")
} else {
  dir.create(file.path(dirname(piotr_exe), "tmp"))
}
if (isTRUE(file.path(dirname(piotr_exe), "out"))) {
  cat("already exists")
} else {
  dir.create(file.path(dirname(piotr_exe), "out"))
}


#read tif raster
DGM = rast(DGM_path)

#read AOI 
AOI = vect(AOI_path)

if (isTRUE(rockmask)) {
  #read rock mask
  rockmask = vect(rockmask_path)
}

#clip to AOI
DGM = crop(DGM, ext(AOI))

# rast features
resol = res(DGM)
nx = ncol(DGM)
ny = nrow(DGM)
extent = ext(DGM)
na_b = NAflag(DGM)

# prepare NAs
na = -32768
DGM <- classify(DGM, cbind(na_b, na))
NAflag(DGM) = na


# tiling with overlap
cte = create_tiles_ext(DGM, n_row = n_size, n_col = n_size, overlap = overlap)

# extent initial DGM for overlap tiles
DGM_ext = extend(DGM, cte$new_ext, fill = na)


if (isTRUE(rockmask)) {
  #rasterize rockmask
  rockmask.r = rasterize(rockmask, DGM_ext)
  rockmask.r <- classify(rockmask.r, cbind(NAflag(rockmask.r), 0))
  NAflag(rockmask.r) = na
}

################ MAIN #####################

#prepare ASC
#crop big by extent
message("Cropping BIG Raster by extent")
worth_checking_i = c()
not_worth_checking_i = c()
for (i in seq(1, length(cte$xy_ol_lst))) {
  #crop big DGM by extent
  tmp_cropped = crop(DGM_ext, ext(unname(cte$xy_ol_lst[[i]])))
  NAflag(tmp_cropped) = na #account for NAs
  
  if (isTRUE(rockmask)) {
    #crop rockmask by extent
    tmp_cropped_rockmask = crop(rockmask.r, ext(unname(cte$xy_ol_lst[[i]])))
    NAflag(tmp_cropped_rockmask) = na #account for NAs
  }
  
  #check whether all values NA
  if (unname(allNA(tmp_cropped)[1])[[1]] == T) {
    not_worth_checking_i = append(not_worth_checking_i, i)
  } else {
    worth_checking_i = append(worth_checking_i, i)
    tmpout = file.path(dirname(piotr_exe),"tmp", sprintf("tmp_%s.asc", i))
    writeRaster(tmp_cropped, tmpout, NAflag = na, overwrite = T) 
    
    if (isTRUE(rockmask)) {
      # rockmask tmp
      tmpout_rm = file.path(dirname(piotr_exe),"tmp", sprintf("tmp_rm_%s.asc", i))
      writeRaster(tmp_cropped_rockmask, tmpout_rm, NAflag = na, overwrite = T)
    }
    
  }
  cat("\r","finished percent: ", i/length(cte$xy_ol_lst)*100)
}
cat("")

# problem raster work with parallelization
#https://stackoverflow.com/questions/70191164/null-value-passed-as-symbol-address-error-in-foreach-loop-r



message("Multi Piotr")
# #setup parallel backend to use many processors
cores = detectCores()
cl <- makeCluster(cores[1] - 1) #not to overload your computer
registerDoSNOW(cl)

ntasks = max(worth_checking_i) #length(cte$xy_ol_lst)#nrow(iter) #TODO

pb <- tkProgressBar(max = ntasks)
progress <- function(n) setTkProgressBar(pb, n)
opts <- list(progress = progress)

st = Sys.time() #get starttime

finalMatrix <- foreach(i = 1:ntasks, .packages = c("terra"),.options.snow = opts, .verbose = T, .errorhandling = "stop") %dopar% {   ##load packages for forech!  #.combine=rbind #'pass'
  
  prog = multi_piotr(worth = worth_checking_i, extent = cte$xy_ol_lst, piotr_path = piotr_exe, l = l, out_f_name = "out", tmp_f_name = "tmp", rm = rockmask, iter = i) #calling a function
  #do other things if you want
  
  prog #Equivalent to finalMatrix = cbind(finalMatrix, tempMatrix)
}

stopCluster(cl) #stop cluster
close(pb) #close progress


#delete empty tmp folders
folders <- list.dirs(path = file.path(dirname(piotr_exe),"out") ,recursive = FALSE)
for (folder in folders) {
  if (length(dir(folder)) == 0) {
    unlink(folder, recursive = TRUE)
  }
}


progress_report = bind_rows(finalMatrix) #combine


##checks
print(sprintf("Average time per worked step %s min. %s sec.",unlist(strsplit(x = as.character(round(mean(progress_report$end_step[progress_report$worked == T]),digits = 2)), split = ".",fixed = T))[1], as.numeric(unlist(strsplit(x = as.character(round(mean(progress_report$end_step[progress_report$worked == T]),digits = 2)), split = ".",fixed = T))[2])/100*60)) #mean time in minutes per worked step

print(sprintf("Total time needed %s min. %s sec.",unlist(strsplit(x = as.character(round(max(progress_report$end_total)*60,digits = 2)), split = ".",fixed = T))[1],round(as.numeric(unlist(strsplit(x = as.character(round(max(progress_report$end_total)*60,digits = 2)), split = ".",fixed = T))[2])/100*60, digits = 0)))


#gpath=r"(C:\Users\dnb\Desktop\piotr_tests\PiotrWin64\Piotr\tmp)"
#worth_checking_i = sort(as.numeric(gsub(x=gsub(x=list.files(gpath, pattern = "tmp_rm_.*.prj"), replacement = "", pattern = "tmp_rm_",fixed = T), replacement = "", pattern=".prj", fixed = T)))

# mosaic to final res
rlist_RH = list()
rlist_SR = list()

# read out tiles into list and also remove overlap for better matching
for (i in seq(1,length(worth_checking_i))) {
  rlist_RH[[i]] = crop(rast(file.path(dirname(piotr_exe),"out", sprintf("RH_%s.png", worth_checking_i[i]))), ext(unname(cte$xy_ol_lst[[worth_checking_i[i]]])) - rep(overlap*2,4))
  rlist_SR[[i]] = crop(rast(file.path(dirname(piotr_exe),"out", sprintf("SR_%s.png", worth_checking_i[i]))), ext(unname(cte$xy_ol_lst[[worth_checking_i[i]]])) - rep(overlap*2,4))
}


# many SpatRasters to SpatRasterCollection 
rsrc_RH = sprc(rlist_RH)
rsrc_SR = sprc(rlist_SR)

# testing output to all different mosaic functions
mode_m = c("sum", "mean", "median", "min", "max")

for (i in seq(1,length(mode_m))) {
  # create mosaics
  m_RH = mosaic(rsrc_RH, fun = mode_m[i])
  m_SR = mosaic(rsrc_SR, fun = mode_m[i])
  
  # write mosaics
  writeRaster(m_RH, file.path(dirname(piotr_exe),"out",sprintf("ALL_RH_%s.tif", mode_m[i])), overwrite = T)
  writeRaster(m_SR, file.path(dirname(piotr_exe),"out",sprintf("ALL_SR_%s.tif", mode_m[i])), overwrite = T)  
}

message("Done!")

# TODO
# maybe add reason to why not worked = DGM not existing / DGM all NAs / no Rockmask / ...
