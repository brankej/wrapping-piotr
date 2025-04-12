## ---------------------------
## Script name: piotr_parallel.R
## Description: Wrapper for Piotr Software to utilize multi-core processing. Piotr software was written by Roman Geisthövel (c) 2019 accompanying PhD thesis Automatic Swiss style rock depiction. See (http://motlimot.net/software.html)
## Author: BrankeJ
## Date created: 2025-02-07
## Last updated: 2025-04-12
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

rm(list = ls()) # clean up

################ INIT #####################
#VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV

# FUNCTIONS
source("piotr_functions.R", chdir = T)

# initial paths
DGM_path = r"(/mnt/nvm2_data/TIROL_DGM/DGM_Tirol_5m_epsg31254.tif)" #EDIT HERE path to DGM 
AOI_path = r"(/mnt/nvm2_data/piotr_tests/PiotrWin64/Piotr/data/AOI_Venediger_31254.shp)" #EDIT HERE path to AOI shp
rockmask_path = r"(/mnt/nvm2_data/piotr_tests/PiotrWin64/Piotr/data/FELS_Polygone_31254.shp)" #EDIT HERE path to rockmask shp (optional if wanted => rockmask = T)
rockmask = T # if no rockmask given F!
clip_RH_rockmask = T # if a rockmask is given -> should final RH mosaic be masked by rockmask
piotr_exe = r"(/mnt/nvm2_data/piotr_tests/PiotrLinux64/piotr.exe)" #EDIT HERE path to piotr.exe
working_dir = dirname(piotr_exe) # or different
temp_folder = "tmp" # temporary folder for asc grids
output_folder = "out" # folder final grids

# piotr params
l = 15 # piotr param

# tiling options
n_size  = 400 # [cells] 
overlap = n_size/(n_size * 0.02) #50 # added overlap [cells]
crop_overlap_factor = 1 # to fully remove overlap in final merge -> 1 | for half remove overlap -> 2 
#not advised to go higher than 4 because of piotr border artifacts

# mosaicking / merging
stretching = T
mode_m = c("mean")#c("sum", "mean", "median", "min", "max", "first", "last") #mosaicking /merging functions | use minimum 1 
# only really takes effect if crop_overlap_factor > 1

#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
################ BEGIN #####################

# create tmp and out folders if not exist
if (isTRUE(file.path(dirname(piotr_exe), temp_folder))) {
  cat("already exists")
} else {
  dir.create(file.path(dirname(piotr_exe), temp_folder))
}
if (isTRUE(file.path(dirname(piotr_exe), output_folder))) {
  cat("already exists")
} else {
  dir.create(file.path(dirname(piotr_exe), output_folder))
}


#read tif raster
DGM = rast(DGM_path)

#read AOI 
AOI = vect(AOI_path)

if (isTRUE(rockmask)) {
  message("Using Rockmask!")
  #read rock mask
  rockmask_v = vect(rockmask_path)
} else {
  message("Not using Rockmask!")
}

#clip to AOI
DGM = crop(DGM, ext(AOI)) # check if same crs

# rast features
resol = res(DGM)
message(sprintf("Found elevation model resolution [m]: %sx%s", resol[1], resol[2]))
nx = ncol(DGM)
ny = nrow(DGM)
extent = ext(DGM)
na_b = NAflag(DGM)

# prepare NAs
na = -32768
DGM <- classify(DGM, cbind(na_b, na))
NAflag(DGM) = na


# tiling with overlap
cte <- create_tiles_ext(DGM, 
                        n_row = n_size, 
                        n_col = n_size, 
                        overlap = overlap, 
                        crop_overlap_factor = crop_overlap_factor)

# extent initial DGM for overlap tiles
DGM_ext = extend(DGM, cte$new_ext, fill = na)


if (isTRUE(rockmask)) {
  #rasterize rockmask
  rockmask.r = rasterize(rockmask_v, DGM_ext)
  rockmask.r <- classify(rockmask.r, cbind(NAflag(rockmask.r), 0))
  NAflag(rockmask.r) = na
}

################ MAIN #####################

############### PLOT ######################

# plot first three tiles to assess overlap

plot(ext(cte$xy_ol_lst[[1]]) + ext(cte$xy_ol_lst[[3]]))
plot(ext(cte$xy_ol_lst[[1]]), col="red",alpha=.7, add=T)
plot(ext(cte$xy_lst[[1]]), col="blue", alpha=.5, add=T)
plot(ext(cte$xy_ol_lst[[1]]) - cte$rm_overlap, border="black", lwd=2, add=T)
plot(ext(cte$xy_ol_lst[[2]]), col="red",alpha=.7, add=T)
plot(ext(cte$xy_lst[[2]]), col="blue", alpha=.5, add=T)
plot(ext(cte$xy_ol_lst[[2]]) - cte$rm_overlap, border="black", lwd=2, add=T)
plot(ext(cte$xy_ol_lst[[3]]), col="red",alpha=.7, add=T)
plot(ext(cte$xy_lst[[3]]), col="blue", alpha=.5, add=T)
plot(ext(cte$xy_ol_lst[[3]]) - cte$rm_overlap, border="black", lwd=2, add=T)

# of first

xm = as.numeric(ext(cte$xy_ol_lst[[1]])[2] - ext(cte$xy_ol_lst[[1]])[1])
ym = as.numeric(ext(cte$xy_ol_lst[[1]])[4] - ext(cte$xy_ol_lst[[1]])[3])

xmo = as.numeric(ext(cte$xy_lst[[1]])[2] - ext(cte$xy_lst[[1]])[1])
ymo = as.numeric(ext(cte$xy_lst[[1]])[4] - ext(cte$xy_lst[[1]])[3])

overl = (xm - xmo)/2
rm_overl = overl/crop_overlap_factor

message(sprintf("Tile size without overlap [m]: %sx%s", xmo,ymo))
message(sprintf("Tile size with overlap [m]: %sx%s", xm,ym))
message(sprintf("Per side overlap [m]: %s | of which will be deleted [m]: %s", overl, rm_overl))

################# prepare ASC crop big by extent #####################
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
  
  #check whether raster contains all values NA
  if (unname(allNA(tmp_cropped)[1])[[1]] == T) {
    not_worth_checking_i = append(not_worth_checking_i, i)
  } else {
    worth_checking_i = append(worth_checking_i, i)
    tmpout = file.path(dirname(piotr_exe),temp_folder, sprintf("tmp_%s.asc", i))
    writeRaster(tmp_cropped, tmpout, NAflag = na, overwrite = T) 
    
    if (isTRUE(rockmask)) {
      # rockmask tmp
      tmpout_rm = file.path(dirname(piotr_exe),temp_folder, sprintf("tmp_rm_%s.asc", i))
      writeRaster(tmp_cropped_rockmask, tmpout_rm, NAflag = na, overwrite = T)
    }
    
  }
  cat("\r","finished percent: ", round(i/length(cte$xy_ol_lst)*100, 2))
}
cat("")

# problem raster work with parallelization
#https://stackoverflow.com/questions/70191164/null-value-passed-as-symbol-address-error-in-foreach-loop-r


################# piotr multi call per core #####################

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
  
  #calling the function
  prog = multi_piotr(worth = worth_checking_i,
                     extent = cte$xy_ol_lst, 
                     piotr_path = piotr_exe, 
                     l = l, 
                     out_f_name = output_folder, 
                     tmp_f_name = temp_folder, 
                     rm = rockmask, 
                     st = st, 
                     resol = resol, 
                     iter = i) 
  
  prog #Equivalent to cbind(finalMatrix, tempMatrix)
}

stopCluster(cl) #stop cluster
close(pb) #close progress


# delete empty tmp folders
folders <- list.dirs(path = file.path(dirname(piotr_exe),output_folder) ,recursive = FALSE)
for (folder in folders) {
  if (length(dir(folder)) == 0) {
    unlink(folder, recursive = TRUE)
  }
}


progress_report = bind_rows(finalMatrix) #combine

write.table(x = progress_report, file = file.path(dirname(piotr_exe), "progress_report.txt"), quote = F, row.names = F, sep = "\t") #write progress report to csv file

##checks
print(sprintf("Average time per worked step %s min. %s sec.",unlist(strsplit(x = as.character(round(mean(progress_report$end_step[progress_report$worked == T]),digits = 2)), split = ".",fixed = T))[1], as.numeric(unlist(strsplit(x = as.character(round(mean(progress_report$end_step[progress_report$worked == T]),digits = 2)), split = ".",fixed = T))[2])/100*60)) #mean time in minutes per worked step

print(sprintf("Total time needed %s min. %s sec.",unlist(strsplit(x = as.character(round(max(progress_report$end_total)*60,digits = 2)), split = ".",fixed = T))[1],round(as.numeric(unlist(strsplit(x = as.character(round(max(progress_report$end_total)*60,digits = 2)), split = ".",fixed = T))[2])/100*60, digits = 0)))


################# mosaicking #####################

# mosaic to final res
rlist_RH = list()
rlist_SR = list()

# read out tiles into list and also remove overlap for better matching
for (i in seq(1,length(worth_checking_i))) {
  rlist_RH[[i]] = crop(rast(file.path(dirname(piotr_exe),output_folder, sprintf("RH_%s.png", worth_checking_i[i]))), ext(unname(cte$xy_ol_lst[[worth_checking_i[i]]])) - cte$rm_overlap) 
  rlist_SR[[i]] = crop(rast(file.path(dirname(piotr_exe),output_folder, sprintf("SR_%s.png", worth_checking_i[i]))), ext(unname(cte$xy_ol_lst[[worth_checking_i[i]]])) - cte$rm_overlap)
}


# many SpatRasters to SpatRasterCollection 
rsrc_RH = sprc(rlist_RH)
rsrc_SR = sprc(rlist_SR)

# testing output to all different mosaic functions

for (i in seq(1,length(mode_m))) {
  
  if (mode_m[i] %in% c("sum", "mean", "median", "min", "max")) {
    # create mosaics
    m_RH = mosaic(rsrc_RH, fun = mode_m[i])
    
    if (isTRUE(stretching)) {
      m_RH = stretch(m_RH, maxcell = length(cells(m_RH))/2) #attempt stretching ?stretch
    }
    m_SR = mosaic(rsrc_SR, fun = mode_m[i])
    
    if (isTRUE(stretching)) {
      m_SR = stretch(m_SR, maxcell = length(cells(m_SR))/2) #attempt stretching ?stretch
    }
  }
  
  if (mode_m[i] %in% c("first", "last")) {
    
    if (mode_m[i] == "first") {
      # write merge
      m_RH = merge(rsrc_RH, first = T)
      if (isTRUE(stretching)) {
        m_RH = stretch(m_RH, maxcell = length(cells(m_RH))/2) #attempt stretching ?stretch
      }
      
      m_SR = merge(rsrc_SR, first = T)
      if (isTRUE(stretching)) {
        m_SR = stretch(m_SR, maxcell = length(cells(m_SR))/2) #attempt stretching ?stretch
      }
    }
    
    if (mode_m[i] == "last") {
      # write merge
      m_RH = merge(rsrc_RH, first = F)
      if (isTRUE(stretching)) {
        m_RH = stretch(m_RH, maxcell = length(cells(m_RH))/2) #attempt stretching ?stretch
      }
      m_SR = merge(rsrc_SR, first = F)
      if (isTRUE(stretching)) {
        m_SR = stretch(m_SR, maxcell = length(cells(m_SR))/2) #attempt stretching ?stretch
      }
    }
  }
  
  # mask m_RH with rockmask
  if (isTRUE(rockmask) & isTRUE(clip_RH_rockmask)) {
    m_RH = mask(m_RH, rockmask_v)
  }
  
  # write mosaics
  writeRaster(m_RH, file.path(dirname(piotr_exe),output_folder,sprintf("ALL_RH_%s.tif", mode_m[i])), overwrite = T)
  writeRaster(m_SR, file.path(dirname(piotr_exe),output_folder,sprintf("ALL_SR_%s.tif", mode_m[i])), overwrite = T)  
}

message("Done!")

