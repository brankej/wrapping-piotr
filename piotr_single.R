## ---------------------------
## Script name: piotr_single.R
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


################ FUNCTIONS #####################
source("piotr_functions.R", chdir = T)

################ INIT #####################

# initial paths
DGM_path = r"(G:\TIROL_DGM\DGM_Tirol_5m_epsg31254.tif)" # path to DGM
piotr_exe = r"(G:\piotr_tests\PiotrWin64\Piotr\piotr.exe)" #path to piotr.exe
AOI_path = r"(G:\piotr_tests\PiotrWin64\Piotr\data\AOI_Venediger_31254.shp)" #path to AOI shp
rockmask_path = r"(G:\piotr_tests\PiotrWin64\Piotr\data\FELS_Polygone_31254.shp)" #path to rockmask shp

# params
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

#read rock mask
rockmask = vect(rockmask_path)

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
cte = create_tiles_ext(DGM, n_row = n_size,n_col = n_size,overlap = overlap)

# extent initial DGM for overlap tiles
DGM_ext = extend(DGM, cte$new_ext, fill = na)

#rasterize rockmask
rockmask.r = rasterize(rockmask, DGM_ext)
rockmask.r <- classify(rockmask.r, cbind(NAflag(rockmask.r), 0))
NAflag(rockmask.r) = na

################ MAIN #####################
#create list of list to hold extent, name, total time / time
# possibility to multiprocess

st = Sys.time() #get starttime
#progress_report = list(tiles=as.list(sprintf("tile_%s",seq(1, length(cte$xy_ol_lst)))), worked=list(), begin_total=rep(list(st), length(cte$xy_ol_lst)),begin_step=list(),end_step=list(),end_total=list(), extent=cte$xy_ol_lst)
#progress_report = data.frame(tile=c(), worked=c(),begin_total=c(),begin_step=c(),end_step=c(),end_total=c(),ext_xmin = c(),ext_xmax=c(),ext_ymin=c(),ext_ymax=c())

tiles = c()
workedv = c()
begin_step = c()
end_step = c()
end_total = c()

i = 1
for (i in seq(1, length(cte$xy_ol_lst))) {
  tiles = append(tiles, sprintf("tile_%s",i))
  worked = T
  workedv = append(workedv, worked)
  #progress_report$worked[[i]] = worked
  sti = Sys.time() #get init time per step
  begin_step = append(begin_step, sti)
  #progress_report$begin_step[[i]] = sti
  
  #crop big DGM by extent
  tmp_cropped = crop(DGM_ext, ext(unname(cte$xy_ol_lst[[i]])))
  NAflag(tmp_cropped) = na #account for NAs
  
  #crop rockmask by extent
  tmp_cropped_rockmask = crop(rockmask.r, ext(unname(cte$xy_ol_lst[[i]])))
  NAflag(tmp_cropped_rockmask) = na #account for NAs
  
  #check whether all values NA
  if (unname(allNA(tmp_cropped)[1])[[1]] == T) {
    worked = F
    workedv[i] = F
    #progress_report$worked[[i]] = worked
    end_step = append(end_step, as.numeric(Sys.time() - sti, units = "mins"))
    #progress_report$end_step[[i]] = as.numeric(Sys.time()-sti, units="mins")
    end_total = append(end_total, as.numeric(Sys.time() - st, units = "hours"))
    #progress_report$end_total[[i]] = as.numeric(Sys.time()-st, units="hours")
    cat(sprintf("skipping tile %s. took %s min of total %s hours\n", i, as.numeric(Sys.time() - sti, units = "mins"),as.numeric(Sys.time() - st, units = "hours")))
  } else { 
    # do normal
    tmpout = file.path(dirname(piotr_exe),"tmp", sprintf("tmp_%s.asc", i))
    writeRaster(tmp_cropped, tmpout, NAflag = na, overwrite = T)
    
    #
    tmpout_rm = file.path(dirname(piotr_exe),"tmp", sprintf("tmp_rm_%s.asc", i))
    writeRaster(tmp_cropped_rockmask, tmpout_rm, NAflag = na, overwrite = T)
    
    
    # call piotr
    system(sprintf("%s -l %s -d %s -m %s %s", piotr_exe, l,file.path(dirname(piotr_exe),"out"),tmpout_rm, tmpout),show.output.on.console = T, wait = T)
    
    #rename outfiles to get rid of random names
    fl = list.files(file.path(dirname(piotr_exe),"out"))
    for (ii in fl) {
      if (endsWith(ii, suffix = "_rock_hachures.png")) {
        file.rename(file.path(dirname(piotr_exe),"out",ii),file.path(dirname(piotr_exe),"out",sprintf("RH_%s.png", i)))
     } else if (endsWith(ii, suffix = "_session.txt")) {
        file.rename(file.path(dirname(piotr_exe),"out",ii),file.path(dirname(piotr_exe),"out",sprintf("Sess_%s.txt", i)))
     } else if (endsWith(ii, suffix = "_shaded_relief.png")) {
        file.rename(file.path(dirname(piotr_exe),"out",ii),file.path(dirname(piotr_exe),"out",sprintf("SR_%s.png", i)))
      }
    }
    
    #write worldfiles for RH and SR
    writeLines(write_png_wld(cte$xy_ol_lst[[i]], resol),file.path(dirname(piotr_exe),"out",sprintf("RH_%s.wld", i)))
    writeLines(write_png_wld(cte$xy_ol_lst[[i]], resol),file.path(dirname(piotr_exe),"out",sprintf("SR_%s.wld", i)))
    
    #print progress and time
    end_step = append(end_step, as.numeric(Sys.time() - sti, units = "mins"))
    #progress_report$end_step[[i]] = as.numeric(Sys.time()-sti, units="mins")
    end_total = append(end_total, as.numeric(Sys.time() - st, units = "hours"))
    #progress_report$end_total[[i]] = as.numeric(Sys.time()-st, units="hours")
    cat(sprintf("finished tile %s. took %s min of total %s hours\n", i, as.numeric(Sys.time() - sti, units = "mins"),as.numeric(Sys.time() - st, units = "hours")))
  }
}


progress_report = data.frame(tile = tiles, worked = workedv, begin_step = begin_step, end_step = end_step, end_total = end_total)
#progress_report = data.frame(tile=tiles[-1], worked=workedv[-1], begin_step=begin_step[-1], end_step=end_step, end_total=end_total)
#fill for every
progress_report$begin_total = st


## checks
summary(progress_report$worked)
mean(progress_report$end_step[progress_report$worked == T]) #mean time in minutes per worked step

