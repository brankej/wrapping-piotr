## ---------------------------
## Script name: piotr_functions.R
## Description: Functions for wrapper for Piotr Software to utilize multi-core processing. Piotr software was written by Roman Geisthövel (c) 2019 accompanying PhD thesis Automatic Swiss style rock depiction. See (http://motlimot.net/software.html)
## Author: BrankeJ
## Date Created/last updated: 2025-02-07
## License: License: GNU GPL V.3 or later
## Copyright (c) BrankeJ, 2025
## ---------------------------
##  ## Notes:
##   
##
## ---------------------------

################ FUNCTIONS #####################
create_tiles_ext <- function(x,
                             n_row = 5000, 
                             n_col = 5000, 
                             overlap = 500) {
  "function to prepare extents for tiling with overlap"
  "https://stackoverflow.com/a/77562355"
  require(terra)
  
  # Row and col index where the raster will be split
  rows <- seq(1, nrow(x), by = n_row)
  cols <- seq(1, ncol(x), by = n_col)
  
  # Index of cells that will be in the upper-left corner of the tiles
  cells <- cellFromRowColCombine(x, rows, cols)
  # Coordinates of the upper-left corner of the start cells
  xy_ul <- xyFromCell(x, cells)
  
  # Resolution
  rs <- res(x)
  
  # Create matrix of extents (one ext per row)
  xy <- cbind(
    xy_ul[,1],                   # x left
    xy_ul[,1] + n_col * rs[1],   # x right
    xy_ul[,2] - n_row * rs[2],   # y lower
    xy_ul[,2]                    # y upper
  )
  # Matrix of extents with overlaps
  xy_ol <- cbind(
    xy[,1] - overlap * rs[1],
    xy[,2] + overlap * rs[1],
    xy[,3] - overlap * rs[2],
    xy[,4] + overlap * rs[2] 
  )
  
  # Convert matrix to list of extents
  xy_lst    <- as.list(data.frame(t(xy)))
  xy_ol_lst <- as.list(data.frame(t(xy_ol)))
  
  # Expand raster to include external tiles overlaps
  ext(x) <- c(min(xy_ol[,1]), max(xy_ol[,2]), min(xy_ol[,3]), max(xy_ol[,4]))
  
  return(list(
    xy_lst    = xy_lst,
    xy_ol_lst = xy_ol_lst,
    new_ext   = ext(x)
  ))
  
}

write_png_wld <- function(ext_obj, 
                         resolution_xy) {
  "function to write png world files (.wld)"
  "5.0000000000 = x-Komponente der Pixelbreite
  0.0000000000 = y-Komponente der Pixelbreite
  0.0000000000 = x-Komponente der Pixelhöhe
  -5.0000000000 = y-Komponente der Pixelhöhe (meist negativ)
  143140.0000000000 = x-Koordinate des Zentrums des obersten linken Bildpunkts
  289725.0000000000 = y-Koordinate des Zentrums des obersten linken Bildpunkts"
  out = sprintf("%0.10f\n%0.10f\n%0.10f\n%0.10f\n%0.10f\n%0.10f", resolution_xy[1],0,0, -resolution_xy[2], ext_obj[1] + resolution_xy[1], ext_obj[4])
  return(out)
}

multi_piotr <- function(worth,
                       extent, 
                       piotr_path, 
                       l, 
                       out_f_name, 
                       tmp_f_name,
                       rm, #rockmask flag
                       iter) {
  "piotr wrapper function to be called in parallel"
  
  tile = sprintf("tile_%s",iter)
  worked = T
  sti = Sys.time() #get init time per step
  if (i %in% worth) {
    # do normal
    tmpout = file.path(dirname(piotr_path),tmp_f_name, sprintf("tmp_%s.asc", iter))
    tmp_cropped = rast(tmpout)
    
    if (isTRUE(rm)) {
      tmpout_rm = file.path(dirname(piotr_path),tmp_f_name, sprintf("tmp_rm_%s.asc", iter))
      tmp_cropped_rockmask = rast(tmpout_rm)
    }
    
    #create tmp out dir
    dir.create(file.path(dirname(piotr_exe), out_f_name, sprintf("tmp_%s", iter)))
    
    if (isTRUE(rm)) {
      # call piotr
      system(sprintf("%s -l %s -d %s -m %s %s", piotr_exe, l,file.path(dirname(piotr_path),out_f_name, sprintf("tmp_%s", iter)),tmpout_rm, tmpout),show.output.on.console = F, wait = T)
      
    } else {
      # call piotr
      system(sprintf("%s -l %s -d %s %s", piotr_exe, l,file.path(dirname(piotr_path),out_f_name, sprintf("tmp_%s", iter)), tmpout),show.output.on.console = F, wait = T) #if show.output.on.console = T -> piotr prints
    }
      
    #rename outfiles to get rid of random names
    fl = list.files(file.path(dirname(piotr_path),out_f_name, sprintf("tmp_%s", iter)))
    for (ii in fl) {
      if (endsWith(ii, suffix = "_rock_hachures.png")) {
        file.rename(file.path(dirname(piotr_path),out_f_name, sprintf("tmp_%s", iter),ii),file.path(dirname(piotr_path),out_f_name,sprintf("RH_%s.png", iter)))
      } else if (endsWith(ii, suffix = "_session.txt")) {
        file.rename(file.path(dirname(piotr_path),out_f_name,sprintf("tmp_%s", iter),ii),file.path(dirname(piotr_path),out_f_name,sprintf("Sess_%s.txt", iter)))
      } else if (endsWith(ii, suffix = "_shaded_relief.png")) {
        file.rename(file.path(dirname(piotr_path),out_f_name, sprintf("tmp_%s", iter),ii),file.path(dirname(piotr_path),out_f_name,sprintf("SR_%s.png", iter)))
      }
    }
    
    #write worldfiles for RH and SR
    writeLines(write_png_wld(extent[[iter]], resol),file.path(dirname(piotr_path),out_f_name,sprintf("RH_%s.wld", iter)))
    writeLines(write_png_wld(extent[[iter]], resol),file.path(dirname(piotr_path),out_f_name,sprintf("SR_%s.wld", iter)))
    
    #print progress and time
    end_step = as.numeric(Sys.time() - sti, units = "mins")
    end_total = as.numeric(Sys.time() - st, units = "hours")
  } else {
    # if to be skipped
    worked = F
    end_step = as.numeric(Sys.time() - sti, units = "mins")
    end_total = as.numeric(Sys.time() - st, units = "hours")
  }
  return(data.frame(tile = tile, 
                    worked = worked, 
                    begin_step = sti, 
                    end_step = end_step, 
                    end_total = end_total))
}