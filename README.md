# Wrapping Piotr

Wrapping Piotr in order to utilize multi-core processing.

## Input

- path to digital elevation model (required)
- path to piotr.exe (for latest version see below) (required)
- path to aoi shapefile (required)
- path to rockmask (required for script, theoretically optional)
 
## Folder structure

```
PiotrWin64/
├─ Piotr/
  ├─ data/ <- hosts data
  ├─ piotr/ <- piotr default folder
  ├─ out/ <- will be created / stores final output
  ├─ tmp/ <- will be created / stores temporary output
  ├─ piotr.exe <- piotr executable
```


## OG - Piotr
                                                                             
  __/\\\\\\\\\\\\\__________________________________________________         
   _\/\\\/////////\\\________________________________________________        
    _\/\\\_______\/\\\__/\\\__________________/\\\____________________       
     _\/\\\\\\\\\\\\\/__\///______/\\\\\____/\\\\\\\\\\\__/\\/\\\\\\\__      
      _\/\\\/////////_____/\\\___/\\\///\\\_\////\\\////__\/\\\/////\\\_     
       _\/\\\_____________\/\\\__/\\\__\//\\\___\/\\\______\/\\\___\///__    
        _\/\\\_____________\/\\\_\//\\\__/\\\____\/\\\_/\\__\/\\\_________   
         _\/\\\_____________\/\\\__\///\\\\\/_____\//\\\\\___\/\\\_________  
          _\///______________\///_____\/////________\/////____\///__________ 
                                                                            
Usage: 

Enter    piotr.exe -h    for general help

Example: 
piotr.exe -l 15 -d /tmp/piotr_out -m ~/mask.asc ~/test.asc 

-l 15               generalization using LIC; integration length is 15
-d /tmp/piotr_out   put images and session file in this directory
-m ~/mask.asc       path to rock mask
~/test.asc          elevation model file path


For an in-depth description of the method, please refer to

Roman Geisthövel: Automatic Swiss style rock depiction. PhD thesis, ETH Zurich,
Diss. No. 24328, 2017
Dowload link: https://www.research-collection.ethz.ch/bitstream/handle/20.500.11850/201368/automatic_swiss_style_rock_depiction_roman_geisthoevel_2017.pdf


piotr is copyright (c) 2019 Roman Geisthövel

Find the latest version @ http://motlimot.net/software.html


### Requirements

Tested with: 
R version 4.4.2 (2024-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26100)

other attached packages:
[1] doSNOW_1.0.20     snow_0.4-4        doParallel_1.0.17 iterators_1.0.14 
[5] foreach_1.5.2     dplyr_1.1.4       terra_1.8-10    