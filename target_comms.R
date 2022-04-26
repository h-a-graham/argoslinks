# targets commands for inspection and running

library(targets)
library(tmap)
# tar_edit() # opens the _targets.R file for editing

# tar_manifest(fields = "command") # prints targets and commands

tar_glimpse()


tar_visnetwork(label='time', level_separation=300)


tar_make() # run the targets workflow
# tar_make(callr_function = NULL) # for debugging
targets::tar_make_future(workers = 5L) 

# targets::tar_make_future(c('AllWood_interactiveMap',
#                          'Boar_interactiveMap',
#                          'Squirrel_interactiveMap',
#                          'Marten_interactiveMap',
#                          'Wildcat_interactiveMap'),workers = 5L) 



# not using this anymore.
tmap_options(max.categories = 460)
tmap_mode("view")
t <- tar_read(boar)
t

# tmap_save(t, 'dev_maps/argosLinkTEST1.html')



tar_make('warp_to_region')




# tar_meta(fields = error, complete_only = TRUE) # show errors to help debugging.

# tar_read(download_OS_grid)# to view the results the arg is the target name....

# tar_read(split_iterator)[[1]]

# tar_read(warp_gb_bfi)
# tar_read(proc_veg_tiles)

# purrr::map(tar_read(chunk_nfi), ~print(.))


# debug(process_veg)
# tar_make(names = proc_veg_tilesChunk1, callr_function = NULL)

