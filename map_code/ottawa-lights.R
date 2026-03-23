source('./map_function_lights.R')

# For loop ----
for (x in c('centretown', 'west', 'east', 'riverpath')) {
  
  filepath_start = paste0('./outputs/', format(Sys.Date(), "%Y"), '_')
  filepath_end = '-map.png'
  
  if (x == 'centretown') { # Golden Triangle
    query_params = c(-75.66851, 45.39212, -75.73073, 45.43508)
  } else if (x == 'west') { # Hintonburg & Westboro
    query_params = c(-75.76953, 45.41400, -75.70799, 45.37120)
  } else if (x == 'east') { # Vanier & Rockcliffe
    query_params = c(-75.68816, 45.46103, -75.62259, 45.41562)
  } else if (x == 'riverpath') { # Ottawa River Pathway
    query_params = c(-75.72552, 45.42114, -75.71101, 45.41110)
  } else {
    NULL
  }
  
  make_map(
    query_params,
    paste0(filepath_start, 'unlit_', x, filepath_end),
    paste0(filepath_start, 'lit_', x, filepath_end)
  )
  
}

# # Separate ----
# filepath_start_unlit = './outputs/2026_unlit_'
# filepath_start_lit = './outputs/2026_lit_'
# filepath_end = '-map.png'
# 
# ## Golden Triangle ----
# x = 'centretown'
# make_map(
#   c(-75.66851, 45.39212, -75.73073, 45.43508),
#   paste0(filepath_start_unlit, x, filepath_end),
#   paste0(filepath_start_lit, x, filepath_end)
# )
# 
# ## Hintonburg & Westboro ----
# x = 'west'
# make_map(
#   c(-75.76953, 45.41400, -75.70799, 45.37120),
#   paste0(filepath_start_unlit, x, filepath_end),
#   paste0(filepath_start_lit, x, filepath_end)
# )
# 
# ## Vanier & Rockcliffe ----
# x = 'east'
# make_map(
#   c(-75.68816, 45.46103, -75.62259, 45.41562),
#   paste0(filepath_start_unlit, x, filepath_end),
#   paste0(filepath_start_lit, x, filepath_end)
# )
# 
# ## Ottawa River Pathway ----
# x = 'riverpath'
# make_map(
#   c(-75.72552, 45.42114, -75.71101, 45.41110),
#   paste0(filepath_start_unlit, x, filepath_end),
#   paste0(filepath_start_lit, x, filepath_end)
# )
