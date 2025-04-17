load("data/tables.RDATA")

master = reactiveVal(tables$master)

experts = reactiveVal(tables$experts)

species = reactiveVal(tables$species)

norm_dist = reactiveVal(tables$norm_dist)