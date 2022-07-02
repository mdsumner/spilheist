

spil <- readr::read_csv("inst/extdata/grid214.csv")

library(terra)
spil_grid <- rast(spil[c("Spil_X", "Spil_Y", "Lon", "Lat")])


## fill missing
spil_gridgrid <- focal(spil_grid, w=9, fun=mean, na.policy="only", na.rm=T)
writeRaster(spil_grid, "spil.tif", overwrite = TRUE)


## increase res
rr <- rast(spil_grid)
fac <- 4
res(rr) <- res(rr)/fac

info <- vapour::vapour_raster_info("spil.tif")
spil_gridgrid <- c(setValues(rr[[1]], vapour::vapour_warp_raster_dbl("spil.tif", extent = info$extent, dim = info$dimension * fac, bands = 1, resample = "bilinear")),
     setValues(rr[[1]], vapour::vapour_warp_raster_dbl("spil.tif", extent = info$extent, dim = info$dimension * fac, bands = 2, resample = "bilinear")))



library(raadtools)
f <- tail(sstfiles()$fullname, 1)



library(RANN)
library(terra)
r <- rotate(rast(f, subds = "sst"))


qu <- RANN::nn2(xyFromCell(r, seq_len(ncell(r))), values(spgrid), k = 1)

par(bg = "black")
image(focal(setValues(spgrid[[1]], values(r[[1]])[c(qu$nn.idx)]), w = 3, fun = median, na.policy = "only", na.rm = TRUE),
     col = hcl.colors(256))

xy <- do.call(cbind, maps::map(plot = F)[1:2])
xy <- xy[!is.na(xy[,1]), ]
qup <- RANN::nn2(values(spgrid), xy, k = 1)
points(xyFromCell(spgrid, qup$nn.idx[,1]), col = "firebrick", pch = ".")
