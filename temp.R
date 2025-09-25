library(SELECTRshed)
library(terra)

dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
dem <- terra::rast(dem)
plot(dem)


## probably need to use least cost method....
dem_breached <- create_breach_depression(dem = dem)
plot(dem_breached)


pointer <- create_d8_pointer(dem = dem_breached)
plot(pointer)

## I don't think I'm using the right notation for the args right now....
## wonder if the flags can take a simple true false????
## it does!!! wtf
fa <- create_D8_fa(D8pointer = pointer, pntr = TRUE)
plot(fa)

stream_ras <- create_streams(flow_accumulation = fa,
                             threshold = 1500,
                             zero_background = TRUE)
plot(stream_ras)

stream_vec <- create_streams_vector(streams = stream_ras,
                                    d8_pointer = pointer)
plot(stream_vec)

stream_vec

## getting the stream vector seems problematic right now.
## maybe delineate watershed from main pour point first.
## then create subwatersheds using stream nodes.

# import pour point... need to provide tutorial on how to get pour point from end of poly line or point.
