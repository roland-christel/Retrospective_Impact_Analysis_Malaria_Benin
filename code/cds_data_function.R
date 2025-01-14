# =========================================================
# European Centre for Medium-Range Weather Forecasts (ECMWF) 
# Copernicus's Climate Data Store (CDS)
# Updated for the new API
# https://cds.climate.copernicus.eu
# DOI: 10.24381/cds.e2161bac

# spatial resolution: 0.1x0.1 degrees
# temporal resolution: monthly
# temporal availability: from January 1981 to 2-3 months before the present
# =========================================================


get_cds <- local({
  library("tidyverse")
  library("ecmwfr")
  library("terra")
  
  # climate variables 
  #cvars <- c("10m_u_component_of_wind", 
  #"10m_v_component_of_wind", 
  #"leaf_area_index_high_vegetation", 
  # "leaf_area_index_low_vegetation", 
  #  "skin_temperature", 
  #    "surface_pressure",
  #       "total_precipitation", 
  #        "volumetric_soil_water_layer_1",
  #         "potential_evaporation",
  #         "surface_net_solar_radiation")
  
  cvars <- c("2m_temperature",
             "total_precipitation")
  
  get_cds_area <- function(key, user,
                           year, 
                           month=sprintf("%02d", 1:12),
                           day=sprintf("%02d", 1:31), 
                           time="12:00",
                           area, 
                           temp_dir=NULL)
  {
    # set secret ECMWF token
    wf_set_key(user=user, key=key)
    
    # create a temporary directory to extract the downloaded file
    if (is.null(temp_dir))
      temp_dir <- tempdir()
    if (!dir.exists(temp_dir))
    {
      dir.create(temp_dir)
    }
    dfile <- paste0("cds_hourly_", year, ".grib")
    
    # set the working directory to the temporary directory
    setwd(temp_dir)
    
    # request for getting land data
    request <- list(
      # dataset name
      dataset_short_name = "reanalysis-era5-land",
      # climate variables 
      variable = cvars,
      # temporal framework: year, month, day, hour
      year = as.character(year),
      month = as.character(month),
      day = as.character(day),
      time = as.character(time),
      # geographical region
      #      North, West, South, East
      area = area,
      # output file format
      format = "grib", 
      # output file name
      target = dfile
    )
    
    # check the validity of a data request and login credentials
    wf_check_request(request=request)
    
    # download the data request
    wf_request(user=user, 
               request=request,
               transfer=TRUE, 
               path=getwd(),
               # waiting time for download to start
               time_out=3 * 60 * 60,
               verbose=TRUE)
    
    # create a SpatRaster object from the downloaded data file
    rdata <- terra::rast(dfile)
    return(rdata)
  }
  
  extrfromrast <- function(raster, coords)
  {
    dts <- terra::time(raster)
    dat <- terra::extract(raster, coords)
    colnames(dat) <- paste(cvars, dts, sep="|")
    
    dat %>%
      mutate(longitude=coords[, 1], 
             latitude=coords[, 2]) %>%
      pivot_longer(
        -c(longitude, latitude),
        cols_vary = "slowest",
        names_to = c("variable", "date"),
        names_sep = "\\|"
      ) %>%
      pivot_wider(names_from="variable",
                  values_from="value",
                  values_fn = list) %>%
      unnest(-c(longitude, latitude, date)) %>%
      mutate(date=as.POSIXct(date, tz=attr(dts, "tzone")))
  }
  
  get_cds_points <- function(key, user,
                             year,
                             month=sprintf("%02d", 1:12),
                             day=sprintf("%02d", 1:31),
                             time="12:00",
                             coords,
                             temp_dir=NULL)
  {
    # coordinates of desired locations should be
    #   longitude: coords[, 1]
    #   latitude: coords[, 2]
    
    #         North, West, South, East
    area <- c(max(coords[, 2]) + 0.2, min(coords[, 1]) - 0.2, 
              min(coords[, 2]) - 0.2, max(coords[, 1]) + 0.2)
    
    cds_dat <- get_cds_area(key=key, user=user, 
                            year=year, month=month, 
                            day=day, time=time,
                            area=area, temp_dir=temp_dir)
    
    extrfromrast(cds_dat, coords)
  }
  
  get_cds_map <- function(key, user="ecmwfr",
                          year,
                          month=sprintf("%02d", 1:12),
                          day=sprintf("%02d", 1:31),
                          time="12:00",
                          map, 
                          raster=FALSE,
                          temp_dir=NULL)
  {
    library("sf")
    area <- st_bbox(map)
    #         North, West, South, East
    area <- unname(area[c(4, 1:3)])
    
    cds_dat <- get_cds_area(key=key, user=user, 
                            year=year, month=month, 
                            day=day, time=time,
                            area=area, temp_dir=temp_dir)
    if (raster)
    {
      terra::crop(cds_dat, map)
    } else{
      coords <- xyFromCell(cds_dat, 1:ncell(cds_dat))
      cds_dat <- extrfromrast(cds_dat, xyFromCell(cds_dat, 1:ncell(cds_dat)))
      
      cds_dat <- st_as_sf(cds_dat, 
                          coords=c("longitude", "latitude"),
                          crs=st_crs(map))
      
      # conduct a spatial join to determine points inside specific map regions
      st_join(cds_dat, 
              map, 
              join=st_within) %>%
        select(1:ncol(cds_dat)) %>%
        na.omit()
    }
  }
  
  get_cds_data <- function(key, user="ecmwfr",
                           year, 
                           month=sprintf("%02d", 1:12),
                           day=sprintf("%02d", 1:31), 
                           time="12:00",
                           what,
                           raster=FALSE,
                           temp_dir=NULL)
  {
    switch(class(what)[1], numeric={
      if (length(what) == 4)
        get_cds_area(key=key, user=user, 
                     year=year, month=month, 
                     day=day, time=time,
                     area=what, temp_dir=temp_dir)
      else
        stop("numeric vector of length 4 is required")
    }, matrix={
      if (ncol(what) != 2)
        stop("a matrix of coordinates with two columns (Long, Lat) is required")
      get_cds_points(key=key, user=user, 
                     year=year, month=month, 
                     day=day, time=time,
                     coords=what, temp_dir=temp_dir)
    }, sf={
      get_cds_map(key=key, user=user, 
                  year=year, month=month, 
                  day=day, time=time,
                  map=what, raster=raster, 
                  temp_dir=temp_dir)
    })
  }
  
})


