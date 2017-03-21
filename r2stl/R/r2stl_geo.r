geolibs <- c("spdep","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster", "dplyr", "tidyr","assertthat","data.table",'readr','pryr','combinat','gstat')
lapply(geolibs, require, character.only = TRUE)

# r2stl.r - produce an STL file containing a 3D surface plot
# suitable for printing using a 3D printer or rapid prototyper
# Version 0.1, 2012 by Ian Walker, University of Bath, i.walker@bath.ac.uk
# Released under a Creative Commons BY-NC-SA licence

# The data take the same form as in R's persp() plots: x and y
# represent a grid and z gives heights above this grid

#SMI edit: function for returning consistent values for z
#Proportional to the projection units
consistent_ZValues <- function(shapefile=NULL,variable=NULL){
  
  
  
}



# SMI edit: add a shapefile to be rasterised before stling
# gridResolution is in the projection units e.g. 100 in BNG: each grid square represents 100 metres
# tweak for control over z scale and normalising x/y to keep proportions
# keepXYratio = T: will maintain the ratio, normalising to [0,1] the larger of the two.
# zRatio: numeric. Will make max z height proportional to 1.
# relief layer: e.g. a road shapefile that will be removed/added to the STL surface
# If interpolate >0, do a simple inverse-distance-weighting interpolation using the input data. Use the value as the exponent.
r2stl_geo <- function(shapefile=NULL, variable=NULL, gridResolution = 100, keepXYratio = TRUE, zRatio = 0.5, filename='3d-R-object.stl', object.name='r2stl-object', min.height=0.008, show.persp=FALSE, strict.stl=FALSE, reliefLayer = NULL, interpolate = 0) {

  # NB assuming a 60mm height for printed object, default min.height of 
  # 0.008 gives a minimum printed height of 0.5mm
  
  # *Auto setting* If min.height >= 1, we interpret this not as the minimum 
  # proportion of the object to be printed, but as the height
  # of the printed object in mm, and provide a 0.5 mm minimum
  # (0.5 mm seems a common minimum recommended height for many 3D printers)
  #print(class(variable))
  
  
  if (is.null(shapefile)|class(shapefile)!='SpatialPolygonsDataFrame') stop('Argument <<shapefile>> should be a SpatialPolygonsDataFrame')
  if (!is.numeric(gridResolution)|gridResolution<=0) stop('Argument <<gridResolution>> should be a positive integer')
  if (!is.character(variable)&!is.numeric(variable)) stop('Argument <<variable>> should be character name of column or integer index of column')
  # if (!is.numeric(x)) stop('Argument <<x>> should be a number')
  # if (!is.numeric(y)) stop('Argument <<y>> should be a number')
  # if (!is.numeric(z)) stop('Argument <<z>> should be a number')
  if (!is.character(filename)) stop('Argument <<filename>> should be a string')
  if (!is.character(object.name)) stop('Argument <<object.name>> should be a string')
  #if (!is.logical(z.expand)) stop('Argument <<z.expand>> should be a boolean')
  if (!is.numeric(min.height)) stop('Argument <<min.height>> should be a number')
  if (!is.numeric(zRatio) | zRatio <= 0 | zRatio > 1) stop('Argument <<zRatio>> should be a number and should be more than zero and less than or equal to one.')
  if (!is.logical(show.persp)) stop('Argument <<show.persp>> should be a boolean')
  if (!is.logical(keepXYratio)) stop('Argument <<keepXYratio>> should be a boolean')
  if (!is.logical(strict.stl)) stop('Argument <<strict.stl>> should be a boolean')
  
  #Won't work directly with the SPDF if accessing var name programmatically
  df <- data.frame(shapefile)
  
  if (is.numeric(variable)){
    if (ncol(df) > variable) stop('Argument <<variable>>: too high, no column in that index')
  } else {
    if (!(variable %in% names(df))) stop('Argument <<variable>>: doesn\'t match any column name in this shapefile')
  }

  #SMI additions
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Create raster
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #These are needed in either case
  width <- (xmax(shapefile)-xmin(shapefile))/gridResolution
  height <- (ymax(shapefile)-ymin(shapefile))/gridResolution
  
  #If interpolate > 0, make an interpolation raster based on the shapefile centroids
  if(interpolate){
    
    #This code to get the underlying interpolation grid lifted from:
    #http://gis.stackexchange.com/questions/158021/plotting-map-resulted-from-kriging-in-r
    
    #Assuming same-length polygons and data got passed in.
    
    #Use extent of shapefile, not extent of centroid points.
    min_x = min(coordinates(shapefile)[,1]) #minimun x coordinate
    min_y = min(coordinates(shapefile)[,2]) #minimun y coordinate
    
    x_length = max(coordinates(shapefile)[,1] - min_x) #easting amplitude
    y_length = max(coordinates(shapefile)[,2] - min_y) #northing amplitude
    #cellsize = 50 #pixel size
    ncol = round(x_length/gridResolution,0) #number of columns in grid
    nrow = round(y_length/gridResolution,0) #number of rows in grid
    
    grid = GridTopology(cellcentre.offset=c(min_x,min_y),cellsize=c(gridResolution,gridResolution),cells.dim=c(ncol,nrow))
    
    #Convert GridTopolgy object to SpatialPixelsDataFrame object.
    grid = SpatialPixelsDataFrame(grid,
                                  data=data.frame(id=1:prod(ncol,nrow)),
                                  proj4string=CRS(proj4string(shapefile)))
    
    interp <- idw(df[,variable]~1, gCentroid(shapefile,byid=T), grid, idp = interpolate) 
    
    useRaster <- raster(interp)
    proj4string(useRaster) <- proj4string(shapefile)
    extent(useRaster) <- extent(shapefile)
    
    #Also need a matching empty raster for any overlay
    r <- raster(ncols = ncol(useRaster), nrows = nrow(useRaster))
    proj4string(r) <- proj4string(shapefile)
    extent(r) <- extent(shapefile)
    
  } else {
  
    #Otherwise, create a raster where heights connect to each polygon's data
    
    print(paste0("Grid resolution gives: ",as.integer(width),"x",as.integer(height)))
    
    r <- raster(ncols = width, nrows = height)
    proj4string(r) <- proj4string(shapefile)
    extent(r) <- extent(shapefile)
    
    useRaster <- rasterize(shapefile,r,df[,variable])
  
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~
  #relief layer---
  #~~~~~~~~~~~~~~~~~~~~~~~~~~
    #If a relief layer is included, add/remove it from surface
  #Assume it's a single value from the 'relief' variable
  if(!is.null(reliefLayer)){
    reliefRaster <- rasterize(reliefLayer,r,reliefLayer$relief)
    reliefRaster[is.na(reliefRaster)] <- 0#NAs set that point to zero rather than the underlying raster layer
    
    print(paste0('useRaster dim: ',dim(useRaster),", reliefRaster dim: ",dim(reliefRaster)))
    
    useRaster <- useRaster + reliefRaster
  }
  
  x = 1:nrow(useRaster) 
  y = 1:ncol(useRaster)
  z <- as.matrix(useRaster)
  z[is.na(z)] <- 0
  
  #Back to orig with some edits for proportions

    if (min.height >= 1) {
        min.height <- 0.5 / min.height
    }

	# sanity checks
    if (length(x) < 3 | length(y) < 3 | length(z) < 3) {
        stop("You do not appear to have enough data for a plot to be generated")
    }
    
    ##
    # Define some functions to be used later
    ##

	# function to normalize scores on a scale from 0 to 1
	normit <- function(x) { 
		xprime <- (x - min(x, na.rm=TRUE)) / ( max(x, na.rm=TRUE) - min(x, na.rm=TRUE) )
		return(xprime)
	}

    # function to provide a minimum z height (to avoid too-thin printing)
    correct.min <- function(x) {
        xprime <- x
        xprime[xprime < min.height] <- min.height
        return(xprime)
    }
    
    ##
    # Enough functions, let's get processing
    ##
    
	# open file for writing
	fp <- file(filename, open="w")
    if (!fp) { stop("There was a problem opening the file for writing") }

	# normalize all data onto a scale of 0 to 1
	zz <- (normit(z))*zRatio
	
	if(!keepXYratio){
	  #normalise both
  	xx <- normit(x)
  	yy <- normit(y)
  	
	} else {
	  #which is the larger? Normalise that one, make the other in proportion, and centre it
	  if(width>height){
    	xx <- normit(x)
    	yy <- normit(y) * (height/width)
    	#centre
    	# yy <- yy + ((1-(height/width))/2)
	  } else {
    	yy <- normit(y)
    	xx <- normit(x) * (width/height)
    	#centre
    	# xx <- xx + ((1-(width/height))/2)
	  }
	}
	
	# z range has been normalized to fill the same 0 to 1 range as the x and y data. 
    # This is necessary to get everything onto a size-neutral 0 to 1 range, but
    # often messes up prints. So if required, rescale the z scores back to the original data range
	# if(!z.expand) { 
	# 	zz <- zz * ( (max(z) - min(z)) / max(z) ) 
	# 	if (max(zz) > 1 | min(zz) < 0) zz <- normit(zz) # if -ve numbers have messed things
	# }

    # to avoid trying to print infintesimally thin surfaces, provide a minimum height in 
    # the z data
    if (min.height) { # gives the option to set it to FALSE. Don't know why you would though. SMI: if it's zero, this will return false. Correct.min just replaces any z values below the floor with the min height.
        zz <- correct.min(zz)
    }

	# Option to see a surfaceplot of your data as the 3D version is generated
    if (show.persp) {
    	persp(xx,yy,zz, xlim=c(0,1), ylim=c(0,1), zlim=c(0,1), theta=120, phi=15, col="lightgreen")
    }
    
	# Output file header 	
	write(sprintf('solid %s created using r2stl.r by Ian Walker, University of Bath', object.name), file=fp)
	
	###
	# Begin the six faces
	###

	# The approach is to picture the object as sitting inside a cube and go around
	# the six faces producing triangles from the x, y, z grid.
	#
	# The run along each face divides the rectangles in the data into triangles.
	# The two triangles in each rectangle are arbitrarily called A and B. On the side
	# faces, A is the lower triangle on the z axis and B the upper; on the top and 
	# bottom faces, A is the triangle lower on the y axis. 
		
	# First side face, y is fixed at 0 and x increments
	for (i in 1:(length(xx)-1)) { 
		# to length-1 as we triangulate from a point to its next neighbour and so
		# the penultimate step will take us to the far edge

		j = 0 # fix the non-moving axis

		# triangle A
		write('  facet normal 0.0 -1.0 0.0', file=fp)
		write('    outer loop', file=fp)
		write(sprintf('      vertex %f %f %f', xx[i], j, 0), file=fp)
		write(sprintf('      vertex %f %f %f', xx[i+1], j, 0), file=fp)
		write(sprintf('      vertex %f %f %f', xx[i+1], j, zz[i+1, j+1]), file=fp)
		write('    endloop', file=fp)
		write('  endfacet', file=fp)
		
		#triangle B
		write('  facet normal 0.0 -1.0 0.0', file=fp)
		write('    outer loop', file=fp)
		write(sprintf('      vertex %f %f %f', xx[i], j, 0), file=fp)
		write(sprintf('      vertex %f %f %f', xx[i+1], j, zz[i+1, j+1]), file=fp)
		write(sprintf('      vertex %f %f %f', xx[i], j, zz[i+1,j+1]), file=fp)
		write('    endloop', file=fp)
		write('  endfacet', file=fp)
	}
	
	# Second side face, x is fixed at 0 and y increments
	for (i in 1:(length(yy)-1) ) { 
		j = 0			

		# triangle A
		write('  facet normal -1.0 0.0 0.0', file=fp)
		write('    outer loop', file=fp)
		write(sprintf('      vertex %f %f %f', j, yy[i], 0), file=fp)
		write(sprintf('      vertex %f %f %f', j, yy[i+1], zz[j+1, i+1]), file=fp)
		write(sprintf('      vertex %f %f %f', j, yy[i+1], 0), file=fp)
		write('    endloop', file=fp)
		write('  endfacet', file=fp)
		
		#triangle B
		write('  facet normal -1.0 0.0 0.0', file=fp)
		write('    outer loop', file=fp)
		write(sprintf('      vertex %f %f %f', j, yy[i], 0), file=fp)
		write(sprintf('      vertex %f %f %f', j, yy[i], zz[j+1,i]), file=fp)
		write(sprintf('      vertex %f %f %f', j, yy[i+1], zz[j+1, i+1]), file=fp)
		write('    endloop', file=fp)
		write('  endfacet', file=fp)
	}

	# Third side face, y is fixed at its max value and x increments
	for (i in 1:(length(xx)-1) ) { 
		#j = 1 #normalized highest value
	  #Proportional
	  j = ifelse(height/width<1,height/width,1)
	  j = ifelse(keepXYratio,j,1)
	  
		k = length(yy) # actual highest value (for addressing the array)

		# triangle A
		write('  facet normal 0.0 1.0 0.0', file=fp)
		write('    outer loop', file=fp)
		write(sprintf('      vertex %f %f %f', xx[i], j, 0), file=fp)
		write(sprintf('      vertex %f %f %f', xx[i+1], j, zz[i+1, k]), file=fp)
		write(sprintf('      vertex %f %f %f', xx[i+1], j, 0), file=fp)
		write('    endloop', file=fp)
		write('  endfacet', file=fp)
		
		#triangle B
		write('  facet normal 0.0 1.0 0.0', file=fp)
		write('    outer loop', file=fp)
		write(sprintf('      vertex %f %f %f', xx[i], j, 0), file=fp)
		write(sprintf('      vertex %f %f %f', xx[i], j, zz[i, k]), file=fp)
		write(sprintf('      vertex %f %f %f', xx[i+1], j, zz[i+1, k]), file=fp)
		write('    endloop', file=fp)
		write('  endfacet', file=fp)
	}
	
	# Fourth side face, x is fixed at its max value and y increments
	for (i in 1:(length(yy)-1) ) { 
		#j = 1		
	  #Proportional
	  j = ifelse(width/height<1,width/height,1)
	  j = ifelse(keepXYratio,j,1)
	  #print(paste0('j:',j))
		k = length(xx)

		# triangle A
		write('  facet normal 1.0 0.0 0.0', file=fp)
		write('    outer loop', file=fp)
		write(sprintf('      vertex %f %f %f', j, yy[i], 0), file=fp)
		write(sprintf('      vertex %f %f %f', j, yy[i+1], 0), file=fp)
		write(sprintf('      vertex %f %f %f', j, yy[i+1], zz[k, i+1]), file=fp)
		write('    endloop', file=fp)
		write('  endfacet', file=fp)
		
		#triangle B
		write('  facet normal 1.0 0.0 0.0', file=fp)
		write('    outer loop', file=fp)
		write(sprintf('      vertex %f %f %f', j, yy[i], 0), file=fp)
		write(sprintf('      vertex %f %f %f', j, yy[i+1], zz[k, i+1]), file=fp)
		write(sprintf('      vertex %f %f %f', j, yy[i], zz[k,i]), file=fp)
		write('    endloop', file=fp)
		write('  endfacet', file=fp)
	}

	# top face - run through the x by y grid as seen from above
	for (i in 1:(length(xx)-1) ) {
		for (j in 1:(length(yy)-1) ) {
	
			# triangle A
			write('  facet normal 0.0 0.0 1.0', file=fp)
			write('    outer loop', file=fp)
			write(sprintf('      vertex %f %f %f', xx[i], yy[j], zz[i,j]), file=fp)
			write(sprintf('      vertex %f %f %f', xx[i+1], yy[j], zz[i+1,j]), file=fp)
			write(sprintf('      vertex %f %f %f', xx[i+1], yy[j+1], zz[i+1,j+1]), file=fp)
			write('    endloop', file=fp)
			write('  endfacet', file=fp)
			
			#triangle B
			write('  facet normal 0.0 0.0 1.0', file=fp)
			write('    outer loop', file=fp)
			write(sprintf('      vertex %f %f %f', xx[i], yy[j], zz[i,j]), file=fp)
			write(sprintf('      vertex %f %f %f', xx[i+1], yy[j+1], zz[i+1,j+1]), file=fp)
			write(sprintf('      vertex %f %f %f', xx[i], yy[j+1], zz[i,j+1]), file=fp)
			write('    endloop', file=fp)
			write('  endfacet', file=fp)
		}
	}
	
	xratio <- ifelse(width/height>1,1,width/height)
	yratio <- ifelse(height/width>1,1,height/width)
	
	xratio <- ifelse(keepXYratio,xratio,1)
	yratio <- ifelse(keepXYratio,yratio,1)
	
	# Bottom face. This is always a flat rectangle so we can cheat by making it two 
	# massive triangles. But as this isn't strict STL format we offer a fully-
	# triangulated version of the grid, albeit at the cost of larger files
	if (!strict.stl) {
		write('  facet normal 0.0 0.0 -1.0', file=fp)
		write('    outer loop', file=fp)
		write(sprintf('      vertex %f %f %f', 0, 0, 0), file=fp)
		write(sprintf('      vertex %f %f %f', xratio, yratio, 0), file=fp)
		write(sprintf('      vertex %f %f %f', xratio, 0, 0), file=fp)
		write('    endloop', file=fp)
		write('  endfacet', file=fp)

		write('  facet normal 0.0 0.0 -1.0', file=fp)
		write('    outer loop', file=fp)
		write(sprintf('      vertex %f %f %f', 0, 0, 0), file=fp)
		write(sprintf('      vertex %f %f %f', 0, yratio, 0), file=fp)
		write(sprintf('      vertex %f %f %f', xratio, yratio, 0), file=fp)
		write('    endloop', file=fp)
		write('  endfacet', file=fp)
		
		# write('  facet normal 0.0 0.0 -1.0', file=fp)
		# write('    outer loop', file=fp)
		# write(sprintf('      vertex %f %f %f', 0, 0, 0), file=fp)
		# write(sprintf('      vertex %f %f %f', 1, 1, 0), file=fp)
		# write(sprintf('      vertex %f %f %f', 1, 0, 0), file=fp)
		# write('    endloop', file=fp)
		# write('  endfacet', file=fp)
		# 
		# write('  facet normal 0.0 0.0 -1.0', file=fp)
		# write('    outer loop', file=fp)
		# write(sprintf('      vertex %f %f %f', 0, 0, 0), file=fp)
		# write(sprintf('      vertex %f %f %f', 0, 1, 0), file=fp)
		# write(sprintf('      vertex %f %f %f', 1, 1, 0), file=fp)
		# write('    endloop', file=fp)
		# write('  endfacet', file=fp)
		
	} else { # copy of top-face code, with all z values set to zero
		for (i in 1:(length(xx)-1) ) {
			for (j in 1:(length(yy)-1) ) {

				# triangle A
				write('  facet normal 0.0 0.0 -1.0', file=fp)
				write('    outer loop', file=fp)
				write(sprintf('      vertex %f %f %f', xx[i], yy[j], 0), file=fp)
				write(sprintf('      vertex %f %f %f', xx[i+1], yy[j], 0), file=fp)
				write(sprintf('      vertex %f %f %f', xx[i+1], yy[j+1], 0), file=fp)
				write('    endloop', file=fp)
				write('  endfacet', file=fp)
			
				#triangle B
				write('  facet normal 0.0 0.0 -1.0', file=fp)
				write('    outer loop', file=fp)
				write(sprintf('      vertex %f %f %f', xx[i], yy[j], 0), file=fp)
				write(sprintf('      vertex %f %f %f', xx[i+1], yy[j+1], 0), file=fp)
				write(sprintf('      vertex %f %f %f', xx[i], yy[j+1], 0), file=fp)
				write('    endloop', file=fp)
				write('  endfacet', file=fp)
			}
		}
	} 
	
	###
	# End of the six faces
	###

	# Write the footer and end the file
	write(sprintf('endsolid %s', object.name), file=fp)
	close(fp)
}

