#############################################
# IBM?SPSS?Statistics - Essentials for R
# (c) Copyright IBM Corp. 1989, 2012
#
#This program is free software; you can redistribute it and/or modify
#it under the terms of the GNU General Public License version 2 as published by
#the Free Software Foundation.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License version 2
#along with this program; if not, write to the Free Software
#Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA.
#############################################

## These constants represent the display formats that are supported
## for SPSS variables. Internal use only.


## format set which could be transformed into POSIXt 
dataFormatSet <- c("D-M-Y","M-D-Y","Y-M-D","Q-Y","W-Y","D-monthName-Y",
						"monthName-Y","Y-dayNo","dayName","monthName")
timeFormatSetTest <- c("H-M","H-M-S","M-S")

ibmspsscfdata.GetData <- function(fields=NULL,
									rowCount=NULL,   
									missingValue = NA, 
									factorMode = "levels", 
									rDate = "None",
									logicalFields=FALSE)

{
	spssError.reset()
	err <- 0     
	fields <- unlist(fields)
	if(is.null(fields))
	{
		fieldCount <- ibmspsscfdatamodel.GetFieldCount()
		if(fieldCount > 0)
		{
			fields <- 0:(fieldCount-1)
		}
		else
		{
			return(NULL)
		}
	}
	else if(is.character(fields))
	{
		## need to look
		fields <- ibmspsscfpkg.ParseFieldNames(fields)
	}
	
	if(length(fields) == 0)
	{
		return(NULL)
	}
	
	if(rDate != "None" && rDate != "POSIXct" && rDate != "POSIXlt")
	{
		last.SpssCfError <<- 1009
		if(is.SpssCfError(last.SpssCfError))
		{
			stop(printSpssError(last.SpssCfError),call. = FALSE, domain = NA)
		}
	}

	if(factorMode != "none" && factorMode != "labels" && factorMode != "levels")
	{
		last.SpssCfError <<- 1008
		if(is.SpssCfError(last.SpssCfError))
		{
			stop(printSpssError(last.SpssCfError),call. = FALSE, domain = NA)
		}
	}
	
	if(is.null(rowCount) || rowCount <= 0)
	{
		rowCount <- ibmspsscfdata.GetRecordCount()
	}

	miss <- 0
	if(is.nan(missingValue))
	{
		miss <- 1
	} else if(is.na(missingValue))
	{
		miss <- 0
	} else if("asis" == missingValue)
	{
		miss <- 2
	} else
	{
		last.SpssCfError <<- 1010
		if(is.SpssCfError(last.SpssCfError))
		{
			stop(printSpssError(last.SpssCfError),call. = FALSE, domain = NA)
		}
	}
	out <- .Call("ext_GetData",as.list(fields),as.integer(rowCount),
										as.integer(miss),
										as.integer(err),
										PACKAGE=ibmspsscf_package)

	n <- length(out)
	last.SpssCfError <- out[[n]]
	if(is.SpssCfError(last.SpssCfError))
	{
		stop(printSpssError(last.SpssCfError),call. = FALSE, domain = NA)
	}

	result <- out[1:(n-1)]
	rm(out)

	n <- length(result)

	out <- .Call("ext_GetFieldFormats",as.list(fields),as.integer(err),
											PACKAGE=ibmspsscf_package)
	n <- length(out)
	last.SpssCfError <<- out[n]
	if(is.SpssCfError(last.SpssCfError))
	{
		stop(printSpssError(last.SpssCfError),call. = FALSE, domain = NA)
	}
	fieldFormatTypes <- out[1:n-1]
	
	## get the field storages
	out <- .Call("ext_GetFieldStorages",as.list(fields),as.integer(err),PACKAGE=ibmspsscf_package)
	n <- length(out)
	last.SpssCfError <<- out[n]
	if(is.SpssCfError(last.SpssCfError))
	{
		stop(printSpssError(last.SpssCfError),call. = FALSE, domain = NA)
	}
	fieldStorages <- out[1:n-1]
	
	## transform spss datetime into POSIXct which is supported by R
	POSIXltFields <- vector()
	if(rDate != "None")
	{
		dateFields <- 1:length(fields)

		for(i in dateFields)
		{
			if(fieldStorages[i]== "date" || fieldStorages[i]== "timestamp" )
			{
				## the date in CF is days since Jan 1st 1970 
				## the datetime in CF seconds since midnight Jan 1st 1970
				## this code is to convert days to seconds
				if(fieldStorages[i] != "timestamp")
				{
					result[[i]] <- result[[i]]*(24*60*60)
				}
				
				if(rDate == "POSIXct")
				{
					result[[i]] <- as.POSIXct(result[[i]],origin="1970-01-01 00:00:00", tz="GMT")
				}
				else if(rDate == "POSIXlt")
				{ 
					POSIXltFields <- c(POSIXltFields, i)
					result[[i]] <- as.POSIXlt(result[[i]],origin="1970-01-01 00:00:00", tz="GMT")
				}
			}
			##timeFormatSetTest
			##if(fieldFormatTypes[i]%in%timeFormatSetTest)
			##{

				##result[[i]] <- as.POSIXlt(result[[i]], origin="1970-01-01 00:00:00",tz="GMT")
				## this is transfer to character, not ok
				##result[[i]] <- strftime(result[[i]], format="%H:%M:%S")
				
			##}
			
		}
	}
	##result <- unicodeConverterOutput(result)

	out <- .Call("ext_GetFieldMeasures",as.list(fields),as.integer(err),
											PACKAGE=ibmspsscf_package)
	n <- length(out)
	last.SpssCfError <<- out[n]
	if(is.SpssCfError(last.SpssCfError))
	{
		stop(printSpssError(last.SpssCfError),call. = FALSE, domain = NA)
	}
	fieldMeasure <- out[1:n-1] 
	if(factorMode != "none")
	{
		j<-1
		for(i in fields)
		{
			if("nominal" == fieldMeasure[[j]] || "discrete" == fieldMeasure[[j]])
			{
				valueLabels <- ibmspsscfdatamodel.GetValueLabels(i)
				uniqueset<- unique(result[[j]])

				for(i in uniqueset)
				{
					if(!(i%in%valueLabels$values)&&!(is.na(i)&&!is.nan(i)))
					{
						valueLabels$values <- append(valueLabels$values,i)
						valueLabels$labels <- append(valueLabels$labels,i)  
					}
				} 				
				if(factorMode == "labels")
				{
					result[[j]] <- factor(result[[j]],levels = valueLabels$values, labels= valueLabels$labels, ordered = FALSE)
				}
				else if(factorMode == "levels")
				{
					result[[j]] <- factor(result[[j]],levels = valueLabels$values, ordered = FALSE)
				}
			}
			if("ordinal" == fieldMeasure[[j]])
			{
				valueLabels <- ibmspsscfdatamodel.GetValueLabels(i)
				uniqueset<- unique(result[[j]])
				for(i in uniqueset)
				{                       
					if(!(i%in%valueLabels$values)&&!(is.na(i)&&!is.nan(i)))
					{
						valueLabels$values <- append(valueLabels$values,i)
						valueLabels$labels <- append(valueLabels$labels,i)  
					}
				}     
				if(factorMode == "labels")
				{
					result[[j]] <- factor(result[[j]],levels = valueLabels$values, labels= valueLabels$labels, ordered=TRUE)
				}
				else if(factorMode == "levels")
				{
					result[[j]] <- factor(result[[j]],levels = valueLabels$values, ordered=TRUE)
				}
			}
			j<-j+1
		}
	}
	
	## process converting flag fields to logical fields
	if(logicalFields == TRUE)
	{
		j<-1
		for(i in fields)
		{
			if("flag" == fieldMeasure[[j]])
			{
				## flagValues[1] is true value, flagValues[2] is false value
				flagValues <- ibmspsscfdatamodel.GetFlagValues(i)
				for(index in 1:length(result[[j]]))
				{
					if(!is.na(result[[j]][index])) {
						if(result[[j]][index] == flagValues[1])	
						{
							result[[j]][index] <- TRUE
						} else if(result[[j]][index] == flagValues[2])
						{
							result[[j]][index] <- FALSE
						} else 
						{
							result[[j]][index] <- NA
						}
					} 
				}
				result[[j]] <- as.logical(result[[j]])
			}
			j <- j+1
		}
	}
	
	## add variable names as the column names of the data frame.
	out <- .Call("ext_GetFieldNames",as.list(fields),as.integer(err),
									PACKAGE=ibmspsscf_package)
	last.SpssCfError <<- out[length(out)] 
	if(is.SpssCfError(last.SpssCfError))
	{
		stop(printSpssError(last.SpssCfError),call. = FALSE, domain = NA)
	}
	##fieldNames <- unicodeConverterOutput(out[1:length(out)-1])
	fieldNames <- out[1:length(out)-1]
	rm(out)

	#force to do a garbage collection
	gc(verbose = FALSE)
	stringsAsFactors <- unlist(options("stringsAsFactors"))[[1]]
	options("stringsAsFactors" = TRUE )
	warn <- unlist(options("warn"))[[1]]
	options("warn" = 2 )
	
	
		value <- data.frame(result)
		rm(result)
		gc(verbose = FALSE)
		names(value) <- fieldNames
		rm(fieldNames)
		gc(verbose = FALSE)
	
	options("stringsAsFactors" = stringsAsFactors)
	options("warn" = warn )
	
	for(i in POSIXltFields)
	{
		value[[i]] <- as.POSIXlt(value[[i]])	
	}
	value

}

ibmspsscfdata.GetRecordCount <- function()
{
	err <- 0
	out <- .C("ext_GetRecordCount",as.integer(0), as.integer(err),PACKAGE=ibmspsscf_package)
	last.SpssCfError <<- out[[2]] 
	if( is.SpssCfError(last.SpssCfError))
		stop(printSpssError(last.SpssCfError),call. = FALSE, domain = NA)

	records <- out[[1]]
	records
}

ibmspsscfdata.HasMoreData <- function()
{
	err <- 0
	out <- .C("ext_HasMoreData",as.logical(0), as.integer(err),PACKAGE=ibmspsscf_package)

	last.SpssCfError <<- out[[2]] 
	if( is.SpssCfError(last.SpssCfError))
		stop(printSpssError(last.SpssCfError),call. = FALSE, domain = NA)

	hasMoreData <- out[[1]]
	hasMoreData
}


ibmspsscfdata.SetData <- function(x)
{
    ##spssError.reset()
    ##if( !getOption("is.dataStepRunning") )
    ##{
    ##    last.SpssError <<- 1009 
    ##    if( is.SpssError(last.SpssError))
    ##        stop(printSpssError(last.SpssError),call. = FALSE, domain = NA)
    ##}
	x <- data.frame(x)
    x <- as.list(x)
    fieldNums <- length(x)
    err <- 0
	## the outputDataModel get in ibmspsscfdatamodel.SetDataModel
	if(is.null(outputStorages))
	{
		last.SpssCfError <<- 1011
		if(is.SpssCfError(last.SpssCfError))
		{
			out <- .C("ext_SendErrorCode",as.integer(last.SpssCfError), as.integer(err),PACKAGE=ibmspsscf_package)
			stop(printSpssError(last.SpssCfError),call. = FALSE, domain = NA)
		}
	} else if(0 == length(outputStorages))
	{
		return(NULL)
	}
	
    fieldStorages <- outputStorages
    fieldName <- ""
    for(i in 1:fieldNums)
    {
		fieldStorage <- fieldStorages[[i]]
		if("POSIXt"%in%class(x[[i]]))
		{
			x[[i]]<-as.double(difftime(as.POSIXct(x[[i]]),as.POSIXct(0,origin="1970-01-01 00:00:00",tz="GMT"),units = "secs"))
			if("date" == fieldStorage)
			{
				## convert seconds to days
				x[[i]] <- x[[i]]/((24*60*60))
			}
		}
		
        x[[i]] <- unlist(x[[i]])
        x[[i]] <- as.vector(x[[i]])

        if( "string" == fieldStorage) {
            x[[i]] <- sapply(x[[i]],as.character)
        }
        else if("integer" == fieldStorage) {  	
			x[[i]] <- sapply(x[[i]],as.integer)
		} else {
			x[[i]] <- sapply(x[[i]],as.double)
		}
            
    }
    
    ##isUnicodeOn <- .C("ext_IsUTF8mode",as.logical(FALSE),PACKAGE=ibmspsscf_package)[[1]]
    ##if(isUnicodeOn)
    ##{
    for(i in 1:fieldNums)
		x[i] <- UnicodeConverterInput(x[i])
    ##}
    out <- .Call("ext_SetData",x,as.list(fieldStorages),as.integer(err),PACKAGE=ibmspsscf_package)
    ##last.SpssError <<- out[1] 
    ##if( is.SpssError(last.SpssError))
    ##    stop(printSpssError(last.SpssError),call. = FALSE, domain = NA)
}
