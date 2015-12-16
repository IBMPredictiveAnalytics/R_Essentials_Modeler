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
		processSpssCFError(last.SpssCfError)
	}

	if(factorMode != "none" && factorMode != "labels" && factorMode != "levels")
	{
		last.SpssCfError <<- 1008
		processSpssCFError(last.SpssCfError)
	}
	
	if(is.null(rowCount) || rowCount <= 0)
	{
		rowCount <- ibmspsscfdata.GetRecordCount()
	}

	missSign <- ibmspsscfdata.CheckMissArgument(missingValue)
	out <- .Call("ext_GetData",as.list(fields),as.integer(rowCount),
										as.integer(missSign),
										as.integer(err),
										PACKAGE=ibmspsscf_package)

	n <- length(out)
	last.SpssCfError <- out[[n]]
	if(last.SpssCfError !=0)
	{
		processSpssCFError(last.SpssCfError)
	}

	result <- out[1:(n-1)]
	rm(out)
	n <- length(result)

	fieldFormatTypes <- modelerDataModel[5,]
	fieldStorages <- modelerDataModel[3,]
	fieldMeasure <- modelerDataModel[4,]
	fieldNames <- modelerDataModel[1,]
	
	## transform spss datetime into POSIXct which is supported by R
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
					result[[i]] <- as.POSIXlt(result[[i]],origin="1970-01-01 00:00:00", tz="GMT")
				}
			}			
		}
	}

	if(factorMode != "none")
	{
		j<-1
		for(i in fields)
		{	
			if("nominal" == fieldMeasure[[j]] 
			|| "discrete" == fieldMeasure[[j]]
			|| (logicalFields == FALSE && "flag" == fieldMeasure[[j]] && "string"==fieldStorages[[j]]))
			{
				if(factorMode == "labels")
				{
					valueLabels <- ibmspsscfdatamodel.GetValueLabels(i)
					if(length(valueLabels$values) == 0) 
					{
						result[[j]] <- factor(result[[j]], ordered = FALSE)
					}
					else 
					{
						result[[j]] <- factor(result[[j]],levels = valueLabels$values, labels = valueLabels$labels, ordered = FALSE)
					} 
				}
				else if(factorMode == "levels")
				{
					# let it has the same behaviour of read.table
					result[[j]] <- factor(result[[j]], ordered = FALSE)
				}
			}
			if("ordinal" == fieldMeasure[[j]])
			{		
				valueLabels <- ibmspsscfdatamodel.GetValueLabels(i)
				if(factorMode == "labels")
				{
					if(length(valueLabels$values) == 0) {
						result[[j]] <- factor(result[[j]], ordered=TRUE)
					}
					else {
						result[[j]] <- factor(result[[j]],levels = valueLabels$values, labels= valueLabels$labels, ordered=TRUE)
					}
				}
				else if(factorMode == "levels")
				{
					if(length(valueLabels$values) == 0) 
					{
						result[[j]] <- factor(result[[j]], ordered=TRUE)
					}
					else 
					{
						result[[j]] <- factor(result[[j]],levels = valueLabels$values, ordered=TRUE)
					}
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

	#this is a tricky way to work around as.data.frame
	#as.data.frame has a bad performance
	rowNum <- length(result[[1]])
	class(result) <- "data.frame"
	attr(result, "row.names") <- 1:rowNum
	names(result) <- fieldNames
	
	result
}

ibmspsscfdata.CheckMissArgument <- function(missingValue)
{
	missSign <- 0
	if(is.nan(missingValue))
	{
		missSign <- 1
	} else if(is.na(missingValue))
	{
		missSign <- 0
	} else if("asis" == missingValue)
	{
		missSign <- 2
	} else
	{
		last.SpssCfError <<- 1010
		processSpssCFError(last.SpssCfError)
	}
	missSign
}


# this function is to solve batch issue, it uses read.table to get data
# from temp data file created by R component
ibmspsscfdata.GetDataFromTemp <- function(missingValue = NA,
                                                  factorMode = "levels",
												  rDate = "None",
                                                  logicalFields=FALSE)
{
	spssError.reset()
	## 1. Convert missing values
	## check argument: missingValue
    missSign <- ibmspsscfdata.CheckMissArgument(missingValue)
	
	out <- .C("ext_GetTempDataFile",as.character(""),as.integer(missSign),as.integer(0),PACKAGE=ibmspsscf_package)
    dataFileName <- out[[1]]
	
	## need to give colClasses, it will treat "F" as logical by default
	fieldStorages <- modelerDataModel[3,]
	colClassesVec <- vector(mode="character")
	for(i in fieldStorages)
	{
		if("string" == i) 
		{
			colClassesVec <- c(colClassesVec, "factor")
		} else if("integer" == i) 
		{
			colClassesVec <- c(colClassesVec, "integer")
		} else 
		{
			colClassesVec <- c(colClassesVec, "numeric")
		}
	} 
	dataFromTempFile <- read.table(dataFileName, header=TRUE, sep=" ",colClasses=colClassesVec, encoding="UTF-8")
	unlink(dataFileName)
	
	
	## 2. Convert flag fields
	## process converting flag fields to logical fields
	fieldMeasure <- modelerDataModel[4,]
	fieldCount <- ibmspsscfdatamodel.GetFieldCount()
	fields <- 0:(fieldCount-1)
   	if(logicalFields == TRUE)
	{
		j<-1
		for(i in fields)
		{
			if("flag" == fieldMeasure[[j]])
			{
				## flagValues[1] is true value, flagValues[2] is false value
				flagValues <- ibmspsscfdatamodel.GetFlagValues(i)
				## need to convert character firstly, factor will have value check 
				dataFromTempFile[[j]] <- as.character(dataFromTempFile[[j]])
				for(index in 1:length(dataFromTempFile[[j]]))
				{
					if(!is.na(dataFromTempFile[[j]][index])) {
						if(dataFromTempFile[[j]][index] == flagValues[1])	
						{
							dataFromTempFile[[j]][index] <- TRUE
						} else if(dataFromTempFile[[j]][index] == flagValues[2])
						{
							dataFromTempFile[[j]][index] <- FALSE
						} else 
						{
							dataFromTempFile[[j]][index] <- NA
						}
					} 
				}
				dataFromTempFile[[j]] <- as.logical(dataFromTempFile[[j]])
			}
			j <- j+1
		}
	}
	
	## 3. Convert date/time fields
	fieldStorages <- modelerDataModel[3,]
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
					dataFromTempFile[[i]] <- dataFromTempFile[[i]]*(24*60*60)
				}
				if(rDate == "POSIXct")
				{
					dataFromTempFile[[i]] <- as.POSIXct(dataFromTempFile[[i]],origin="1970-01-01 00:00:00", tz="GMT")
				}
				else if(rDate == "POSIXlt")
				{ 
					dataFromTempFile[[i]] <- as.POSIXlt(dataFromTempFile[[i]],origin="1970-01-01 00:00:00", tz="GMT")
				}
			}			
		}
	}
	
	
	## 4. Convert factor
	## now in this package, it just supports factorMode == "levels" and 
	## it means factor levels are values of the fields
	if(factorMode != "none")
	{
		j<-1
		for(i in fields)
		{	
			if("ordinal" == fieldMeasure[[j]])
			{		
				valueLabels <- ibmspsscfdatamodel.GetValueLabels(i)
				if(factorMode == "levels")
				{
					if(length(valueLabels$values) == 0) 
					{
						dataFromTempFile[[j]] <- factor(dataFromTempFile[[j]], ordered=TRUE)
					}
					else 
					{
						dataFromTempFile[[j]] <- factor(dataFromTempFile[[j]],levels = valueLabels$values, ordered=TRUE)
					}
				}
			}
			j<-j+1
		}
	}
	
	dataFromTempFile
}

ibmspsscfdata.GetRecordCount <- function()
{
	err <- 0
	out <- .C("ext_GetRecordCount",as.integer(0), as.integer(err),PACKAGE=ibmspsscf_package)
	last.SpssCfError <<- out[[2]] 
	if(last.SpssCfError !=0)
		processSpssCFError(last.SpssCfError)
		
	records <- out[[1]]
	records
}

ibmspsscfdata.HasMoreData <- function()
{
	err <- 0
	out <- .C("ext_HasMoreData",as.logical(0), as.integer(err),PACKAGE=ibmspsscf_package)

	last.SpssCfError <<- out[[2]] 
	if(last.SpssCfError !=0)
		processSpssCFError(last.SpssCfError)

	hasMoreData <- out[[1]]
	hasMoreData
}

ibmspsscfdata.SetDataToTemp <- function(x)
{
	ibmspsscfdata.DataModelCheck(x)
	fieldStorages <- outputStorages
	fieldNums <- ncol(x)
	
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
		
		# check if the variable is a list type
		# if a list, report a warning to cf R component
		if(is.list(x[[i]])) {
			last.SpssCfError <<- 1014
			fieldNames <- names(x)
			.Call("ext_SendErrorCode",as.integer(last.SpssCfError), as.integer(4), as.list(fieldNames[i]), as.integer(err),PACKAGE=ibmspsscf_package)
			#stop(printSpssError(last.SpssCfError),call. = FALSE, domain = NA)
		} 
		
		# factor and with numeric storage needs convert
		if("string" != fieldStorage) {
			if(is.factor(x[[i]])) {
				x[[i]] <- sapply(x[[i]], as.character)
				if("integer" == fieldStorage) {
					x[[i]] <- sapply(x[[i]],as.integer)
				} else {
					x[[i]] <- sapply(x[[i]],as.double)
				}
			}
		}

		if("string" == fieldStorage) {
			if(is.factor(x[[i]]) || is.character(x[[i]])) {
				# do not need to convert
			} else {
				x[[i]] <- sapply(x[[i]],as.character)
			}
        }
        else if("integer" == fieldStorage && !is.integer(x[[i]])) { 
			x[[i]] <- sapply(x[[i]],as.integer)
		} else if(!is.numeric(x[[i]])){
			x[[i]] <- sapply(x[[i]],as.double)
		}
		
	}
	
	outputPath <- ibmspsscfoutput.GetOutputDir()
	temp <- paste("r_to_modeler", Sys.getpid(), sep="_")
	dataFileName <- file.path(outputPath, temp)
	write.table(x, file=dataFileName, col.names = FALSE, row.names = FALSE, qmethod ="double", fileEncoding ="UTF-8")
	out <- .C("ext_SetDataToTemp",as.character(dataFileName),as.integer(0),PACKAGE=ibmspsscf_package)
}

ibmspsscfdata.DataModelCheck <- function(x)
{
	## the outputDataModel get in ibmspsscfdatamodel.SetDataModel
	if(is.null(outputStorages))
	{
		last.SpssCfError <<- 1011
		if(is.SpssCfError(last.SpssCfError))
		{
			.Call("ext_SendErrorCode",as.integer(last.SpssCfError), as.integer(3), as.list(""), as.integer(0),PACKAGE=ibmspsscf_package)
			stop(printSpssError(last.SpssCfError),call. = FALSE, domain = NA)
		}
	} else if(0 == length(outputStorages))
	{
		return(NULL)
	}
	
	## check the length of modelerData and outputStorages
	if(ncol(modelerData) != length(outputStorages))
	{
		last.SpssCfError <<- 1012
		if(is.SpssCfError(last.SpssCfError))
		{
			.Call("ext_SendErrorCode",as.integer(last.SpssCfError), as.integer(3), as.list(""), as.integer(0),PACKAGE=ibmspsscf_package)
			stop(printSpssError(last.SpssCfError),call. = FALSE, domain = NA)
		}
	}
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
	err <- 0
	x <- data.frame(x)
    x <- as.list(x)

	ibmspsscfdata.DataModelCheck(x)
	fieldStorages <- outputStorages
	fieldNums <- length(x)
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
		
		# check if the variable is a list type
		# if a list, report a warning to cf R component
		if(is.list(x[[i]])) {
			last.SpssCfError <<- 1014
			fieldNames <- names(x)
			.Call("ext_SendErrorCode",as.integer(last.SpssCfError), as.integer(4), as.list(fieldNames[i]), as.integer(err),PACKAGE=ibmspsscf_package)
			#stop(printSpssError(last.SpssCfError),call. = FALSE, domain = NA)
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
    
    for(i in 1:fieldNums)
		x[i] <- UnicodeConverterInput(x[i])
	
    out <- .Call("ext_SetData",x,as.list(fieldStorages),as.integer(err),PACKAGE=ibmspsscf_package)
    ##last.SpssError <<- out[1] 
    ##if( is.SpssError(last.SpssError))
    ##    stop(printSpssError(last.SpssError),call. = FALSE, domain = NA)
}
