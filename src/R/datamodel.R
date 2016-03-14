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

baseInfo <- c("fieldName","fieldLabel","fieldStorage","fieldMeasure","fieldFormat","fieldRole")

checkBaseInfo <- function(x)
{
    check <- TRUE
    if(is.data.frame(x))
    {
        info <- row.names(x)
        if( !is.null(info) && 6 == length(info) )
            check <- any(info != baseInfo)
    }
    !check
}

ibmspsscfdatamodel.GetDataModel <- function(fields=NULL)
{
	err <- 0
	if( is.null(fields) )
	{
		fieldNum <- ibmspsscfdatamodel.GetFieldCount()
		if(fieldNum > 0)
		{
			fields <- 0:(fieldNum-1)
		}
		else
		{
			return(NULL)
		}
	}
	##else if( is.character(variables) )
	##    variables <- ParseVarNames(variables)
	
	if(length(fields)==0)
	{
		return(NULL)
	}
	## get field names
	out <- .Call("ext_GetFieldNames",as.list(fields),as.integer(err),
											PACKAGE=ibmspsscf_package)
	n <- length(out)
	last.SpssCfError <<- out[n] 
	if(last.SpssCfError !=0)
		processSpssCFError(last.SpssCfError)
	##varName <- unicodeConverterOutput(out[1:n-1])
	fieldName <- out[1:n-1]

	
	## get field name label
	out <- .Call("ext_GetFieldLabels",as.list(fields),as.integer(err),
											PACKAGE=ibmspsscf_package)
	n <- length(out)
	last.SpssCfError <<- out[n] 
	if(last.SpssCfError !=0)
		processSpssCFError(last.SpssCfError)
    fieldLabel <- out[1:n-1]
	
	## get field storage
	out <- .Call("ext_GetFieldStorages",as.list(fields),as.integer(err),
											PACKAGE=ibmspsscf_package)
	n <- length(out)
	last.SpssCfError <<- out[n] 
	if(last.SpssCfError !=0)
		processSpssCFError(last.SpssCfError)
	fieldStorage <- out[1:n-1]
	
	
	## get field measure
	out <- .Call("ext_GetFieldMeasures",as.list(fields),as.integer(err),
											PACKAGE=ibmspsscf_package)
	n <- length(out)
	last.SpssCfError <<- out[n] 
	if(last.SpssCfError !=0)
		processSpssCFError(last.SpssCfError)
	fieldMeasure <- out[1:n-1]
	## get field format
	out <- .Call("ext_GetFieldFormats",as.list(fields),as.integer(err),
											PACKAGE=ibmspsscf_package)
	n <- length(out)
	last.SpssCfError <<- out[n] 
	if(last.SpssCfError !=0)
		processSpssCFError(last.SpssCfError)
	fieldFormat <- out[1:n-1]
	
	out <- .Call("ext_GetFieldRoles",as.list(fields),as.integer(err),
											PACKAGE=ibmspsscf_package)
	n <- length(out)
	last.SpssCfError <<- out[n] 
	if(last.SpssCfError !=0)
		processSpssCFError(last.SpssCfError)
	fieldRole <- out[1:n-1]
	
	fields <- rbind(fieldName,fieldLabel,fieldStorage,fieldMeasure,fieldFormat,fieldRole)
	value <- data.frame(fields,stringsAsFactors=FALSE)
	value
}

ibmspsscfdatamodel.GetFieldCount <- function()
{
	err <- 0
	out <- .C("ext_GetFieldCount",as.integer(0), as.integer(err),PACKAGE=ibmspsscf_package)
	last.SpssCfError <<- out[[2]] 
	if(last.SpssCfError)
		processSpssCFError(last.SpssCfError)

	columns <- out[[1]]
	columns
}

## the parameter field should be a numeric
GetFieldName <- function(field)
{
	##spssError.reset()
	if(!is.numeric(field))
	{
		last.SpssCfError <<- 1017 
		processSpssCFError(last.SpssCfError)
	}
	fieldIndex <- field

	err <- 0
	out <- .C("ext_GetFieldName",as.character(""), as.integer(fieldIndex),as.integer(err),PACKAGE=ibmspsscf_package)

	last.SpssCfError <<- out[[3]] 
	if(last.SpssCfError)
		processSpssCFError(last.SpssCfError)

	fieldName <- out[[1]]
	##varName <- unicodeConverterOutput(varName)
	fieldName
}

GetFieldStorage <- function(field)
{
	##spssError.reset()
	fieldIndex <- GetFieldIndex(field)
	
	err <- 0
	out <- .C("ext_GetFieldStorage",as.character(""), as.integer(fieldIndex),as.integer(err),PACKAGE=ibmspsscf_package)

	last.SpssCfError <<- out[[3]] 
	if(last.SpssCfError)
		processSpssCFError(last.SpssCfError)

	fieldStorage <- out[[1]]
	fieldStorage
}

GetFieldIndex <- function(field)
{
	##oldwarn = getOption("warn")
	##options(warn = -1)
	try(temp <- as.integer(field),TRUE)
	##options(warn = oldwarn)
	if(is.na(temp))
	{
		temp <- field
	}

	field <- temp
	result <- NULL
	if(is.numeric(field))
	{
		result <- field
	}
	else
	{
		fieldNum <- ibmspsscfdatamodel.GetFieldCount()
		for(i in 0:(fieldNum-1))
		{
			fieldName <- GetFieldName(i)
			if(nchar(fieldName) == nchar(field) && !is.na(charmatch(fieldName,field)))
			{
				result <- i
				break
			}
		}
	}
	if( is.null(result))
	{
		## can not match the parameter field with field names in data
		last.SpssCfError <<- 1020
		processSpssCFError(last.SpssCfError)
	}
	result
}

ibmspsscfdatamodel.GetValueLabels <- function(field)
{
    ##spssError.reset()
    field <- GetFieldIndex(field)
    fieldIndex <- field

    err <- 0
    out <- NULL
    fieldStorage <- GetFieldStorage(fieldIndex)
	out <- .Call("ext_GetValueLabels",as.integer(fieldIndex),as.integer(err),PACKAGE=ibmspsscf_package)
	
	last.SpssCfError <<- out[[3]][1]
	if(last.SpssCfError)
		processSpssCFError(last.SpssCfError)

    result <- out[1:2]
    names(result) <- c("values","labels")
	
	##last.SpssCfError <<- out[[3]][1]
	##if(is.SpssWarning(last.SpssCfError))
	##{
		##warning(gettextf("%s %d", "Warning of Field:", fieldIndex),call. = FALSE, domain = NA)
		##warning(printSpssWarning(last.SpssCfError),call. = FALSE, domain = NA)
		##if(getData)
			##result$labels <- list()
	##}
    
	if("string" != fieldStorage) {
		result$values <- as.numeric(result$values)
	} else {
		result$values <- result$values
	}
    result
}

## this function does not finish, related with data mapping
ibmspsscfdatamodel.GetMissingValues <- function(field)
{
	##spssError.reset()
	fieldIndex <- GetFieldIndex(field)
	##missingType <- c("Discrete","Range","Range Discrete")
	err <- 0
	##missingFormat <- 0
	value <- NULL
	fieldStorage <- GetFieldStorage(fieldIndex)
	
	out <- .Call("ext_GetMissingValues",as.integer(fieldIndex),
											as.integer(err),
											PACKAGE=ibmspsscf_package)
	
	last.SpssCfError <<- out[[2]][1] 
	if(last.SpssCfError)
		processSpssCFError(last.SpssCfError)
	
	
	if("string" == fieldStorage)
	{
		value <- out[[1]]
	} else if("integer" == fieldStorage){
		value <- as.integer(out[[1]])
	} else {
		value <- as.double(out[[1]])
	}
	value
}

ibmspsscfdatamodel.GetFlagValues <- function(field)
{
	##spssError.reset()
	fieldIndex <- GetFieldIndex(field)
	err <- 0
	value <- NULL
	fieldStorage <- GetFieldStorage(fieldIndex)
	
	out <- .Call("ext_GetFlagValues",as.integer(fieldIndex),
											as.integer(err),
											PACKAGE=ibmspsscf_package)
	
	last.SpssCfError <<- out[[2]][1] 
	if(last.SpssCfError)
		processSpssCFError(last.SpssCfError)
	
	if("string" == fieldStorage)
	{
		value <- out[[1]]
	} else if("integer" == fieldStorage){
		value <- as.integer(out[[1]])
	} else {
		value <- as.double(out[[1]])
	}
	value
}

ibmspsscfdatamodel.SetDataModel <- function(dataModel)
{
	##spssError.reset()
	outputStorages <<- vector()
	err <- 0
	if(!checkBaseInfo(dataModel) )
	{
		last.SpssCfError <<- 1001 
	    if(is.SpssCfError(last.SpssCfError))
		{
			.Call("ext_SendErrorCode",as.integer(last.SpssCfError), as.integer(3), as.list(""), as.integer(err),PACKAGE=ibmspsscf_package)
			stop(printSpssError(last.SpssCfError),call. = FALSE, domain = NA)
		}
	}

	
	fieldName <- vector()
	fieldLabel <- vector()
	fieldStorage <- vector()
	fieldMeasure <- vector()
	fieldFormat <- vector()
	fieldRole <- vector()
	fieldNums <- length(dataModel)
	
	for(i in 1:fieldNums)
	{	
		fieldName <- c(fieldName, UnicodeConverterInput(as.character(dataModel[1, i])))
		fieldLabel <- c(fieldLabel, UnicodeConverterInput(as.character(dataModel[2, i])))
		
		##check the storage
		if(dataModel[3, i] == "string" || dataModel[3, i] == "integer" || dataModel[3, i] == "real"
		|| dataModel[3, i] == "date" || dataModel[3, i] == "time" || dataModel[3, i] == "timestamp")
		{}
		else 
		{
			last.SpssCfError <<- 1004
		    if(is.SpssCfError(last.SpssCfError))
		    {
			  invalidStorage <- as.character(dataModel[3, i])
			  .Call("ext_SendErrorCode",as.integer(last.SpssCfError), as.integer(3), as.list(invalidStorage), as.integer(err),PACKAGE=ibmspsscf_package)
			  stop(printSpssError(last.SpssCfError),call. = FALSE, domain = NA)
		    }
		}
		fieldStorage <- c(fieldStorage, as.character(dataModel[3, i]))
		
		##check the measure
		if(dataModel[4, i] =="" || dataModel[4, i] == "continuous" || dataModel[4, i] == "discrete" 
		|| dataModel[4, i] == "flag"|| dataModel[4, i] == "nominal" || dataModel[4, i] == "ordinal"
		|| dataModel[4, i] == "typeless" || dataModel[4, i] == "unknown")
		{} 
		else 
		{
			last.SpssCfError <<- 1005
		    if(is.SpssCfError(last.SpssCfError))
		    {
			  invalidMeasure <- as.character(dataModel[4, i])
			  .Call("ext_SendErrorCode",as.integer(last.SpssCfError), as.integer(3), as.list(invalidMeasure), as.integer(err),PACKAGE=ibmspsscf_package)
			  stop(printSpssError(last.SpssCfError),call. = FALSE, domain = NA)
		    }
		}
		fieldMeasure <- c(fieldMeasure, as.character(dataModel[4, i]))
		fieldFormat <- c(fieldFormat, as.character(dataModel[5, i]))
		
		##check  the role
		dataModelRole <- dataModel[6, i]
		if(dataModelRole =="" || dataModelRole == "input" ||dataModelRole == "target" 
		|| dataModelRole == "both"|| dataModelRole == "partition" || dataModelRole == "split"
		|| dataModelRole == "freqWeight" || dataModelRole == "recordId" || dataModelRole == "none")
		{}
		else 
		{
		    last.SpssCfError <<- 1007
		    if(is.SpssCfError(last.SpssCfError))
		    {
			  invalidRole <- as.character(dataModelRole)
			  .Call("ext_SendErrorCode",as.integer(last.SpssCfError), as.integer(3), as.list(invalidRole), as.integer(err),PACKAGE=ibmspsscf_package)
			  stop(printSpssError(last.SpssCfError),call. = FALSE, domain = NA)
		    }
		}
		fieldRole <- c(fieldRole, as.character(dataModel[6, i]))
	}
	
	##check if fieldNames have duplicated
	if(0 != anyDuplicated(fieldName))
	{
		last.SpssCfError <<- 1002
		if(is.SpssCfError(last.SpssCfError))
		{
			duplicatedFieldName <-  fieldName[anyDuplicated(fieldName)]
			.Call("ext_SendErrorCode",as.integer(last.SpssCfError), as.integer(3), as.list(duplicatedFieldName), as.integer(err),PACKAGE=ibmspsscf_package)
			stop(printSpssError(last.SpssCfError),call. = FALSE, domain = NA)
		}
	}
	
	
	fields <- rbind(fieldName,fieldLabel,fieldStorage,fieldMeasure,fieldFormat,fieldRole)
	value <- data.frame(fields,stringsAsFactors=FALSE)
	
	out <- .Call("ext_SetDataModel", value, as.integer(err), PACKAGE=ibmspsscf_package)
	outputStorages <<- fieldStorage
}

UnicodeConverterInput <- function(x)
{
	if(is.character(x))
	{
		if(length(x)>0)
		{
			for(i in 1:length(x))
			{
				if(Encoding(x[[i]])!="UTF-8")
				{
					x[[i]] <- iconv(x[[i]],to="UTF-8")
				}
			}
		}
	}
	x
}

