#############################################
# IBM® SPSS® Statistics - Essentials for R
# (c) Copyright IBM Corp. 1989, 2011
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

toSPSSFormat <- function(fmt)
{
	if( "E" == fmt )
	{
		FIELD_Type.fmt_E
	}
    else
    {
		switch(fmt,
				A        = FIELD_Type.fmt_A,
				AHEX     = FIELD_Type.fmt_AHEX,
				COMMA    = FIELD_Type.fmt_COMMA,
				DOLLAR   = FIELD_Type.fmt_DOLLAR,
				F        = FIELD_Type.fmt_F,
				IB       = FIELD_Type.fmt_IB,
				IBHEX    = FIELD_Type.fmt_IBHEX,
				PIBHEX   = FIELD_Type.fmt_IBHEX, #use IBHEX for both PIBHEX and IBHEX.
				P        = FIELD_Type.fmt_P,
				PIB      = FIELD_Type.fmt_PIB,
				PK       = FIELD_Type.fmt_PK,
				RB       = FIELD_Type.fmt_RB,
				RBHEX    = FIELD_Type.fmt_RBHEX,
				Z        = FIELD_Type.fmt_Z,
				N        = FIELD_Type.fmt_N,
				#E       = FIELD_Type.fmt_E,
				DATE     = FIELD_Type.fmt_DATE,
				TIME     = FIELD_Type.fmt_TIME,
				DATETIME = FIELD_Type.fmt_DATETIME,
				ADATE    = FIELD_Type.fmt_ADATE,
				JDATE    = FIELD_Type.fmt_JDATE,
				DTIME    = FIELD_Type.fmt_DTIME,
				WKDAY    = FIELD_Type.fmt_WKDAY,
				MONTH    = FIELD_Type.fmt_MONTH,
				MOYR     = FIELD_Type.fmt_MOYR,
				QYR      = FIELD_Type.fmt_QYR,
				WKYR     = FIELD_Type.fmt_WKYR,
				PERCENT  = FIELD_Type.fmt_PERCENT,
				PCT      = FIELD_Type.fmt_PERCENT, #use PERCENT for both PCT and PERCENT.
				DOT      = FIELD_Type.fmt_DOT,
				CCA      = FIELD_Type.fmt_CCA,
				CCB      = FIELD_Type.fmt_CCB,
				CCC      = FIELD_Type.fmt_CCC,
				CCD      = FIELD_Type.fmt_CCD,
				CCE      = FIELD_Type.fmt_CCE,
				EDATE    = FIELD_Type.fmt_EDATE,
				SDATE    = FIELD_Type.fmt_SDATE,
				)
	}
}

ParseFieldName <- function(field)
{
	result <- NULL
	if(is.numeric(field))
	{
		result <- c(field)
	}
	else
	{
		#support TO construct
		comma <- strsplit(field, ",")[[1]]
		str <- ""
		for( i in 1:length(comma) )
		{
			str <- paste(str, strTrim(comma[i]))
		}
		str <- strTrim(str)
		space <- strsplit(str,"[[:space:]]")[[1]]
		fieldlist <- unlist(space)
		#tolist <- grep("to", fieldlist, ignore.case = TRUE)
		tolist <- NULL
		for ( v in 1:length(fieldlist) )
		{
			if ( "to" == tolower(fieldlist[v]) )
			{
				tolist <- c(tolist, v)
			}
		}
		tolist <- as.list(tolist)
		if(length(tolist) > 0)
		{
			for(x in tolist)
			{
				if(1 == x )
				{
					fieldIndex <- GetFieldIndex(fieldlist[x+1])
					result <- c(0:fieldIndex)
				}
				else if(length(fieldlist) == x)
				{
					fieldIndex <- GetFieldIndex(fieldlist[x-1])
					
					fieldNum <- ibmspsscfdatamodel.GetFieldCount()
					result <- c(result,fieldIndex:(fieldNum-1))
				}
				else
				{
					if(x > 2)
					{
						result <- ParsefieldList(fieldlist, 1, x-2, FALSE)
					}
					result <- c(result,ParsefieldList(fieldlist, x-1, x+1, TRUE))
					if(x+1 < length(fieldlist))
					{
						result <- c(result,ParsefieldList(fieldlist, x+2, length(fieldlist), FALSE))
					}
				}
			}
		}
		else
		{
			result <- ParsefieldList(fieldlist, 1, length(fieldlist), FALSE)
		}
	}
	#sort(result)
	result
}

ParsefieldList <- function(fieldlist, fieldIndexStart, fieldIndexEnd, tolist)
{
	result <-NULL
	if(tolist)
	{
		fieldIndexStart <- GetFieldIndex(fieldlist[fieldIndexStart])
		fieldIndexEnd <- GetFieldIndex(fieldlist[fieldIndexEnd])
		if ( fieldIndexStart > fieldIndexEnd )
		{
			##last.SpssError <<- 1017
			##if( is.SpssError(last.SpssError))
			##{
				##stop(printSpssError(last.SpssError),call. = FALSE, domain = NA)
			##}
		}
		else
		{
			result <- c(result,fieldIndexStart:fieldIndexEnd)
		}
	}
	else
	{
		if(length(fieldlist)>0)
		{
			for( x in fieldlist[fieldIndexStart:fieldIndexEnd] )
			{
				if( any(fieldlist == x ) && "to" != tolower(x))
				{
					fieldIndex = GetFieldIndex(x)
					if( !any(result == fieldIndex ))
					{
						result <- c(result,fieldIndex)
					}
				}
			}
		}
	}
	result
}

ibmspsscfpkg.ParseFieldNames <- function(fields)
{
	result <- NULL
	if( is.vector(fields))
	{
		for(field in fields)
		{
		result <- c(result,ParseFieldName(field))
		}
	}
	else if( is.list(fields))
	{
		fields <- unlist(fields)
		for(field in fields)
		{
		result <- c(result,ParseFieldName(field))
		}
	}
	else if( is.character(fields))
	{
		result <- ParseFieldName(fields)
	}
	else if( !is.null(fields))
	{
		last.SpssError <<- 1011
		if( is.SpssError(last.SpssError))
		{
			stop(printSpssError(last.SpssError),call. = FALSE, domain = NA)
		}
	}
	result
}

#reverse strings
strReverse <- function(str)
{
  sapply(lapply(strsplit(str, NULL), rev), paste, collapse="")
}

#trim leading and trailing white space
strTrim <- function(str)
{
  str <- sub('[[:space:]]+$', '', str)
  str <- sub('[[:space:]]+$', '', strReverse(str))
  str <- strReverse(str)
}

ibmspsscfpkg.UnicodeConverterInput <- function(x)
{
	if(is.character(x))
	{
		if(length(x)>0)
		{
			for(i in 1:length(x))
			{
				isUnicodeOn <- .C("ext_IsUTF8mode",as.logical(FALSE),PACKAGE=spss_package)[[1]]  
				if ("windows" == .Platform$OS.type)
				{
					if(isUnicodeOn)
					{
						if(Encoding(x[[i]])!="UTF-8")
						{
							x[[i]] <- iconv(x[[i]],to="UTF-8")
						}
					}
					else
					{
						if(Encoding(x[[i]]) == "UTF-8")
						{
							x[[i]] <- iconv(x[[i]], from="UTF-8")
						}
					}
					Encoding(x[[i]])<-"native.enc"
				}
				else
				{
					Encoding(x[[i]])<-"native.enc"
				}
			}
		}
	}
	x
}

ibmspsscfpkg.unicodeConverterOutput <- function(x)
{
	if(is.character(x))
	{
		if(length(x)>0)
		{
			for(i in 1: length(x))
			{
				isUnicodeOn <- .C("ext_IsUTF8mode",as.logical(FALSE),PACKAGE=spss_package)[[1]]
				if ("windows" == .Platform$OS.type)
				{
					if(isUnicodeOn)
					{
						Encoding(x[[i]])<-"UTF-8"
					}
				}
				else
				{
					if(isUnicodeOn)
					{
						Encoding(x[[i]]) <- "UTF-8"
					}
					else
					{
						Encoding(x[[i]])<-"native.enc"
					}
				}
			}
		}
	}
	x
}

ibmspsscfpkg.GetLocale <- function()
{
	spssError.reset()
	err <- 0
	out <- .C("ext_GetSPSSLocale",as.character(""),as.integer(err),PACKAGE=spss_package)
	last.SpssError <<- out[[2]]
	if( is.SpssError(last.SpssError))
	{
		stop(printSpssError(last.SpssError),call. = FALSE, domain = NA)
	}
	locfield <- out[[1]]
	if("" == locfield)
	{
		locfield <- NULL
	}
	unicodeConverterOutput(locfield)
}

ibmspsscfpkg.SetOutputLanguage <- function(lang)
{
	spssError.reset()
	err <- 0
	lang <- unicodeConverterInput(lang)
	out <- .C("ext_SetOutputLanguage",as.character(lang),as.integer(err),PACKAGE=spss_package)
	supportlang <- c("English", "French","German", "Italian", "Japanese", "Korean", "Polish", "Russian", "Simplified Chinese", "Spanish", "Traditional Chinese", "SChinese", "TChinese", "BPortugu")
	last.SpssError <<- out[[2]]
	if( is.SpssError(last.SpssError))
	{
		stop(printSpssError(last.SpssError),call. = FALSE, domain = NA)
	}
	if( is.SpssWarning(last.SpssError))
	{
		if (out[[1]]%in%supportlang)
		{
			warning(printSpssWarning(last.SpssError),call. = FALSE, domain = NA)
		}
		else
		{
			last.SpssError <<-1074
			stop(printSpssError(last.SpssError),call. = FALSE, domain = NA)
		}
	}
}

ibmspsscfpkg.GetOutputLanguage <- function()
{
	spssError.reset()
	err <- 0
	out <- .C("ext_GetOutputLanguage",as.character(""),as.integer(err),PACKAGE=spss_package)
	last.SpssError <<- out[[2]] 
	if( is.SpssError(last.SpssError))
	{
		stop(printSpssError(last.SpssError),call. = FALSE, domain = NA)
	}
	olang <- out[[1]]  
	if("" == olang)
	{
		olang <- NULL
	}
	unicodeConverterOutput(olang)
}
