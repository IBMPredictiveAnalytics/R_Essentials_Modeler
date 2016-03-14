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

ibmspsscfpkg.preaction <- function()
{
	options(hasBrowser = FALSE)
	.C("ext_StartProcedure", as.integer(0),PACKAGE=ibmspsscf_package)
	
	## the output file names list
	fileNamesList <<- list()
	fileNamesList <<- c(fileNamesList, "R_result.txt")
	zipFileNames <<- list()
	htmlFilesCount <<- as.integer(0)
	
	outputPath <- ibmspsscfoutput.GetOutputDir()
	##set the temp workspace, put the console result and graphs in this dir
	setwd(outputPath)
	tryCatch({png(filename="Rplot%03d.png")}, error=function(ex) {print(ex)},finally= {})
	
	# Stop all existing error message diversion  if any.
	if(sink.number(type = "message") > 0)
	{
		for( i in 1:sink.number(type = "message") )
		{
			sink(type = "message")
		}
	}

	# Stop all existing output diversion  if any.
	if(sink.number() > 0)
	{
		for( i in 1:sink.number() )
		{
			sink()
		}
	}
	
	##Sys.setlocale("LC_ALL","ja_JP.utf8")
	## set the locale of embedding R in Linux
	if ("windows" != .Platform$OS.type)
	{
		out <- .C("ext_GetSystemLocale", as.character(""),as.integer(0),PACKAGE=ibmspsscf_package)
		Sys.setlocale("LC_ALL",out[[1]])
	}
	
	out <- .C("ext_IsDisplayTextOutput",as.integer(0),as.integer(0),PACKAGE=ibmspsscf_package)
	last.SpssCfError <<- out[[2]] 
	if( is.SpssCfError(last.SpssCfError))
		stop(printSpssError(last.SpssCfError),call. = FALSE, domain = NA)
	
	
	consoleOutputFileName <- file.path(outputPath, "R_result.txt")
	fp <- file(consoleOutputFileName, open="at", encoding="UTF-8")
	
	# move the sink text output from output.R to here
	if(out[[1]])
	{
		textOutputFileName <- file.path(outputPath, "TextOutput.txt")
		textFile <- file(textOutputFileName, open="at", encoding="UTF-8")
		sink(fp, append = TRUE, type = "message")
		sink(textFile, append = TRUE)
		fileNamesList <<- c(fileNamesList, "TextOutput.txt")
	} else {
		##sink all output to the console output
		sink(fp, append = TRUE, type = "message")
		sink(fp, append = TRUE)
	}
}

ibmspsscfpkg.postaction <- function()
{	
	tryCatch(
		{
			if(sink.number(type = "message") > 0)
			{
				for( i in 1:sink.number(type = "message") )
				{
					sink(type = "message")
				}
			}
			# Stop all existing output diversion  if any.
			if(sink.number() > 0)
			{
				for( i in 1:sink.number() )
				{
					sink()
				}
			}
			
			if(length(zipFileNames) > 0)
			{
			zip("HTMLOutput", as.character(zipFileNames))
			fileNamesList <<- c(fileNamesList, "HTMLOutput.zip")
			}
			else
			{
				## if there is zipFileNames, the dev.off has been called in SetHTMLWithAllGraphs
				tryCatch({dev.off()}, error=function(ex) {},finally= {})
			}
		},
		error=function(ex) {
			##checkoutput<-'File did not exist or invalid!'
			print(ex)
		},
		finally= {
			.C("ext_PostOutput", as.character(fileNamesList),length(fileNamesList),as.integer(0),PACKAGE=ibmspsscf_package)
			.C("ext_StopProcedure",as.integer(0),PACKAGE=ibmspsscf_package)
		}
	)
	
	
}
