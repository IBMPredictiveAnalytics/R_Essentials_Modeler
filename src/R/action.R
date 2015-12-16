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

ibmspsscfpkg.startprocedure <- function()
{
	options(hasBrowser = FALSE)
	.C("ext_StartProcedure", as.integer(0),PACKAGE=ibmspsscf_package)
}

ibmspsscfpkg.preaction <- function()
{	
	## the output file names list
	ibmspsscfpkg.fileNamesList <<- list()
	ibmspsscfpkg.fileNamesList <<- c(ibmspsscfpkg.fileNamesList, "R_result.txt")
	ibmspsscfpkg.zipFileNames <<- list()
	ibmspsscfpkg.htmlFilesCount <<- as.integer(0)
	
	outputPath <- ibmspsscfoutput.GetOutputDir()
	##set the temp workspace, put the console result and graphs in this dir
	setwd(outputPath)
	
	
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
	if(last.SpssCfError != 0)
		processSpssCFError(last.SpssCfError)
	
	consoleOutputFileName <- file.path(outputPath, "R_result.txt")
	fp <- file(consoleOutputFileName, open="at", encoding="UTF-8")
	ibmspsscfpkg.connections <<- vector()
	ibmspsscfpkg.connections <<- c(ibmspsscfpkg.connections, fp)
	
	# move the sink text output from output.R to here
	if(out[[1]])
	{
		textOutputFileName <- file.path(outputPath, "TextOutput.txt")
		textFile <- file(textOutputFileName, open="at", encoding="UTF-8")
		sink(fp, append = TRUE, type = "message")
		sink(textFile, append = TRUE)
		ibmspsscfpkg.fileNamesList <<- c(ibmspsscfpkg.fileNamesList, "TextOutput.txt")
		ibmspsscfpkg.connections <<- c(ibmspsscfpkg.connections, textFile)
	} else {
		##sink all output to the console output
		sink(fp, append = TRUE, type = "message")
		sink(fp, append = TRUE)
	}
	tryCatch({png(filename="Rplot%03d.png")}, error=function(ex) {warning(ex)},finally= {})
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
			
			if(length(ibmspsscfpkg.zipFileNames) > 0)
			{
				zip("HTMLOutput", as.character(ibmspsscfpkg.zipFileNames))
				ibmspsscfpkg.fileNamesList <<- c(ibmspsscfpkg.fileNamesList, "HTMLOutput.zip")
			}
			else
			{
				## if there is ibmspsscfpkg.zipFileNames, the dev.off has been called in SetHTMLWithAllGraphs
				tryCatch({dev.off()}, error=function(ex) {},finally= {})
			}
			
			# Close all open connections
			for( i in ibmspsscfpkg.connections)
			{
				if(isOpen(i))
				{
					close(getConnection(i))
				}
			}
		},
		error=function(ex) {
			##checkoutput<-'File did not exist or invalid!'
			print(ex)
		},
		finally= {
			.C("ext_PostOutput", as.character(ibmspsscfpkg.fileNamesList),length(ibmspsscfpkg.fileNamesList),as.integer(0),PACKAGE=ibmspsscf_package)
		}
	)
}

ibmspsscfpkg.stopprocedure <- function()
{
	.C("ext_StopProcedure",as.integer(0),PACKAGE=ibmspsscf_package)
}
