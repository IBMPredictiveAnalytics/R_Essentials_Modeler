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

ibmspsscfoutput.GetOutputDir <- function()
{
	out <- .C("ext_GetOutputDir",as.character(""),as.integer(0),PACKAGE=ibmspsscf_package)
	last.SpssCfError <<- out[[2]] 
	if(last.SpssCfError)
		processSpssCFError(last.SpssCfError)
		
	dir <- out[[1]]
	outputPath <- file.path(dirname(dir), basename(dir), 'ROutput')
	if (!file.exists(outputPath))
	{
		dir.create(outputPath, showWarnings = TRUE, recursive = TRUE)
	}
	outputPath
}

ibmspsscfoutput.SetHTML <- function(htmlOutput, imageNames=NULL)
{
	if(is.null(htmlOutput)) {
		warning("No html output provided")
		return
	}
	ibmspsscfpkg.htmlFilesCount <<- ibmspsscfpkg.htmlFilesCount + 1
	outputPath <- ibmspsscfoutput.GetOutputDir()
	tempFileName <- paste("Output", sprintf("%03d", ibmspsscfpkg.htmlFilesCount), ".html", sep="")
	fileName <- file.path(outputPath, tempFileName)
	if(file.exists(fileName))
	{
		warning("Existing file is overwrited")
	}
	write(htmlOutput, fileName)
	ibmspsscfpkg.zipFileNames <<- c(ibmspsscfpkg.zipFileNames, tempFileName)
	for(imageName in imageNames)
	{
		ibmspsscfpkg.zipFileNames <<- c(ibmspsscfpkg.zipFileNames, imageName)
	}
}

ibmspsscfoutput.SetHTMLWithAllGraphs <- function()
{
	## get all graph names
	tryCatch({dev.off()}, error=function(ex) {},finally= {})
	count <- ibmspsscfoutput.GetOutputCount()
	graphNames <- ibmspsscfoutput.GetOutputNames()
	
	
	if(is.null(graphNames)) {
		warning("No graph output provided")
		return
	}
	
	htmlBegin <- "<html><body>"
	htmlEnd <- "</body></html>"
	
	for(name in graphNames)
	{
		temp <- NULL
		temp1 <- paste("<p>", as.character(name))
		temp2 <- "<img src=\""
		temp3 <- name
		temp4 <- "\",width=\"500\" height=\"500\"></p>"
		temp <- paste(temp1, temp2, temp3, temp4)
		htmlBegin <- paste(htmlBegin, temp)
	}
	
	html <- paste(htmlBegin, htmlEnd)
	
	ibmspsscfpkg.htmlFilesCount <<- ibmspsscfpkg.htmlFilesCount + 1
	outputPath <- ibmspsscfoutput.GetOutputDir()
	tempFileName <- paste("Output", sprintf("%03d", ibmspsscfpkg.htmlFilesCount), ".html", sep="")
	fileName <- file.path(outputPath, tempFileName)
	if(file.exists(fileName))
	{
		warning("Existing file is overwrited")
	}
	write(html, fileName)
	ibmspsscfpkg.zipFileNames <<- c(ibmspsscfpkg.zipFileNames, tempFileName)
	for(imageName in graphNames)
	{
		ibmspsscfpkg.zipFileNames <<- c(ibmspsscfpkg.zipFileNames, imageName)
	}
}

ibmspsscfoutput.SinkOn <- function()
{
	outputPath <-ibmspsscfoutput.GetOutputDir()
	fileName <- file.path(outputPath, "TextOutput.txt")
	sink(fileName, FALSE, "output", FALSE)
}

ibmspsscfoutput.SinkOff <- function()
{
	ibmspsscfpkg.fileNamesList <<- c(ibmspsscfpkg.fileNamesList, "TextOutput.txt")
	sink()
}

ibmspsscfoutput.SetPMML <- function(PMML, statsXML=NULL)
{
	if(is.null(PMML)) 
	{
		warning("No PMML provided")
		return
	}
	outputPath <- ibmspsscfoutput.GetOutputDir()
	pmmlFileName <- file.path(outputPath, "PMML.xml")
	if(file.exists(pmmlFileName))
	{
		warning("Existing PMML output is overwrited")
	}
	write(PMML, pmmlFileName)
	ibmspsscfpkg.fileNamesList <<- c(ibmspsscfpkg.fileNamesList, "PMML.xml")
	if(!is.null(statsXML))
	{
		statsFileName <- file.path(outputPath, "StatsXML.xml")
		if(file.exists(statsFileName))
		{
			warning("Existing Statistics XML is overwrited")
		}
		write(statsXML, statsFileName)
		ibmspsscfpkg.fileNamesList <<- c(ibmspsscfpkg.fileNamesList, "StatsXML.xml")
	}
}

ibmspsscfoutput.GetModel <- function()
{
	out <- .C("ext_GetModel",as.character(""),as.integer(0),PACKAGE=ibmspsscf_package)
	last.SpssCfError <<- out[[2]] 
	if(last.SpssCfError)
		processSpssCFError(last.SpssCfError)
	
	modelFileName <- out[[1]]
	modelFile <- file(modelFileName, "r+")
	model <- unserialize(modelFile)
	close(modelFile)
	model
}

ibmspsscfoutput.SetModel <- function(model)
{
	if(is.null(model)) 
	{
		warning("No Model provided")
		return
	}
	outputPath <- ibmspsscfoutput.GetOutputDir()
	modelFileName <- file.path(outputPath, "model")
	modelFile <- file(modelFileName, "w")
	serialize(model, modelFile)
	flush(modelFile)
	close(modelFile)
	ibmspsscfpkg.fileNamesList <<- c(ibmspsscfpkg.fileNamesList, "model")
	##write(tempModel, modelFileName)
	##.C("ext_SetModel",as.character(""),0,PACKAGE=ibmspsscf_package)
}

ibmspsscfoutput.GetOutputCount <- function()
{
	outputPath <- ibmspsscfoutput.GetOutputDir()
	outputFiles <- list.files(outputPath, pattern = "*.png")
	
	if (length(outputFiles) == 1)
	{
		if(file.info(outputFiles[1])$size < 500 )
		{
			file.remove(outputFiles[1])
			return(0)
		}
	}
	
	length(outputFiles)
}

ibmspsscfoutput.GetOutputNames <- function(indices=NULL)
{
	outputPath <- ibmspsscfoutput.GetOutputDir()
	outputFiles <- list.files(outputPath, pattern = "*.png")
	
	if(!is.null(indices))
	{
		resultFiles <- list()
		for(index in indices)
		{
			if(index < 1 || index > length(outputFiles))
			{
				last.SpssCfError <<- 1013
				processSpssCFError(last.SpssCfError)
			}
			resultFiles <- c(resultFiles, outputFiles[[index]])
		}
		resultFiles
	}
	else
	{
		outputFiles
	}
}
