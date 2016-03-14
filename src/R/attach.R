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

ibmspsscf.errtable <- NULL
ibmspsscf.generalErr <- NULL
ibmspsscf.language <- NULL
ibmspsscf_package <- NULL
ibmspsscf_version <- NULL
plugin_version <- NULL
ibmspsscf.lib <- NULL
ibmspsscfNamespace <- "ibmspsscf83"
last.SpssCfError <- 0
## when the package is attached(via library), the hook function .onAttach is called
## before the package environment is sealed
.onAttach <- function(lib, pkg)
{
	if(bindingIsLocked("ibmspsscf_package", asNamespace(ibmspsscfNamespace)))
	{
		unlockBinding("ibmspsscf_package", asNamespace(ibmspsscfNamespace))
	}

	## modeler version
	if(bindingIsLocked("ibmspsscf_version", asNamespace(ibmspsscfNamespace)))
	{
		unlockBinding("ibmspsscf_version", asNamespace(ibmspsscfNamespace))
	}
		
	## plugin version, get from discription file    
	if(bindingIsLocked("plugin_version", asNamespace(ibmspsscfNamespace)))
	{
		unlockBinding("plugin_version", asNamespace(ibmspsscfNamespace))
	}

	if(bindingIsLocked("ibmspsscf.errtable", asNamespace(ibmspsscfNamespace)))
		unlockBinding("ibmspsscf.errtable", asNamespace(ibmspsscfNamespace))

	if(bindingIsLocked("ibmspsscf.generalErr", asNamespace(ibmspsscfNamespace)))
		unlockBinding("ibmspsscf.generalErr", asNamespace(ibmspsscfNamespace))

	if(bindingIsLocked("ibmspsscf.lib", asNamespace(ibmspsscfNamespace)))
	{
		unlockBinding("ibmspsscf.lib", asNamespace(ibmspsscfNamespace))
	}
	
	if(bindingIsLocked("last.SpssCfError", asNamespace(ibmspsscfNamespace)))
		unlockBinding("last.SpssCfError", asNamespace(ibmspsscfNamespace))

	#if(bindingIsLocked("spss.language", asNamespace(ibmspsscfNamespace)))
	#{
		#unlockBinding("spss.language", asNamespace(ibmspsscfNamespace))
	#}

	ibmspsscf_package <<- pkg
	ibmspsscf.lib <<- lib
	plugin_version <<- getPkgVersion(lib, pkg)
	ibmspsscf.errtable <<- getErrTable(ibmspsscf.language,lib,pkg)
	ibmspsscf.generalErr <<-  getGeneralErr(ibmspsscf.language,lib,pkg)
	##this storages will be set in SetDataModel
	##and used in SetData
	outputStorages <<- NULL
	##ibmspsscf_version <<- readSpssVersion()
	##plugin_version <<- readPkgVersion(lib, pkg)
}

getPkgVersion <- function(lib, pkg)
{
	ver <- NULL
	pfile <- file.path(lib, pkg, "Meta", "package.rds")
	if(file.exists(pfile))
	{
		# Read version from package.rds file
		ver <- readRDS(pfile)$DESCRIPTION["Version"]
	}
	else
	{
		# Read version from DESCRIPTION file
		dfile <- file.path(lib, pkg, "DESCRIPTION")
		if(!file.exists(dfile))
		{
			stop(gettextf("There is no 'DESCRIPTION' file in '%s'",file.path(lib, pkg)))
		}
		ver <- read.dcf(dfile,"Version")[1, ]
	}
	ver
}

ibmspsscfpkg.GetPkgVersion <- function()
{
	plugin_version
}