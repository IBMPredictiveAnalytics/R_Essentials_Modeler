/************************************************************************
** IBM?? SPSS?? Modeler - Essentials for R
** (c) Copyright IBM Corp. 1989, 2012
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License version 2 as published by
** the Free Software Foundation.
** 
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
** 
** You should have received a copy of the GNU General Public License version 2
** along with this program; if not, write to the Free Software
** Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA.
************************************************************************/

#ifndef __R_PLUGIN_IBM_SPSS_H__
#define __R_PLUGIN_IBM_SPSS_H__

#ifdef _WINDOWS
  #ifdef RINVOKEMODELER_EXPORTS
  #define RINVOKEMODELER_API __declspec(dllexport)
  #else
  #define RINVOKEMODELER_API __declspec(dllimport)
  #endif
#else
  #define RINVOKEMODELER_API
#endif

#include <wchar.h>
#include <Rdefines.h>

extern "C" {
#include <R_ext/Rdynload.h>
	RINVOKEMODELER_API void ext_PostOutput(const char **text, int *length, int *errLevel);
	RINVOKEMODELER_API void ext_StartProcedure(int* errLevel);
	RINVOKEMODELER_API void ext_StopProcedure(int* errLevel);
	RINVOKEMODELER_API void ext_GetOutputDir(const char **dirPath, int* errLevel);
	RINVOKEMODELER_API void ext_GetModel(const char** modelName, int* errLevel);
	RINVOKEMODELER_API void ext_SendErrorCode(int* errCode, int* errLevel);
	RINVOKEMODELER_API void ext_IsDisplayTextOutput(int* isDisplayTextOutput, int* errLevel);
	RINVOKEMODELER_API void ext_GetSystemLocale(const char** locale, int* errLevel);


	RINVOKEMODELER_API void ext_GetFieldName(const char **fieldName, int* index, int* errLevel);
	RINVOKEMODELER_API void ext_GetFieldStorage(const char **fieldStorage, int* index, int* errLevel);
	RINVOKEMODELER_API void ext_GetFieldCount(int* count, int* errLevel);

	RINVOKEMODELER_API SEXP ext_GetFieldNames(SEXP fields, SEXP errLevel);
	RINVOKEMODELER_API SEXP ext_GetFieldStorages(SEXP fields, SEXP errLevel);
	RINVOKEMODELER_API SEXP ext_GetFieldMeasures(SEXP fields, SEXP errLevel);
	RINVOKEMODELER_API SEXP ext_GetFieldLabels(SEXP fields, SEXP errLevel);
	RINVOKEMODELER_API SEXP ext_GetFieldFormats(SEXP fields, SEXP errLevel);
	RINVOKEMODELER_API SEXP ext_GetFieldRoles(SEXP fields, SEXP errLevel);

	RINVOKEMODELER_API SEXP ext_GetMissingValues(SEXP field, SEXP errLevel);
	RINVOKEMODELER_API SEXP ext_GetFlagValues(SEXP field, SEXP errLevel);
	RINVOKEMODELER_API SEXP ext_GetValueLabels(SEXP field, SEXP errLevel);

	// data functions
	RINVOKEMODELER_API bool ext_NextRecord(int* errLevel);
	RINVOKEMODELER_API void ext_HasMoreData(bool* hasMoreData, int* errLevel);
	RINVOKEMODELER_API void ext_GetRecordCount(int* count, int* errLevel);
	RINVOKEMODELER_API SEXP ext_GetData(SEXP fieldIndexs, SEXP recordCount, SEXP missing, SEXP errLevel);
	RINVOKEMODELER_API SEXP ext_SetData(SEXP data, SEXP fieldStorages, SEXP errLevel);
	RINVOKEMODELER_API SEXP ext_SetDataModel(SEXP dataModel, SEXP errLevel);

	//===================R initialize=========================
	RINVOKEMODELER_API void R_init_RInvokeModeler(DllInfo *info);
	RINVOKEMODELER_API void R_unload_RInvokeModeler(DllInfo *info);  
}

#endif //__R_PLUGIN_IBM_SPSS_H__
