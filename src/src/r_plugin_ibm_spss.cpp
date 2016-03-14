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

#include <iostream>
#include <locale.h>
#include <string.h>
#ifdef _WINDOWS
  #include <windows.h>
#else
  #include <dlfcn.h>
  #include <stdlib.h>
#endif

#include "r_plugin_ibm_spss.h"
#include <assert.h>
#include <Rversion.h>

#ifdef _WINDOWS
  #define LIBHANDLE        HMODULE
  #define GETADDRESS       GetProcAddress
  #define LIBNAME          "plugin_callback.dll"
#else
  #define LIBHANDLE        void*
  #define GETADDRESS       dlsym
  #ifdef DARWIN
   #define LIBNAME         "plugin_callback.dylib"
  #else
   #define LIBNAME         "plugin_callback.so"
  #endif
#endif

#if R_VERSION < 0x2600
  typedef char* R_CHAR_STAR;
#else
  typedef const char* R_CHAR_STAR;
#endif

#define BATCH_SIZE 100

extern "C"{
    LIBHANDLE pLib = 0;
    const char libName[] = LIBNAME;

    const int LOAD_FAIL = 8011;
	const int EXCEPTION_THROWN = 9999;
    const int LOAD_SUCCESS = 0;

    // The types of arguments, required by .C function calling.
	static R_NativePrimitiveArgType StartProcedureArgs[1] = {INTSXP};
	static R_NativePrimitiveArgType StopProcedureArgs[1] = {INTSXP};
    static R_NativePrimitiveArgType PostOutputArgs[3] = {STRSXP,INTSXP,INTSXP};
	static R_NativePrimitiveArgType GetOutputDirArgs[2] = {STRSXP, INTSXP};
	static R_NativePrimitiveArgType GetFieldNameArgs[3] = {STRSXP, INTSXP, INTSXP};
	static R_NativePrimitiveArgType GetFieldStorageArgs[3] = {STRSXP, INTSXP, INTSXP};
	static R_NativePrimitiveArgType GetModelArgs[2] = {STRSXP, INTSXP};
	static R_NativePrimitiveArgType GetFieldCountArgs[2] = {INTSXP, INTSXP};
	static R_NativePrimitiveArgType SendErrorCodeArgs[2] = {INTSXP, INTSXP};
	static R_NativePrimitiveArgType IsDisplayTextOutputArgs[2] = {INTSXP, INTSXP};

	// set
	static R_NativePrimitiveArgType SetFieldNameArgs[3] = {STRSXP, INTSXP, INTSXP};
	static R_NativePrimitiveArgType SetFieldLabelArgs[3] = {STRSXP, INTSXP, INTSXP};
	static R_NativePrimitiveArgType SetFieldStorageArgs[3] = {STRSXP, INTSXP, INTSXP};
	static R_NativePrimitiveArgType SetFieldMeasureArgs[3] = {STRSXP, INTSXP, INTSXP};
	static R_NativePrimitiveArgType SetFieldFormatArgs[3] = {STRSXP, INTSXP, INTSXP};
	static R_NativePrimitiveArgType InsertFieldArgs[3] = {STRSXP, INTSXP, INTSXP};

	// data function
	static R_NativePrimitiveArgType NextRecordArgs[1] = {INTSXP};
	static R_NativePrimitiveArgType HasMoreDataArgs[2] = {LGLSXP, INTSXP};
	static R_NativePrimitiveArgType GetRecordCountArgs[2] = {INTSXP, INTSXP};


    
    // The method of using .C from R
    static const R_CMethodDef cMethods[] = {
        {"ext_StartProcedure",(DL_FUNC)&ext_StartProcedure, 1, StartProcedureArgs},
		{"ext_StopProcedure",(DL_FUNC)&ext_StopProcedure, 1, StopProcedureArgs},
		{"ext_GetOutputDir",(DL_FUNC)&ext_GetOutputDir, 2, GetOutputDirArgs},
		{"ext_GetFieldName",(DL_FUNC)&ext_GetFieldName, 3, GetFieldNameArgs},
		{"ext_GetFieldStorage",(DL_FUNC)&ext_GetFieldStorage, 3, GetFieldStorageArgs},
		{"ext_GetModel",(DL_FUNC)&ext_GetModel, 2, GetModelArgs},
		{"ext_GetFieldCount",(DL_FUNC)&ext_GetFieldCount, 2, GetFieldCountArgs},

		{"ext_NextRecord",(DL_FUNC)&ext_NextRecord, 1, NextRecordArgs},
		{"ext_HasMoreData",(DL_FUNC)&ext_HasMoreData, 2, HasMoreDataArgs},
		{"ext_GetRecordCount",(DL_FUNC)&ext_GetRecordCount, 2, GetRecordCountArgs},

		{"ext_SendErrorCode",(DL_FUNC)&ext_SendErrorCode, 2, SendErrorCodeArgs},
		{"ext_IsDisplayTextOutput",(DL_FUNC)&ext_IsDisplayTextOutput, 2, IsDisplayTextOutputArgs},
        {0,0,0}
    };

    // The method of using .Call from R. 
    // Notes: .C should be used when it is good enough.
    // However, .C can pass simple argument only, for example, int, double, char*.
    // That is not good enough for below functions.
    static const R_CallMethodDef callMethods[] = {
        //{"ext_GetHandleList",(DL_FUNC)&ext_GetHandleList,1},   // returns vector.
		{"ext_GetFieldNames",(DL_FUNC)&ext_GetFieldNames, 2},
		{"ext_GetFieldStorages",(DL_FUNC)&ext_GetFieldStorages, 2},
		{"ext_GetFieldMeasures",(DL_FUNC)&ext_GetFieldMeasures, 2},
		{"ext_GetFieldLabels",(DL_FUNC)&ext_GetFieldLabels, 2},
		{"ext_GetFieldFormats",(DL_FUNC)&ext_GetFieldFormats, 2},
		{"ext_GetFieldRoles", (DL_FUNC)&ext_GetFieldRoles, 2},
		{"ext_GetMissingValues",(DL_FUNC)&ext_GetMissingValues, 2},
		{"ext_GetFlagValues",(DL_FUNC)&ext_GetFlagValues, 2},
		{"ext_GetValueLabels",(DL_FUNC)&ext_GetValueLabels,2},
		{"ext_GetData",(DL_FUNC)&ext_GetData, 4},
		{"ext_SetDataModel", (DL_FUNC)&ext_SetDataModel, 2},
		{"ext_SetData",(DL_FUNC)&ext_SetData,3},
        {0,0,0}
    };

    // The function pointer
    static int (*PostSpssOutput)(const char **text, int length)  = 0;
	static int (*StartProcedure)() = 0;
    static int (*StopProcedure)() = 0;
	static const char* (*GetOutputDir)(int& errLevel) = 0;
	static int (*GetFieldCount)(int& errLevel) = 0;
	static const char* (*GetFieldName)(int index, int& errLevel) = 0;
	static const char* (*GetFieldStorage)(int index, int& errLevel) = 0;
	static const char* (*GetFieldMeasure)(int index, int& errLevel) = 0;
	static const char* (*GetFieldLabel)(int index, int& errLevel) = 0;
	static const char* (*GetFieldFormat)(int index, int& errLevel) = 0;
	static const char* (*GetFieldRole)(int index, int& errLevel) = 0;
	static const char** (*GetStringMissingValues)(int index, int& count, int& errLevel) = 0;
	static const char** (*GetFlagValues)(int index, int& count, int& errLevel) = 0;
	

	// the return is value labels
	static const char** (*GetValueLabels)(int index, char*** values, int& numOfValues, int& errLevel) = 0;
	
	static const char* (*GetModel) (int& errLevel) = 0;
	
	static int (*InsertField)(char** fieldInfo, int index) = 0;
	static int (*SetDataModel) () = 0;

	static bool (*NextRecord)(int& errLevel) = 0;
	static bool (*HasMoreData)(int& errLevel) = 0;
	static int (*GetRecordCount)(int& errLevel) = 0;
	static int (*GetData)(int* fieldIndexs, int fieldCount, int recordCount) = 0;
	static int (*GetIntegerValue)(int fieldIndex, int& isMissing, int& errLevel) = 0;
	static char* (*GetStringValue)(int fieldIndex, int& isMissing, int& errLevel) = 0;
	static double (*GetRealValue)(int fieldIndex, int& isMissing, int& errLevel) = 0;
	static int (*SetData)(double** dValue, int** iValue, char*** sValue, bool** missing, int recordCount)	= 0;
	static int (*SendErrorCode)(int errorCode) = 0;
	static bool (*IsDisplayTextOutput)(int& errLevel) = 0;
	static const char* (*GetSystemLocale)(int& errLevel) = 0;

    //Initialize the function pointer
    void InitializeFP()
    {
		StartProcedure = (int (*)())GETADDRESS(pLib, "StartProcedure");
		StopProcedure =  (int (*)())GETADDRESS(pLib, "StopProcedure");
        PostSpssOutput = (int (*)(const char**, int ))GETADDRESS(pLib, "PostSpssOutput");
		GetOutputDir = (const char*(*)(int&))GETADDRESS(pLib, "GetOutputDir");
		GetFieldCount = (int (*)(int&))GETADDRESS(pLib, "GetFieldCount");
		GetFieldName = (const char*(*)(int, int&))GETADDRESS(pLib, "GetFieldName");
		GetFieldStorage = (const char*(*)(int, int&))GETADDRESS(pLib, "GetFieldStorage");
		GetFieldMeasure = (const char*(*)(int, int&))GETADDRESS(pLib, "GetFieldMeasure");
		GetFieldLabel = (const char*(*)(int, int&))GETADDRESS(pLib, "GetFieldLabel");
		GetFieldFormat = (const char*(*)(int, int&))GETADDRESS(pLib, "GetFieldFormat");
		GetFieldRole = (const char*(*)(int, int&))GETADDRESS(pLib, "GetFieldRole");
		GetStringMissingValues = (const char**(*)(int, int&, int&))GETADDRESS(pLib, "GetStringMissingValues");
		GetFlagValues = (const char**(*)(int, int&, int&))GETADDRESS(pLib, "GetFlagValues");
		GetValueLabels = (const char**(*)(int, char***, int&, int&))GETADDRESS(pLib, "GetValueLabels");

		GetModel = (const char*(*)(int&))GETADDRESS(pLib, "GetModel");

		InsertField = (int (*)(char**, int))GETADDRESS(pLib, "InsertField");
		SetDataModel = (int (*)())GETADDRESS(pLib, "SetDataModel");

		NextRecord = (bool (*)(int&))GETADDRESS(pLib, "NextRecord");
		HasMoreData = (bool (*)(int&))GETADDRESS(pLib, "HasMoreData");
		GetRecordCount = (int (*)(int&))GETADDRESS(pLib, "GetRecordCount");
		GetData = (int (*)(int*, int, int))GETADDRESS(pLib, "GetData");
		GetIntegerValue = (int (*)(int, int&, int&))GETADDRESS(pLib, "GetIntegerValue");
		GetStringValue = (char* (*)(int, int&, int&))GETADDRESS(pLib, "GetStringValue");
		GetRealValue = (double (*)(int, int&, int&))GETADDRESS(pLib, "GetRealValue");
		SetData = (int (*)(double** dValue, int** iValue,  char*** sValue, bool** missing, 
			int recordCount))GETADDRESS(pLib, "SetData");
		SendErrorCode = (int(*)(int))GETADDRESS(pLib, "SendErrorCode");
		IsDisplayTextOutput = (bool(*)(int&))GETADDRESS(pLib, "IsDisplayTextOutput");
		GetSystemLocale = (const char*(*)(int&))GETADDRESS(pLib, "GetSystemLocale");
    }

    //load r_plugin_callback.dll
	int LoadLib()
	{
		// The object holding the new PATH env string cannot go out of scope.
		const char* extPath = NULL;
		char* libPath = NULL;
		if (getenv("EXT_PATH")) {
			extPath = getenv("EXT_PATH");
		}
		if (extPath == NULL) {
			size_t libLen = strlen(libName) + 1;
			libPath = new char[libLen];
			memset(libPath,'\0',libLen);
			strcpy(libPath,libName);
		}
		else {
#ifdef _WINDOWS
			int libLen = strlen(extPath) + strlen(libName) + 2;
			libPath = new char[libLen];
			memset(libPath, '\0', libLen);
			strcpy(libPath, extPath);
			if(extPath[strlen(extPath) - 1] != '\\') {
				strcat(libPath, "\\");
			}
			strcat(libPath,libName);
#else
			int libLen = strlen(extPath) + strlen(libName) + 20;
			libPath = new char[libLen];
			memset(libPath, '\0', libLen);
			strcpy(libPath, extPath);
			strcat(libPath, "/");
			strcat(libPath, libName);
#endif
		}
		if(pLib == NULL) {
#ifdef _WINDOWS
			pLib = GetModuleHandle(libPath);
			if(pLib == NULL) {
				pLib = LoadLibraryEx(libPath,NULL,LOAD_WITH_ALTERED_SEARCH_PATH);
			}
#else
			int mode;
#ifdef DARWIN
			mode = RTLD_LOCAL;     
#else
			mode = RTLD_NOW | RTLD_GLOBAL;
#endif
			pLib = dlopen(libPath,mode);
#endif
		}
		if (pLib) {
			InitializeFP();
		}
		else {//load failure
#ifndef _WINDOWS
			char *perr = dlerror();
			if(perr) {
				Rprintf("dlopen fails with error: %s.\n",perr);
			}
#endif
			delete []libPath;
			return LOAD_FAIL;
		}
		delete []libPath;
		return LOAD_SUCCESS;
	}

    void FreeLib()
    {
#ifdef _WINDOWS
        FreeLibrary(pLib);
#else
        dlclose(pLib);
#endif
        pLib                    = 0;
        PostSpssOutput          = 0;
		StartProcedure			= 0;
		StopProcedure			= 0;
		GetOutputDir            = 0;
		GetModel				= 0;
		GetFieldCount			= 0;
		GetFieldName			= 0;
		GetFieldStorage			= 0;
		GetFieldMeasure		    = 0;
		GetFieldLabel    	    = 0;
		GetFieldFormat   	    = 0;
		GetStringMissingValues  = 0;
		GetFlagValues  = 0;
		GetValueLabels		    = 0;
		InsertField				= 0;
		NextRecord				= 0;
		HasMoreData				= 0;
		GetRecordCount			= 0;
		GetData					= 0;
		SetData					= 0;
	}

	void ext_StartProcedure(int* errLevel)
    {
		try {
			*errLevel = LoadLib();
			if(*errLevel == LOAD_SUCCESS) {
				*errLevel = StartProcedure();
			}
		}
		catch(...) {
			*errLevel = EXCEPTION_THROWN;
		}
    }

	void ext_StopProcedure(int* errLevel)
    {
		try {
			*errLevel = LoadLib();
			if(*errLevel == LOAD_SUCCESS) {
				*errLevel = StopProcedure();
			}
		}
		catch(...) {
			*errLevel = EXCEPTION_THROWN;
		}
    }

	void ext_SendErrorCode(int* errCode, int* errLevel)
	{
		try {
			*errLevel = LoadLib();
			if(*errLevel == LOAD_SUCCESS) {
				*errLevel = SendErrorCode(*errCode);
			}
		}
		catch(...) {
			*errLevel = EXCEPTION_THROWN;
		}
	}

	void ext_IsDisplayTextOutput(int* is, int* errLevel)
	{
		try {
			*errLevel = LoadLib();
			if(*errLevel == LOAD_SUCCESS) {
				*is = IsDisplayTextOutput(*errLevel);
			}
		}
		catch(...) {
			*errLevel = EXCEPTION_THROWN;
		}
	}

	void ext_GetSystemLocale(const char** locale, int* errLevel)
	{
		try {
			*errLevel = LoadLib();
			if(*errLevel == LOAD_SUCCESS) {
				*locale = GetSystemLocale(*errLevel);
			}
		}
		catch(...) {
			*errLevel = EXCEPTION_THROWN;
		}
	}

    void ext_PostOutput(const char** text, int* length, int* errLevel)
    {
		try {
			*errLevel = LoadLib();
			//For Chinese character, R will send wrong character length.
			//Re-calculate here.
			if( LOAD_SUCCESS == *errLevel ){
				*errLevel = PostSpssOutput(text, *length);
			}
		}
		catch(...) {
			*errLevel = EXCEPTION_THROWN;
		}
    }

	void ext_GetOutputDir(const char** dirPath, int* errLevel)
	{
		try {
			*errLevel = LoadLib();
			 if( LOAD_SUCCESS == *errLevel ){
				*dirPath = GetOutputDir(*errLevel);
			}
		}
		catch(...) {
			*errLevel = EXCEPTION_THROWN;
		}
	}

	void ext_GetFieldName(const char** fieldName, int* index, int* errLevel)
	{
		try {
			*errLevel = LoadLib();
			 if( LOAD_SUCCESS == *errLevel ){
				*fieldName = GetFieldName(*index, *errLevel);
			}
		}
		catch(...) {
			*errLevel = EXCEPTION_THROWN;
		}
	}

	void ext_GetFieldStorage(const char** fieldStorage, int* index, int* errLevel)
	{
		try {
			*errLevel = LoadLib();
			 if( LOAD_SUCCESS == *errLevel ){
				*fieldStorage = GetFieldStorage(*index, *errLevel);
			}
		}
		catch(...) {
			*errLevel = EXCEPTION_THROWN;
		}
	}

	void ext_GetFieldCount(int* count, int* errLevel)
	{
		try {
			*errLevel = LoadLib();
			 if( LOAD_SUCCESS == *errLevel ){
				*count = GetFieldCount(*errLevel);
			}
		}
		catch(...) {
			*errLevel = EXCEPTION_THROWN;
		}
	}

	SEXP ext_GetFieldNames(SEXP fields, SEXP errLevel)
    {
		try {
			int nvar = LENGTH(fields);
			int *cErr = INTEGER(errLevel);

			SEXP ans = PROTECT(allocVector(STRSXP, nvar+1));//The last element is for errLevel.
	    
			int i, fieldIndex;
			for (i = 0; i < nvar && 0==*cErr; ++i) {
				fieldIndex = INTEGER_VALUE(VECTOR_ELT(fields,i));
				const char* tmpStr = GetFieldName(fieldIndex,*cErr);
				/*
				if(*cErr == 0)            
				  SET_STRING_ELT(ans, i, IsUTF8mode()?mkCharCE(tmpStr,CE_UTF8):mkChar(tmpStr));
				else
				  break;
				  */
				SET_STRING_ELT(ans, i, mkCharCE(tmpStr,CE_UTF8));
			}

			SET_STRING_ELT(ans, nvar, asChar(errLevel));
			UNPROTECT(1);
			return ans;
		}
		catch(...) {
			int *cErr = INTEGER(errLevel);
			*cErr = EXCEPTION_THROWN;
			SEXP ans = PROTECT(allocVector(STRSXP, 1));
			SET_STRING_ELT(ans, 0, asChar(errLevel));
			return ans;
		}
    }

	SEXP ext_GetFieldStorages(SEXP fields, SEXP errLevel)
    {
		try {
			int nvar = LENGTH(fields);
			int *cErr = INTEGER(errLevel);

			SEXP ans = PROTECT(allocVector(STRSXP, nvar+1));//The last element is for errLevel.
	    
			int i, fieldIndex;
			for (i = 0; i < nvar && 0==*cErr; ++i) {
				fieldIndex = INTEGER_VALUE(VECTOR_ELT(fields,i));
				const char* tmpStr = GetFieldStorage(fieldIndex,*cErr);
				/*
				if(*cErr == 0)            
				  SET_STRING_ELT(ans, i, IsUTF8mode()?mkCharCE(tmpStr,CE_UTF8):mkChar(tmpStr));
				else
				  break;
				  */
				SET_STRING_ELT(ans, i, mkCharCE(tmpStr,CE_UTF8));
			}

			SET_STRING_ELT(ans, nvar, asChar(errLevel));
			UNPROTECT(1);
			return ans;
		}
		catch(...) {
			int *cErr = INTEGER(errLevel);
			*cErr = EXCEPTION_THROWN;
			SEXP ans = PROTECT(allocVector(STRSXP, 1));
			SET_STRING_ELT(ans, 0, asChar(errLevel));
			return ans;
		}
    }

	SEXP ext_GetFieldMeasures(SEXP fields, SEXP errLevel)
    {
		try {
			int nvar = LENGTH(fields);
			int *cErr = INTEGER(errLevel);

			SEXP ans = PROTECT(allocVector(STRSXP, nvar+1));//The last element is for errLevel.
	    
			int i, fieldIndex;
			for (i = 0; i < nvar && 0==*cErr; ++i) {
				fieldIndex = INTEGER_VALUE(VECTOR_ELT(fields,i));
				const char* tmpStr = GetFieldMeasure(fieldIndex,*cErr);
				/*
				if(*cErr == 0)            
				  SET_STRING_ELT(ans, i, IsUTF8mode()?mkCharCE(tmpStr,CE_UTF8):mkChar(tmpStr));
				else
				  break;
				  */
				SET_STRING_ELT(ans, i, mkCharCE(tmpStr,CE_UTF8));
			}

			SET_STRING_ELT(ans, nvar, asChar(errLevel));
			UNPROTECT(1);
			return ans;
		}
		catch(...) {
			int *cErr = INTEGER(errLevel);
			*cErr = EXCEPTION_THROWN;
			SEXP ans = PROTECT(allocVector(STRSXP, 1));
			SET_STRING_ELT(ans, 0, asChar(errLevel));
			return ans;
		}
    }

	SEXP ext_GetFieldLabels(SEXP fields, SEXP errLevel)
    {
		try {
			int nvar = LENGTH(fields);
			int *cErr = INTEGER(errLevel);

			SEXP ans = PROTECT(allocVector(STRSXP, nvar+1));//The last element is for errLevel.
	    
			int i, fieldIndex;
			for (i = 0; i < nvar && 0==*cErr; ++i) {
				fieldIndex = INTEGER_VALUE(VECTOR_ELT(fields,i));
				const char* tmpStr = GetFieldLabel(fieldIndex,*cErr);
				/*
				if(*cErr == 0)            
				  SET_STRING_ELT(ans, i, IsUTF8mode()?mkCharCE(tmpStr,CE_UTF8):mkChar(tmpStr));
				else
				  break;
				  */
				SET_STRING_ELT(ans, i, mkCharCE(tmpStr,CE_UTF8));
			}
			SET_STRING_ELT(ans, nvar, asChar(errLevel));
			UNPROTECT(1);
			return ans;
		}
		catch(...) {
			int *cErr = INTEGER(errLevel);
			*cErr = EXCEPTION_THROWN;
			SEXP ans = PROTECT(allocVector(STRSXP, 1));
			SET_STRING_ELT(ans, 0, asChar(errLevel));
			return ans;
		}
    }

	SEXP ext_GetFieldFormats(SEXP fields, SEXP errLevel)
    {
		try {
			int nvar = LENGTH(fields);
			int *cErr = INTEGER(errLevel);

			SEXP ans = PROTECT(allocVector(STRSXP, nvar+1));//The last element is for errLevel.
	    
			int i, fieldIndex;
			for (i = 0; i < nvar && 0==*cErr; ++i) {
				fieldIndex = INTEGER_VALUE(VECTOR_ELT(fields,i));
				const char* tmpStr = GetFieldFormat(fieldIndex,*cErr);
				/*
				if(*cErr == 0)            
				  SET_STRING_ELT(ans, i, IsUTF8mode()?mkCharCE(tmpStr,CE_UTF8):mkChar(tmpStr));
				else
				  break;
				  */
				SET_STRING_ELT(ans, i, mkCharCE(tmpStr,CE_UTF8));
			}
			SET_STRING_ELT(ans, nvar, asChar(errLevel));
			UNPROTECT(1);
			return ans;
		}
		catch(...) {
			int *cErr = INTEGER(errLevel);
			*cErr = EXCEPTION_THROWN;
			SEXP ans = PROTECT(allocVector(STRSXP, 1));
			SET_STRING_ELT(ans, 0, asChar(errLevel));
			return ans;
		}
    }

	SEXP ext_GetFieldRoles(SEXP fields, SEXP errLevel)
	{
		try {
			int nvar = LENGTH(fields);
			int *cErr = INTEGER(errLevel);

			SEXP ans = PROTECT(allocVector(STRSXP, nvar+1));//The last element is for errLevel.
	    
			int i, fieldIndex;
			for (i = 0; i < nvar && 0==*cErr; ++i) {
				fieldIndex = INTEGER_VALUE(VECTOR_ELT(fields,i));
				const char* tmpStr = GetFieldRole(fieldIndex,*cErr);
				/*
				if(*cErr == 0)            
				  SET_STRING_ELT(ans, i, IsUTF8mode()?mkCharCE(tmpStr,CE_UTF8):mkChar(tmpStr));
				else
				  break;
				  */
				SET_STRING_ELT(ans, i, mkCharCE(tmpStr,CE_UTF8));
			}
			SET_STRING_ELT(ans, nvar, asChar(errLevel));
			UNPROTECT(1);
			return ans;
		}
		catch(...) {
			int *cErr = INTEGER(errLevel);
			*cErr = EXCEPTION_THROWN;
			SEXP ans = PROTECT(allocVector(STRSXP, 1));
			SET_STRING_ELT(ans, 0, asChar(errLevel));
			return ans;
		}
	}

	SEXP ext_GetMissingValues(SEXP field, SEXP errLevel)
    {
		try {
			int nvar = LENGTH(field);
			int *index = INTEGER(field);
			int *cErr = INTEGER(errLevel);
			int count;

			const char** sMissValues;
			SEXP ans;

			sMissValues = GetStringMissingValues(*index, count, *cErr);
			ans = PROTECT(allocVector(STRSXP, count));
				
			int i;
			for (i = 0; i < count && 0==*cErr; ++i) {
				const char* tmpStr = sMissValues[i];
				SET_STRING_ELT(ans, i, mkCharCE(tmpStr,CE_UTF8));
			}

			SEXP rErr;
			PROTECT(rErr = allocVector(INTSXP,1));
			INTEGER(rErr)[0] = *cErr;

			SEXP result;
			PROTECT(result = allocVector(VECSXP, 2));
			
			SET_VECTOR_ELT(result,0,ans); 
			SET_VECTOR_ELT(result,1,rErr);

			UNPROTECT(3);
			return result;
		}
		catch(...) {
			int *cErr = INTEGER(errLevel);
			*cErr = EXCEPTION_THROWN;
			SEXP ans = PROTECT(allocVector(STRSXP, 1));
			SET_STRING_ELT(ans, 0, asChar(errLevel));
			return ans;
		}
    }

	SEXP ext_GetFlagValues(SEXP field, SEXP errLevel)
    {
		try {
			int nvar = LENGTH(field);
			int *index = INTEGER(field);
			int *cErr = INTEGER(errLevel);
			int count;

			const char** sFlagValues;
			SEXP ans;

			sFlagValues = GetFlagValues(*index, count, *cErr);
			ans = PROTECT(allocVector(STRSXP, count));
				
			int i;
			for (i = 0; i < count && 0==*cErr; ++i) {
				const char* tmpStr = sFlagValues[i];
				SET_STRING_ELT(ans, i, mkCharCE(tmpStr,CE_UTF8));
			}

			SEXP rErr;
			PROTECT(rErr = allocVector(INTSXP,1));
			INTEGER(rErr)[0] = *cErr;

			SEXP result;
			PROTECT(result = allocVector(VECSXP, 2));
			
			SET_VECTOR_ELT(result,0,ans); 
			SET_VECTOR_ELT(result,1,rErr);

			UNPROTECT(3);
			return result;
		}
		catch(...) {
			int *cErr = INTEGER(errLevel);
			*cErr = EXCEPTION_THROWN;
			SEXP ans = PROTECT(allocVector(STRSXP, 1));
			SET_STRING_ELT(ans, 0, asChar(errLevel));
			return ans;
		}
    }


	SEXP ext_GetValueLabels(SEXP field, SEXP errLevel)
	{
		try {
			int nvar = LENGTH(field);
			int *index = INTEGER(field);
			int *cErr = INTEGER(errLevel);
			double* dValues = NULL;
			int* iValues = NULL;
			char** sValues = NULL;
			const char** labels = NULL;
			int numOfValues;
	  
			SEXP result = R_NilValue;
			*cErr = LoadLib();

			
			if(LOAD_SUCCESS != *cErr) {
				return result;
			}

			labels = GetValueLabels(*index, &sValues,numOfValues, *cErr);
			
			SEXP rValues;
			PROTECT(rValues = allocVector(STRSXP, numOfValues));
			if ( (0 == *cErr || 403 == *cErr || 404 == *cErr) && numOfValues > 0 ){
				for( int i=0; i<numOfValues; ++i ){
					SET_STRING_ELT(rValues, i, mkCharCE(sValues[i], CE_UTF8));
				}
			}

			SEXP rLabels;
			PROTECT(rLabels = allocVector(STRSXP, numOfValues));
			if ( (0 == *cErr || 403 == *cErr) && numOfValues > 0){
				for( int i=0; i<numOfValues; ++i ){
					SET_STRING_ELT(rLabels, i, mkCharCE(labels[i], CE_UTF8));
				}
			}

			SEXP rErr;
			PROTECT(rErr = allocVector(INTSXP,1));
			INTEGER(rErr)[0] = *cErr;
			PROTECT(result = allocVector(VECSXP, 3));
			SET_VECTOR_ELT(result,0,rValues); 
			SET_VECTOR_ELT(result,1,rLabels);
			SET_VECTOR_ELT(result,2,rErr);           
			UNPROTECT(4);   
			assert(0 == *cErr);

			return result;
		}
		catch(...) {
			int *cErr = INTEGER(errLevel);
			*cErr = EXCEPTION_THROWN;
			SEXP ans = PROTECT(allocVector(STRSXP, 1));
			SET_STRING_ELT(ans, 0, asChar(errLevel));
			return ans;
		}
	}

	void ext_GetModel(const char** modelName, int* errLevel)
	{
		try {
			*errLevel = LoadLib();
			 if( LOAD_SUCCESS == *errLevel ){
				*modelName = GetModel(*errLevel);
			}
		}
		catch(...) {
			*errLevel = EXCEPTION_THROWN;
		}
	}

	void ext_GetRecordCount(int* count, int* errLevel)
	{
		try {
			*errLevel = LoadLib();
			if( LOAD_SUCCESS == *errLevel ){
				*count = GetRecordCount(*errLevel);
			}
		}
		catch(...) {
			*errLevel = EXCEPTION_THROWN;
		}
	}

	RINVOKEMODELER_API bool ext_NextRecord(int* errLevel)
	{
		try {
			*errLevel = LoadLib();
			if( LOAD_SUCCESS == *errLevel ){
				return NextRecord(*errLevel);
			}
			else {
				return false;
			}
		}
		catch(...) {
			*errLevel = EXCEPTION_THROWN;
			return false;
		}
	}

	RINVOKEMODELER_API void ext_HasMoreData(bool* hasMoreData, int* errLevel)
	{
		try {
			*errLevel = LoadLib();
			if( LOAD_SUCCESS == *errLevel ){
				*hasMoreData = HasMoreData(*errLevel);
			}
		}
		catch(...) {
			*errLevel = EXCEPTION_THROWN;
		}
	}

	RINVOKEMODELER_API SEXP ext_GetData(SEXP fieldIndexs, SEXP count, SEXP missing, SEXP errLevel)
	{
		try {
			int fieldCount = LENGTH(fieldIndexs);
			int* recordCount = INTEGER(count);
			int* cErr = INTEGER(errLevel);

			// 0 means converting all missing to Na
			// 1 means converting string to Na, numeric to NaN
			// 2 means do nothing

			int missingFlag = INTEGER_VALUE(missing);

			
			SEXP ans = PROTECT(allocVector(VECSXP, fieldCount+1));//The last element is for errLevel.
			SEXP ans_names = PROTECT(allocVector(STRSXP, fieldCount));
	     

			int* indexs = new int[fieldCount];
			const char** fieldStorages = new const char*[fieldCount];
			int fieldIndex;

			for(int i = 0; i < fieldCount; ++i)
			{
				fieldIndex = INTEGER_VALUE(VECTOR_ELT(fieldIndexs,i));
				indexs[i] = fieldIndex;
			}
			GetData(indexs,fieldCount, *recordCount);

			for (int i = 0; i < fieldCount; ++i) {
				fieldStorages[i] = GetFieldStorage(indexs[i],*cErr);
				if(*cErr != 0) {
					break;
				}
				const char* tmpStr = GetFieldName(indexs[i],*cErr);
				if(*cErr != 0) {
					break;
				}
				SET_STRING_ELT(ans_names, i, mkCharCE(tmpStr,CE_UTF8));
				if (strcmp("real", fieldStorages[i])==0 || strcmp("date",fieldStorages[i])==0 || strcmp("time",fieldStorages[i]) == 0 || strcmp("timestamp",fieldStorages[i]) == 0) {
					SET_VECTOR_ELT(ans, i, allocVector(REALSXP, *recordCount));
				} else if (strcmp("integer", fieldStorages[i]) == 0) {
					SET_VECTOR_ELT(ans, i, allocVector(INTSXP, *recordCount));
				}else {
					SET_VECTOR_ELT(ans, i, allocVector(STRSXP, *recordCount));
				}
			}

			int nRecords = 0;
			int iValue;
			double dValue;
			char* sValue;
			int isMissing = 0;
			while(*cErr == 0 && nRecords < *recordCount && true == NextRecord(*cErr)) { // records
				for(int j = 0; j < fieldCount; ++j) { // fields
					isMissing = 0;
					if( strcmp("string", fieldStorages[j]) == 0) {
						sValue = GetStringValue(indexs[j], isMissing, *cErr);
						if(isMissing==1 || (isMissing && missingFlag != 2)) {
							SET_STRING_ELT(VECTOR_ELT(ans, j), nRecords, R_NaString);
						} else {
							SET_STRING_ELT(VECTOR_ELT(ans, j), nRecords, mkCharCE(sValue, CE_UTF8));
						}
					} else if(strcmp("integer", fieldStorages[j]) == 0) {
						iValue = GetIntegerValue(indexs[j], isMissing, *cErr);
						// missingFlag 0 means converting all missing to Na
						// 1 means converting string to Na, numeric to NaN
						if(isMissing==1 || (isMissing && missingFlag != 2)) {
							if(missingFlag == 1) {
								iValue = R_NaN;
							} else {
								iValue = R_NaInt;
							}
						} 
						INTEGER(VECTOR_ELT(ans, j))[nRecords] = iValue;
					} else {
						dValue = GetRealValue(indexs[j], isMissing, *cErr);
						if(isMissing==1 || (isMissing && missingFlag != 2)) {
							if(missingFlag == 1) {
								dValue = R_NaN;
							} else {
								dValue = R_NaReal;
							}
						} 
						REAL(VECTOR_ELT(ans, j))[nRecords] = dValue;
					}
				} //fields
				++nRecords;
			} // records
			if (*recordCount != nRecords) {
				for (int i = 0; i < fieldCount; ++i) {
					SEXP elt = VECTOR_ELT(ans, i);
					elt = lengthgets(elt, nRecords);
					SET_VECTOR_ELT(ans, i, elt);
				}
			}
			
			SEXP rErr;
			PROTECT(rErr = allocVector(INTSXP,1));
			INTEGER(rErr)[0] = *cErr;
			SET_VECTOR_ELT(ans, fieldCount, rErr);
			UNPROTECT(3);
			return ans;
		}
		catch(...) {
			int *cErr = INTEGER(errLevel);
			*cErr = EXCEPTION_THROWN;
			SEXP ans = PROTECT(allocVector(STRSXP, 1));
			SET_STRING_ELT(ans, 0, asChar(errLevel));
			return ans;
		}
	}

	// this function is used in the SetDataModel
	SEXP ext_SetDataModel(SEXP dataModel, SEXP errLevel)
	{
		try {
			int *cErr = INTEGER(errLevel);
			*cErr = LoadLib();
			SEXP elts;
			if(LOAD_SUCCESS == *cErr) {
				int fieldCount = LENGTH(dataModel);
				for(int index = 0; index < fieldCount; index++) {
					elts = VECTOR_ELT(dataModel, index);
					char** attrs = new char*[6];
					for(int j = 0; j < 6; j++) {
						//SET_STRING_ELT(ans, i, mkCharCE(tmpStr,CE_UTF8));
						R_CHAR_STAR sVal = CHAR(STRING_ELT(elts, j));
						char* value = new char[strlen(sVal) + 1];
						memset(value, '\0', strlen(sVal) + 1);
						strcpy(value, sVal);
						attrs[j] = value;
					}
					*cErr = InsertField(attrs, index);
					delete[] attrs;
				}
				*cErr = SetDataModel();
			}
			return errLevel;
		}
		catch(...) {
			int *cErr = INTEGER(errLevel);
			*cErr = EXCEPTION_THROWN;
			SEXP ans = PROTECT(allocVector(STRSXP, 1));
			SET_STRING_ELT(ans, 0, asChar(errLevel));
			return ans;
		}
	}

	SEXP ext_SetData(SEXP data, SEXP fStorages, SEXP errLevel)
	{
		try {
			int *cErr = INTEGER(errLevel);
			SEXP elts;
			int fieldNum = LENGTH(data);
			const char** fieldStorages = new const char*[fieldNum];
			int iCount = 0;
			int dCount = 0;
			int sCount = 0;
			for(int  i= 0; i < fieldNum; ++i) {
				fieldStorages[i] = CHARACTER_VALUE(VECTOR_ELT(fStorages,i));
				if(strcmp("real", fieldStorages[i])==0 || 
						strcmp("date",fieldStorages[i])==0 || 
						strcmp("timestamp",fieldStorages[i]) == 0 ||
						strcmp("time",fieldStorages[i]) == 0) {
					dCount++;
				}
				else if(strcmp("integer", fieldStorages[i]) == 0) {
					iCount++;
				}
				else {
					sCount++;
				}
			}
			char*** sValue = new char**[BATCH_SIZE];
			double** dValue = new double*[BATCH_SIZE];
			int** iValue = new int*[BATCH_SIZE];
			bool** missing = new bool*[BATCH_SIZE];
	        
			int recordNum = LENGTH(VECTOR_ELT(data, 0));
			for(int i = 0; i < recordNum; ++i)
			{
				int iIndex = 0;
				int dIndex = 0;
				int sIndex = 0;
				sValue[i % BATCH_SIZE] = new char*[sCount];
				dValue[i % BATCH_SIZE] = new double[dCount];
				iValue[i % BATCH_SIZE] = new int[iCount];
				missing[i % BATCH_SIZE] = new bool[fieldNum];
				for(int j=0; j<fieldNum; ++j) {
					if(strcmp("real", fieldStorages[j])==0 || 
						strcmp("date",fieldStorages[j])==0 || 
						strcmp("timestamp",fieldStorages[j]) == 0 ||
						strcmp("time",fieldStorages[j]) == 0) {
						elts = VECTOR_ELT(data, j);
						double v = REAL(elts)[i];
						dValue[i % BATCH_SIZE][dIndex++] = v;
						if(!isNumeric(elts)) {
							missing[i % BATCH_SIZE][j] = true;
						}
						else {
							missing[i % BATCH_SIZE][j] = !R_finite(v);
						}
					}
					else if(strcmp("integer", fieldStorages[j]) == 0) 
					{
						elts = VECTOR_ELT(data, j);
						int v = INTEGER(elts)[i];
						iValue[i % BATCH_SIZE][iIndex++] = v;
						if(!isNumeric(elts)) {
							missing[i % BATCH_SIZE][j] = true;
						}
						else {	
							missing[i % BATCH_SIZE][j] = (R_NaInt == v);
						}
					}
					else
					{
						elts = VECTOR_ELT(data, j);
						R_CHAR_STAR sVal = CHAR(STRING_ELT(elts, i));
						sValue[i % BATCH_SIZE][sIndex] = new char[strlen(sVal)+1];
						memset(sValue[i % BATCH_SIZE][sIndex], '\0', strlen(sVal)+1);
						strcpy(sValue[i % BATCH_SIZE][sIndex], sVal);
						if(!IS_CHARACTER(elts)) {
							missing[i % BATCH_SIZE][j] = true;
						}                   
						else {
							missing[i % BATCH_SIZE][j] = (strcmp(sVal,"NA") == 0);
						}
						sIndex++;
					}
				}
				if((i + 1) % BATCH_SIZE == 0) {
					*cErr = SetData(dValue, iValue, sValue, missing, BATCH_SIZE);
					for(int j = 0; j < BATCH_SIZE; j++) {
						delete[] dValue[j];
						delete[] iValue[j];
						for(int k = 0; k < sCount; k++) {
							delete[] sValue[j][k];
						}
						delete[] sValue[j];
						delete[] missing[j];
					}
					if(*cErr != 0) {
						delete[] sValue;
						delete[] iValue;
						delete[] dValue;
						delete[] missing;
						return errLevel;
					}
				}
			} 
			if(recordNum % BATCH_SIZE != 0) {
				*cErr = SetData(dValue, iValue, sValue, missing, recordNum % BATCH_SIZE);
				for(int j = 0; j < recordNum % BATCH_SIZE; j++) {
					delete[] dValue[j];
					delete[] iValue[j];
					for(int k = 0; k < sCount; k++) {
						delete[] sValue[j][k];
					}
					delete[] sValue[j];
					delete[] missing[j];
				}
				delete[] sValue;
				delete[] iValue;
				delete[] dValue;
				delete[] missing;
			}
			// record
			//CommitDataInDS(datasetName, fieldStorages, fieldNum, recordNum );
			delete []fieldStorages;
			return errLevel;
		}
		catch(...) {
			int *cErr = INTEGER(errLevel);
			*cErr = EXCEPTION_THROWN;
			SEXP ans = PROTECT(allocVector(STRSXP, 1));
			SET_STRING_ELT(ans, 0, asChar(errLevel));
			return ans;
		}
	}


/*****************************************************************************
 *                          Load & Unload hook                               *
 *****************************************************************************/
    void R_init_ibmspsscf70(DllInfo *info)
    {
        R_registerRoutines(info, cMethods, callMethods, 0, 0);
        LoadLib();
    }

    void R_unload_ibmspsscf70(DllInfo *info)
    {
        FreeLib();
        return;
    }
}
