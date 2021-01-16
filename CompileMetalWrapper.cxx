#include "CompileMetal.h"

namespace Engine
{
	namespace EGSL
	{
		#ifndef ENGINE_MACOSX
		bool CompileMSL(const string & code, const string & output, DynamicString & log)
		{
			log << L"Compilation of MSL for Metal API is not available now.\n";
			return false;
		}
		#endif
	}
}