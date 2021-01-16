#pragma once

#include <EngineRuntime.h>

#include "Parser.h"

namespace Engine
{
	namespace EGSL
	{
		bool CompileMSL(const string & code, const string & output, DynamicString & log);
	}
}