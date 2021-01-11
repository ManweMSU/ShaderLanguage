#pragma once

#include <EngineRuntime.h>

#include "Parser.h"

namespace Engine
{
	namespace EGSL
	{
		DataBlock * CompileHLSL(const string & code, const string & entry, ShaderClass cls, DynamicString & log);
	}
}