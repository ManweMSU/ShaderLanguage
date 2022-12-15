#pragma once

#include "Core.h"
#include "Parser.h"

namespace Engine
{
	namespace EGSL
	{
		bool ProduceOutput(IO::Console * console, const string & source, const string & output_base, OutputForm form, OutputTarget target, bool werror, bool wsupress);
	}
}