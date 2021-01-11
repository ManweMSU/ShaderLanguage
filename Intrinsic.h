#pragma once

#include "Parser.h"

namespace Engine
{
	namespace EGSL
	{
		enum SimpleTypeFlags {
			ST_BOOL   = 0x0001,
			ST_INT    = 0x0002,
			ST_UINT   = 0x0004,
			ST_FLOAT  = 0x0008,
			ST_SCALAR = 0x0010,
			ST_VECTOR = 0x0020,
			ST_MATRIX = 0x0040,
			ST_NCOL2  = 0x0100,
			ST_NCOL3  = 0x0200,
			ST_NCOL4  = 0x0400,
			ST_NROW2  = 0x1000,
			ST_NROW3  = 0x2000,
			ST_NROW4  = 0x4000,
			ST_ANYCOL = 0x0700,
			ST_ANYROW = 0x7000,
			ST_DOMAIN = 0x000F,
			ST_CLASS  = 0x00F0,
			ST_SQUARE = 0x8000,
		};
		class IntrinsicTranslateContext
		{
			const Array<Syntax::Token> & text;
			int & cp;
			CompilerCommonContext & context;
			CompilerShaderContext & scontext;
			ValueDescriptor retval;
			bool first;
			int last_arg_at;
			int func_at;
		public:
			IntrinsicTranslateContext(const Array<Syntax::Token> & _text, int & _cp, CompilerCommonContext & _context, CompilerShaderContext & _scontext, int fp);
			
			ValueDescriptor GetReturnValue(void);
			CompilerCommonContext & GetCommonContext(void);
			CompilerShaderContext & GetShaderContext(void);

			SimpleType * FindSimpleType(SimpleTypeDomain domain, SimpleTypeClass cls, int cols = 0, int rows = 0);
			string TranslateArgument(ValueDescriptor & desc);
			bool ArgumentAvailable(void);
			void CheckTypecast(ValueDescriptor from, ValueDescriptor to);
			bool CheckTypecastNothrow(ValueDescriptor from, ValueDescriptor to);
			void CheckTypecast(ValueDescriptor from, int to);
			void CheckIdentity(ValueDescriptor from, ValueDescriptor to);
			void SetReturnValue(LanguageType * type, bool assignable = false);
			int GetFunctionPosition(void);
			int GetCurrentPosition(void);
			int GetLastArgumentPosition(void);
		};
		typedef string (* IntrinsicTranslator) (const string & name, IntrinsicTranslateContext & context);
		IntrinsicTranslator FindIntrinsicTranslator(CompilerCommonContext & context, const string & name);
	}
}