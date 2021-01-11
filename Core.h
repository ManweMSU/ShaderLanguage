#pragma once

#include <EngineRuntime.h>

namespace Engine
{
	namespace EGSL
	{
		enum class OutputForm { Source, Library };
		enum class OutputTarget { Unknown, Direct3D11, Metal };

		enum class CompilationError : uint32 {
			Success = 0x0000,
			InvalidToken = 0x0001,
			AnotherTokenExpected = 0x0002,
			ObjectRedefinition = 0x0003,
			UnknownIdentifier = 0x0004,
			InappropriateType = 0x0005,
			InvalidStaticArraySize = 0x0006,
			InvalidShaderArgumentSemantic = 0x0007,
			UnknownSemantic = 0x0008,
			InvalidShaderArgumentSemanticSlot = 0x0009,
			DuplicateSemantic = 0x000A,
			SemanticMissing = 0x000B,
			UnavailableStatement = 0x000C,
			UnavailableIntrinsic = 0x000D,
			InvalidStatementLocation = 0x000E,
			ArrayInitializerTooLong = 0x0010,
			ExpressionTypeMismatch = 0x0011,
			ExpressionIsNotAssignable = 0x0012,
			OperatorIsNotApplicable = 0x0013,
		};
		class CompilationException : public Exception
		{
		public:
			CompilationError Error;
			int TokenIndex;
			string Comments;

			CompilationException(CompilationError error, int index, const string & comments);
			virtual string ToString(void) const override;
		};

		string DescriptionForError(CompilationError error);
		void LocateErrorPosition(const string & source, const Syntax::ParserSpellingException & error, string & desc, string & line, int & line_x, int & line_y, int & offset, int & length);
		void LocateErrorPosition(const string & source, const Array<Syntax::Token> & text, const CompilationException & error, string & desc, string & line, int & line_x, int & line_y, int & offset, int & length);
	}
}