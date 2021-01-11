#include "Core.h"

namespace Engine
{
	namespace EGSL
	{
		CompilationException::CompilationException(CompilationError error, int index, const string & comments) : Error(error), TokenIndex(index), Comments(comments) {}
		string CompilationException::ToString(void) const { return L"CompilationException"; }

		string DescriptionForError(CompilationError error)
		{
			if (error == CompilationError::Success) return L"Finished successfully";
			else if (error == CompilationError::InvalidToken) return L"Invalid token";
			else if (error == CompilationError::AnotherTokenExpected) return L"Another token expected";
			else if (error == CompilationError::ObjectRedefinition) return L"Object redefinition";
			else if (error == CompilationError::UnknownIdentifier) return L"Unknown object identifier";
			else if (error == CompilationError::InappropriateType) return L"Inappropriate type";
			else if (error == CompilationError::InvalidStaticArraySize) return L"Invalid static array size";
			else if (error == CompilationError::InvalidShaderArgumentSemantic) return L"Invalid shader argument semantic or register";
			else if (error == CompilationError::UnknownSemantic) return L"Unknown semantic or register name";
			else if (error == CompilationError::InvalidShaderArgumentSemanticSlot) return L"Invalid semantic or register slot";
			else if (error == CompilationError::DuplicateSemantic) return L"Duplicate argument IO specification";
			else if (error == CompilationError::SemanticMissing) return L"Necessary semantic is missing";
			else if (error == CompilationError::UnavailableStatement) return L"Statement is not allowed for current shader type";
			else if (error == CompilationError::UnavailableIntrinsic) return L"Intrinsic function is not allowed for current shader type";
			else if (error == CompilationError::InvalidStatementLocation) return L"Invalid statement location";
			else if (error == CompilationError::ArrayInitializerTooLong) return L"Array initializer list is too long";
			else if (error == CompilationError::ExpressionTypeMismatch) return L"Expression type mismatch";
			else if (error == CompilationError::ExpressionIsNotAssignable) return L"Expression is not assignable";
			else if (error == CompilationError::OperatorIsNotApplicable) return L"Operator is not applicable";
			else return L"Unknown error";
		}
		void LocateErrorPosition(const string & source, const Syntax::ParserSpellingException & error, string & desc, string & line, int & line_x, int & line_y, int & offset, int & length)
		{
			desc = error.Comments;
			int y = 0;
			for (int i = 0; i < error.Position; i++) if (source[i] == L'\n') y++;
			length = 1;
			int cp = error.Position;
			while (cp > 0 && source[cp] != L'\n') cp--;
			if (source[cp] == L'\n') while (cp < error.Position && source[cp] < 32 && source[cp] != L'\t') cp++;
			line_x = error.Position - cp;
			line_y = y;
			while (cp < error.Position && (source[cp] == L' ' || source[cp] == L'\t')) cp++;
			offset = error.Position - cp;
			DynamicString result;
			while (cp < source.Length() && source[cp] != L'\n') { result << ((source[cp] < 128) ? source[cp] : L'?'); cp++; }
			line = result.ToString();
		}
		void LocateErrorPosition(const string & source, const Array<Syntax::Token> & text, const CompilationException & error, string & desc, string & line, int & line_x, int & line_y, int & offset, int & length)
		{
			auto position = text[error.TokenIndex].SourcePosition;
			length = (error.TokenIndex < text.Length() - 1) ? (text[error.TokenIndex + 1].SourcePosition - text[error.TokenIndex].SourcePosition) : 1;
			desc = error.Comments;
			int y = 0;
			for (int i = 0; i < position; i++) if (source[i] == L'\n') y++;
			int cp = position;
			while (cp > 0 && source[cp] != L'\n') cp--;
			if (source[cp] == L'\n') while (cp < position && source[cp] < 32 && source[cp] != L'\t') cp++;
			line_x = position - cp;
			line_y = y;
			while (cp < position && (source[cp] == L' ' || source[cp] == L'\t')) cp++;
			offset = position - cp;
			DynamicString result;
			while (cp < source.Length() && source[cp] != L'\n') { result << ((source[cp] < 128) ? source[cp] : L'?'); cp++; }
			line = result.ToString();
			while (length > 1 && source[position + length - 1] <= 32) length--;
		}
	}
}