#include "Intrinsic.h"

using namespace Engine::Syntax;

namespace Engine
{
	namespace EGSL
	{
		string MakeEnumeration(const Array<string> & words)
		{
			DynamicString result;
			result << words[0];
			auto len = words.Length();
			for (int i = 1; i < len; i++) {
				if (i == len - 1) result << L" or ";
				else result << L", ";
				result << words[i];
			}
			return result.ToString();
		}
		IntrinsicTranslateContext::IntrinsicTranslateContext(const Array<Syntax::Token> & _text, int & _cp, CompilerCommonContext & _context, CompilerShaderContext & _scontext, int fp) :
			text(_text), cp(_cp), context(_context), scontext(_scontext)
		{
			retval.IsAssignable = false;
			retval.Type = &context.Void;
			retval.ArraySize = 0;
			first = true;
			last_arg_at = 0;
			func_at = fp;
		}
		ValueDescriptor IntrinsicTranslateContext::GetReturnValue(void) { return retval; }
		CompilerCommonContext & IntrinsicTranslateContext::GetCommonContext(void) { return context; }
		CompilerShaderContext & IntrinsicTranslateContext::GetShaderContext(void) { return scontext; }
		string IntrinsicTranslateContext::TranslateArgument(ValueDescriptor & desc)
		{
			if (!first) {
				if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L",")
					throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"',' expected");
				cp++;
			} else first = false;
			last_arg_at = cp;
			return TranslateExpression(text, cp, context, scontext, 0, &desc);
		}
		bool IntrinsicTranslateContext::ArgumentAvailable(void)
		{
			if (text[cp].Class == TokenClass::CharCombo && text[cp].Content == L")") return false;
			else return true;
		}
		SimpleType * IntrinsicTranslateContext::FindSimpleType(SimpleTypeDomain domain, SimpleTypeClass cls, int cols, int rows)
		{
			for (auto & type : context.SimpleTypes) {
				if (type.Class == cls && type.Domain == domain) {
					if (cls == SimpleTypeClass::Scalar) return &type;
					else if (cls == SimpleTypeClass::Vector && cols == type.Columns) return &type;
					else if (cls == SimpleTypeClass::Matrix && cols == type.Columns && rows == type.Rows) return &type;
				}
			}
			return 0;
		}
		void IntrinsicTranslateContext::CheckTypecast(ValueDescriptor from, ValueDescriptor to) { CheckAsymmetricAutocast(from, to, last_arg_at); }
		bool IntrinsicTranslateContext::CheckTypecastNothrow(ValueDescriptor from, ValueDescriptor to) { return CheckAsymmetricAutocast(from, to); }
		void IntrinsicTranslateContext::CheckTypecast(ValueDescriptor from, int to)
		{
			if (from.ArraySize) throw CompilationException(CompilationError::ExpressionTypeMismatch, last_arg_at, L"Argument must not be an array");
			DynamicString message;
			message << FormatString(L"Argument is '%0', while ", from.Type->ToString());
			Array<string> words(0x10);
			if (to & ST_SCALAR) words << L"a scalar";
			if (to & ST_VECTOR) words << L"a vector";
			if (to & ST_MATRIX) {
				if (to & ST_SQUARE) words << L"a square matrix";
				else words << L"a matrix";
			}
			message << MakeEnumeration(words);
			words.Clear();
			string ncol, nrow;
			if (to & ST_NCOL2) ncol = L"2";
			else if (to & ST_NCOL3) ncol = L"3";
			else if (to & ST_NCOL4) ncol = L"4";
			if (to & ST_NROW2) nrow = L"2";
			else if (to & ST_NROW3) nrow = L"3";
			else if (to & ST_NROW4) nrow = L"4";
			if (((to & ST_CLASS) == ST_VECTOR) && ((to & ST_ANYCOL) != ST_ANYCOL)) {
				message << L" of size " << ncol;
			} else if (((to & ST_CLASS) == ST_MATRIX) && (((to & ST_ANYCOL) != ST_ANYCOL) || ((to & ST_ANYROW) != ST_ANYROW))) {
				message << L" of dimensions " << nrow << L" x " << ncol;
			}
			if ((to & ST_DOMAIN) != ST_DOMAIN) {
				if (to & ST_BOOL) words << L"'bool'";
				else if (to & ST_INT) words << L"'int'";
				else if (to & ST_UINT) words << L"'uint'";
				else if (to & ST_FLOAT) words << L"'float'";
				message << L" of type " << MakeEnumeration(words);
			}
			message << L" is expected";
			auto msg = message.ToString();
			auto err = CompilationError::ExpressionTypeMismatch;
			if (from.Type->GetClass() != TypeClass::Simple) throw CompilationException(err, last_arg_at, msg);
			auto s = static_cast<SimpleType *>(from.Type);
			if (s->Class == SimpleTypeClass::Scalar && (to & ST_SCALAR)) return;
			if (s->Class == SimpleTypeClass::Scalar && !(to & ST_SCALAR)) throw CompilationException(err, last_arg_at, msg);
			if (s->Class == SimpleTypeClass::Vector && !(to & ST_VECTOR)) throw CompilationException(err, last_arg_at, msg);
			if (s->Class == SimpleTypeClass::Matrix && !(to & ST_MATRIX)) throw CompilationException(err, last_arg_at, msg);
			if (s->Domain == SimpleTypeDomain::Boolean && !(to & ST_SCALAR)) throw CompilationException(err, last_arg_at, msg);
			if (s->Domain == SimpleTypeDomain::Integer && !(to & ST_INT)) throw CompilationException(err, last_arg_at, msg);
			if (s->Domain == SimpleTypeDomain::UnsignedInteger && !(to & ST_UINT)) throw CompilationException(err, last_arg_at, msg);
			if (s->Domain == SimpleTypeDomain::Float && !(to & ST_FLOAT)) throw CompilationException(err, last_arg_at, msg);
			if ((to & ST_SQUARE) && s->Columns != s->Rows) throw CompilationException(err, last_arg_at, msg);
			if (s->Class == SimpleTypeClass::Vector || s->Class == SimpleTypeClass::Matrix) {
				if (s->Columns == 2 && !(to & ST_NCOL2)) throw CompilationException(err, last_arg_at, msg);
				if (s->Columns == 3 && !(to & ST_NCOL3)) throw CompilationException(err, last_arg_at, msg);
				if (s->Columns == 4 && !(to & ST_NCOL4)) throw CompilationException(err, last_arg_at, msg);
			}
			if (s->Class == SimpleTypeClass::Matrix) {
				if (s->Rows == 2 && !(to & ST_NROW2)) throw CompilationException(err, last_arg_at, msg);
				if (s->Rows == 3 && !(to & ST_NROW3)) throw CompilationException(err, last_arg_at, msg);
				if (s->Rows == 4 && !(to & ST_NROW4)) throw CompilationException(err, last_arg_at, msg);
			}
		}
		void IntrinsicTranslateContext::CheckIdentity(ValueDescriptor from, ValueDescriptor to) { EGSL::CheckIdentity(from, to, last_arg_at); }
		void IntrinsicTranslateContext::SetReturnValue(LanguageType * type, bool assignable)
		{
			retval.Type = type;
			retval.IsAssignable = assignable;
		}
		int IntrinsicTranslateContext::GetFunctionPosition(void) { return func_at; }
		int IntrinsicTranslateContext::GetCurrentPosition(void) { return cp; }
		int IntrinsicTranslateContext::GetLastArgumentPosition(void) { return last_arg_at; }

		SimpleTypeDomain GetTypeDomain(const ValueDescriptor & desc) { return static_cast<SimpleType *>(desc.Type)->Domain; }
		bool IsSimple(const ValueDescriptor & desc)
		{
			if (desc.Type->GetClass() == TypeClass::Simple) return true;
			else return false;
		}
		bool IsScalar(const ValueDescriptor & desc)
		{
			if (desc.Type->GetClass() == TypeClass::Simple && static_cast<SimpleType *>(desc.Type)->Class == SimpleTypeClass::Scalar) return true;
			else return false;
		}
		bool IsVector(const ValueDescriptor & desc)
		{
			if (desc.Type->GetClass() == TypeClass::Simple && static_cast<SimpleType *>(desc.Type)->Class == SimpleTypeClass::Vector) return true;
			else return false;
		}
		bool IsMatrix(const ValueDescriptor & desc)
		{
			if (desc.Type->GetClass() == TypeClass::Simple && static_cast<SimpleType *>(desc.Type)->Class == SimpleTypeClass::Matrix) return true;
			else return false;
		}
		bool IsSampler(const ValueDescriptor & desc)
		{
			if (desc.Type->GetClass() == TypeClass::Register && static_cast<RegisterType *>(desc.Type)->Class == RegisterTypeClass::Sampler) return true;
			else return false;
		}
		bool IsTexture(const ValueDescriptor & desc)
		{
			if (desc.Type->GetClass() == TypeClass::Register && static_cast<RegisterType *>(desc.Type)->Class == RegisterTypeClass::Texture) return true;
			else return false;
		}
		int GetColumns(const ValueDescriptor & desc) { return static_cast<SimpleType *>(desc.Type)->Columns; }
		int GetRows(const ValueDescriptor & desc) { return static_cast<SimpleType *>(desc.Type)->Rows; }
		SimpleTypeClass GetSimpleClass(const ValueDescriptor & desc) { return static_cast<SimpleType *>(desc.Type)->Class; }
		SimpleType * GetTextureElementType(const ValueDescriptor & desc)
		{
			return static_cast<SimpleType *>(static_cast<RegisterType *>(desc.Type)->InnerType.Inner());
		}
		SimpleType * GetTextureSampleCoordType(const ValueDescriptor & desc, IntrinsicTranslateContext & context)
		{
			auto tex = static_cast<RegisterType *>(desc.Type);
			if (tex->Dimensions == 0) return context.FindSimpleType(SimpleTypeDomain::Float, SimpleTypeClass::Scalar);
			else if (tex->Dimensions == 1) return context.FindSimpleType(SimpleTypeDomain::Float, SimpleTypeClass::Vector, 2);
			else if (tex->Dimensions == 2) return context.FindSimpleType(SimpleTypeDomain::Float, SimpleTypeClass::Vector, 3);
			else if (tex->Dimensions == 3) return context.FindSimpleType(SimpleTypeDomain::Float, SimpleTypeClass::Vector, 3);
			else if (tex->Dimensions == 4) return context.FindSimpleType(SimpleTypeDomain::Float, SimpleTypeClass::Vector, 2);
			else if (tex->Dimensions == 5) return context.FindSimpleType(SimpleTypeDomain::Float, SimpleTypeClass::Vector, 3);
			else if (tex->Dimensions == 6) return context.FindSimpleType(SimpleTypeDomain::Float, SimpleTypeClass::Vector, 4);
			else return 0;
		}
		ValueDescriptor MakeDesc(LanguageType * type, bool assignable = false)
		{
			ValueDescriptor result;
			result.Type = type;
			result.IsAssignable = assignable;
			result.ArraySize = 0;
			return result;
		}

		string TranslateAbsolute(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_INT | ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			return L"abs(" + val + L")";
		}
		string TranslateArcCos(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			return L"acos(" + val + L")";
		}
		string TranslateAll(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_DOMAIN | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(context.FindSimpleType(SimpleTypeDomain::Boolean, SimpleTypeClass::Scalar));
			return L"all(" + val + L")";
		}
		string TranslateAny(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_DOMAIN | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(context.FindSimpleType(SimpleTypeDomain::Boolean, SimpleTypeClass::Scalar));
			return L"any(" + val + L")";
		}
		string TranslateAsFloat(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_INT | ST_UINT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(context.FindSimpleType(SimpleTypeDomain::Float, GetSimpleClass(desc), GetColumns(desc), GetRows(desc)));
			return L"asfloat(" + val + L")";
		}
		string TranslateAsInt(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_UINT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(context.FindSimpleType(SimpleTypeDomain::Integer, GetSimpleClass(desc), GetColumns(desc), GetRows(desc)));
			return L"asint(" + val + L")";
		}
		string TranslateAsUInt(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_INT | ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(context.FindSimpleType(SimpleTypeDomain::UnsignedInteger, GetSimpleClass(desc), GetColumns(desc), GetRows(desc)));
			return L"asuint(" + val + L")";
		}
		string TranslateArcSin(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			return L"asin(" + val + L")";
		}
		string TranslateArcTg(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			if (context.ArgumentAvailable()) {
				ValueDescriptor desc2;
				auto val2 = context.TranslateArgument(desc2);
				context.CheckTypecast(desc2, desc);
				return L"atan2(" + val + L", " + val2 + L")";
			} else return L"atan(" + val + L")";
		}
		string TranslateCeil(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			return L"ceil(" + val + L")";
		}
		string TranslateClamp(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc, desc_min, desc_max;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_INT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			auto val_min = context.TranslateArgument(desc_min);
			context.CheckTypecast(desc_min, desc);
			auto val_max = context.TranslateArgument(desc_max);
			context.CheckTypecast(desc_max, desc);
			return L"clamp(" + val + L", " + val_min + L", " + val_max + L")";
		}
		string TranslateClip(const string & name, IntrinsicTranslateContext & context)
		{
			if (context.GetShaderContext().Class != ShaderClass::Pixel)
				throw CompilationException(CompilationError::UnavailableIntrinsic, context.GetFunctionPosition(),
				L"'clip' is available in pixel shader only");
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			return L"clip(" + val + L")";
		}
		string TranslateCos(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			return L"cos(" + val + L")";
		}
		string TranslateCosHyperbolic(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			return L"cosh(" + val + L")";
		}
		string TranslateCross(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc1, desc2;
			auto a = context.TranslateArgument(desc1);
			context.CheckTypecast(desc1, ST_FLOAT | ST_VECTOR | ST_NCOL3 | ST_ANYROW);
			auto b = context.TranslateArgument(desc2);
			context.CheckTypecast(desc2, ST_FLOAT | ST_VECTOR | ST_NCOL3 | ST_ANYROW);
			context.SetReturnValue(desc1.Type);
			return L"cross(" + a + L", " + b + L")";
		}
		string TranslateDeterminant(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_MATRIX | ST_SQUARE | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(context.FindSimpleType(GetTypeDomain(desc), SimpleTypeClass::Scalar));
			return L"determinant(" + val + L")";
		}
		string TranslateDistance(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc_a, desc_b;
			auto val = context.TranslateArgument(desc_a);
			context.CheckTypecast(desc_a, ST_FLOAT | ST_VECTOR | ST_ANYCOL | ST_ANYROW);
			auto val2 = context.TranslateArgument(desc_b);
			context.CheckTypecast(desc_b, desc_a);
			context.SetReturnValue(context.FindSimpleType(GetTypeDomain(desc_a), SimpleTypeClass::Scalar));
			return L"distance(" + val + L", " + val2 + L")";
		}
		string TranslateDot(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc_a, desc_b;
			auto val = context.TranslateArgument(desc_a);
			context.CheckTypecast(desc_a, ST_FLOAT | ST_INT | ST_VECTOR | ST_ANYCOL | ST_ANYROW);
			auto val2 = context.TranslateArgument(desc_b);
			context.CheckTypecast(desc_b, desc_a);
			context.SetReturnValue(context.FindSimpleType(GetTypeDomain(desc_a), SimpleTypeClass::Scalar));
			return L"dot(" + val + L", " + val2 + L")";
		}
		string TranslateExponent(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			return L"exp(" + val + L")";
		}
		string TranslateExponentBinary(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			return L"exp2(" + val + L")";
		}
		string TranslateFaceForward(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc, desc2, desc3;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_VECTOR | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			auto val2 = context.TranslateArgument(desc2);
			context.CheckTypecast(desc2, desc);
			auto val3 = context.TranslateArgument(desc3);
			context.CheckTypecast(desc3, desc);
			return L"faceforward(" + val + L", " + val2 + L", " + val3 + L")";
		}
		string TranslateFloor(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			return L"floor(" + val + L")";
		}
		string TranslateFMod(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc, desc2;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			auto val2 = context.TranslateArgument(desc2);
			context.CheckTypecast(desc2, desc);
			return L"fmod(" + val + L", " + val2 + L")";
		}
		string TranslateIsFinite(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(context.FindSimpleType(SimpleTypeDomain::Boolean, GetSimpleClass(desc), GetColumns(desc), GetRows(desc)));
			return L"isfinite(" + val + L")";
		}
		string TranslateIsInf(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(context.FindSimpleType(SimpleTypeDomain::Boolean, GetSimpleClass(desc), GetColumns(desc), GetRows(desc)));
			return L"isinf(" + val + L")";
		}
		string TranslateIsNaN(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(context.FindSimpleType(SimpleTypeDomain::Boolean, GetSimpleClass(desc), GetColumns(desc), GetRows(desc)));
			return L"isnan(" + val + L")";
		}
		string TranslateLdExp(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc, desc2;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			auto val2 = context.TranslateArgument(desc2);
			context.CheckTypecast(desc2, desc);
			return L"ldexp(" + val + L", " + val2 + L")";
		}
		string TranslateLength(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_VECTOR | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(context.FindSimpleType(GetTypeDomain(desc), SimpleTypeClass::Scalar));
			return L"length(" + val + L")";
		}
		string TranslateLerp(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc_a, desc_b, desc_c;
			auto val = context.TranslateArgument(desc_a);
			context.CheckTypecast(desc_a, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			auto val2 = context.TranslateArgument(desc_b);
			context.CheckTypecast(desc_b, desc_a);
			auto val3 = context.TranslateArgument(desc_c);
			context.CheckTypecast(desc_c, desc_a);
			context.SetReturnValue(desc_a.Type);
			return L"lerp(" + val + L", " + val2 + L", " + val3 + L")";
		}
		string TranslateLog(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			return L"log(" + val + L")";
		}
		string TranslateLog2(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			return L"log2(" + val + L")";
		}
		string TranslateLog10(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			return L"log10(" + val + L")";
		}
		string TranslateMax(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc_a, desc_b;
			auto val = context.TranslateArgument(desc_a);
			context.CheckTypecast(desc_a, ST_FLOAT | ST_INT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			auto val2 = context.TranslateArgument(desc_b);
			context.CheckTypecast(desc_b, desc_a);
			context.SetReturnValue(desc_a.Type);
			return L"max(" + val + L", " + val2 + L")";
		}
		string TranslateMin(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc_a, desc_b;
			auto val = context.TranslateArgument(desc_a);
			context.CheckTypecast(desc_a, ST_FLOAT | ST_INT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			auto val2 = context.TranslateArgument(desc_b);
			context.CheckTypecast(desc_b, desc_a);
			context.SetReturnValue(desc_a.Type);
			return L"min(" + val + L", " + val2 + L")";
		}
		string TranslateModF(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc_a, desc_b;
			auto val = context.TranslateArgument(desc_a);
			context.CheckTypecast(desc_a, ST_FLOAT | ST_INT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			auto val2 = context.TranslateArgument(desc_b);
			context.CheckIdentity(desc_b, desc_a);
			if (!desc_b.IsAssignable) throw CompilationException(CompilationError::ExpressionIsNotAssignable, context.GetLastArgumentPosition(), L"");
			context.SetReturnValue(desc_a.Type);
			return L"modf(" + val + L", " + val2 + L")";
		}
		string TranslateMul(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc_a, desc_b;
			auto val = context.TranslateArgument(desc_a);
			context.CheckTypecast(desc_a, ST_FLOAT | ST_INT | ST_VECTOR | ST_MATRIX | ST_ANYCOL | ST_ANYROW);
			auto val2 = context.TranslateArgument(desc_b);
			if (!IsSimple(desc_b) || IsScalar(desc_b) || GetTypeDomain(desc_b) != GetTypeDomain(desc_a))
				throw CompilationException(CompilationError::ExpressionTypeMismatch, context.GetLastArgumentPosition(),
				L"Must be a vector or a matrix of the same type as the first argument");
			if (IsVector(desc_a)) {
				if (!IsMatrix(desc_b)) throw CompilationException(CompilationError::ExpressionTypeMismatch, context.GetLastArgumentPosition(), L"Must be a matrix");
				if (GetColumns(desc_a) != GetRows(desc_b))
					throw CompilationException(CompilationError::ExpressionTypeMismatch, context.GetLastArgumentPosition(),
					L"Number of rows must be the same as the vector size");
				context.SetReturnValue(context.FindSimpleType(GetTypeDomain(desc_a), SimpleTypeClass::Vector, GetColumns(desc_b)));
			} else {
				if (IsMatrix(desc_b)) {
					if (GetColumns(desc_a) != GetRows(desc_b))
						throw CompilationException(CompilationError::ExpressionTypeMismatch, context.GetLastArgumentPosition(),
						L"Number of rows must be the same as the number of columns of the first argument");
					context.SetReturnValue(context.FindSimpleType(GetTypeDomain(desc_a), SimpleTypeClass::Matrix, GetColumns(desc_b), GetRows(desc_a)));
				} else {
					if (GetColumns(desc_a) != GetColumns(desc_b))
						throw CompilationException(CompilationError::ExpressionTypeMismatch, context.GetLastArgumentPosition(),
						L"Vector size must be the same as the number of columns of the first argument");
					context.SetReturnValue(context.FindSimpleType(GetTypeDomain(desc_a), SimpleTypeClass::Vector, GetRows(desc_a)));
				}
			}
			return L"mul(" + val + L", " + val2 + L")";
		}
		string TranslateNormalize(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_VECTOR | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			return L"normalize(" + val + L")";
		}
		string TranslatePower(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc_a, desc_b;
			auto val = context.TranslateArgument(desc_a);
			context.CheckTypecast(desc_a, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			auto val2 = context.TranslateArgument(desc_b);
			context.CheckTypecast(desc_b, desc_a);
			context.SetReturnValue(desc_a.Type);
			return L"pow(" + val + L", " + val2 + L")";
		}
		string TranslateReflect(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc, desc2;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_VECTOR | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			auto val2 = context.TranslateArgument(desc2);
			context.CheckTypecast(desc2, desc);
			return L"reflect(" + val + L", " + val2 + L")";
		}
		string TranslateRefract(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc, desc2, desc3;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_VECTOR | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			auto val2 = context.TranslateArgument(desc2);
			context.CheckTypecast(desc2, desc);
			auto val3 = context.TranslateArgument(desc3);
			context.CheckTypecast(desc3, MakeDesc(context.FindSimpleType(SimpleTypeDomain::Float, SimpleTypeClass::Scalar)));
			return L"refract(" + val + L", " + val2 + L", " + val3 + L")";
		}
		string TranslateRound(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			return L"round(" + val + L")";
		}
		string TranslateReverseSquareRoot(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			return L"rsqrt(" + val + L")";
		}
		string TranslateSaturate(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			return L"saturate(" + val + L")";
		}
		string TranslateSignum(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_INT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(context.FindSimpleType(SimpleTypeDomain::Integer, GetSimpleClass(desc), GetColumns(desc), GetRows(desc)));
			return L"sign(" + val + L")";
		}
		string TranslateSin(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			return L"sin(" + val + L")";
		}
		string TranslateSinCos(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc_a, desc_b, desc_c;
			auto val = context.TranslateArgument(desc_a);
			context.CheckTypecast(desc_a, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			auto val2 = context.TranslateArgument(desc_b);
			context.CheckIdentity(desc_b, desc_a);
			if (!desc_b.IsAssignable) throw CompilationException(CompilationError::ExpressionIsNotAssignable, context.GetLastArgumentPosition(), L"");
			auto val3 = context.TranslateArgument(desc_c);
			context.CheckIdentity(desc_c, desc_a);
			if (!desc_c.IsAssignable) throw CompilationException(CompilationError::ExpressionIsNotAssignable, context.GetLastArgumentPosition(), L"");
			return L"sincos(" + val + L", " + val2 + L", " + val3 + L")";
		}
		string TranslateSinHyperbolic(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			return L"sinh(" + val + L")";
		}
		string TranslateSmoothStep(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc_a, desc_b, desc_c;
			auto val = context.TranslateArgument(desc_a);
			context.CheckTypecast(desc_a, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			auto val2 = context.TranslateArgument(desc_b);
			context.CheckTypecast(desc_b, desc_a);
			auto val3 = context.TranslateArgument(desc_c);
			context.CheckTypecast(desc_c, desc_a);
			context.SetReturnValue(desc_a.Type);
			return L"smoothstep(" + val2 + L", " + val3 + L", " + val + L")";
		}
		string TranslateSquareRoot(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			return L"sqrt(" + val + L")";
		}
		string TranslateStep(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc_a, desc_b;
			auto val = context.TranslateArgument(desc_a);
			context.CheckTypecast(desc_a, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			auto val2 = context.TranslateArgument(desc_b);
			context.CheckTypecast(desc_b, desc_a);
			context.SetReturnValue(desc_a.Type);
			return L"step(" + val2 + L", " + val + L")";
		}
		string TranslateTg(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			return L"tan(" + val + L")";
		}
		string TranslateTgHyperbolic(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			return L"tanh(" + val + L")";
		}
		string TranslateTranspose(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_DOMAIN | ST_MATRIX | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(context.FindSimpleType(GetTypeDomain(desc), SimpleTypeClass::Matrix, GetRows(desc), GetColumns(desc)));
			return L"transpose(" + val + L")";
		}
		string TranslateTruncate(const string & name, IntrinsicTranslateContext & context)
		{
			ValueDescriptor desc;
			auto val = context.TranslateArgument(desc);
			context.CheckTypecast(desc, ST_FLOAT | ST_CLASS | ST_ANYCOL | ST_ANYROW);
			context.SetReturnValue(desc.Type);
			return L"trunc(" + val + L")";
		}

		string TranslateTypecast(const string & type, IntrinsicTranslateContext & context)
		{
			SimpleType * t;
			for (auto & st : context.GetCommonContext().SimpleTypes) if (st.Name == type) { t = &st; break; }
			if (t->Class == SimpleTypeClass::Scalar) {
				ValueDescriptor desc;
				auto val = context.TranslateArgument(desc);
				context.CheckTypecast(desc, MakeDesc(t));
				context.SetReturnValue(t);
				return type + L"(" + val + L")";
			} else if (t->Class == SimpleTypeClass::Vector) {
				ValueDescriptor desc;
				auto val = context.TranslateArgument(desc);
				if (IsVector(desc) && GetColumns(desc) == t->Columns) {
					context.SetReturnValue(t);
					return type + L"(" + val + L")";
				} else {
					int rem_dim = t->Columns;
					DynamicString out;
					if (IsVector(desc) && GetTypeDomain(desc) == t->Domain) {
						rem_dim -= GetColumns(desc);
						if (rem_dim < 0) context.CheckTypecast(desc, MakeDesc(t));
					} else if (IsScalar(desc)) {
						rem_dim--;
					} else context.CheckTypecast(desc, MakeDesc(t));
					out << type << L"(" << val;
					while (rem_dim) {
						val = context.TranslateArgument(desc);
						auto rem_type = context.FindSimpleType(t->Domain, rem_dim == 1 ? SimpleTypeClass::Scalar : SimpleTypeClass::Vector, rem_dim);
						if (IsVector(desc) && GetTypeDomain(desc) == t->Domain) {
							rem_dim -= GetColumns(desc);
							if (rem_dim < 0) context.CheckTypecast(desc, MakeDesc(rem_type));
						} else if (IsScalar(desc)) {
							rem_dim--;
						} else context.CheckTypecast(desc, MakeDesc(rem_type));
						out << L", " << val;
					}
					out << L")";
					context.SetReturnValue(t);
					return out.ToString();
				}
			} else if (t->Class == SimpleTypeClass::Matrix) {
				ValueDescriptor desc;
				auto val = context.TranslateArgument(desc);
				if (IsMatrix(desc) && GetColumns(desc) == t->Columns && GetRows(desc) == t->Rows) {
					context.SetReturnValue(t);
					return type + L"(" + val + L")";
				} else {
					int rem_dim = t->Rows;
					auto row_type = context.FindSimpleType(t->Domain, SimpleTypeClass::Vector, t->Columns);
					DynamicString out;
					context.CheckIdentity(desc, MakeDesc(row_type));
					rem_dim--;
					out << type << L"(" << val;
					while (rem_dim) {
						val = context.TranslateArgument(desc);
						context.CheckIdentity(desc, MakeDesc(row_type));
						rem_dim--;
						out << L", " << val;
					}
					out << L")";
					context.SetReturnValue(t);
					return out.ToString();
				}
			}
			return L"";
		}
		string TranslateSample(const string & type, IntrinsicTranslateContext & context)
		{
			if (context.GetShaderContext().Class != ShaderClass::Pixel)
				throw CompilationException(CompilationError::UnavailableIntrinsic, context.GetFunctionPosition(),
				L"'sample' is available in pixel shader only");
			ValueDescriptor texture, sampler, coord, mip;
			auto tex_val = context.TranslateArgument(texture);
			if (!IsTexture(texture)) throw CompilationException(CompilationError::ExpressionTypeMismatch,
				context.GetLastArgumentPosition(), L"Texture type is expected");
			auto sam_val = context.TranslateArgument(sampler);
			if (!IsSampler(sampler)) throw CompilationException(CompilationError::ExpressionTypeMismatch,
				context.GetLastArgumentPosition(), L"Sampler type is expected");
			auto coord_type = GetTextureSampleCoordType(texture, context);
			auto ret_type = GetTextureElementType(texture);
			auto coord_val = context.TranslateArgument(coord);
			context.CheckTypecast(coord, MakeDesc(coord_type));
			context.SetReturnValue(ret_type);
			if (context.ArgumentAvailable()) {
				auto mip_val = context.TranslateArgument(mip);
				context.CheckTypecast(mip, MakeDesc(context.FindSimpleType(SimpleTypeDomain::Float, SimpleTypeClass::Scalar)));
				return tex_val + L".SampleLevel(" + sam_val + L", " + coord_val + L", " + mip_val + L")";
			} else return tex_val + L".Sample(" + sam_val + L", " + coord_val + L")";
		}
		IntrinsicTranslator FindIntrinsicTranslator(CompilerCommonContext & context, const string & name)
		{
			for (auto & type : context.SimpleTypes) if (type.Name == name) return TranslateTypecast;
			if (name == L"sample") return TranslateSample;
			// A
			else if (name == L"abs") return TranslateAbsolute;
			else if (name == L"all") return TranslateAll;
			else if (name == L"any") return TranslateAny;
			else if (name == L"arccos") return TranslateArcCos;
			else if (name == L"arctg") return TranslateArcTg;
			else if (name == L"arcsin") return TranslateArcSin;
			else if (name == L"asfloat") return TranslateAsFloat;
			else if (name == L"asint") return TranslateAsInt;
			else if (name == L"asuint") return TranslateAsUInt;
			// C
			else if (name == L"ceil") return TranslateCeil;
			else if (name == L"clamp") return TranslateClamp;
			else if (name == L"clip") return TranslateClip;
			else if (name == L"cos") return TranslateCos;
			else if (name == L"ch") return TranslateCosHyperbolic;
			else if (name == L"cross") return TranslateCross;
			// D
			else if (name == L"det") return TranslateDeterminant;
			else if (name == L"distance") return TranslateDistance;
			else if (name == L"dot") return TranslateDot;
			// E
			else if (name == L"exp") return TranslateExponent;
			else if (name == L"exp2") return TranslateExponentBinary;
			// F
			else if (name == L"faceforward") return TranslateFaceForward;
			else if (name == L"floor") return TranslateFloor;
			else if (name == L"fmod") return TranslateFMod;
			// I
			else if (name == L"isfinite") return TranslateIsFinite;
			else if (name == L"isinf") return TranslateIsInf;
			else if (name == L"isnan") return TranslateIsNaN;
			// L
			else if (name == L"ldexp") return TranslateLdExp;
			else if (name == L"length") return TranslateLength;
			else if (name == L"lerp") return TranslateLerp;
			else if (name == L"ln") return TranslateLog;
			else if (name == L"lb") return TranslateLog2;
			else if (name == L"log") return TranslateLog10;
			// M
			else if (name == L"max") return TranslateMax;
			else if (name == L"min") return TranslateMin;
			else if (name == L"modf") return TranslateModF;
			else if (name == L"mul") return TranslateMul;
			// N
			else if (name == L"normalize") return TranslateNormalize;
			// P
			else if (name == L"pow") return TranslatePower;
			// R
			else if (name == L"reflect") return TranslateReflect;
			else if (name == L"refract") return TranslateRefract;
			else if (name == L"round") return TranslateRound;
			else if (name == L"rsqrt") return TranslateReverseSquareRoot;
			// S
			else if (name == L"saturate") return TranslateSaturate;
			else if (name == L"sign") return TranslateSignum;
			else if (name == L"sin") return TranslateSin;
			else if (name == L"sincos") return TranslateSinCos;
			else if (name == L"sh") return TranslateSinHyperbolic;
			else if (name == L"smoothstep") return TranslateSmoothStep;
			else if (name == L"sqrt") return TranslateSquareRoot;
			else if (name == L"step") return TranslateStep;
			// T
			else if (name == L"tg") return TranslateTg;
			else if (name == L"th") return TranslateTgHyperbolic;
			else if (name == L"transpose") return TranslateTranspose;
			else if (name == L"trunc") return TranslateTruncate;
			else return 0;
		}
	}
}