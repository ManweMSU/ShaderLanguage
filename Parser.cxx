#include "Parser.h"

#include "Intrinsic.h"

using namespace Engine::Syntax;

namespace Engine
{
	namespace EGSL
	{
		TypeClass SimpleType::GetClass(void) const { return TypeClass::Simple; }
		TypeClass RegisterType::GetClass(void) const { return TypeClass::Register; }
		TypeClass StructureType::GetClass(void) const { return TypeClass::Structure; }
		string SimpleType::ToString(void) const { return Name; }
		string RegisterType::ToString(void) const
		{
			if (Class == RegisterTypeClass::Sampler) return L"sampler";
			else if (Class == RegisterTypeClass::Array) return L"array<" + InnerType->ToString() + L">";
			else {
				if (Dimensions == 0) return L"texture<1, " + InnerType->ToString() + L">";
				else if (Dimensions == 1) return L"texture<2, " + InnerType->ToString() + L">";
				else if (Dimensions == 2) return L"texture<cube, " + InnerType->ToString() + L">";
				else if (Dimensions == 3) return L"texture<3, " + InnerType->ToString() + L">";
				else if (Dimensions == 4) return L"texture array<1, " + InnerType->ToString() + L">";
				else if (Dimensions == 5) return L"texture array<2, " + InnerType->ToString() + L">";
				else if (Dimensions == 6) return L"texture array<cube, " + InnerType->ToString() + L">";
				else return L"";
			}
		}
		string StructureType::ToString(void) const { return Name; }
		TypeClass VoidType::GetClass(void) const { return TypeClass::Void; }
		string VoidType::ToString(void) const { return L"$void$"; }

		Syntax::Spelling spelling;
		const Syntax::Spelling & GetSpelling(void)
		{
			if (!spelling.CombinableChars.Length()) {
				spelling.CombinableChars << L'=';
				spelling.CombinableChars << L'+';
				spelling.CombinableChars << L'-';
				spelling.CombinableChars << L'*';
				spelling.CombinableChars << L'/';
				spelling.CombinableChars << L'%';
				spelling.CombinableChars << L'!';
				spelling.CombinableChars << L'~';
				spelling.CombinableChars << L'^';
				spelling.CombinableChars << L'&';
				spelling.CombinableChars << L'|';
				spelling.CombinableChars << L'<';
				spelling.CombinableChars << L'>';

				spelling.IsolatedChars << L'(';
				spelling.IsolatedChars << L')';
				spelling.IsolatedChars << L'{';
				spelling.IsolatedChars << L'}';
				spelling.IsolatedChars << L'[';
				spelling.IsolatedChars << L']';
				spelling.IsolatedChars << L',';
				spelling.IsolatedChars << L';';
				spelling.IsolatedChars << L'.';
				spelling.IsolatedChars << L':';
				spelling.IsolatedChars << L'?';

				spelling.CommentBlockOpeningWord = L"/*";
				spelling.CommentBlockClosingWord = L"*/";
				spelling.CommentEndOfLineWord = L"//";

				spelling.BooleanTrueLiteral = L"true";
				spelling.BooleanFalseLiteral = L"false";

				spelling.AllowNonLatinNames = false;
				spelling.Keywords << L"array";
				spelling.Keywords << L"sampler";
				spelling.Keywords << L"texture";
				spelling.Keywords << L"bool";
				spelling.Keywords << L"int";
				spelling.Keywords << L"uint";
				spelling.Keywords << L"float";
				for (int i = 2; i <= 4; i++) {
					string post = string(i); 
					spelling.Keywords << L"bool" + post;
					spelling.Keywords << L"int" + post;
					spelling.Keywords << L"uint" + post;
					spelling.Keywords << L"float" + post;
					for (int j = 2; j <= 4; j++) {
						string post = string(i) + L"x" + string(j); 
						spelling.Keywords << L"bool" + post;
						spelling.Keywords << L"int" + post;
						spelling.Keywords << L"uint" + post;
						spelling.Keywords << L"float" + post;
					}
				}
				spelling.Keywords << L"struct";
				spelling.Keywords << L"vertex";
				spelling.Keywords << L"pixel";
				spelling.Keywords << L"linear";
				spelling.Keywords << L"nointerpolation";
				spelling.Keywords << L"noperspective";
				spelling.Keywords << L"in";
				spelling.Keywords << L"out";
				spelling.Keywords << L"if";
				spelling.Keywords << L"else";
				spelling.Keywords << L"for";
				spelling.Keywords << L"while";
				spelling.Keywords << L"do";
				spelling.Keywords << L"return";
				spelling.Keywords << L"continue";
				spelling.Keywords << L"break";
				spelling.Keywords << L"discard";
			}
			return spelling;
		}
		Array<Syntax::Token> * ParseCode(const string & code) { return Syntax::ParseText(code, GetSpelling()); }

		void CreateSimpleType(CompilerCommonContext & context, SimpleTypeDomain domain, SimpleTypeClass cls, int ncols = 0, int nrows = 0)
		{
			SafePointer<SimpleType> type = new SimpleType;
			type->Domain = domain;
			type->Class = cls;
			if (domain == SimpleTypeDomain::Boolean) type->Name = L"bool";
			else if (domain == SimpleTypeDomain::Integer) type->Name = L"int";
			else if (domain == SimpleTypeDomain::UnsignedInteger) type->Name = L"uint";
			else if (domain == SimpleTypeDomain::Float) type->Name = L"float";
			if (cls == SimpleTypeClass::Matrix) {
				type->Name += string(nrows) + L"x" + string(ncols);
				type->Columns = ncols;
				type->Rows = nrows;
			} else if (cls == SimpleTypeClass::Vector) {
				type->Name += string(ncols);
				type->Columns = ncols;
				type->Rows = 0;
			} else if (cls == SimpleTypeClass::Scalar) {
				type->Columns = 0;
				type->Rows = 0;
			}
			context.SimpleTypes.Append(type);
		}
		void InitializeContext(CompilerCommonContext & context, OutputTarget target)
		{
			context.Target = target;
			CreateSimpleType(context, SimpleTypeDomain::Boolean, SimpleTypeClass::Scalar);
			CreateSimpleType(context, SimpleTypeDomain::Integer, SimpleTypeClass::Scalar);
			CreateSimpleType(context, SimpleTypeDomain::UnsignedInteger, SimpleTypeClass::Scalar);
			CreateSimpleType(context, SimpleTypeDomain::Float, SimpleTypeClass::Scalar);
			for (int i = 2; i <= 4; i++) {
				CreateSimpleType(context, SimpleTypeDomain::Boolean, SimpleTypeClass::Vector, i);
				CreateSimpleType(context, SimpleTypeDomain::Integer, SimpleTypeClass::Vector, i);
				CreateSimpleType(context, SimpleTypeDomain::UnsignedInteger, SimpleTypeClass::Vector, i);
				CreateSimpleType(context, SimpleTypeDomain::Float, SimpleTypeClass::Vector, i);
				for (int j = 2; j <= 4; j++) {
					CreateSimpleType(context, SimpleTypeDomain::Boolean, SimpleTypeClass::Matrix, i, j);
					CreateSimpleType(context, SimpleTypeDomain::Integer, SimpleTypeClass::Matrix, i, j);
					CreateSimpleType(context, SimpleTypeDomain::UnsignedInteger, SimpleTypeClass::Matrix, i, j);
					CreateSimpleType(context, SimpleTypeDomain::Float, SimpleTypeClass::Matrix, i, j);
				}
			}
		}
		LanguageType * FindType(const string & name, CompilerCommonContext & context)
		{
			for (auto & t : context.SimpleTypes) if (t.Name == name) return &t;
			for (auto & t : context.StructureTypes) if (t.Name == name) return &t;
			return 0;
		}
		LanguageType * ReadTypeDefinition(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context)
		{
			if (text[cp].Class != TokenClass::Identifier && text[cp].Class != TokenClass::Keyword)
				throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"An identifier or a keyword expected");
			if (text[cp].Content == L"sampler") {
				cp++;
				for (auto & t : context.RegisterTypes) if (t.Class == RegisterTypeClass::Sampler) return &t;
				SafePointer<RegisterType> type = new RegisterType;
				type->Class = RegisterTypeClass::Sampler;
				type->Dimensions = 0;
				context.RegisterTypes.Append(type);
				return type;
			} else if (text[cp].Content == L"array") {
				cp++;
				if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L"<")
					throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"'<' expected");
				cp++;
				auto tp = cp;
				auto inner = ReadTypeDefinition(text, cp, context);
				if (inner->GetClass() == TypeClass::Register) throw CompilationException(CompilationError::InappropriateType, tp, L"Array type must be a scalar, a vector, a matrix or a structure");
				if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L">")
					throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"'>' expected");
				cp++;
				SafePointer<RegisterType> type = new RegisterType;
				type->Class = RegisterTypeClass::Array;
				type->InnerType.SetRetain(inner);
				type->Dimensions = 0;
				context.RegisterTypes.Append(type);
				return type;
			} else if (text[cp].Content == L"texture") {
				cp++;
				bool array = false;
				auto inner = FindType(L"float4", context);
				int dim = 1;
				if (text[cp].Class == TokenClass::Keyword && text[cp].Content == L"array") { array = true; cp++; }
				if (text[cp].Class == TokenClass::CharCombo && text[cp].Content == L"<") {
					while (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L">") {
						if (text[cp].Class != TokenClass::CharCombo || (text[cp].Content != L"<" && text[cp].Content != L","))
							throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"',' expected");
						cp++;
						if (text[cp].Class == TokenClass::Constant && text[cp].ValueClass == TokenConstantClass::Numeric &&
							text[cp].NumericClass() == NumericTokenClass::Integer && text[cp].AsInteger() >= 1 && text[cp].AsInteger() <= 3) {
							auto v = text[cp].AsInteger();
							if (v == 1) dim = 0;
							else if (v == 2) dim = 1;
							else dim = 3;
							if (array && dim == 3) throw CompilationException(CompilationError::InappropriateType, cp, L"3D texture arrays are not supported");
							cp++;
						} else if (text[cp].Class == TokenClass::Identifier && text[cp].Content == L"cube") {
							dim = 2; cp++;
						} else if (text[cp].Class == TokenClass::Keyword) {
							auto it = FindType(text[cp].Content, context);
							if (!it) throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"Expected '1', '2', '3', 'cube' or a type name");
							if (it->GetClass() != TypeClass::Simple || static_cast<SimpleType *>(it)->Class == SimpleTypeClass::Matrix)
								throw CompilationException(CompilationError::InappropriateType, cp, L"Texture type must be a scalar or a vector type");
							inner = it;
							cp++;
						} else throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"Expected '1', '2', '3', 'cube' or a type name");
					}
					cp++;
				}
				SafePointer<RegisterType> type = new RegisterType;
				type->Class = RegisterTypeClass::Texture;
				type->InnerType.SetRetain(inner);
				type->Dimensions = array ? (dim + 4) : dim;
				context.RegisterTypes.Append(type);
				return type;
			} else {
				for (auto & t : context.SimpleTypes) if (t.Name == text[cp].Content) { cp++; return &t; }
				for (auto & t : context.StructureTypes) if (t.Name == text[cp].Content) { cp++; return &t; }
			}
			throw CompilationException(CompilationError::UnknownIdentifier, cp, FormatString(L"'%0' is not an identifier of a type", text[cp].Content));
		}
		void ParseFieldsDefinition(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, StructureType & type)
		{
			InterpolationMode mode = InterpolationMode::Default;
			if (text[cp].Class == TokenClass::Keyword) {
				if (text[cp].Content == L"linear") {
					mode = InterpolationMode::Linear; cp++;
				} else if (text[cp].Content == L"nointerpolation") {
					mode = InterpolationMode::None; cp++;
				} else if (text[cp].Content == L"noperspective") {
					mode = InterpolationMode::NoPerspective; cp++;
				}
			}
			auto tp = cp;
			auto field_type = ReadTypeDefinition(text, cp, context);
			if (field_type->GetClass() != TypeClass::Simple)
				throw CompilationException(CompilationError::InappropriateType, tp, L"Structure field type must be a scalar, a vector or a matrix type");
			if (context.Target == OutputTarget::Metal) {
				auto sim = static_cast<SimpleType *>(field_type);
				if (sim->Class == SimpleTypeClass::Matrix && sim->Domain != SimpleTypeDomain::Float) {
					throw CompilationException(CompilationError::InappropriateType, tp, L"MSL translation supports 'float' matricies only");
				}
			}
			if (text[cp].Class != TokenClass::Identifier) throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"An identifier expected");
			auto field_name = text[cp].Content;
			for (auto & fn : type.FieldNames) if (fn == field_name)
				throw CompilationException(CompilationError::ObjectRedefinition, cp, FormatString(L"'%0' have been already used for another field", field_name));
			cp++;
			if (text[cp].Class == TokenClass::CharCombo && text[cp].Content == L"[") {
				cp++;
				uint64 vol = 0;
				if (text[cp].Class == TokenClass::Constant && text[cp].ValueClass == TokenConstantClass::Numeric && text[cp].NumericClass() == NumericTokenClass::Integer) {
					vol = text[cp].AsInteger();
					if (vol == 0 || vol > MaximalStaticArray) {
						throw CompilationException(CompilationError::InvalidStaticArraySize, cp, FormatString(L"The size must be in range [%0, %1]", 1, MaximalStaticArray));
					}
					cp++;
					if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L"]")
						throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"']' expected");
					cp++;
				} else throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"An integer value expected");
				type.FieldNames << field_name;
				type.FieldTranslateNames << L"egsl_field_" + field_name;
				type.FieldTypes.Append(static_cast<SimpleType *>(field_type));
				type.FieldArraySizes << vol;
				type.FieldModes << mode;
			} else {
				type.FieldNames << field_name;
				type.FieldTranslateNames << L"egsl_field_" + field_name;
				type.FieldTypes.Append(static_cast<SimpleType *>(field_type));
				type.FieldArraySizes << 0;
				type.FieldModes << mode;
				while (text[cp].Class == TokenClass::CharCombo && text[cp].Content == L",") {
					cp++;
					if (text[cp].Class != TokenClass::Identifier) throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"An identifier expected");
					field_name = text[cp].Content;
					for (auto & fn : type.FieldNames) if (fn == field_name)
						throw CompilationException(CompilationError::ObjectRedefinition, cp, FormatString(L"'%0' have been already used for another field", field_name));
					cp++;
					type.FieldNames << field_name;
					type.FieldTranslateNames << L"egsl_field_" + field_name;
					type.FieldTypes.Append(static_cast<SimpleType *>(field_type));
					type.FieldArraySizes << 0;
					type.FieldModes << mode;
				}
			}
			if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L";")
				throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"';' expected");
			cp++;
		}
		void ParseStructure(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context)
		{
			if (text[cp].Class != TokenClass::Identifier) throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"An identifier expected");
			SafePointer<StructureType> type = new StructureType;
			type->IsExternal = false;
			type->Name = text[cp].Content;
			type->TranslateName = L"egsl_type_" + type->Name;
			for (auto & s : context.StructureTypes) if (s.Name == type->Name)
				throw CompilationException(CompilationError::ObjectRedefinition, cp, FormatString(L"'%0' have been already used for a structure", type->Name));
			for (auto & s : context.Shaders) if (s.Name == type->Name)
				throw CompilationException(CompilationError::ObjectRedefinition, cp, FormatString(L"'%0' have been already used for a shader", type->Name));
			cp++;
			if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L"{")
				throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"'{' expected");
			cp++;
			while (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L"}") ParseFieldsDefinition(text, cp, context, *type);
			cp++;
			context.StructureTypes.Append(type);
		}
		ValueSemantic ReadSemantic(const Array<Syntax::Token> & text, int & cp)
		{
			if (text[cp].Class != TokenClass::Identifier && text[cp].Class != TokenClass::Keyword)
				throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"A semantic or a register name expected");
			ValueSemantic result;
			result.Index = -1;
			uint64 max_slots = 0;
			if (text[cp].Content == L"vertex") {
				result.Semantic = SemanticClass::Vertex;
			} else if (text[cp].Content == L"instance") {
				result.Semantic = SemanticClass::Instance;
			} else if (text[cp].Content == L"position") {
				result.Semantic = SemanticClass::Position;
			} else if (text[cp].Content == L"front") {
				result.Semantic = SemanticClass::Front;
			} else if (text[cp].Content == L"color") {
				result.Semantic = SemanticClass::Color;
				result.Index = 0;
				max_slots = NumRenderTargets;
			} else if (text[cp].Content == L"depth") {
				result.Semantic = SemanticClass::Depth;
			} else if (text[cp].Content == L"stencil") {
				result.Semantic = SemanticClass::Stencil;
			} else if (text[cp].Content == L"texture") {
				result.Semantic = SemanticClass::Texture;
				result.Index = 0;
				max_slots = NumTextures;
			} else if (text[cp].Content == L"constant") {
				result.Semantic = SemanticClass::Constant;
				result.Index = 0;
				max_slots = NumConstants;
			} else if (text[cp].Content == L"buffer") {
				result.Semantic = SemanticClass::Buffer;
				result.Index = 0;
				max_slots = NumBuffers;
			} else if (text[cp].Content == L"sampler") {
				result.Semantic = SemanticClass::Sampler;
				result.Index = 0;
				max_slots = NumSamplers;
			} else throw CompilationException(CompilationError::UnknownSemantic, cp, FormatString(L"'%0' is not a valid semantic or register name", text[cp].Content));
			cp++;
			if (result.Index >= 0 && text[cp].Class == TokenClass::CharCombo && text[cp].Content == L"[") {
				cp++;
				if (text[cp].Class == TokenClass::Constant && text[cp].ValueClass == TokenConstantClass::Numeric && text[cp].NumericClass() == NumericTokenClass::Integer) {
					auto index = text[cp].AsInteger();
					if (index >= max_slots) {
						throw CompilationException(CompilationError::InvalidShaderArgumentSemanticSlot, cp, FormatString(L"The slot index must be in range [%0, %1]", 0, max_slots - 1));
					}
					result.Index = index;
					cp++;
					if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L"]")
						throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"']' expected");
					cp++;
				} else throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"An integer value expected");
			}
			return result;
		}
		void ParseShaderArgument(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext)
		{
			ShaderArgumentUsage usage = ShaderArgumentUsage::Input;
			if (text[cp].Class == TokenClass::Keyword) {
				if (text[cp].Content == L"in") {
					usage = ShaderArgumentUsage::Input;
					cp++;
				} else if (text[cp].Content == L"out") {
					usage = ShaderArgumentUsage::Output;
					cp++;
				}
			}
			auto type = ReadTypeDefinition(text, cp, context);
			int tp = cp;
			if (text[cp].Class != TokenClass::Identifier) throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"An identifier expected");
			auto arg_name = text[cp].Content;
			for (auto & an : scontext.Arguments) if (an.Name == arg_name)
				throw CompilationException(CompilationError::ObjectRedefinition, cp, FormatString(L"'%0' have been already used for another argument", arg_name));
			cp++;
			ShaderArgument arg;
			arg.Name = arg_name;
			arg.TranslateName = L"egsl_arg_" + arg_name;
			arg.TokenPosition = tp;
			arg.Type.SetRetain(type);
			arg.Usage = usage;
			arg.Semantic.Semantic = SemanticClass::NoSemantic;
			arg.Semantic.Index = -1;
			if (text[cp].Class == TokenClass::CharCombo && text[cp].Content == L":") {
				cp++;
				arg.Semantic = ReadSemantic(text, cp);
			}
			if (arg.Semantic.Semantic == SemanticClass::Vertex || arg.Semantic.Semantic == SemanticClass::Instance) {
				string sem;
				if (arg.Semantic.Semantic == SemanticClass::Vertex) sem = L"vertex";
				else if (arg.Semantic.Semantic == SemanticClass::Instance) sem = L"instance";
				if (scontext.Class != ShaderClass::Vertex)
					throw CompilationException(CompilationError::InvalidShaderArgumentSemantic, tp, FormatString(L"'%0' semantic is only valid as an input to a vertex shader", sem));
				if (arg.Usage != ShaderArgumentUsage::Input)
					throw CompilationException(CompilationError::InvalidShaderArgumentSemantic, tp, FormatString(L"'%0' semantic is only valid as an input to a vertex shader", sem));
				if (arg.Type->GetClass() != TypeClass::Simple || static_cast<SimpleType *>(arg.Type.Inner())->Name != L"uint")
					throw CompilationException(CompilationError::InvalidShaderArgumentSemantic, tp, FormatString(L"'%0' semantic must have the 'uint' type", sem));
			} else if (arg.Semantic.Semantic == SemanticClass::Position) {
				string sem = L"position";
				if (scontext.Class != ShaderClass::Vertex)
					throw CompilationException(CompilationError::InvalidShaderArgumentSemantic, tp, FormatString(L"'%0' semantic is only valid as an output from a vertex shader", sem));
				if (arg.Usage != ShaderArgumentUsage::Output)
					throw CompilationException(CompilationError::InvalidShaderArgumentSemantic, tp, FormatString(L"'%0' semantic is only valid as an output from a vertex shader", sem));
				if (arg.Type->GetClass() != TypeClass::Simple || static_cast<SimpleType *>(arg.Type.Inner())->Name != L"float4")
					throw CompilationException(CompilationError::InvalidShaderArgumentSemantic, tp, FormatString(L"'%0' semantic must have the 'float4' type", sem));
			} else if (arg.Semantic.Semantic == SemanticClass::Front) {
				string sem = L"front";
				if (scontext.Class != ShaderClass::Pixel)
					throw CompilationException(CompilationError::InvalidShaderArgumentSemantic, tp, FormatString(L"'%0' semantic is only valid as an input to a pixel shader", sem));
				if (arg.Usage != ShaderArgumentUsage::Input)
					throw CompilationException(CompilationError::InvalidShaderArgumentSemantic, tp, FormatString(L"'%0' semantic is only valid as an input to a pixel shader", sem));
				if (arg.Type->GetClass() != TypeClass::Simple || static_cast<SimpleType *>(arg.Type.Inner())->Name != L"bool")
					throw CompilationException(CompilationError::InvalidShaderArgumentSemantic, tp, FormatString(L"'%0' semantic must have the 'bool' type", sem));
			} else if (arg.Semantic.Semantic == SemanticClass::Color || arg.Semantic.Semantic == SemanticClass::Depth || arg.Semantic.Semantic == SemanticClass::Stencil) {
				string sem;
				if (arg.Semantic.Semantic == SemanticClass::Color) sem = L"color";
				else if (arg.Semantic.Semantic == SemanticClass::Depth) sem = L"depth";
				else if (arg.Semantic.Semantic == SemanticClass::Stencil) sem = L"stencil";
				if (scontext.Class != ShaderClass::Pixel)
					throw CompilationException(CompilationError::InvalidShaderArgumentSemantic, tp, FormatString(L"'%0' semantic is only valid as an output from a pixel shader", sem));
				if (arg.Usage != ShaderArgumentUsage::Output)
					throw CompilationException(CompilationError::InvalidShaderArgumentSemantic, tp, FormatString(L"'%0' semantic is only valid as an output from a pixel shader", sem));
				if (arg.Semantic.Semantic == SemanticClass::Color) {
					auto type = (arg.Type->GetClass() == TypeClass::Simple) ? static_cast<SimpleType *>(arg.Type.Inner()) : 0;
					if (!type || type->Domain != SimpleTypeDomain::Float || type->Class != SimpleTypeClass::Vector || type->Columns < 2)
						throw CompilationException(CompilationError::InvalidShaderArgumentSemantic, tp, FormatString(L"'%0' semantic must be a float vector of size 2, 3 or 4", sem));
				} else if (arg.Semantic.Semantic == SemanticClass::Depth) {
					if (arg.Type->GetClass() != TypeClass::Simple || static_cast<SimpleType *>(arg.Type.Inner())->Name != L"float")
						throw CompilationException(CompilationError::InvalidShaderArgumentSemantic, tp, FormatString(L"'%0' semantic must have the 'float' type", sem));
				} else if (arg.Semantic.Semantic == SemanticClass::Stencil) {
					if (arg.Type->GetClass() != TypeClass::Simple || static_cast<SimpleType *>(arg.Type.Inner())->Name != L"uint")
						throw CompilationException(CompilationError::InvalidShaderArgumentSemantic, tp, FormatString(L"'%0' semantic must have the 'uint' type", sem));
				}
			} else if (arg.Semantic.Semantic == SemanticClass::Constant || arg.Semantic.Semantic == SemanticClass::Texture ||
				arg.Semantic.Semantic == SemanticClass::Buffer || arg.Semantic.Semantic == SemanticClass::Sampler) {
				string sem;
				if (arg.Semantic.Semantic == SemanticClass::Constant) sem = L"constant";
				else if (arg.Semantic.Semantic == SemanticClass::Texture) sem = L"texture";
				else if (arg.Semantic.Semantic == SemanticClass::Buffer) sem = L"buffer";
				else if (arg.Semantic.Semantic == SemanticClass::Sampler) sem = L"sampler";
				if (arg.Usage != ShaderArgumentUsage::Input)
					throw CompilationException(CompilationError::InvalidShaderArgumentSemantic, tp, FormatString(L"'%0' register is only valid as an input to a shader", sem));
				if (arg.Semantic.Semantic == SemanticClass::Constant) {
					if (arg.Type->GetClass() != TypeClass::Simple && arg.Type->GetClass() != TypeClass::Structure)
						throw CompilationException(CompilationError::InvalidShaderArgumentSemantic, tp, FormatString(L"'%0' register must be attached to a scalar, a vector, a matrix or a structure", sem));
				} else if (arg.Semantic.Semantic == SemanticClass::Texture) {
					if (arg.Type->GetClass() != TypeClass::Register || static_cast<RegisterType *>(arg.Type.Inner())->Class != RegisterTypeClass::Texture)
						throw CompilationException(CompilationError::InvalidShaderArgumentSemantic, tp, FormatString(L"'%0' register must be attached to a texture", sem));
				} else if (arg.Semantic.Semantic == SemanticClass::Buffer) {
					if (arg.Type->GetClass() != TypeClass::Register || static_cast<RegisterType *>(arg.Type.Inner())->Class != RegisterTypeClass::Array)
						throw CompilationException(CompilationError::InvalidShaderArgumentSemantic, tp, FormatString(L"'%0' register must be attached to an array", sem));
				} else if (arg.Semantic.Semantic == SemanticClass::Sampler) {
					if (arg.Type->GetClass() != TypeClass::Register || static_cast<RegisterType *>(arg.Type.Inner())->Class != RegisterTypeClass::Sampler)
						throw CompilationException(CompilationError::InvalidShaderArgumentSemantic, tp, FormatString(L"'%0' register must be attached to a sampler", sem));
				}
			}
			if (arg.Type->GetClass() != TypeClass::Structure) {
				if (arg.Semantic.Semantic == SemanticClass::NoSemantic)
					throw CompilationException(CompilationError::InvalidShaderArgumentSemantic, tp, L"Non-structure argument must have a semantic or be attached to a register");
				if (arg.Type->GetClass() == TypeClass::Register && static_cast<RegisterType *>(arg.Type.Inner())->InnerType &&
					static_cast<RegisterType *>(arg.Type.Inner())->InnerType->GetClass() == TypeClass::Structure) {
					static_cast<StructureType *>(static_cast<RegisterType *>(arg.Type.Inner())->InnerType.Inner())->IsExternal = true;
				}
			} else {
				if (arg.Semantic.Semantic == SemanticClass::NoSemantic) {
					auto str = static_cast<StructureType *>(arg.Type.Inner());
					for (int i = 0; i < str->FieldNames.Length(); i++) if (str->FieldTypes[i].Domain != SimpleTypeDomain::Float || str->FieldTypes[i].Class == SimpleTypeClass::Matrix)
						throw CompilationException(CompilationError::InvalidShaderArgumentSemantic, tp, L"A structure argument without a register must have only scalar and vector float fields");
					if (scontext.Class == ShaderClass::Vertex) {
						if (arg.Usage != ShaderArgumentUsage::Output)
							throw CompilationException(CompilationError::InvalidShaderArgumentSemantic, tp, L"A structure argument without a register must be an output from a vertex shader");
					} else if (scontext.Class == ShaderClass::Pixel) {
						if (arg.Usage != ShaderArgumentUsage::Input)
							throw CompilationException(CompilationError::InvalidShaderArgumentSemantic, tp, L"A structure argument without a register must be an input to a pixel shader");
					}
				} else static_cast<StructureType *>(arg.Type.Inner())->IsExternal = true;
			}
			scontext.Arguments.Append(arg);
		}
		void ParseShaderArguments(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext)
		{
			int sp = cp + 1;
			while (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L")") {
				if (text[cp].Class != TokenClass::CharCombo || (text[cp].Content != L"(" && text[cp].Content != L","))
					throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"',' expected");
				cp++;
				ParseShaderArgument(text, cp, context, scontext);
			}
			cp++;
			bool ret_present = false;
			for (auto & a : scontext.Arguments) {
				if (a.Semantic.Semantic == SemanticClass::Position || a.Semantic.Semantic == SemanticClass::Color) ret_present = true;
				for (auto & b : scontext.Arguments) {
					if (&a == &b) continue;
					if (a.Semantic.Semantic == b.Semantic.Semantic && a.Semantic.Index == b.Semantic.Index)
						throw CompilationException(CompilationError::DuplicateSemantic, a.TokenPosition,
							FormatString(L"Shader arguments '%0' and '%1' have the same IO specifications", a.Name, b.Name));
				}
			}
			if (!ret_present) throw CompilationException(CompilationError::SemanticMissing, sp,
				L"Shader must return a value (use 'position' or 'color' semantics)");
		}

		void CheckMetalArithmeticLimitations(CompilerCommonContext & context, LanguageType * type1, LanguageType * type2, const string & op, int at)
		{
			SimpleType * s1 = (type1->GetClass() == TypeClass::Simple) ? static_cast<SimpleType *>(type1) : 0;
			SimpleType * s2 = (type1->GetClass() == TypeClass::Simple) ? static_cast<SimpleType *>(type2) : 0;
			if (s1 && s2 && (s1->Class == SimpleTypeClass::Matrix || s2->Class == SimpleTypeClass::Matrix)) {
				if (op != L"+" && op != L"-" && op != "*") {
					string text = FormatString(L"SIMD '%0' is not available for matricies in MSL", op);
					if (context.Target == OutputTarget::Metal) {
						throw CompilationException(CompilationError::OperatorIsNotApplicable, at, text);
					} else context.hints.Append(text);
				}
				if (op == L"*") {
					bool ms = false;
					if (s1->Class == SimpleTypeClass::Matrix && s2->Class == SimpleTypeClass::Scalar) ms = true;
					else if (s2->Class == SimpleTypeClass::Matrix && s1->Class == SimpleTypeClass::Scalar) ms = true;
					if (!ms) {
						string text = FormatString(L"Operator '*' in MSL takes only matrix-scalar arguments", op);
						if (context.Target == OutputTarget::Metal) {
							throw CompilationException(CompilationError::OperatorIsNotApplicable, at, text);
						} else context.hints.Append(text);
					}
				}
			}
		}
		void CheckArithmeticOperation(LanguageType * type, const string & op, int at)
		{
			if (type->GetClass() != TypeClass::Simple)
				throw CompilationException(CompilationError::OperatorIsNotApplicable, at, L"Arithmetical operators are only supported on scalars, vectors and matricies");
			auto sim = static_cast<SimpleType *>(type);
			if (op == L"&" || op == L"|" || op == L"^" || op == L"<<" || op == L">>" || op == L"~") {
				if (sim->Domain == SimpleTypeDomain::Float)
					throw CompilationException(CompilationError::OperatorIsNotApplicable, at, L"Bitwise operators are not supported on float types");
			}
		}
		void CheckSymmetricAutocast(ValueDescriptor d1, ValueDescriptor d2, ValueDescriptor * result, int throw_at)
		{
			if (d1.ArraySize != d2.ArraySize) throw CompilationException(CompilationError::ExpressionTypeMismatch, throw_at,
				FormatString(L"Static array size mismatch: '%0[%2]' and '%1[%3]'", d1.Type->ToString(), d2.Type->ToString(), d1.ArraySize, d2.ArraySize));
			if (d1.Type->GetClass() == TypeClass::Simple && d2.Type->GetClass() == TypeClass::Simple && d1.ArraySize == 0) {
				auto s1 = static_cast<SimpleType *>(d1.Type);
				auto s2 = static_cast<SimpleType *>(d2.Type);
				if (s1->Class == SimpleTypeClass::Scalar || s2->Class == SimpleTypeClass::Scalar) {
					if (s1->Class == SimpleTypeClass::Scalar && s2->Class == SimpleTypeClass::Scalar) {
						if (result) {
							if (s1->Domain == SimpleTypeDomain::Float || s2->Domain == SimpleTypeDomain::Float) {
								if (s1->Domain == SimpleTypeDomain::Float) *result = d1;
								else *result = d2;
							} else if (s1->Domain == SimpleTypeDomain::Integer || s2->Domain == SimpleTypeDomain::Integer) {
								if (s1->Domain == SimpleTypeDomain::Integer) *result = d1;
								else *result = d2;
							} else if (s1->Domain == SimpleTypeDomain::UnsignedInteger || s2->Domain == SimpleTypeDomain::UnsignedInteger) {
								if (s1->Domain == SimpleTypeDomain::UnsignedInteger) *result = d1;
								else *result = d2;
							} else *result = d1;
						}
					} else {
						if (s1->Class == SimpleTypeClass::Scalar) { if (result) *result = d2; }
						else { if (result) *result = d1; }
					}
					return;
				} else {
					if (s1->Domain != s2->Domain || s1->Class != s2->Class || s1->Columns != s2->Columns || s1->Rows != s2->Rows)
						throw CompilationException(CompilationError::ExpressionTypeMismatch, throw_at,
						FormatString(L"Type mismatch: expression types '%0' and '%1' are not compatible", d1.Type->ToString(), d2.Type->ToString()));
					if (result) *result = d1;
				}
			} else {
				if (d1.Type->ToString() != d1.Type->ToString()) {
					if (d1.ArraySize) {
						throw CompilationException(CompilationError::ExpressionTypeMismatch, throw_at,
							FormatString(L"Type mismatch: static array types '%0' and '%1' are not compatible", d1.Type->ToString(), d1.Type->ToString()));
					} else {
						throw CompilationException(CompilationError::ExpressionTypeMismatch, throw_at,
							FormatString(L"Type mismatch: expression types '%0' and '%1' are not compatible", d1.Type->ToString(), d1.Type->ToString()));
					}
				}
				if (result) *result = d1;
			}
		}
		bool CheckSymmetricAutocast(ValueDescriptor d1, ValueDescriptor d2, ValueDescriptor * result)
		{
			try { CheckSymmetricAutocast(d1, d2, result, 0); } catch (...) { return false; }
			return true;
		}
		void CheckAsymmetricAutocast(ValueDescriptor from, ValueDescriptor to, int throw_at)
		{
			if (from.ArraySize != to.ArraySize) throw CompilationException(CompilationError::ExpressionTypeMismatch, throw_at,
				FormatString(L"Static array size mismatch: '%0[%2]' and '%1[%3]'", from.Type->ToString(), to.Type->ToString(), from.ArraySize, to.ArraySize));
			if (from.Type->GetClass() == TypeClass::Simple && to.Type->GetClass() == TypeClass::Simple && from.ArraySize == 0) {
				auto f = static_cast<SimpleType *>(from.Type);
				auto t = static_cast<SimpleType *>(to.Type);
				if (f->Class == SimpleTypeClass::Scalar) {
					return;
				} else {
					if (f->Domain != t->Domain || f->Class != t->Class || f->Columns != t->Columns || f->Rows != t->Rows)
						throw CompilationException(CompilationError::ExpressionTypeMismatch, throw_at,
						FormatString(L"Type mismatch: expression is '%0', required '%1'", from.Type->ToString(), to.Type->ToString()));
				}
			} else if (from.Type->ToString() != to.Type->ToString()) throw CompilationException(CompilationError::ExpressionTypeMismatch, throw_at,
				FormatString(L"Type mismatch: expression is '%0', required '%1'", from.Type->ToString(), to.Type->ToString()));
		}
		bool CheckAsymmetricAutocast(ValueDescriptor from, ValueDescriptor to)
		{
			try { CheckAsymmetricAutocast(from, to, 0); } catch (...) { return false; }
			return true;
		}
		void CheckIdentity(ValueDescriptor d1, ValueDescriptor d2, int throw_at)
		{
			if (d1.ArraySize != d2.ArraySize) throw CompilationException(CompilationError::ExpressionTypeMismatch, throw_at,
				FormatString(L"Static array size mismatch: '%0[%2]' and '%1[%3]'", d1.Type->ToString(), d2.Type->ToString(), d1.ArraySize, d2.ArraySize));
			if (d1.Type->ToString() != d2.Type->ToString()) throw CompilationException(CompilationError::ExpressionTypeMismatch, throw_at,
				FormatString(L"Type mismatch: expression types '%0' and '%1' are not identical", d1.Type->ToString(), d1.Type->ToString()));
		}
		bool CheckIdentity(ValueDescriptor d1, ValueDescriptor d2)
		{
			try { CheckIdentity(d1, d2, 0); } catch (...) { return false; }
			return true;
		}
		bool FindConstant(const string & name, float & result)
		{
			if (name == L"C_PI") {
				result = ENGINE_PI;
				return true;
			} else if (name == L"C_2PI") {
				result = 2.0 * ENGINE_PI;
				return true;
			} else if (name == L"C_PI2") {
				result = ENGINE_PI / 2.0;
				return true;
			} else if (name == L"C_PI3") {
				result = ENGINE_PI / 3.0;
				return true;
			} else if (name == L"C_PI4") {
				result = ENGINE_PI / 4.0;
				return true;
			} else if (name == L"C_PI6") {
				result = ENGINE_PI / 6.0;
				return true;
			} else if (name == L"C_E") {
				result = ENGINE_E;
				return true;
			} else return false;
		}
		string TranslateExpression(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext, const ValueDescriptor * req_ret_desc, ValueDescriptor * ret_desc);
		string TranslateRootLevel(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext, ValueDescriptor * ret_desc)
		{
			if (text[cp].Class == TokenClass::CharCombo && text[cp].Content == L"(") {
				cp++;
				auto val = TranslateExpression(text, cp, context, scontext, 0, ret_desc);
				if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L")")
					throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"')' expected");
				cp++;
				if (ret_desc) ret_desc->IsAssignable = false;
				return L"(" + val + L")";
			} else if (text[cp].Class == TokenClass::Constant) {
				if (text[cp].ValueClass == TokenConstantClass::Numeric && text[cp].NumericClass() != NumericTokenClass::Integer) {
					if (ret_desc) {
						ret_desc->Type = FindType(L"float", context);
						ret_desc->ArraySize = 0;
						ret_desc->IsAssignable = false;
					}
					auto val = string(text[cp].AsFloat());
					if (val.FindFirst(L'.') == -1) val += L".0";
					val += L"f";
					cp++;
					return val;
				} else if (text[cp].ValueClass == TokenConstantClass::Numeric && text[cp].NumericClass() == NumericTokenClass::Integer) {
					if (ret_desc) {
						ret_desc->Type = FindType(L"int", context);
						ret_desc->ArraySize = 0;
						ret_desc->IsAssignable = false;
					}
					auto val = string(text[cp].AsInteger());
					cp++;
					return val;
				} else if (text[cp].ValueClass == TokenConstantClass::Boolean) {
					if (ret_desc) {
						ret_desc->Type = FindType(L"bool", context);
						ret_desc->ArraySize = 0;
						ret_desc->IsAssignable = false;
					}
					auto val = (text[cp].AsBoolean()) ? L"true" : L"false";
					cp++;
					return val;
				} else throw CompilationException(CompilationError::InvalidToken, cp, L"Strings are not supported");
			} else {
				if (text[cp].Class == TokenClass::Identifier) {
					LocalVariable * lvt = 0;
					for (auto & lvl : scontext.Locals.InversedElements()) {
						for (auto & lv : lvl) if (lv.Name == text[cp].Content) { lvt = &lv; break; }
						if (lvt) break;
					}
					ShaderArgument * sa = 0;
					if (!lvt) for (auto & la : scontext.Arguments) if (la.Name == text[cp].Content) { sa = &la; break; }
					if (lvt) {
						if (ret_desc) {
							ret_desc->ArraySize = lvt->ArraySize;
							ret_desc->IsAssignable = true;
							ret_desc->Type = lvt->Type;
						}
						cp++;
						return lvt->TranslateName;
					} else if (sa) {
						if (ret_desc) {
							ret_desc->ArraySize = 0;
							ret_desc->IsAssignable = true;
							ret_desc->Type = sa->Type;
						}
						cp++;
						return sa->TranslateName;
					}
				}
				if (text[cp].Class == TokenClass::Identifier || text[cp].Class == TokenClass::Keyword) {
					auto fp = cp;
					auto name = text[cp].Content;
					auto trans = FindIntrinsicTranslator(context, name);
					if (!trans) {
						float result;
						if (FindConstant(name, result)) {
							if (ret_desc) {
								ret_desc->Type = FindType(L"float", context);
								ret_desc->ArraySize = 0;
								ret_desc->IsAssignable = false;
							}
							auto val = string(result);
							if (val.FindFirst(L'.') == -1) val += L".0";
							val += L"f";
							cp++;
							return val;
						} else throw CompilationException(CompilationError::UnknownIdentifier, cp,
							FormatString(L"'%0' is not a name for a function, a type, a variable, a constant or an argument", text[cp].Content));
					}
					cp++;
					if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L"(")
						throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"'(' expected");
					cp++;
					IntrinsicTranslateContext tcontext(text, cp, context, scontext, fp);
					auto val = trans(name, tcontext);
					if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L")")
						throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"')' expected");
					cp++;
					if (ret_desc) *ret_desc = tcontext.GetReturnValue();
					return val;
				} else throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"Expected either a constant literal, a function, a type, a variable or an argument name");
			}
		}
		string TranslateSubscriptLevel(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext, ValueDescriptor * ret_desc)
		{
			ValueDescriptor root;
			DynamicString out;
			out << TranslateRootLevel(text, cp, context, scontext, &root);
			while (true) {
				if (text[cp].Class == TokenClass::CharCombo && text[cp].Content == L".") {
					cp++;
					if (root.ArraySize) throw CompilationException(CompilationError::OperatorIsNotApplicable, cp - 1, L"Array does not support '.' operator");
					if (root.Type->GetClass() == TypeClass::Structure) {
						auto type = static_cast<StructureType *>(root.Type);
						if (text[cp].Class != TokenClass::Identifier) throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"An identifier expected");
						auto field = text[cp].Content;
						bool found = false;
						for (int i = 0; i < type->FieldNames.Length(); i++) if (type->FieldNames[i] == field) {
							out << L"." << type->FieldTranslateNames[i];
							root.ArraySize = type->FieldArraySizes[i];
							root.Type = type->FieldTypes.ElementAt(i);
							found = true;
						}
						if (!found) throw CompilationException(CompilationError::UnknownIdentifier, cp, L"Unknown field identifier");
						cp++;
					} else if (root.Type->GetClass() == TypeClass::Simple && static_cast<SimpleType *>(root.Type)->Class == SimpleTypeClass::Vector) {
						auto type = static_cast<SimpleType *>(root.Type);
						if (text[cp].Class != TokenClass::Identifier) throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"An identifier expected");
						auto field = text[cp].Content;
						if (field.Length() > 4) throw CompilationException(CompilationError::UnknownIdentifier, cp, L"Unknown field identifier");
						int sdim = type->Columns;
						int dim = field.Length();
						DynamicString field_norm;
						for (int i = 0; i < field.Length(); i++) {
							if (field[i] == L'x') field_norm << L'x';
							else if (field[i] == L'y' && sdim > 1) field_norm << L'y';
							else if (field[i] == L'z' && sdim > 2) field_norm << L'z';
							else if (field[i] == L'w' && sdim > 3) field_norm << L'w';
							else if (field[i] == L'r') field_norm << L'x';
							else if (field[i] == L'g' && sdim > 1) field_norm << L'y';
							else if (field[i] == L'b' && sdim > 2) field_norm << L'z';
							else if (field[i] == L'a' && sdim > 3) field_norm << L'w';
							else if (field[i] == L'u') field_norm << L'x';
							else if (field[i] == L'v' && sdim > 1) field_norm << L'y';
							else throw CompilationException(CompilationError::UnknownIdentifier, cp, L"Unknown field identifier");
						}
						bool duplicate = false;
						for (int i = 1; i < dim; i++) for (int j = 0; j < i; j++) if (field_norm[i] == field_norm[j]) duplicate = true;
						if (root.IsAssignable) root.IsAssignable = !duplicate;
						if (dim == 1) {
							for (auto & s : context.SimpleTypes) {
								if (s.Domain == type->Domain && s.Class == SimpleTypeClass::Scalar) {
									root.Type = &s;
									break;
								}
							}
						} else {
							for (auto & s : context.SimpleTypes) {
								if (s.Domain == type->Domain && s.Class == SimpleTypeClass::Vector && s.Columns == dim) {
									root.Type = &s;
									break;
								}
							}
						}
						cp++;
						out << L"." << field_norm.ToString();
					} else throw CompilationException(CompilationError::OperatorIsNotApplicable, cp - 1, L"This type does not support '.' operator");
				} else if (text[cp].Class == TokenClass::CharCombo && text[cp].Content == L"[") {
					cp++;
					if (root.ArraySize) {
						ValueDescriptor req;
						req.ArraySize = 0;
						req.IsAssignable = false;
						req.Type = FindType(L"int", context);
						auto val = TranslateExpression(text, cp, context, scontext, &req);
						root.ArraySize = 0;
						out << L"[" << val << L"]";
					} else if (root.Type->GetClass() == TypeClass::Register && static_cast<RegisterType *>(root.Type)->Class == RegisterTypeClass::Array) {
						auto type = static_cast<RegisterType *>(root.Type);
						ValueDescriptor req;
						req.ArraySize = 0;
						req.IsAssignable = false;
						req.Type = FindType(L"int", context);
						auto val = TranslateExpression(text, cp, context, scontext, &req);
						root.IsAssignable = false;
						root.Type = type->InnerType;
						out << L"[" << val << L"]";
					} else if (root.Type->GetClass() == TypeClass::Register && static_cast<RegisterType *>(root.Type)->Class == RegisterTypeClass::Texture) {
						auto type = static_cast<RegisterType *>(root.Type);
						ValueDescriptor req;
						req.ArraySize = 0;
						req.IsAssignable = false;
						if (type->Dimensions == 0) req.Type = FindType(L"int2", context);
						else if (type->Dimensions == 1) req.Type = FindType(L"int3", context);
						else if (type->Dimensions == 4) req.Type = FindType(L"int3", context);
						else if (type->Dimensions == 3) req.Type = FindType(L"int4", context);
						else if (type->Dimensions == 5) req.Type = FindType(L"int4", context);
						else throw CompilationException(CompilationError::OperatorIsNotApplicable, cp - 1, L"Cube textures does not support '[]' operator");
						auto val = TranslateExpression(text, cp, context, scontext, &req);
						root.IsAssignable = false;
						root.Type = type->InnerType;
						if (context.Target == OutputTarget::Metal) {
							if (type->Dimensions == 0) out << L".read(uint(" << val << L".x), 0)";
							else if (type->Dimensions == 1) out << L".read(uint2(" << val << L".xy), uint(" << val << L".z))";
							else if (type->Dimensions == 3) out << L".read(uint3(" << val << L".xyz), uint(" << val << L".w))";
							else if (type->Dimensions == 4) out << L".read(uint(" << val << L".x), uint(" << val << L".y), 0)";
							else if (type->Dimensions == 5) out << L".read(uint2(" << val << L".xy), uint(" << val << L".z), uint(" << val << L".w))";
							auto sim = static_cast<SimpleType *>(type->InnerType.Inner());
							if (sim->Class == SimpleTypeClass::Scalar) out << L".x";
							else if (sim->Class == SimpleTypeClass::Vector && sim->Columns == 2) out << L".xy";
							else if (sim->Class == SimpleTypeClass::Vector && sim->Columns == 3) out << L".xyz";
						} else {
							out << L".Load(" << val << L")";
						}
					} else if (root.Type->GetClass() == TypeClass::Simple && static_cast<SimpleType *>(root.Type)->Class == SimpleTypeClass::Matrix) {
						auto type = static_cast<SimpleType *>(root.Type);
						ValueDescriptor req;
						req.ArraySize = 0;
						req.IsAssignable = false;
						req.Type = FindType(L"int", context);
						auto val = TranslateExpression(text, cp, context, scontext, &req);
						for (auto & s : context.SimpleTypes) {
							if (s.Domain == type->Domain && s.Class == SimpleTypeClass::Vector && s.Columns == type->Columns) {
								root.Type = &s;
								break;
							}
						}
						out << L"[" << val << L"]";
					} else if (root.Type->GetClass() == TypeClass::Simple && static_cast<SimpleType *>(root.Type)->Class == SimpleTypeClass::Vector) {
						auto type = static_cast<SimpleType *>(root.Type);
						ValueDescriptor req;
						req.ArraySize = 0;
						req.IsAssignable = false;
						req.Type = FindType(L"int", context);
						auto val = TranslateExpression(text, cp, context, scontext, &req);
						for (auto & s : context.SimpleTypes) {
							if (s.Domain == type->Domain && s.Class == SimpleTypeClass::Scalar) {
								root.Type = &s;
								break;
							}
						}
						out << L"[" << val << L"]";
					} else throw CompilationException(CompilationError::OperatorIsNotApplicable, cp - 1, L"This type does not support '[]' operator");
					if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L"]")
						throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"']' expected");
					cp++;
				} else if (text[cp].Class == TokenClass::CharCombo && text[cp].Content == L"++") {
					if (!root.IsAssignable) throw CompilationException(CompilationError::ExpressionIsNotAssignable, cp, L"");
					if (root.ArraySize) throw CompilationException(CompilationError::OperatorIsNotApplicable, cp, L"Arithmetic is not supported on arrays");
					CheckArithmeticOperation(root.Type, text[cp].Content, cp);
					CheckMetalArithmeticLimitations(context, root.Type, root.Type, L"++", cp);
					cp++;
					out << L"++";
					root.IsAssignable = false;
				} else if (text[cp].Class == TokenClass::CharCombo && text[cp].Content == L"--") {
					if (!root.IsAssignable) throw CompilationException(CompilationError::ExpressionIsNotAssignable, cp, L"");
					if (root.ArraySize) throw CompilationException(CompilationError::OperatorIsNotApplicable, cp, L"Arithmetic is not supported on arrays");
					CheckArithmeticOperation(root.Type, text[cp].Content, cp);
					CheckMetalArithmeticLimitations(context, root.Type, root.Type, L"--", cp);
					cp++;
					out << L"--";
					root.IsAssignable = false;
				} else break;
			}
			if (ret_desc) *ret_desc = root;
			return out.ToString();
		}
		string TranslateUnaryLevel(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext, ValueDescriptor * ret_desc)
		{
			if (text[cp].Class == TokenClass::CharCombo &&
				(text[cp].Content == L"+" || text[cp].Content == L"-" || text[cp].Content == L"!" || text[cp].Content == L"~")) {
				auto op = cp;
				auto o = text[cp].Content;
				cp++;
				ValueDescriptor desc;
				auto val = TranslateUnaryLevel(text, cp, context, scontext, &desc);
				CheckMetalArithmeticLimitations(context, desc.Type, desc.Type, o, op);
				if (desc.ArraySize) throw CompilationException(CompilationError::OperatorIsNotApplicable, op, L"Arithmetic is not supported on arrays");
				CheckArithmeticOperation(desc.Type, o, op);
				desc.IsAssignable = false;
				if (o == L"!") {
					auto s = static_cast<SimpleType *>(desc.Type);
					for (auto & e : context.SimpleTypes) {
						if (e.Domain == SimpleTypeDomain::Boolean && e.Class == s->Class && e.Columns == s->Columns && e.Rows == s->Rows) {
							desc.Type = &e; break;
						}
					}
				}
				if (ret_desc) *ret_desc = desc;
				return o + L"(" + val + L")";
			} else return TranslateSubscriptLevel(text, cp, context, scontext, ret_desc);
		}
		string TranslateMultiplyLevel(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext, ValueDescriptor * ret_desc)
		{
			ValueDescriptor left;
			auto val = TranslateUnaryLevel(text, cp, context, scontext, &left);
			bool first = true;
			while (text[cp].Class == TokenClass::CharCombo &&
				(text[cp].Content == L"*" || text[cp].Content == L"/" || text[cp].Content == L"%" || text[cp].Content == L"&")) {
				auto o = text[cp].Content;
				left.IsAssignable = false;
				auto op = cp;
				cp++;
				ValueDescriptor right;
				auto with = TranslateUnaryLevel(text, cp, context, scontext, &right);
				if (first) { val = L"(" + val + L")"; first = false; }
				val += L" " + o + L" (" + with + L")";
				if (left.ArraySize || right.ArraySize)
					throw CompilationException(CompilationError::OperatorIsNotApplicable, op, L"Arithmetic is not supported on arrays");
				CheckMetalArithmeticLimitations(context, left.Type, right.Type, o, op);
				CheckArithmeticOperation(left.Type, o, op);
				ValueDescriptor common;
				CheckSymmetricAutocast(left, right, &common, op);
				left.ArraySize = common.ArraySize;
				left.Type = common.Type;
			}
			if (ret_desc) *ret_desc = left;
			return val;
		}
		string TranslateAddLevel(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext, ValueDescriptor * ret_desc)
		{
			ValueDescriptor left;
			auto val = TranslateMultiplyLevel(text, cp, context, scontext, &left);
			bool first = true;
			while (text[cp].Class == TokenClass::CharCombo &&
				(text[cp].Content == L"+" || text[cp].Content == L"-" || text[cp].Content == L"|" ||
				text[cp].Content == L"^" || text[cp].Content == L"<<" || text[cp].Content == L">>")) {
				auto o = text[cp].Content;
				left.IsAssignable = false;
				auto op = cp;
				cp++;
				ValueDescriptor right;
				auto with = TranslateMultiplyLevel(text, cp, context, scontext, &right);
				if (first) { val = L"(" + val + L")"; first = false; }
				val += L" " + o + L" (" + with + L")";
				if (left.ArraySize || right.ArraySize)
					throw CompilationException(CompilationError::OperatorIsNotApplicable, op, L"Arithmetic is not supported on arrays");
				CheckMetalArithmeticLimitations(context, left.Type, right.Type, o, op);
				CheckArithmeticOperation(left.Type, o, op);
				ValueDescriptor common;
				CheckSymmetricAutocast(left, right, &common, op);
				left.ArraySize = common.ArraySize;
				left.Type = common.Type;
			}
			if (ret_desc) *ret_desc = left;
			return val;
		}
		string TranslateCompareLevel(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext, ValueDescriptor * ret_desc)
		{
			ValueDescriptor left;
			auto val = TranslateAddLevel(text, cp, context, scontext, &left);
			bool first = true;
			while (text[cp].Class == TokenClass::CharCombo &&
				(text[cp].Content == L"==" || text[cp].Content == L"!=" || text[cp].Content == L"<" ||
				text[cp].Content == L">" || text[cp].Content == L"<=" || text[cp].Content == L">=")) {
				auto o = text[cp].Content;
				left.IsAssignable = false;
				auto op = cp;
				cp++;
				ValueDescriptor right;
				auto with = TranslateAddLevel(text, cp, context, scontext, &right);
				if (first) { val = L"(" + val + L")"; first = false; }
				val += L" " + o + L" (" + with + L")";
				if (left.ArraySize || right.ArraySize)
					throw CompilationException(CompilationError::OperatorIsNotApplicable, op, L"Comparison is not supported on arrays");
				if (left.Type->GetClass() != TypeClass::Simple || right.Type->GetClass() != TypeClass::Simple)
					throw CompilationException(CompilationError::OperatorIsNotApplicable, op, L"Comparison is only supported on scalars, vectors and matricies");
				CheckMetalArithmeticLimitations(context, left.Type, right.Type, o, op);
				auto sl = static_cast<SimpleType *>(left.Type);
				auto sr = static_cast<SimpleType *>(right.Type);
				if (sl->Class != sr->Class || sl->Columns != sr->Columns || sl->Rows != sr->Rows)
					throw CompilationException(CompilationError::ExpressionTypeMismatch, op, L"Dimension mismatch");
				for (auto & s : context.SimpleTypes) {
					if (s.Domain == SimpleTypeDomain::Boolean && s.Class == sl->Class && s.Columns == sl->Columns && s.Rows == sl->Rows) {
						left.Type = &s; break;
					}
				}
			}
			if (ret_desc) *ret_desc = left;
			return val;
		}
		string TranslateAndLevel(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext, ValueDescriptor * ret_desc)
		{
			ValueDescriptor left;
			auto val = TranslateCompareLevel(text, cp, context, scontext, &left);
			bool first = true;
			while (text[cp].Class == TokenClass::CharCombo && text[cp].Content == L"&&") {
				left.IsAssignable = false;
				auto op = cp;
				cp++;
				ValueDescriptor right;
				auto with = TranslateCompareLevel(text, cp, context, scontext, &right);
				if (first) { val = L"(" + val + L")"; first = false; }
				val += L" && (" + with + L")";
				if (left.ArraySize || right.ArraySize)
					throw CompilationException(CompilationError::OperatorIsNotApplicable, op, L"Logical 'and' is not supported on arrays");
				if (left.Type->GetClass() != TypeClass::Simple || right.Type->GetClass() != TypeClass::Simple)
					throw CompilationException(CompilationError::OperatorIsNotApplicable, op, L"Logical 'and' is only supported on scalars, vectors and matricies");
				CheckMetalArithmeticLimitations(context, left.Type, right.Type, L"&&", op);
				auto sl = static_cast<SimpleType *>(left.Type);
				auto sr = static_cast<SimpleType *>(right.Type);
				if (sl->Class != sr->Class || sl->Columns != sr->Columns || sl->Rows != sr->Rows)
					throw CompilationException(CompilationError::ExpressionTypeMismatch, op, L"Dimension mismatch");
				for (auto & s : context.SimpleTypes) {
					if (s.Domain == SimpleTypeDomain::Boolean && s.Class == sl->Class && s.Columns == sl->Columns && s.Rows == sl->Rows) {
						left.Type = &s; break;
					}
				}
			}
			if (ret_desc) *ret_desc = left;
			return val;
		}
		string TranslateOrLevel(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext, ValueDescriptor * ret_desc)
		{
			ValueDescriptor left;
			auto val = TranslateAndLevel(text, cp, context, scontext, &left);
			bool first = true;
			while (text[cp].Class == TokenClass::CharCombo && text[cp].Content == L"||") {
				left.IsAssignable = false;
				auto op = cp;
				cp++;
				ValueDescriptor right;
				auto with = TranslateAndLevel(text, cp, context, scontext, &right);
				if (first) { val = L"(" + val + L")"; first = false; }
				val += L" || (" + with + L")";
				if (left.ArraySize || right.ArraySize)
					throw CompilationException(CompilationError::OperatorIsNotApplicable, op, L"Logical 'or' is not supported on arrays");
				if (left.Type->GetClass() != TypeClass::Simple || right.Type->GetClass() != TypeClass::Simple)
					throw CompilationException(CompilationError::OperatorIsNotApplicable, op, L"Logical 'or' is only supported on scalars, vectors and matricies");
				CheckMetalArithmeticLimitations(context, left.Type, right.Type, L"||", op);
				auto sl = static_cast<SimpleType *>(left.Type);
				auto sr = static_cast<SimpleType *>(right.Type);
				if (sl->Class != sr->Class || sl->Columns != sr->Columns || sl->Rows != sr->Rows)
					throw CompilationException(CompilationError::ExpressionTypeMismatch, op, L"Dimension mismatch");
				for (auto & s : context.SimpleTypes) {
					if (s.Domain == SimpleTypeDomain::Boolean && s.Class == sl->Class && s.Columns == sl->Columns && s.Rows == sl->Rows) {
						left.Type = &s; break;
					}
				}
			}
			if (ret_desc) *ret_desc = left;
			return val;
		}
		string TranslateTrinaryLevel(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext, ValueDescriptor * ret_desc)
		{
			ValueDescriptor left;
			auto sp = cp;
			auto val = TranslateOrLevel(text, cp, context, scontext, &left);
			if (text[cp].Class == TokenClass::CharCombo && text[cp].Content == L"?") {
				cp++;
				ValueDescriptor case1, case2;
				auto val1 = TranslateOrLevel(text, cp, context, scontext, &case1);
				if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L":")
					throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"':' expected");
				auto tp = cp;
				cp++;
				auto val2 = TranslateOrLevel(text, cp, context, scontext, &case2);
				ValueDescriptor bool_desc;
				bool_desc.ArraySize = 0;
				bool_desc.Type = FindType(L"bool", context);
				bool_desc.IsAssignable = false;
				CheckAsymmetricAutocast(left, bool_desc, sp);
				CheckIdentity(case1, case2, tp);
				case1.IsAssignable = false;
				if (ret_desc) *ret_desc = case1;
				return val + L" ? " + val1 + L" : " + val2;
			} else {
				if (ret_desc) *ret_desc = left;
				return val;
			}
		}
		string TranslateExpression(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext, const ValueDescriptor * req_ret_desc, ValueDescriptor * ret_desc)
		{
			ValueDescriptor left;
			auto xs = cp;
			DynamicString out;
			out << TranslateTrinaryLevel(text, cp, context, scontext, &left);
			if (text[cp].Class == TokenClass::CharCombo) {
				if (text[cp].Content == L"=" || text[cp].Content == L"+=" || text[cp].Content == L"-=" ||
					text[cp].Content == L"*=" || text[cp].Content == L"/=" || text[cp].Content == L"%=" ||
					text[cp].Content == L"&=" || text[cp].Content == L"|=" || text[cp].Content == L"^=" ||
					text[cp].Content == L"<<=" || text[cp].Content == L">>=") {
					auto sp = cp;
					if (!left.IsAssignable) throw CompilationException(CompilationError::ExpressionIsNotAssignable, xs, L"");
					if (text[cp].Content == L"=") {
						if (left.Type->GetClass() == TypeClass::Register)
							throw CompilationException(CompilationError::OperatorIsNotApplicable, cp, L"Register types can not be assigned to");
					} else {
						if (left.ArraySize) throw CompilationException(CompilationError::OperatorIsNotApplicable, cp, L"Arithmetic is not supported on arrays");
						CheckArithmeticOperation(left.Type, text[cp].Content.Fragment(0, text[cp].Content.Length() - 1), cp);
					}
					auto op = text[cp].Content;
					cp++;
					ValueDescriptor right;
					right.Type = left.Type;
					right.IsAssignable = false;
					right.ArraySize = left.ArraySize;
					auto arg = TranslateExpression(text, cp, context, scontext, &right);
					if (text[sp].Content != L"=") CheckMetalArithmeticLimitations(context, left.Type, right.Type, text[sp].Content.Fragment(0, text[sp].Content.Length() - 1), sp);
					out << L" " << op << L" " << arg;
				}
			}
			if (req_ret_desc) {
				if (!left.IsAssignable && req_ret_desc->IsAssignable)
					throw CompilationException(CompilationError::ExpressionIsNotAssignable, xs, L"");
				CheckAsymmetricAutocast(left, *req_ret_desc, xs);
			}
			if (ret_desc) *ret_desc = left;
			return out.ToString();
		}
		string TranslateVariableCreation(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext, LanguageType * type)
		{
			if (text[cp].Class != TokenClass::Identifier) throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"An identifier expected");
			auto var_name = text[cp].Content;
			auto & var_pool = scontext.Locals.LastElement();
			for (auto & v : var_pool) if (v.Name == var_name)
				throw CompilationException(CompilationError::ObjectRedefinition, cp, FormatString(L"'%0' have been already used for another local variable in this scope", var_name));
			cp++;
			int array_size = 0;
			if (text[cp].Class == TokenClass::CharCombo && text[cp].Content == L"[") {
				cp++;
				uint64 vol = 0;
				if (text[cp].Class == TokenClass::Constant && text[cp].ValueClass == TokenConstantClass::Numeric && text[cp].NumericClass() == NumericTokenClass::Integer) {
					vol = text[cp].AsInteger();
					if (vol == 0 || vol > MaximalStaticArray) {
						throw CompilationException(CompilationError::InvalidStaticArraySize, cp, FormatString(L"The size must be in range [%0, %1]", 1, MaximalStaticArray));
					}
					cp++;
					if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L"]")
						throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"']' expected");
					cp++;
				} else throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"An integer value expected");
				array_size = vol;
			}
			LocalVariable local;
			local.Name = var_name;
			local.TranslateName = L"egsl_local_" + string(scontext.Locals.Length() - 1) + L"_" + var_name;
			local.Type.SetRetain(type);
			local.ArraySize = array_size;
			DynamicString out;
			out << local.TranslateName;
			if (array_size) out << L"[" << string(array_size) << L"]";
			if (text[cp].Class == TokenClass::CharCombo && text[cp].Content == L"=") {
				cp++;
				if (text[cp].Class == TokenClass::CharCombo && text[cp].Content == L"{") {
					if (!array_size) throw CompilationException(CompilationError::ArrayInitializerTooLong, cp, L"Variable is not an array");
					cp++;
					ValueDescriptor desc;
					desc.Type = type;
					desc.IsAssignable = false;
					desc.ArraySize = 0;
					out << L" = {" << TranslateExpression(text, cp, context, scontext, &desc);
					int cv = 1;
					while (text[cp].Class == TokenClass::CharCombo && text[cp].Content == L",") {
						cv++;
						if (cv > array_size) throw CompilationException(CompilationError::ArrayInitializerTooLong, cp, FormatString(L"%0 elements is maximum", array_size));
						cp++;
						out << L", " << TranslateExpression(text, cp, context, scontext, &desc);
					}
					if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L"}")
						throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"'}' expected");
					cp++;
					out << L"}";
				} else {
					ValueDescriptor desc;
					desc.Type = type;
					desc.IsAssignable = false;
					desc.ArraySize = array_size;
					out << L" = " << TranslateExpression(text, cp, context, scontext, &desc);
				}
			}
			var_pool.Append(local);
			return out.ToString();
		}
		string TranslateVariableExpression(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext)
		{
			LanguageType * decl_var_type = 0;
			if (text[cp].Class == TokenClass::Keyword || text[cp].Class == TokenClass::Identifier) decl_var_type = FindType(text[cp].Content, context);
			if (decl_var_type) {
				if (context.Target == OutputTarget::Metal && decl_var_type->GetClass() == TypeClass::Simple) {
					auto sim = static_cast<SimpleType *>(decl_var_type);
					if (sim->Class == SimpleTypeClass::Matrix && sim->Domain != SimpleTypeDomain::Float) {
						throw CompilationException(CompilationError::InappropriateType, cp, L"MSL translation supports 'float' matricies only");
					}
				}
				cp++;
				string type_name;
				if (decl_var_type->GetClass() == TypeClass::Simple) type_name = static_cast<SimpleType *>(decl_var_type)->Name;
				else if (decl_var_type->GetClass() == TypeClass::Structure) type_name = static_cast<StructureType *>(decl_var_type)->TranslateName;
				DynamicString out;
				auto init = TranslateVariableCreation(text, cp, context, scontext, decl_var_type);
				out << type_name << L" " << init;
				while (text[cp].Class == TokenClass::CharCombo && text[cp].Content == L",") {
					cp++;
					out << L", " << TranslateVariableCreation(text, cp, context, scontext, decl_var_type);
				}
				return out.ToString();
			} else return TranslateExpression(text, cp, context, scontext, 0, 0);
		}
		string TranslateReturn(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext)
		{
			cp++;
			if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L";")
				throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"';' expected");
			cp++;
			if (context.Target == OutputTarget::Metal) return L"return egsl_retval;";
			else return L"return;";
		}
		string TranslateContinue(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext)
		{
			if (!scontext.LoopLevel)
				throw CompilationException(CompilationError::InvalidStatementLocation, cp, L"'continue' is available inside loops only");
			cp++;
			if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L";")
				throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"';' expected");
			cp++;
			return L"continue;";
		}
		string TranslateBreak(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext)
		{
			if (!scontext.LoopLevel)
				throw CompilationException(CompilationError::InvalidStatementLocation, cp, L"'break' is available inside loops only");
			cp++;
			if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L";")
				throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"';' expected");
			cp++;
			return L"break;";
		}
		string TranslateDiscard(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext)
		{
			if (scontext.Class != ShaderClass::Pixel)
				throw CompilationException(CompilationError::UnavailableStatement, cp, L"'discard' is available for pixel shaders only");
			cp++;
			if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L";")
				throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"';' expected");
			cp++;
			if (context.Target == OutputTarget::Metal) return L"discard_fragment();";
			else return L"discard;";
		}
		void TranslateBlock(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext, DynamicString & out, const string & ident);
		void TranslateStatement(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext, DynamicString & out, const string & ident);
		void TranslateIf(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext, DynamicString & out, const string & ident)
		{
			cp++;
			ValueDescriptor desc;
			desc.Type = FindType(L"bool", context);
			desc.IsAssignable = false;
			desc.ArraySize = 0;
			if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L"(")
				throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"'(' expected");
			cp++;
			auto cond = TranslateExpression(text, cp, context, scontext, &desc);
			if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L")")
				throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"')' expected");
			cp++;
			out << L"if (" << cond << L") ";
			TranslateStatement(text, cp, context, scontext, out, ident);
			if (text[cp].Class == TokenClass::Keyword && text[cp].Content == L"else") {
				cp++;
				out << L"else ";
				TranslateStatement(text, cp, context, scontext, out, ident);
			}
		}
		void TranslateFor(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext, DynamicString & out, const string & ident)
		{
			cp++;
			ValueDescriptor desc;
			desc.Type = FindType(L"bool", context);
			desc.IsAssignable = false;
			desc.ArraySize = 0;
			scontext.Locals.Append(SafeArray<LocalVariable>(0x20));
			if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L"(")
				throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"'(' expected");
			cp++;
			auto init = TranslateVariableExpression(text, cp, context, scontext);
			if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L";")
				throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"';' expected");
			cp++;
			auto cond = TranslateExpression(text, cp, context, scontext, &desc);
			if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L";")
				throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"';' expected");
			cp++;
			auto inc = TranslateExpression(text, cp, context, scontext, 0);
			if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L")")
				throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"')' expected");
			cp++;
			out << L"for (" << init << L"; " << cond << L"; " << inc << L") ";
			scontext.LoopLevel++;
			TranslateStatement(text, cp, context, scontext, out, ident);
			scontext.LoopLevel--;
			scontext.Locals.RemoveLast();
		}
		void TranslateWhile(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext, DynamicString & out, const string & ident)
		{
			cp++;
			ValueDescriptor desc;
			desc.Type = FindType(L"bool", context);
			desc.IsAssignable = false;
			desc.ArraySize = 0;
			if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L"(")
				throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"'(' expected");
			cp++;
			auto cond = TranslateExpression(text, cp, context, scontext, &desc);
			if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L")")
				throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"')' expected");
			cp++;
			out << L"while (" << cond << L") ";
			scontext.LoopLevel++;
			TranslateStatement(text, cp, context, scontext, out, ident);
			scontext.LoopLevel--;
		}
		void TranslateDoWhile(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext, DynamicString & out, const string & ident)
		{
			cp++;
			ValueDescriptor desc;
			desc.Type = FindType(L"bool", context);
			desc.IsAssignable = false;
			desc.ArraySize = 0;
			out << L"do ";
			scontext.LoopLevel++;
			TranslateStatement(text, cp, context, scontext, out, ident);
			scontext.LoopLevel--;
			if (text[cp].Class != TokenClass::Keyword || text[cp].Content != L"while")
				throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"'while' expected");
			cp++;
			if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L"(")
				throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"'(' expected");
			cp++;
			auto cond = TranslateExpression(text, cp, context, scontext, &desc);
			if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L")")
				throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"')' expected");
			cp++;
			if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L";")
				throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"';' expected");
			cp++;
			out << L" while (" << cond << L");";
		}
		void TranslateStatement(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext, DynamicString & out, const string & ident)
		{
			if (text[cp].Class == TokenClass::Keyword) {
				if (text[cp].Content == L"return") {
					out << TranslateReturn(text, cp, context, scontext); return;
				} else if (text[cp].Content == L"if") {
					TranslateIf(text, cp, context, scontext, out, ident); return;
				} else if (text[cp].Content == L"for") {
					TranslateFor(text, cp, context, scontext, out, ident); return;
				} else if (text[cp].Content == L"while") {
					TranslateWhile(text, cp, context, scontext, out, ident); return;
				} else if (text[cp].Content == L"do") {
					TranslateDoWhile(text, cp, context, scontext, out, ident); return;
				} else if (text[cp].Content == L"continue") {
					out << TranslateContinue(text, cp, context, scontext); return;
				} else if (text[cp].Content == L"break") {
					out << TranslateBreak(text, cp, context, scontext); return;
				} else if (text[cp].Content == L"discard") {
					out << TranslateDiscard(text, cp, context, scontext); return;
				}
			}
			if (text[cp].Class == TokenClass::CharCombo && text[cp].Content == L"{") {
				TranslateBlock(text, cp, context, scontext, out, ident + L"\t");
			} else if (text[cp].Class == TokenClass::CharCombo && text[cp].Content == L";") {
				out << L";"; cp++;
			} else {
				out << TranslateVariableExpression(text, cp, context, scontext) << L";";
				if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L";")
					throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"';' expected");
				cp++;
			}
		}
		void TranslateBlock(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext, DynamicString & out, const string & ident)
		{
			scontext.Locals.Append(SafeArray<LocalVariable>(0x20));
			out << L"{\n";
			if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L"{")
				throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"'{' expected");
			cp++;
			while (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L"}") {
				out << ident << L"\t";
				TranslateStatement(text, cp, context, scontext, out, ident);
				out << L"\n";
			}
			cp++;
			out << ident << L"}";
			scontext.Locals.RemoveLast();
		}
		void ParseShader(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, ShaderClass shader_class)
		{
			if (text[cp].Class != TokenClass::Identifier) throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"An identifier expected");
			ShaderObject shader;
			CompilerShaderContext scontext;
			scontext.LoopLevel = 0;
			shader.Class = scontext.Class = shader_class;
			shader.Name = scontext.Name = text[cp].Content;
			shader.TranslateName = scontext.TranslateName = L"egsl_shader_" + shader.Name;
			for (auto & s : context.StructureTypes) if (s.Name == shader.Name)
				throw CompilationException(CompilationError::ObjectRedefinition, cp, FormatString(L"'%0' have been already used for a structure", shader.Name));
			for (auto & s : context.Shaders) if (s.Name == shader.Name)
				throw CompilationException(CompilationError::ObjectRedefinition, cp, FormatString(L"'%0' have been already used for a shader", shader.Name));
			cp++;
			if (text[cp].Class != TokenClass::CharCombo || text[cp].Content != L"(")
				throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"'(' expected");
			ParseShaderArguments(text, cp, context, scontext);
			if (context.Target == OutputTarget::Direct3D11) {
				shader.Code = MakeHlslCodeForShaderSignature(scontext);
				shader.Code += L"{\n\t";
			} else if (context.Target == OutputTarget::Metal) {
				string msl_rvt;
				shader.Code = MakeMslCodeForShaderSignature(scontext, msl_rvt);
				shader.Code += L"{\n\t";
				shader.Code += msl_rvt;
				shader.Code += L" egsl_retval;\n\t";
			}
			DynamicString translated;
			TranslateBlock(text, cp, context, scontext, translated, L"\t");
			if (context.Target == OutputTarget::Direct3D11) {
				shader.Code += translated;
				shader.Code += L"\n}\n";
			} else if (context.Target == OutputTarget::Metal) {
				shader.Code += translated;
				shader.Code += L"\n\t";
				shader.Code += L"return egsl_retval;";
				shader.Code += L"\n}\n";
			}
			context.Shaders.Append(shader);
		}
		void TranslateCode(const Array<Syntax::Token> & text, CompilerCommonContext & context)
		{
			int cp = 0;
			while (text[cp].Class != TokenClass::EndOfStream) {
				if (text[cp].Class != TokenClass::Keyword) throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"'pixel', 'vertex' or 'struct' keyword expected");
				if (text[cp].Content == L"vertex") {
					cp++;
					ParseShader(text, cp, context, ShaderClass::Vertex);
				} else if (text[cp].Content == L"pixel") {
					cp++;
					ParseShader(text, cp, context, ShaderClass::Pixel);
				} else if (text[cp].Content == L"struct") {
					cp++;
					ParseStructure(text, cp, context);
				} else throw CompilationException(CompilationError::AnotherTokenExpected, cp, L"'pixel', 'vertex' or 'struct' keyword expected");
			}
		}
		
		string MakeHlslCodeForStructure(StructureType * type)
		{
			DynamicString result;
			result << L"struct " << type->TranslateName << L" {\n";
			uint si = 0;
			for (int i = 0; i < type->FieldNames.Length(); i++) {
				auto mode = type->FieldModes[i];
				result << L"\t";
				if (mode == InterpolationMode::Linear) result << L"linear ";
				else if (mode == InterpolationMode::None) result << L"nointerpolation ";
				else if (mode == InterpolationMode::NoPerspective) result << L"noperspective ";
				result << type->FieldTypes[i].Name << L" " << type->FieldTranslateNames[i];
				if (type->FieldArraySizes[i]) result << L"[" << string(type->FieldArraySizes[i]) << L"]";
				if (type->FieldTypes[i].Domain == SimpleTypeDomain::Float && type->FieldTypes[i].Class != SimpleTypeClass::Matrix) {
					if (si < 8) result << L" : TEXCOORD" << string(si);
					else if (si < 16) result << L" : COLOR" << string(si - 8);
					si++;
				}
				result << L";\n";
			}
			result << L"};\n";
			return result.ToString();
		}
		string MakeHlslCodeForShaderSignature(CompilerShaderContext & scontext)
		{
			DynamicString result;
			for (auto & a : scontext.Arguments) {
				if (a.Semantic.Semantic == SemanticClass::Texture) {
					auto reg = static_cast<RegisterType *>(a.Type.Inner());
					if (reg->Dimensions == 0) {
						result << L"Texture1D<" << static_cast<SimpleType *>(reg->InnerType.Inner())->Name << L"> ";
					} else if (reg->Dimensions == 1) {
						result << L"Texture2D<" << static_cast<SimpleType *>(reg->InnerType.Inner())->Name << L"> ";
					} else if (reg->Dimensions == 2) {
						result << L"TextureCube<" << static_cast<SimpleType *>(reg->InnerType.Inner())->Name << L"> ";
					} else if (reg->Dimensions == 3) {
						result << L"Texture3D<" << static_cast<SimpleType *>(reg->InnerType.Inner())->Name << L"> ";
					} else if (reg->Dimensions == 4) {
						result << L"Texture1DArray<" << static_cast<SimpleType *>(reg->InnerType.Inner())->Name << L"> ";
					} else if (reg->Dimensions == 5) {
						result << L"Texture2DArray<" << static_cast<SimpleType *>(reg->InnerType.Inner())->Name << L"> ";
					} else if (reg->Dimensions == 6) {
						result << L"TextureCubeArray<" << static_cast<SimpleType *>(reg->InnerType.Inner())->Name << L"> ";
					}
					result << a.TranslateName << L" : register(t[" << string(a.Semantic.Index) << L"]);\n";
				} else if (a.Semantic.Semantic == SemanticClass::Buffer) {
					auto reg = static_cast<RegisterType *>(a.Type.Inner());
					string element;
					if (reg->InnerType->GetClass() == TypeClass::Simple) element = static_cast<SimpleType *>(reg->InnerType.Inner())->Name;
					else if (reg->InnerType->GetClass() == TypeClass::Structure) element = static_cast<StructureType *>(reg->InnerType.Inner())->TranslateName;
					result << L"StructuredBuffer<" << element << L"> " << a.TranslateName << L" : register(t[" << string(a.Semantic.Index) << L"]);\n";
				} else if (a.Semantic.Semantic == SemanticClass::Sampler) {
					result << L"SamplerState " << a.TranslateName << L" : register(s[" << string(a.Semantic.Index) << L"]);\n";
				} else if (a.Semantic.Semantic == SemanticClass::Constant) {
					string element;
					if (a.Type->GetClass() == TypeClass::Simple) element = static_cast<SimpleType *>(a.Type.Inner())->Name;
					else if (a.Type->GetClass() == TypeClass::Structure) element = static_cast<StructureType *>(a.Type.Inner())->TranslateName;
					result << element << L" " << a.TranslateName << L" : register(b[" << string(a.Semantic.Index) << L"]);\n";
				}
			}
			result << L"void " << scontext.TranslateName << L"(";
			bool first = true;
			for (auto & a : scontext.Arguments) {
				if (a.Semantic.Semantic != SemanticClass::Constant && a.Semantic.Semantic != SemanticClass::Buffer &&
					a.Semantic.Semantic != SemanticClass::Texture && a.Semantic.Semantic != SemanticClass::Sampler) {
					if (!first) result << L", "; else first = false;
					if (a.Usage == ShaderArgumentUsage::Input) result << L"in ";
					else if (a.Usage == ShaderArgumentUsage::Output) result << L"out ";
					string type_name;
					if (a.Type->GetClass() == TypeClass::Simple) type_name = static_cast<SimpleType *>(a.Type.Inner())->Name;
					else if (a.Type->GetClass() == TypeClass::Structure) type_name = static_cast<StructureType *>(a.Type.Inner())->TranslateName;
					result << type_name << L" " << a.TranslateName;
					if (a.Semantic.Semantic != SemanticClass::NoSemantic) {
						result << L" : ";
						if (a.Semantic.Semantic == SemanticClass::Vertex) {
							result << L"SV_VertexID";
						} else if (a.Semantic.Semantic == SemanticClass::Instance) {
							result << L"SV_InstanceID";
						} else if (a.Semantic.Semantic == SemanticClass::Position) {
							result << L"SV_Position";
						} else if (a.Semantic.Semantic == SemanticClass::Front) {
							result << L"SV_IsFrontFace";
						} else if (a.Semantic.Semantic == SemanticClass::Color) {
							result << L"SV_Target" << string(a.Semantic.Index);
						} else if (a.Semantic.Semantic == SemanticClass::Depth) {
							result << L"SV_Depth";
						} else if (a.Semantic.Semantic == SemanticClass::Stencil) {
							result << L"SV_StencilRef";
						}
					}
				}
			}
			result << L")\n";
			return result.ToString();
		}
		string MakeMslCodeForStructure(StructureType * type)
		{
			DynamicString result;
			result << L"struct " << type->TranslateName << L" {\n";
			for (int i = 0; i < type->FieldNames.Length(); i++) {
				result << L"\t";
				if (type->IsExternal && type->FieldTypes[i].Class == SimpleTypeClass::Vector && type->FieldTypes[i].Domain != SimpleTypeDomain::Boolean) {
					result << L"simd::packed_" + type->FieldTypes[i].Name;
				} else result << type->FieldTypes[i].Name;
				result << L" " << type->FieldTranslateNames[i];
				if (type->FieldArraySizes[i]) result << L"[" << string(type->FieldArraySizes[i]) << L"]";
				auto mode = type->FieldModes[i];
				if (mode == InterpolationMode::Linear) result << L" [[center_perspective]]";
				else if (mode == InterpolationMode::None) result << L" [[flat]]";
				else if (mode == InterpolationMode::NoPerspective) result << L" [[center_no_perspective]]";
				result << L";\n";
			}
			result << L"};\n";
			result << L"struct pw_" << type->TranslateName << L" {\n";
			result << L"\t" << type->TranslateName << L" vdata;\n";
			result << L"\tfloat4 vpos [[position]];\n";
			result << L"};\n";
			return result.ToString();
		}
		string MakeMslCodeForShaderSignature(CompilerShaderContext & scontext, string & rvt)
		{
			DynamicString result;
			string retval_struct;
			for (auto & arg : scontext.Arguments) if (arg.Usage == ShaderArgumentUsage::Output && arg.Semantic.Semantic == SemanticClass::NoSemantic) {
				retval_struct = static_cast<StructureType *>(arg.Type.Inner())->TranslateName;
				break;
			}
			if (scontext.Class == ShaderClass::Vertex) {
				if (retval_struct.Length()) {
					for (auto & arg : scontext.Arguments) if (arg.Usage == ShaderArgumentUsage::Output) {
						if (arg.Semantic.Semantic == SemanticClass::Position) {
							if (retval_struct.Length()) arg.TranslateName = L"egsl_retval.vpos";
							else arg.TranslateName = L"egsl_retval";
						} else if (arg.Semantic.Semantic == SemanticClass::NoSemantic) {
							arg.TranslateName = L"egsl_retval.vdata";
						}
					}
					result << L"vertex pw_" << retval_struct;
					rvt = L"pw_" + retval_struct;
				} else {
					result << L"vertex float4";
					rvt = L"float4";
				}
			} else if (scontext.Class == ShaderClass::Pixel) {
				result << L"struct " << scontext.TranslateName << L" {\n";
				uint index = 0;
				for (auto & arg : scontext.Arguments) if (arg.Usage == ShaderArgumentUsage::Output) {
					auto fn = L"value_" + string(index);
					arg.TranslateName = L"egsl_retval." + fn;
					index++;
					if (arg.Semantic.Semantic == SemanticClass::Color) {
						result << L"\t" << static_cast<SimpleType *>(arg.Type.Inner())->Name << L" " << fn << L" [[color(" << string(arg.Semantic.Index) << L")]];\n";
					} else if (arg.Semantic.Semantic == SemanticClass::Depth) {
						result << L"\t" << static_cast<SimpleType *>(arg.Type.Inner())->Name << L" " << fn << L" [[depth_argument(any)]];\n";
					} else if (arg.Semantic.Semantic == SemanticClass::Stencil) {
						result << L"\t" << static_cast<SimpleType *>(arg.Type.Inner())->Name << L" " << fn << L" [[stencil]];\n";
					}
				}
				result << L"};\n";
				result << L"fragment " << scontext.TranslateName;
				rvt = scontext.TranslateName;
			}
			result << L" " << scontext.Name << L"(";
			int index = 0;
			for (auto & arg : scontext.Arguments) if (arg.Usage == ShaderArgumentUsage::Input) {
				if (index) result << L", ";
				if (arg.Semantic.Semantic == SemanticClass::Vertex) {
					result << L"uint " << arg.TranslateName << L" [[vertex_id]]";
				} else if (arg.Semantic.Semantic == SemanticClass::Instance) {
					result << L"uint " << arg.TranslateName << L" [[instance_id]]";
				} else if (arg.Semantic.Semantic == SemanticClass::Front) {
					result << L"bool " << arg.TranslateName << L" [[front_facing]]";
				} else if (arg.Semantic.Semantic == SemanticClass::Texture) {
					auto reg = static_cast<RegisterType *>(arg.Type.Inner());
					auto sim = static_cast<SimpleType *>(reg->InnerType.Inner());
					if (reg->Dimensions == 0) result << L"texture1d<";
					else if (reg->Dimensions == 1) result << L"texture2d<";
					else if (reg->Dimensions == 2) result << L"texturecube<";
					else if (reg->Dimensions == 3) result << L"texture3d<";
					else if (reg->Dimensions == 4) result << L"texture1d_array<";
					else if (reg->Dimensions == 5) result << L"texture2d_array<";
					else if (reg->Dimensions == 6) result << L"texturecube_array<";
					if (sim->Domain == SimpleTypeDomain::Float) result << L"float";
					else if (sim->Domain == SimpleTypeDomain::Integer) result << L"int";
					else if (sim->Domain == SimpleTypeDomain::UnsignedInteger) result << L"uint";
					else if (sim->Domain == SimpleTypeDomain::Boolean) result << L"uint";
					result << L"> " << arg.TranslateName << L" [[texture(" << string(arg.Semantic.Index) << L")]]";
				} else if (arg.Semantic.Semantic == SemanticClass::Buffer) {
					result << L"constant ";
					auto reg = static_cast<RegisterType *>(arg.Type.Inner());
					string element;
					if (reg->InnerType->GetClass() == TypeClass::Simple) element = static_cast<SimpleType *>(reg->InnerType.Inner())->Name;
					else if (reg->InnerType->GetClass() == TypeClass::Structure) element = static_cast<StructureType *>(reg->InnerType.Inner())->TranslateName;
					result << element << L" * " << arg.TranslateName << L" [[buffer(" << string(arg.Semantic.Index) << L")]]";
				} else if (arg.Semantic.Semantic == SemanticClass::Constant) {
					result << L"constant ";
					string element;
					if (arg.Type->GetClass() == TypeClass::Simple) element = static_cast<SimpleType *>(arg.Type.Inner())->Name;
					else if (arg.Type->GetClass() == TypeClass::Structure) element = static_cast<StructureType *>(arg.Type.Inner())->TranslateName;
					result << element << L" * " << arg.TranslateName << L" [[buffer(" << string(arg.Semantic.Index) << L")]]";
					arg.TranslateName = L"(*" + arg.TranslateName + L")";
				} else if (arg.Semantic.Semantic == SemanticClass::Sampler) {
					result << L"sampler " << arg.TranslateName << L" [[sampler(" << string(arg.Semantic.Index) << L")]]";
				} else if (arg.Semantic.Semantic == SemanticClass::NoSemantic) {
					result << L"pw_" << static_cast<StructureType *>(arg.Type.Inner())->TranslateName << L" egsl_vs [[stage_in]]";
					arg.TranslateName = L"egsl_vs.vdata";
				}
				index++;
			}
			result << L")\n";
			return result.ToString();
		}
	}
}