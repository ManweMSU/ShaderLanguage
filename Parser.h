#pragma once

#include <EngineRuntime.h>

#include "Core.h"

namespace Engine
{
	namespace EGSL
	{
		constexpr uint MaximalStaticArray = 0xFF;
		constexpr uint NumRenderTargets = 8;
		constexpr uint NumConstants = 14;
		constexpr uint NumSamplers = 16;
		constexpr uint NumTextures = 128;
		constexpr uint NumBuffers = 128;
		enum class SemanticClass {
			NoSemantic,
			// Semantics
			Vertex, Instance, Position, Front, Color, Depth, Stencil,
			// Registers
			Texture, Buffer, Constant, Sampler
		};
		struct ValueSemantic
		{
			SemanticClass Semantic;
			int Index;
		};
		enum class TypeClass { Simple, Structure, Register, Void };
		enum class SimpleTypeDomain { Boolean, Integer, UnsignedInteger, Float };
		enum class SimpleTypeClass { Scalar, Vector, Matrix };
		enum class InterpolationMode { Default, None, Linear, NoPerspective };
		struct LanguageType : public Object
		{
			virtual TypeClass GetClass(void) const = 0;
		};
		struct VoidType : public LanguageType
		{
			virtual TypeClass GetClass(void) const override;
			virtual string ToString(void) const override;
		};
		struct SimpleType : public LanguageType
		{
			string Name;
			SimpleTypeDomain Domain;
			SimpleTypeClass Class;
			int Columns, Rows;

			virtual TypeClass GetClass(void) const override;
			virtual string ToString(void) const override;
		};
		enum class RegisterTypeClass { Sampler, Array, Texture };
		struct RegisterType : public LanguageType
		{
			RegisterTypeClass Class;
			SafePointer<LanguageType> InnerType; // Element type for an array (structure or simple) and a texture (simple only), ignored for a sampler
			int Dimensions; // For a texture: 0 - 1D, 1 - 2D, 2 - Cube, 3 - 3D; +4 - texture array; ignored otherwise

			virtual TypeClass GetClass(void) const override;
			virtual string ToString(void) const override;
		};
		struct StructureType : public LanguageType
		{
			bool IsExternal;
			string Name;
			string TranslateName;
			Array<string> FieldNames = Array<string>(0x10);
			Array<string> FieldTranslateNames = Array<string>(0x10);
			ObjectArray<SimpleType> FieldTypes = ObjectArray<SimpleType>(0x10);
			Array<int> FieldArraySizes = Array<int>(0x10);
			Array<InterpolationMode> FieldModes = Array<InterpolationMode>(0x10);

			virtual TypeClass GetClass(void) const override;
			virtual string ToString(void) const override;
		};
		enum class ShaderArgumentUsage { Input, Output };
		struct ShaderArgument
		{
			string Name;
			string TranslateName;
			SafePointer<LanguageType> Type;
			ValueSemantic Semantic;
			ShaderArgumentUsage Usage;
			int TokenPosition;
		};
		struct LocalVariable
		{
			string Name;
			string TranslateName;
			SafePointer<LanguageType> Type;
			int ArraySize;
		};
		enum class ShaderClass { Vertex, Pixel };
		struct ShaderObject
		{
			string Name;
			string TranslateName;
			ShaderClass Class;
			string Code;
			SafePointer<DataBlock> CompiledBlob;
		};
		struct ValueDescriptor
		{
			LanguageType * Type;
			bool IsAssignable;
			int ArraySize;
		};

		struct CompilerCommonContext
		{
			OutputTarget Target;
			VoidType Void;
			Array<string> hints = Array<string>(0x10);
			ObjectArray<SimpleType> SimpleTypes = ObjectArray<SimpleType>(0x20);
			ObjectArray<RegisterType> RegisterTypes = ObjectArray<RegisterType>(0x20);
			ObjectArray<StructureType> StructureTypes = ObjectArray<StructureType>(0x20);
			SafeArray<ShaderObject> Shaders = SafeArray<ShaderObject>(0x20);
		};
		struct CompilerShaderContext
		{
			string Name;
			string TranslateName;
			ShaderClass Class;
			SafeArray<ShaderArgument> Arguments = SafeArray<ShaderArgument>(0x20);
			SafeArray< SafeArray<LocalVariable> > Locals = SafeArray< SafeArray<LocalVariable> >(0x10);
			int LoopLevel;
		};

		const Syntax::Spelling & GetSpelling(void);
		Array<Syntax::Token> * ParseCode(const string & code);
		void InitializeContext(CompilerCommonContext & context, OutputTarget target);
		void CheckSymmetricAutocast(ValueDescriptor d1, ValueDescriptor d2, ValueDescriptor * result, int throw_at);
		bool CheckSymmetricAutocast(ValueDescriptor d1, ValueDescriptor d2, ValueDescriptor * result);
		void CheckAsymmetricAutocast(ValueDescriptor from, ValueDescriptor to, int throw_at);
		bool CheckAsymmetricAutocast(ValueDescriptor from, ValueDescriptor to);
		void CheckIdentity(ValueDescriptor d1, ValueDescriptor d2, int throw_at);
		bool CheckIdentity(ValueDescriptor d1, ValueDescriptor d2);
		string TranslateExpression(const Array<Syntax::Token> & text, int & cp, CompilerCommonContext & context, CompilerShaderContext & scontext, const ValueDescriptor * req_ret_desc, ValueDescriptor * ret_desc = 0);
		void TranslateCode(const Array<Syntax::Token> & text, CompilerCommonContext & context);

		string MakeHlslCodeForStructure(StructureType * type);
		string MakeHlslCodeForShaderSignature(CompilerShaderContext & scontext);
		string MakeMslCodeForStructure(StructureType * type);
		string MakeMslCodeForShaderSignature(CompilerShaderContext & scontext, string & rvt);
	}
}