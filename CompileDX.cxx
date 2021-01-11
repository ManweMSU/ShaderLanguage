#include "CompileDX.h"

#include <PlatformDependent/Direct3D.h>
#include <d3dcompiler.h>

#pragma comment(lib, "d3dcompiler.lib")

namespace Engine
{
	namespace EGSL
	{
		DataBlock * CompileHLSL(const string & code, const string & entry, ShaderClass cls, DynamicString & log)
		{
			LPCSTR target = 0;
			if (cls == ShaderClass::Vertex) target = "vs_4_1";
			else if (cls == ShaderClass::Pixel) target = "ps_4_1";
			SafePointer<DataBlock> enc = code.EncodeSequence(Encoding::ANSI, false);
			Array<char> eenc(0x100);
			eenc.SetLength(entry.GetEncodedLength(Encoding::ANSI) + 1);
			entry.Encode(eenc.GetBuffer(), Encoding::ANSI, true);
			ID3DBlob * output = 0, * errors = 0;
			if (D3DCompile(enc->GetBuffer(), enc->Length(), "Translated.hlsl", 0, 0, eenc.GetBuffer(), target,
				D3DCOMPILE_PACK_MATRIX_ROW_MAJOR | D3DCOMPILE_OPTIMIZATION_LEVEL3, 0, &output, &errors) == S_OK) {
				if (errors) {
					log << string(errors->GetBufferPointer(), errors->GetBufferSize(), Encoding::ANSI);
					errors->Release();
				}
				if (output) {
					SafePointer<DataBlock> result = new DataBlock(0x10000);
					result->Append(reinterpret_cast<const uint8 *>(output->GetBufferPointer()), output->GetBufferSize());
					result->Retain();
					return result;
				} else return 0;
			} else {
				if (errors) {
					log << string(errors->GetBufferPointer(), errors->GetBufferSize(), Encoding::ANSI);
					errors->Release();
				}
				return 0;
			}
		}
	}
}