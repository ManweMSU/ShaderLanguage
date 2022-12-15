#include "Output.h"

#include "CompileDX.h"
#include "CompileMetal.h"

using namespace Engine::IO;
using namespace Engine::IO::ConsoleControl;
using namespace Engine::Streaming;

namespace Engine
{
	namespace EGSL
	{
		struct UniversalData
		{
			string name;
			uint32 flags;
			SafePointer<DataBlock> data;
		};

		bool ProduceDXOutput(IO::Console * console, CompilerCommonContext & ctx, ShaderObject & shader, bool compile, bool werror, bool wsupress, DataBlock ** output)
		{
			DynamicString code;
			for (auto & str : ctx.StructureTypes) code << EGSL::MakeHlslCodeForStructure(&str);
			code << shader.Code;
			if (compile) {
				DynamicString error_log;
				SafePointer<DataBlock> blob = EGSL::CompileHLSL(code.ToString(), shader.TranslateName, shader.Class, error_log);
				if (blob) {
					if (console && !wsupress) {
						console->SetTextColor(ConsoleColor::Yellow);
						console->Write(error_log.ToString());
						console->SetTextColor(ConsoleColor::Default);
					}
					if (werror) for (int i = 0; i < error_log.Length(); i++) if (error_log[i] > 32) return false;
				} else {
					if (console) {
						console->SetTextColor(ConsoleColor::Red);
						console->Write(error_log.ToString());
						console->SetTextColor(ConsoleColor::Default);
					}
					return false;
				}
				*output = blob.Inner();
				blob->Retain();
				return true;
			} else {
				*output = code.ToString().EncodeSequence(Encoding::ANSI, false);
				return true;
			}
		}
		bool ProduceMetalCode(CompilerCommonContext & ctx, string & output)
		{
			DynamicString code;
			code << L"#include <metal_stdlib>\n";
			code << L"#include <simd/simd.h>\n\n";
			code << L"using namespace metal;\n\n";
			for (auto & str : ctx.StructureTypes) code << EGSL::MakeMslCodeForStructure(&str);
			for (auto & shader : ctx.Shaders) code << shader.Code;
			output = code.ToString();
			return true;
		}
		bool TranslateInput(IO::Console * console, const string & source, OutputTarget target, bool werror, bool wsupress, CompilerCommonContext & context)
		{
			SafePointer< Array<Syntax::Token> > text;
			try {
				text = ParseCode(source);
			} catch (Syntax::ParserSpellingException & e) {
				string line, exdesc;
				int x, y, length, ofs;
				LocateErrorPosition(source, e, exdesc, line, x, y, ofs, length);
				if (console) {
					(*console) << TextColor(ConsoleColor::Red) << L"Failed" << TextColorDefault() << LineFeed();
					(*console) << TextColor(ConsoleColor::Red) << FormatString(L"Compilation error: #%0 - %1 at (%2, %3).",
						string(uint(CompilationError::InvalidToken), HexadecimalBase, 4),
						DescriptionForError(CompilationError::InvalidToken), y + 1, x + 1) << TextColorDefault() << LineFeed();
					if (exdesc.Length()) (*console) << TextColor(ConsoleColor::Red) << exdesc << L"." << TextColorDefault() << LineFeed() << LineFeed();
					(*console) << line << LineFeed();
					(*console) << string(L' ', ofs) << TextColor(ConsoleColor::Red) << L"^" << string(L'~', length - 1) << TextColorDefault() << LineFeed();
				}
				return false;
			}
			InitializeContext(context, target);
			try {
				TranslateCode(*text, context);
			} catch (Syntax::ParserSpellingException & e) {
				string line, exdesc;
				int x, y, length, ofs;
				LocateErrorPosition(source, e, exdesc, line, x, y, ofs, length);
				if (console) {
					(*console) << TextColor(ConsoleColor::Red) << L"Failed" << TextColorDefault() << LineFeed();
					(*console) << TextColor(ConsoleColor::Red) << FormatString(L"Compilation error: #%0 - %1 at (%2, %3).",
						string(uint(CompilationError::InvalidToken), HexadecimalBase, 4),
						DescriptionForError(CompilationError::InvalidToken), y + 1, x + 1) << TextColorDefault() << LineFeed();
					if (exdesc.Length()) (*console) << TextColor(ConsoleColor::Red) << exdesc << L"." << TextColorDefault() << LineFeed() << LineFeed();
					(*console) << line << LineFeed();
					(*console) << string(L' ', ofs) << TextColor(ConsoleColor::Red) << L"^" << string(L'~', length - 1) << TextColorDefault() << LineFeed();
				}
				return false;
			} catch (CompilationException & e) {
				string line, exdesc;
				int x, y, length, ofs;
				LocateErrorPosition(source, *text, e, exdesc, line, x, y, ofs, length);
				if (console) {
					(*console) << TextColor(ConsoleColor::Red) << L"Failed" << TextColorDefault() << LineFeed();
					(*console) << TextColor(ConsoleColor::Red) << FormatString(L"Compilation error: #%0 - %1 at (%2, %3).",
						string(uint(e.Error), HexadecimalBase, 4), DescriptionForError(e.Error), y + 1, x + 1) << TextColorDefault() << LineFeed();
					if (exdesc.Length()) (*console) << TextColor(ConsoleColor::Red) << exdesc << L"." << TextColorDefault() << LineFeed() << LineFeed();
					(*console) << line << LineFeed();
					(*console) << string(L' ', ofs) << TextColor(ConsoleColor::Red) << L"^" << string(L'~', length - 1) << TextColorDefault() << LineFeed();
				}
				return false;
			}
			if (console) {
				if (context.hints.Length()) (*console) << TextColor(ConsoleColor::Yellow) << L"Succeed" << TextColorDefault() << LineFeed();
				else (*console) << TextColor(ConsoleColor::Green) << L"Succeed" << TextColorDefault() << LineFeed();
			}
			for (auto & h : context.hints) {
				if (console && !wsupress) {
					(*console) << TextColor(ConsoleColor::Yellow) << L"Warning: " << TextColorDefault() << h << L"." << LineFeed();
				}
			}
			if (context.hints.Length() && werror) return false;
			return true;
		}
		bool ProduceOutput(IO::Console * console, const string & source, const string & output_base, OutputForm form, OutputTarget target, bool werror, bool wsupress)
		{
			if (form == OutputForm::Source) {
				CompilerCommonContext ctx;
				if (!TranslateInput(console, source, target, werror, wsupress, ctx)) return false;
				if (target == OutputTarget::Direct3D11) {
					for (auto & shader : ctx.Shaders) {
						if (!ProduceDXOutput(console, ctx, shader, false, werror, wsupress, shader.CompiledBlob.InnerRef())) return false;
						FileStream stream(output_base + L"_" + shader.Name + L".hlsl", AccessWrite, CreateAlways);
						stream.WriteArray(shader.CompiledBlob);
					}
				} else if (target == OutputTarget::Metal) {
					string code;
					if (!ProduceMetalCode(ctx, code)) return false;
					FileStream stream(output_base + L".metal", AccessWrite, CreateAlways);
					TextWriter writer(&stream, Encoding::ANSI);
					writer.Write(code);
				} else {
					if (console) (*console) << TextColor(ConsoleColor::Red) << L"Internal error: unknown target interface." << TextColorDefault() << LineFeed();
					return false;
				}
			} else if (form == OutputForm::Library) {
				CompilerCommonContext ctx;
				if (!TranslateInput(console, source, target, werror, wsupress, ctx)) return false;
				if (target == OutputTarget::Direct3D11) {
					for (auto & shader : ctx.Shaders) {
						if (!ProduceDXOutput(console, ctx, shader, true, werror, wsupress, shader.CompiledBlob.InnerRef())) return false;
					}
					SafePointer<MemoryStream> output_raw = new MemoryStream(0x10000);
					SafePointer<Storage::NewArchive> archive = Storage::CreateArchive(output_raw, ctx.Shaders.Length(), Storage::NewArchiveFlags::UseFormat32);
					Storage::ArchiveFile file = 0;
					for (auto & shader : ctx.Shaders) {
						file++;
						archive->SetFileName(file, shader.Name);
						archive->SetFileData(file, shader.CompiledBlob->GetBuffer(), shader.CompiledBlob->Length());
						if (shader.Class == ShaderClass::Vertex) {
							archive->SetFileCustom(file, 0x01);
						} else if (shader.Class == ShaderClass::Pixel) {
							archive->SetFileCustom(file, 0x02);
						}
					}
					archive->Finalize();
					archive.SetReference(0);
					FileStream output(output_base + L".egso", AccessWrite, CreateAlways);
					output_raw->Seek(0, Begin);
					output_raw->CopyTo(&output);
				} else if (target == OutputTarget::Metal) {
					string code;
					if (!ProduceMetalCode(ctx, code)) return false;
					DynamicString error_log;
					auto status = CompileMSL(code, output_base + L".egso", error_log);
					if (status) {
						if (console && !wsupress) {
							console->SetTextColor(ConsoleColor::Yellow);
							console->Write(error_log.ToString());
							console->SetTextColor(ConsoleColor::Default);
						}
						if (werror) for (int i = 0; i < error_log.Length(); i++) if (error_log[i] > 32) return false;
					} else {
						if (console) {
							console->SetTextColor(ConsoleColor::Red);
							console->Write(error_log.ToString());
							console->SetTextColor(ConsoleColor::Default);
						}
						return false;
					}
				} else {
					if (console) (*console) << TextColor(ConsoleColor::Red) << L"Internal error: unknown target interface." << TextColorDefault() << LineFeed();
					return false;
				}
			} else if (form == OutputForm::Universal) {
				Array<UniversalData> data(0x20);
				{
					CompilerCommonContext ctx_dx;
					if (!TranslateInput(console, source, OutputTarget::Direct3D11, werror, wsupress, ctx_dx)) return false;
					for (auto & shader : ctx_dx.Shaders) {
						if (!ProduceDXOutput(0, ctx_dx, shader, false, werror, wsupress, shader.CompiledBlob.InnerRef())) return false;
						UniversalData dta;
						dta.name = shader.Name + L"!" + shader.TranslateName;
						dta.flags = 0x10000;
						dta.data = shader.CompiledBlob;
						if (shader.Class == ShaderClass::Vertex) dta.flags |= 0x01;
						else if (shader.Class == ShaderClass::Pixel) dta.flags |= 0x02;
						data << dta;
					}
				}
				{
					CompilerCommonContext ctx_mtl;
					string code;
					if (!TranslateInput(0, source, OutputTarget::Metal, werror, wsupress, ctx_mtl)) return false;
					if (!ProduceMetalCode(ctx_mtl, code)) return false;
					UniversalData dta;
					dta.name = L"_";
					dta.flags = 0x20000;
					dta.data = code.EncodeSequence(Encoding::ANSI, false);
					data << dta;
				}
				SafePointer<MemoryStream> output_raw = new MemoryStream(0x10000);
				SafePointer<Storage::NewArchive> archive = Storage::CreateArchive(output_raw, data.Length(), Storage::NewArchiveFlags::UseFormat32);
				Storage::ArchiveFile file = 0;
				for (auto & d : data) {
					file++;
					archive->SetFileName(file, d.name);
					archive->SetFileCustom(file, d.flags);
					archive->SetFileData(file, d.data->GetBuffer(), d.data->Length());
				}
				archive->Finalize();
				archive.SetReference(0);
				FileStream output(output_base + L".egsu", AccessWrite, CreateAlways);
				output_raw->Seek(0, Begin);
				output_raw->CopyTo(&output);
			} else {
				if (console) (*console) << TextColor(ConsoleColor::Red) << L"Internal error: unknown output form." << TextColorDefault() << LineFeed();
				return false;
			}
			return true;
		}
	}
}