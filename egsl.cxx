#include <EngineRuntime.h>

#include "Core.h"
#include "Parser.h"
#include "CompileDX.h"
#include "CompileMetal.h"

using namespace Engine;
using namespace Engine::Streaming;
using namespace Engine::IO::ConsoleControl;

EGSL::OutputTarget GetPlatformDefaultTarget(void)
{
	#ifdef ENGINE_WINDOWS
	return EGSL::OutputTarget::Direct3D11;
	#endif
	#ifdef ENGINE_MACOSX
	return EGSL::OutputTarget::Metal;
	#endif
	return EGSL::OutputTarget::Unknown;
}
EGSL::OutputTarget GetTargetFromString(const string & name)
{
	if (string::CompareIgnoreCase(name, L"d3d11") == 0) {
		return EGSL::OutputTarget::Direct3D11;
	} else if (string::CompareIgnoreCase(name, L"metal") == 0) {
		return EGSL::OutputTarget::Metal;
	} else return EGSL::OutputTarget::Unknown;
}

struct {
	bool silent = false;
	bool supress_warnings = false;
	bool warnings_as_errors = false;
	string input;
	string output;
	EGSL::OutputForm output_form = EGSL::OutputForm::Library;
	EGSL::OutputTarget output_target = EGSL::OutputTarget::Unknown;
} state;

bool ParseCommandLine(Console & console)
{
	SafePointer< Array<string> > args = GetCommandLine();
	int i = 1;
	while (i < args->Length()) {
		auto & cmd = args->ElementAt(i);
		if (cmd[0] == L':' || cmd[0] == L'-') {
			i++;
			for (int j = 1; j < cmd.Length(); j++) {
				auto arg = cmd[j];
				if (arg == L'S') {
					state.silent = true;
				} else if (arg == L'W') {
					state.warnings_as_errors = true;
				} else if (arg == L'd') {
					state.output_form = EGSL::OutputForm::Source;
				} else if (arg == L'l') {
					state.output_form = EGSL::OutputForm::Library;
				} else if (arg == L'o') {
					if (i < args->Length()) {
						if (state.output.Length()) {
							console << TextColor(Console::ColorYellow) << FormatString(L"Output name redefinition.", args->ElementAt(i)) << TextColorDefault() << LineFeed();
							return false;
						}
						state.output = IO::ExpandPath(args->ElementAt(i));
						i++;
					} else {
						console << TextColor(Console::ColorYellow) << L"Invalid command line: argument expected." << TextColorDefault() << LineFeed();
						return false;
					}
				} else if (arg == L't') {
					if (i < args->Length()) {
						auto tag = GetTargetFromString(args->ElementAt(i));
						if (tag == EGSL::OutputTarget::Unknown) {
							console << TextColor(Console::ColorYellow) << FormatString(L"Invalid target implementation name: %0.", args->ElementAt(i)) << TextColorDefault() << LineFeed();
							return false;
						} else state.output_target = tag;
						i++;
					} else {
						console << TextColor(Console::ColorYellow) << L"Invalid command line: argument expected." << TextColorDefault() << LineFeed();
						return false;
					}
				} else if (arg == L'w') {
					state.supress_warnings = true;
				} else {
					console << TextColor(Console::ColorYellow) << FormatString(L"Command line argument \"%0\" is invalid.", string(arg, 1)) << TextColorDefault() << LineFeed();
					return false;
				}
			}
		} else {
			if (state.input.Length()) {
				console << TextColor(Console::ColorYellow) << L"Duplicate input file argument on command line." << TextColorDefault() << LineFeed();
				return false;
			}
			state.input = IO::ExpandPath(cmd);
			i++;
		}
	}
	return true;
}

int Main(void)
{
	Console console;
	try {
		if (!ParseCommandLine(console)) return 1;
		if (state.input.Length()) {
			if (state.output_target == EGSL::OutputTarget::Unknown) state.output_target = GetPlatformDefaultTarget();
			if (!state.output.Length()) {
				state.output = IO::Path::GetDirectory(state.input) + L"/" + IO::Path::GetFileNameWithoutExtension(state.input);
				state.output = IO::ExpandPath(state.output);
			} else {
				state.output = IO::Path::GetDirectory(state.output) + L"/" + IO::Path::GetFileNameWithoutExtension(state.output);
				state.output = IO::ExpandPath(state.output);
			}
			string chars;
			try {
				FileStream input_stream(state.input, AccessRead, OpenExisting);
				TextReader reader(&input_stream);
				DynamicString buffer;
				while (!reader.EofReached()) {
					auto line = reader.ReadLine();
					if (line.Length() > 1 && line[0] == L'#' && line[1] == L'#') buffer << L"\n";
					else buffer << line << L"\n";
				}
				chars = buffer.ToString();
			} catch (IO::FileAccessException & e) {
				if (!state.silent) console << TextColor(Console::ColorRed) <<
					FormatString(L"Failed to load the input file: file system error %0.", string(e.code, HexadecimalBase, 4)) << TextColorDefault() << LineFeed();
				return 1;
			}
			SafePointer< Array<Syntax::Token> > text;
			try {
				text = EGSL::ParseCode(chars);
			} catch (Syntax::ParserSpellingException & e) {
				string line, exdesc;
				int x, y, length, ofs;
				EGSL::LocateErrorPosition(chars, e, exdesc, line, x, y, ofs, length);
				if (!state.silent) {
					console << TextColor(Console::ColorRed) << FormatString(L"Compilation error: #%0 - %1 at (%2, %3).",
						string(uint(EGSL::CompilationError::InvalidToken), HexadecimalBase, 4),
						EGSL::DescriptionForError(EGSL::CompilationError::InvalidToken), y + 1, x + 1) << TextColorDefault() << LineFeed();
					if (exdesc.Length()) console << TextColor(Console::ColorRed) << exdesc << L"." << TextColorDefault() << LineFeed() << LineFeed();
					console << line << LineFeed();
					console << string(L' ', ofs) << TextColor(Console::ColorRed) << L"^" << string(L'~', length - 1) << TextColorDefault() << LineFeed();
				}
				return 1;
			}
			EGSL::CompilerCommonContext context;
			EGSL::InitializeContext(context, state.output_target);
			try {
				EGSL::TranslateCode(*text, context);
			} catch (Syntax::ParserSpellingException & e) {
				string line, exdesc;
				int x, y, length, ofs;
				EGSL::LocateErrorPosition(chars, e, exdesc, line, x, y, ofs, length);
				if (!state.silent) {
					console << TextColor(Console::ColorRed) << FormatString(L"Compilation error: #%0 - %1 at (%2, %3).",
						string(uint(EGSL::CompilationError::InvalidToken), HexadecimalBase, 4),
						EGSL::DescriptionForError(EGSL::CompilationError::InvalidToken), y + 1, x + 1) << TextColorDefault() << LineFeed();
					if (exdesc.Length()) console << TextColor(Console::ColorRed) << exdesc << L"." << TextColorDefault() << LineFeed() << LineFeed();
					console << line << LineFeed();
					console << string(L' ', ofs) << TextColor(Console::ColorRed) << L"^" << string(L'~', length - 1) << TextColorDefault() << LineFeed();
				}
				return 1;
			} catch (EGSL::CompilationException & e) {
				string line, exdesc;
				int x, y, length, ofs;
				EGSL::LocateErrorPosition(chars, *text, e, exdesc, line, x, y, ofs, length);
				if (!state.silent) {
					console << TextColor(Console::ColorRed) << FormatString(L"Compilation error: #%0 - %1 at (%2, %3).",
						string(uint(e.Error), HexadecimalBase, 4), EGSL::DescriptionForError(e.Error), y + 1, x + 1) << TextColorDefault() << LineFeed();
					if (exdesc.Length()) console << TextColor(Console::ColorRed) << exdesc << L"." << TextColorDefault() << LineFeed() << LineFeed();
					console << line << LineFeed();
					console << string(L' ', ofs) << TextColor(Console::ColorRed) << L"^" << string(L'~', length - 1) << TextColorDefault() << LineFeed();
				}
				return 1;
			}
			for (auto & h : context.hints) {
				if (!state.silent && !state.supress_warnings) {
					console << TextColor(Console::ColorYellow) << L"Warning: " << TextColorDefault() << h << L"." << LineFeed();
				}
			}
			if (context.hints.Length() && state.warnings_as_errors) return 1;
			if (state.output_target == EGSL::OutputTarget::Direct3D11) {
				for (auto & shader : context.Shaders) {
					DynamicString code;
					for (auto & str : context.StructureTypes) code << EGSL::MakeHlslCodeForStructure(&str);
					code << shader.Code;
					DynamicString error_log;
					shader.CompiledBlob = EGSL::CompileHLSL(code.ToString(), shader.TranslateName, shader.Class, error_log);
					if (shader.CompiledBlob) {
						if (!state.silent && !state.supress_warnings) {
							console.SetTextColor(Console::ColorYellow);
							console.Write(error_log.ToString());
							console.SetTextColor(Console::ColorDefault);
						}
						if (state.warnings_as_errors) for (int i = 0; i < error_log.Length(); i++) if (error_log[i] > 32) return 1;
					} else {
						if (!state.silent) {
							console.SetTextColor(Console::ColorRed);
							console.Write(error_log.ToString());
							console.SetTextColor(Console::ColorDefault);
						}
						if (state.output_form != EGSL::OutputForm::Source) return 1;
					}
					if (state.output_form == EGSL::OutputForm::Source) {
						FileStream stream(state.output + L"_" + shader.Name + L".hlsl", AccessWrite, CreateAlways);
						TextWriter writer(&stream, Encoding::ANSI);
						writer.Write(code);
					}
				}
				if (state.output_form == EGSL::OutputForm::Source) return 0;
				SafePointer<MemoryStream> output_raw = new MemoryStream(0x10000);
				SafePointer<Storage::NewArchive> archive = Storage::CreateArchive(output_raw, context.Shaders.Length(), Storage::NewArchiveFlags::UseFormat32);
				Storage::ArchiveFile file = 0;
				for (auto & shader : context.Shaders) {
					file++;
					archive->SetFileName(file, shader.Name);
					archive->SetFileData(file, shader.CompiledBlob->GetBuffer(), shader.CompiledBlob->Length());
					if (shader.Class == EGSL::ShaderClass::Vertex) {
						archive->SetFileCustom(file, 0x01);
					} else if (shader.Class == EGSL::ShaderClass::Pixel) {
						archive->SetFileCustom(file, 0x02);
					}
				}
				archive->Finalize();
				archive.SetReference(0);
				if (state.output_form == EGSL::OutputForm::Library) {
					FileStream output(state.output + L".egso", AccessWrite, CreateAlways);
					output_raw->Seek(0, Begin);
					output_raw->CopyTo(&output);
				}
			} else if (state.output_target == EGSL::OutputTarget::Metal) {
				DynamicString code;
				code << L"#include <metal_stdlib>\n";
				code << L"#include <simd/simd.h>\n\n";
				code << L"using namespace metal;\n\n";
				for (auto & str : context.StructureTypes) code << EGSL::MakeMslCodeForStructure(&str);
				for (auto & shader : context.Shaders) code << shader.Code;
				if (state.output_form == EGSL::OutputForm::Source) {
					DynamicString error_log;
					auto status = EGSL::CompileMSL(code, L"", error_log);
					if (status) {
						if (!state.silent && !state.supress_warnings) {
							console.SetTextColor(Console::ColorYellow);
							console.Write(error_log.ToString());
							console.SetTextColor(Console::ColorDefault);
						}
					} else if (!state.silent) {
						console.SetTextColor(Console::ColorRed);
						console.Write(error_log.ToString());
						console.SetTextColor(Console::ColorDefault);
					}
					FileStream stream(state.output + L".metal", AccessWrite, CreateAlways);
					TextWriter writer(&stream, Encoding::ANSI);
					writer.Write(code);
					return 0;
				}
				DynamicString error_log;
				auto status = EGSL::CompileMSL(code, state.output + L".egso", error_log);
				if (status) {
					if (!state.silent && !state.supress_warnings) {
						console.SetTextColor(Console::ColorYellow);
						console.Write(error_log.ToString());
						console.SetTextColor(Console::ColorDefault);
					}
					if (state.warnings_as_errors) for (int i = 0; i < error_log.Length(); i++) if (error_log[i] > 32) return 1;
				} else {
					if (!state.silent) {
						console.SetTextColor(Console::ColorRed);
						console.Write(error_log.ToString());
						console.SetTextColor(Console::ColorDefault);
					}
					return 1;
				}
			} else {
				if (!state.silent) console << TextColor(Console::ColorRed) << L"Internal error: unknown target interface." << TextColorDefault() << LineFeed();
				return 1;
			}
		} else if (!state.silent) {
			console << ENGINE_VI_APPNAME << LineFeed();
			console << L"Copyright " << string(ENGINE_VI_COPYRIGHT).Replace(L'\xA9', L"(C)") << LineFeed();
			console << L"Version " << ENGINE_VI_APPVERSION << L", build " << ENGINE_VI_BUILD << LineFeed() << LineFeed();
			console << L"Command line syntax:" << LineFeed();
			console << L"  " << ENGINE_VI_APPSYSNAME << L" <source.egsl> :SWdlotw" << LineFeed();
			console << L"Where source.egsl is the input EGSL file." << LineFeed();
			console << L"You can optionally use the next compile options:" << LineFeed();
			console << L"  :S - use silent mode - supress any text output," << LineFeed();
			console << L"  :W - interpret all warnings as errors," << LineFeed();
			console << L"  :d - translate only, don't compile the shaders," << LineFeed();
			console << L"  :l - build binary shader library file (.egso)," << LineFeed();
			console << L"  :o - specify the output library file (as the next argument)," << LineFeed();
			console << L"  :t - specify the target interface implementation (as the next argument)," << LineFeed();
			console << L"  :w - supress warnings." << LineFeed();
			console << LineFeed();
		}
	} catch (Exception & e) {
		if (!state.silent) console << TextColor(Console::ColorRed) << FormatString(L"Shader compiler failed: %0.", e.ToString()) << TextColorDefault() << LineFeed();
		return 1;
	} catch (...) {
		if (!state.silent) console << TextColor(Console::ColorRed) << L"Shader compiler failed: Unknown exception." << TextColorDefault() << LineFeed();
		return 1;
	}
	return 0;
}