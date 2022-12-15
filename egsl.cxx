#include <EngineRuntime.h>

#include "Core.h"
#include "Parser.h"
#include "Output.h"

using namespace Engine;
using namespace Engine::Streaming;
using namespace Engine::IO;
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
							console << TextColor(ConsoleColor::Yellow) << FormatString(L"Output name redefinition.", args->ElementAt(i)) << TextColorDefault() << LineFeed();
							return false;
						}
						state.output = IO::ExpandPath(args->ElementAt(i));
						i++;
					} else {
						console << TextColor(ConsoleColor::Yellow) << L"Invalid command line: argument expected." << TextColorDefault() << LineFeed();
						return false;
					}
				} else if (arg == L't') {
					if (i < args->Length()) {
						auto tag = GetTargetFromString(args->ElementAt(i));
						if (tag == EGSL::OutputTarget::Unknown) {
							console << TextColor(ConsoleColor::Yellow) << FormatString(L"Invalid target implementation name: %0.", args->ElementAt(i)) << TextColorDefault() << LineFeed();
							return false;
						} else state.output_target = tag;
						i++;
					} else {
						console << TextColor(ConsoleColor::Yellow) << L"Invalid command line: argument expected." << TextColorDefault() << LineFeed();
						return false;
					}
				} else if (arg == L'u') {
					state.output_form = EGSL::OutputForm::Universal;
				} else if (arg == L'w') {
					state.supress_warnings = true;
				} else {
					console << TextColor(ConsoleColor::Yellow) << FormatString(L"Command line argument \"%0\" is invalid.", string(arg, 1)) << TextColorDefault() << LineFeed();
					return false;
				}
			}
		} else {
			if (state.input.Length()) {
				console << TextColor(ConsoleColor::Yellow) << L"Duplicate input file argument on command line." << TextColorDefault() << LineFeed();
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
			if (!state.silent) console << L"Compiling " << TextColor(ConsoleColor::Cyan) <<
				IO::Path::GetFileName(state.input) << TextColorDefault() << L"...";
			if (state.output_target == EGSL::OutputTarget::Unknown) state.output_target = GetPlatformDefaultTarget();
			if (!state.output.Length()) {
				state.output = IO::Path::GetDirectory(state.input) + L"/" + IO::Path::GetFileNameWithoutExtension(state.input);
				state.output = IO::ExpandPath(state.output);
			} else {
				state.output = IO::Path::GetDirectory(state.output) + L"/" + IO::Path::GetFileNameWithoutExtension(state.output);
				state.output = IO::ExpandPath(state.output);
			}
			string code;
			try {
				FileStream input_stream(state.input, AccessRead, OpenExisting);
				TextReader reader(&input_stream);
				DynamicString buffer;
				while (!reader.EofReached()) {
					auto line = reader.ReadLine();
					if (line.Length() > 1 && line[0] == L'#' && line[1] == L'#') buffer << L"\n";
					else buffer << line << L"\n";
				}
				code = buffer.ToString();
			} catch (IO::FileAccessException & e) {
				if (!state.silent) {
					console << TextColor(ConsoleColor::Red) << L"Failed" << TextColorDefault() << LineFeed();
					console << TextColor(ConsoleColor::Red) << FormatString(L"Failed to load the input file: file system error %0.",
						string(e.code, HexadecimalBase, 4)) << TextColorDefault() << LineFeed();
				}
				return 1;
			}
			if (!EGSL::ProduceOutput(state.silent ? 0 : &console, code, state.output, state.output_form, state.output_target, state.warnings_as_errors, state.supress_warnings)) return 1;			
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
			console << L"  :u - build universal translated and not compiled bundle (.egsu)," << LineFeed();
			console << L"  :w - supress warnings." << LineFeed();
			console << LineFeed();
		}
	} catch (Exception & e) {
		if (!state.silent) {
			console << TextColor(ConsoleColor::Red) << L"Failed" << TextColorDefault() << LineFeed();
			console << TextColor(ConsoleColor::Red) << FormatString(L"Shader compiler failed: %0.", e.ToString()) << TextColorDefault() << LineFeed();
		}
		return 1;
	} catch (...) {
		if (!state.silent) {
			console << TextColor(ConsoleColor::Red) << L"Failed" << TextColorDefault() << LineFeed();
			console << TextColor(ConsoleColor::Red) << L"Shader compiler failed: Unknown exception." << TextColorDefault() << LineFeed();
		}
		return 1;
	}
	return 0;
}