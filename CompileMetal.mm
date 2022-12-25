#include "CompileMetal.h"

#include <PlatformDependent/CocoaInterop.h>

#import "Foundation/Foundation.h"
#import "Metal/Metal.h"

using namespace Engine::Streaming;

namespace Engine
{
	namespace EGSL
	{
		bool CompileMSL(const string & code, const string & output, DynamicString & log)
		{
			id<MTLDevice> device = MTLCreateSystemDefaultDevice();
			NSString * source = Cocoa::CocoaString(code);
			NSError * error;
			MTLCompileOptions * options = [[MTLCompileOptions alloc] init];
			id<MTLLibrary> library = [device newLibraryWithSource: source options: options error: &error];
			[options release];
			[source release];
			[device release];
			if (error) {
				auto str = Cocoa::EngineString([error localizedDescription]);
				[error release];
				log << str;
			}
			if (library) {
				[library release];
				if (output.Length()) {
					auto src = output + L"_src.metal";
					auto air = output + L"_obj.air";
					SafePointer<FileStream> file = new FileStream(src, AccessReadWrite, CreateAlways);
					SafePointer<TextWriter> writer = new TextWriter(file, Encoding::ANSI);
					writer->Write(code);
					writer.SetReference(0);
					file.SetReference(0);
					Array<string> cmds(0x10);
					// Common arguments
					cmds << L"-sdk";
					cmds << L"macosx";
					// Compile arguments
					cmds << L"metal";
					cmds << L"-c";
					cmds << src;
					cmds << L"-o";
					cmds << air;
					cmds << L"-std=macos-metal2.4";
					cmds << L"--target=air64-apple-macos11.0";
					SafePointer<Process> process = CreateCommandProcess(L"xcrun", &cmds);
					if (process) process->Wait();
					if (!process || process->GetExitCode()) return false;
					cmds.Clear();
					cmds << L"-sdk";
					cmds << L"macosx";
					// Link arguments
					cmds << L"metallib";
					cmds << air;
					cmds << L"-o";
					cmds << output;
					process = CreateCommandProcess(L"xcrun", &cmds);
					if (process) process->Wait();
					if (!process || process->GetExitCode()) return false;
					try {
						IO::RemoveFile(src);
						IO::RemoveFile(air);
					} catch (...) { return false; }
					log.Clear();
				}
				return true;
			} else return false;
		}
	}
}