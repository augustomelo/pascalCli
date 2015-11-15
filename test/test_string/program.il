.assembly extern mscorlib { }
.assembly 'program' { }
//.module program.exe // GUID = {7E2BF1A3-C2DD-4425-90EE-7DA7AFB3F46B}


  .class public auto ansi abstract sealed beforefieldinit Test extends [mscorlib]System.Object {

    // method line 1
    .method public static hidebysig default string palavra ()  cil managed {
        // Method begins at RVA 0x2050
		// Code size 6 (0x6)
		.maxstack 8
		IL_0000:  ldstr "palavra"
		IL_0005:  ret 
    } // end of method Test::palavra

    // method line 2
    .method private static hidebysig default void Main ()  cil managed {
		// Method begins at RVA 0x2057
		.entrypoint
		// Code size 11 (0xb)
		.maxstack 8
		IL_0000:  call string class Test::palavra()
		IL_0005:  call void class [mscorlib]System.Console::WriteLine(string)
		IL_000a:  ret 
    } // end of method Test::Main

  } // end of class Test

