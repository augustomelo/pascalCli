.assembly extern mscorlib {}
.assembly 'program' {}
//.module program.exe // GUID = {3F2B7A05-BABC-4966-B8DD-F517D94BF38D}


  .class public auto ansi abstract sealed beforefieldinit Test extends [mscorlib]System.Object {

    // method line 1
    .method public static hidebysig default float32 numero ()  cil managed {
		// Method begins at RVA 0x2050
		// Code size 6 (0x6)
		.maxstack 8
		IL_0000:  ldc.r4 1234.
		IL_0005:  ret 
    } // end of method Test::numero

    // method line 2
    .method private static hidebysig default void Main ()  cil managed {
		// Method begins at RVA 0x2057
		.entrypoint
		// Code size 11 (0xb)
		.maxstack 8
		IL_0000:  call float32 class Test::numero()
		IL_0005:  call void class [mscorlib]System.Console::WriteLine(float32)
		IL_000a:  ret 
    } // end of method Test::Main

  } // end of class Test

