.assembly extern mscorlib { }
.assembly 'program' { }
//.module program.exe // GUID = {380F1626-05DE-4EA8-A7C5-823C1542D833}

  .class public auto ansi abstract sealed beforefieldinit Test extends [mscorlib]System.Object {

    // method line 1
    .method public static hidebysig default int32 numero ()  cil managed {
        // Method begins at RVA 0x2050
		// Code size 3 (0x3)
		.maxstack 8
		IL_0000:  ldc.i4.s 0x5a
		IL_0002:  ret 
    } // end of method Test::numero

    // method line 2
    .method private static hidebysig default void Main ()  cil managed {
        // Method begins at RVA 0x2054
		.entrypoint
		// Code size 11 (0xb)
		.maxstack 8
		IL_0000:  call int32 class Test::numero()
		IL_0005:  call void class [mscorlib]System.Console::WriteLine(int32)
		IL_000a:  ret 
    } // end of method Test::Main

  } // end of class Test

