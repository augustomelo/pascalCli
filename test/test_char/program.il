.assembly extern mscorlib { }
.assembly 'program' { }
//.module program.exe // GUID = {F569DEE4-B852-4CDE-97A8-AE70C6E57B4F}


  .class public auto ansi abstract sealed beforefieldinit Test extends [mscorlib]System.Object {

    // method line 1
    .method public static hidebysig default char letra ()  cil managed {
		// Method begins at RVA 0x2050
		// Code size 3 (0x3)
		.maxstack 8
		IL_0000:  ldc.i4.s 0x61
		IL_0002:  ret 
    } // end of method Test::letra

    // method line 2
    .method private static hidebysig default void Main ()  cil managed {
		// Method begins at RVA 0x2054
		.entrypoint
		// Code size 11 (0xb)
		.maxstack 8
		IL_0000:  call char class Test::letra()
		IL_0005:  call void class [mscorlib]System.Console::WriteLine(char)
		IL_000a:  ret 
    } // end of method Test::Main

  } // end of class Test

