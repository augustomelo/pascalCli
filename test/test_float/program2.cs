.assembly extern mscorlib
{
  .ver 4:0:0:0
  .publickeytoken = (B7 7A 5C 56 19 34 E0 89 ) // .z\V.4..
}
.assembly 'program2'
{
  .custom instance void class [mscorlib]System.Runtime.CompilerServices.RuntimeCompatibilityAttribute::'.ctor'() =  (
		01 00 01 00 54 02 16 57 72 61 70 4E 6F 6E 45 78   // ....T..WrapNonEx
		63 65 70 74 69 6F 6E 54 68 72 6F 77 73 01       ) // ceptionThrows.

  .hash algorithm 0x00008004
  .ver  0:0:0:0
}
.module program2.exe // GUID = {E875BB80-8486-497A-98A9-2222AD4B57B0}


  .class public auto ansi abstract sealed beforefieldinit Test
  	extends [mscorlib]System.Object
  {

    // method line 1
    .method public static hidebysig 
           default float32 numero ()  cil managed 
    {
        // Method begins at RVA 0x2050
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldc.r4 1234.989990234375
	IL_0005:  ret 
    } // end of method Test::numero

    // method line 2
    .method private static hidebysig 
           default void Main ()  cil managed 
    {
        // Method begins at RVA 0x2057
	.entrypoint
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  call float32 class Test::numero()
	IL_0005:  call void class [mscorlib]System.Console::WriteLine(float32)
	IL_000a:  ret 
    } // end of method Test::Main

  } // end of class Test

