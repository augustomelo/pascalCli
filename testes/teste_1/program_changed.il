.assembly extern mscorlib {}
.assembly 'program2' {}
.module program2.exe 


  .class private auto ansi beforefieldinit Test
  	extends [mscorlib]System.Object
  {

    // method line 1
    .method public hidebysig specialname rtspecialname 
           instance default void '.ctor' ()  cil managed 
    {
        // Method begins at RVA 0x2050
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void object::'.ctor'()
	IL_0006:  ret 
    } // end of method Test::.ctor

    // method line 2
    .method private static hidebysig 
           default int32 numero ()  cil managed 
    {
        // Method begins at RVA 0x2058
	// Code size 3 (0x3)
	.maxstack 8
	IL_0000:  ldc.i4.s 0x5a
	IL_0002:  ret 
    } // end of method Test::numero

    // method line 3
    .method private static hidebysig 
           default void Main ()  cil managed 
    {
        // Method begins at RVA 0x205c
	.entrypoint
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  call int32 class Test::numero()
	IL_0005:  call void class [mscorlib]System.Console::WriteLine(int32)
	IL_000a:  ret 
    } // end of method Test::Main

  } // end of class Test

