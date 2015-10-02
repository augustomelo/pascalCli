.assembly extern mscorlib {}

    .assembly 'teste' {}

    .class public auto ansi abstract sealed beforefieldinit Test extends [mscorlib]System.Object {
.method public static hidebysig default int32 retorno ()  cil managed {

        .maxstack 8
IL_0000: ldc.i4 1234
IL_0002: ret

    }


    .method private static hidebysig default void Main ()  cil managed {

        .entrypoint

        .maxstack 8

        IL_0000: call int32 class Test::retorno()

        IL_0005: call void class [mscorlib]System.Console::WriteLine(int32)

        IL_000a: ret

        }

    }