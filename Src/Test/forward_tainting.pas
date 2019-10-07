unit forward_tainting;


(* Output:
##
## [tainted] 0x40058e: movsx eax, al
## [tainted] 0x400591: sub eax, 1
## [tainted] 0x400594: xor eax, 0x55
## [tainted] 0x400597: mov ecx, eax
## [tainted] 0x4005ae: cmp ecx, eax
## [tainted] 0x4005b0: je 0x4005b9

*)

interface
      uses Winapi.Windows, Winapi.Messages, System.SysUtils,System.Generics.Collections;
type
  op = record
    addr: UInt64;
    inst :TArray<Byte>;
  end;


const

fun : array[0..29] of op = (
                                                        //   <serial> function
  (addr: $40056d; inst:[$55]),                           //   push    rbp
  (addr: $40056e; inst:[$48,$89,$e5]),                   //   mov     rbp,rsp
  (addr: $400571; inst:[$48,$89,$7d,$e8]),               //   mov     QWORD PTR [rbp-(addr: 18],rdi
  (addr: $400575; inst:[$c7,$45,$fc,$00,$00,$00,$00]),   //   mov     DWORD PTR [rbp-(addr: 4],(addr: 0
  (addr: $40057c; inst:[$eb,$3f]),                       //   jmp     4005bd <check+(addr: 50>
  (addr: $40057e; inst:[$8b,$45,$fc]),                   //   mov     eax,DWORD PTR [rbp-(addr: 4]
  (addr: $400581; inst:[$48,$63,$d0]),                   //   movsxd  rdx,eax
  (addr: $400584; inst:[$48,$8b,$45,$e8]),               //   mov     rax,QWORD PTR [rbp-(addr: 18]
  (addr: $400588; inst:[$48,$01,$d0]),                   //   add     rax,rdx
  (addr: $40058b; inst:[$0f,$b6,$00]),                   //   movzx   eax,BYTE PTR [rax]
  (addr: $40058e; inst:[$0f,$be,$c0]),                   //   movsx   eax,al
  (addr: $400591; inst:[$83,$e8,$01]),                   //   sub     eax,(addr: 1
  (addr: $400594; inst:[$83,$f0,$55]),                   //   xor     eax,(addr: 55
  (addr: $400597; inst:[$89,$c1]),                       //   mov     ecx,eax
  (addr: $400599; inst:[$48,$8b,$15,$a0,$0a,$20,$00]),   //   mov     rdx,QWORD PTR [rip+(addr: 200aa0]        // 601040 <serial>
  (addr: $4005a0; inst:[$8b,$45,$fc]),                   //   mov     eax,DWORD PTR [rbp-(addr: 4]
  (addr: $4005a3; inst:[$48,$98]),                       //   cdqe
  (addr: $4005a5; inst:[$48,$01,$d0]),                   //   add     rax,rdx
  (addr: $4005a8; inst:[$0f,$b6,$00]),                   //   movzx   eax,BYTE PTR [rax]
  (addr: $4005ab; inst:[$0f,$be,$c0]),                   //   movsx   eax,al
  (addr: $4005ae; inst:[$39,$c1]),                       //   cmp     ecx,eax
  (addr: $4005b0; inst:[$74,$07]),                       //   je      4005b9 <check+(addr: 4c>
  (addr: $4005b2; inst:[$b8,$01,$00,$00,$00]),           //   mov     eax,(addr: 1
  (addr: $4005b7; inst:[$eb,$0f]),                       //   jmp     4005c8 <check+(addr: 5b>
  (addr: $4005b9; inst:[$83,$45,$fc,$01]),               //   add     DWORD PTR [rbp-(addr: 4],(addr: 1
  (addr: $4005bd; inst:[$83,$7d,$fc,$04]),               //   cmp     DWORD PTR [rbp-(addr: 4],(addr: 4
  (addr: $4005c1; inst:[$7e,$bb]),                       //   jle     40057e <check+(addr: 11>
  (addr: $4005c3; inst:[$b8,$00,$00,$00,$00]),           //   mov     eax,(addr: 0
  (addr: $4005c8; inst:[$5d]),                           //   pop     rbp
  (addr: $4005c9; inst:[$c3])                            //   ret
);


        procedure main_taint;

implementation
       uses Triton.Api,
            Triton.Core,
            Triton.Instruction,
            Triton.AstContext,
            Triton.AstNode,
            Triton.SymbolicExpression,
            Triton.SymbolicVariable,
            Triton.pathConstraint,
            Triton.SolverModel,
            Triton.Register,
            Triton.BitVector,
            Triton.Immediate,
            Triton.MemoryAccess,
            Triton.OperandWrapper,
            Triton.Define,
            UntMain;

var
 Triton : TApi;
 dFunc   : TDictionary<UInt64,TArray<Byte>>;



procedure main_taint;
var
  inst       : istruzione;
  i          : integer;
  pc         : UInt64;

begin
    Triton.Create;

    dFunc :=  TDictionary<UInt64,TArray<Byte>>.Create;
    for i := 0to High(fun) do
     dFunc.Add(fun[i].addr,fun[i].inst);


    // Set the architecture
    Triton.setArchitecture(ARCH_X86_64);

    // Symbolic optimization
    Triton.enableMode(ALIGNED_MEMORY, True) ;
    Triton.setAstRepresentationMode(Ord(PYTHON_REPRESENTATION));

    // Define entry point
    pc := $40056d;

    // We start the execution with a random value located at 0x1000.

     while dFunc.ContainsKey(pc) do
     begin
        // Build an instruction
        inst.Create;

        // Setup opcode
        inst.setOpcode(topcode( dFunc.Items[pc])) ;

        // Setup Address
        inst.setAddress(pc) ;

        // Process everything
        Triton.processing(inst) ;

        // I know that at 0x40058b the user can control eax, so i'm tainting it.
        if inst.address = $40058b then
            Triton.taintRegister( Triton.getRegister(ID_REG_X86_RAX) ) ;

        // Print only instructions that are tainted.
        if inst.tainted then
             Form1.Log(inst.ToStr);

        // Next instruction
        pc := (Triton.getConcreteRegisterValue( Triton.getRegister(ID_REG_X86_RIP)));

     end;

end;
end.
