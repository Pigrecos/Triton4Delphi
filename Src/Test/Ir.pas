unit Ir;

(* Output:
##
## [slicing] 0x40058b: movzx eax, byte ptr [rax]
## [slicing] 0x40058e: movsx eax, al
## [slicing] 0x400591: sub eax, 1
## [slicing] 0x400594: xor eax, 0x55
## [slicing] 0x400597: mov ecx, eax
*)

interface
      uses Winapi.Windows, Winapi.Messages, System.SysUtils,System.Generics.Collections;
type
  op = record
    addr: UInt64;
    inst :TArray<Byte>;
  end;


const

fun : array[0..6] of op = (

  (addr: $400000; inst:[$48,$8b,$05,$b8,$13,$00,$00]), // mov        rax, QWORD PTR [rip+0x13b8]
  (addr: $400007; inst:[$48,$8d,$34,$c3]),             // lea        rsi, [rbx+rax*8]
  (addr: $40000b; inst:[$67,$48,$8D,$74,$C3,$0A]),     // lea        rsi, [ebx+eax*8+0xa]
  (addr: $400011; inst:[$66,$0F,$D7,$D1]),             // pmovmskb   edx, xmm1
  (addr: $400015; inst:[$89,$d0]),                     // mov        eax, edx
  (addr: $400017; inst:[$80,$f4,$99]),                 // xor        ah, 0x99
  (addr: $40001a; inst:[$C5,$FD,$6F,$CA])              // vmovdqa    ymm1, ymm2
  );


        procedure main_Ir;

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

procedure main_Ir;
var
  inst       : istruzione;
  i          : integer;
  pc         : UInt64;
  se         : symbolicExp;

begin
    Triton.Create;

    dFunc :=  TDictionary<UInt64,TArray<Byte>>.Create;
    for i := 0to High(fun) do
     dFunc.Add(fun[i].addr,fun[i].inst);


    // Set the architecture
    Triton.setArchitecture(ARCH_X86_64);


    for i := 0 to Length(fun)- 1 do
    begin
        // Build an instruction
        inst.Create;

        // Setup opcode
        inst.setOpcode(topcode( fun[i].inst)) ;

        // Setup Address
        inst.setAddress(fun[i].addr) ;

        // Process everything
        Triton.processing(inst) ;

        Form1.Log(inst.ToStr) ;

        // Spread the instruction address and its disassembly into its symbolic
        // expressions. Used to refer an instruction to its expressions. Will
        // be useful to understand the slicing part.
        for se in inst.SymbolicExpressions do
           Form1.Log('    '+se.ToStr);

    end;

end;


end.
