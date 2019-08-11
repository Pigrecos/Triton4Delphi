unit Ir;

(* Output:
##
0x400000: mov rax, qword ptr [rip + 0x13b8]
   (define-fun ref!0 () (_ BitVec 64) (concat (_ bv0 8) (_ bv0 8) (_ bv0 8) (_ bv0 8) (_ bv0 8) (_ bv0 8) (_ bv0 8) (_ bv0 8))) ; MOV operation
   (define-fun ref!1 () (_ BitVec 64) (_ bv4194311 64)) ; Program Counter

0x400007: lea rsi, [rbx + rax*8]
   (define-fun ref!2 () (_ BitVec 64) (bvadd (_ bv0 64) (bvadd (_ bv0 64) (bvmul ref!0 (_ bv8 64))))) ; LEA operation
   (define-fun ref!3 () (_ BitVec 64) (_ bv4194315 64)) ; Program Counter

0x40000b: lea rsi, [ebx + eax*8 + 0xa]
  (define-fun ref!4 () (_ BitVec 64) ((_ zero_extend 32) (bvadd (_ bv10 32) (bvadd (_ bv0 32) (bvmul ((_ extract 31 0) ref!0) (_ bv8 32)))))) ; LEA operation
  (define-fun ref!5 () (_ BitVec 64) (_ bv4194321 64)) ; Program Counter

0x400011: pmovmskb edx, xmm1
  (define-fun ref!6 () (_ BitVec 64) ((_ zero_extend 32) ((_ zero_extend 16) (concat ((_ extract 127 127) (_ bv0 128)) ((_ extract 119 119) (_ bv0 128)) ((_ extract 111 111) (_ bv0 128)) ((_ extract 103 103) (_ bv0 128)) ((_ extract 95 95) (_ bv0 128)) ((_ extract 87 87) (_ bv0 128)) ((_ extract 79 79) (_ bv0 128)) ((_ extract 71 71) (_ bv0 128)) ((_ extract 63 63) (_ bv0 128)) ((_ extract 55 55) (_ bv0 128)) ((_ extract 47 47) (_ bv0 128)) ((_ extract 39 39) (_ bv0 128)) ((_ extract 31 31) (_ bv0 128)) ((_ extract 23 23) (_ bv0 128)) ((_ extract 15 15) (_ bv0 128)) ((_ extract 7 7) (_ bv0 128)))))) ; PMOVMSKB operation
  (define-fun ref!7 () (_ BitVec 64) (_ bv4194325 64)) ; Program Counter

0x400015: mov eax, edx
  (define-fun ref!8 () (_ BitVec 64) ((_ zero_extend 32) ((_ extract 31 0) ref!6))) ; MOV operation
  (define-fun ref!9 () (_ BitVec 64) (_ bv4194327 64)) ; Program Counter

0x400017: xor ah, 0x99
  (define-fun ref!10 () (_ BitVec 64) (concat ((_ extract 63 16) ref!8) (concat (bvxor ((_ extract 15 8) ref!8) (_ bv153 8)) ((_ extract 7 0) ref!8)))) ; XOR operation
  (define-fun ref!11 () (_ BitVec 1) (_ bv0 1)) ; Clears carry flag
  (define-fun ref!12 () (_ BitVec 1) (_ bv0 1)) ; Clears overflow flag
  (define-fun ref!13 () (_ BitVec 1) (bvxor (bvxor (bvxor (bvxor (bvxor (bvxor (bvxor (bvxor (_ bv1 1) ((_ extract 0 0) (bvlshr ((_ extract 15 8) ref!10) (_ bv0 8)))) ((_ extract 0 0) (bvlshr ((_ extract 15 8) ref!10) (_ bv1 8)))) ((_ extract 0 0) (bvlshr ((_ extract 15 8) ref!10) (_ bv2 8)))) ((_ extract 0 0) (bvlshr ((_ extract 15 8) ref!10) (_ bv3 8)))) ((_ extract 0 0) (bvlshr ((_ extract 15 8) ref!10) (_ bv4 8)))) ((_ extract 0 0) (bvlshr ((_ extract 15 8) ref!10) (_ bv5 8)))) ((_ extract 0 0) (bvlshr ((_ extract 15 8) ref!10) (_ bv6 8)))) ((_ extract 0 0) (bvlshr ((_ extract 15 8) ref!10) (_ bv7 8))))) ; Parity flag
  (define-fun ref!14 () (_ BitVec 1) ((_ extract 15 15) ref!10)) ; Sign flag
  (define-fun ref!15 () (_ BitVec 1) (ite (= ((_ extract 15 8) ref!10) (_ bv0 8)) (_ bv1 1) (_ bv0 1))) ; Zero flag
  (define-fun ref!16 () (_ BitVec 64) (_ bv4194330 64)) ; Program Counter

0x40001a: vmovdqa ymm1, ymm2
  (define-fun ref!17 () (_ BitVec 512) ((_ zero_extend 256) (_ bv0 256))) ; VMOVDQA operation
  (define-fun ref!18 () (_ BitVec 64) (_ bv4194334 64)) ; Program Counter
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
