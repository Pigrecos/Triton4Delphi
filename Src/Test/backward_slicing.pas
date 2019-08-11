unit backward_slicing;


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


        procedure main_slicing;

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



// This function initializes the context memory.
procedure initContext;
begin
    // Define our input context
    Triton.setConcreteMemoryValue($1000, ord('a'));
    Triton.setConcreteMemoryValue($1001, ord('b'));
    Triton.setConcreteMemoryValue($1002, ord('c'));
    Triton.setConcreteMemoryValue($1003, ord('d'));
    Triton.setConcreteMemoryValue($1004, ord('e'));

    // Define the address of the serial pointer. The address of the serial pointer
    // must be the same that the one hardcoded into the targeted function. However,
    // the serial pointer (here $900000) is arbitrary.
    Triton.setConcreteMemoryValue($601040, $00) ;
    Triton.setConcreteMemoryValue($601041, $00) ;
    Triton.setConcreteMemoryValue($601042, $90) ;

    // Define the serial context. We store the serial content located on our arbitrary
    // serial pointer ($900000).
    Triton.setConcreteMemoryValue($900000, $31) ;
    Triton.setConcreteMemoryValue($900001, $3e) ;
    Triton.setConcreteMemoryValue($900002, $3d) ;
    Triton.setConcreteMemoryValue($900003, $26) ;
    Triton.setConcreteMemoryValue($900004, $31) ;

    // Point RDI on our buffer. The address of our buffer is arbitrary. We just need
    // to point the RDI register on it as first argument of our targeted function.
    Triton.setConcreteRegisterValue( Triton.getRegister(ID_REG_X86_RDI), $1000);

    // Setup stack on an abitrary address.
    Triton.setConcreteRegisterValue( Triton.getRegister(ID_REG_X86_RSP), $7fffffff);
    Triton.setConcreteRegisterValue( Triton.getRegister(ID_REG_X86_RBP), $7fffffff);

end;


procedure main_slicing;
var
  inst       : istruzione;
  i          : integer;
  pc         : UInt64;
  rcxExpr,se : symbolicExp;
  slicing    : TDictionary<usize,symbolicExp> ;
  sliceKey   : usize;
  sizeArray  : TArray<usize>;
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

        // Spread the instruction address and its disassembly into its symbolic
        // expressions. Used to refer an instruction to its expressions. Will
        // be useful to understand the slicing part.
        for se in inst.SymbolicExpressions do
            se.Commento := inst.ToStr;

        // Next instruction
        pc := (Triton.getConcreteRegisterValue( Triton.getRegister(ID_REG_X86_RIP)));

        // Let's slice (backward) the rcx expression at 0x4005ae
        if pc = $4005ae then
        begin
            // Get the symbolic expression of RCX
            if Triton.getSymbolicRegisters.ContainsKey(ID_REG_X86_RCX) then
                 rcxExpr := Triton.getSymbolicRegisters.Items[ID_REG_X86_RCX];

            // Backward slice the RCX expression.
            slicing := Triton.sliceExpressions(rcxExpr);

            // sorting
            sizeArray := slicing.Keys.ToArray;
            TArray.Sort<usize>(sizeArray);

            for sliceKey in sizeArray do
               Form1.Log('[slicing]' + slicing.Items[sliceKey].Commento)

        end;

     end;

end;

end.
