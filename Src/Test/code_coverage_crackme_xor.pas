unit code_coverage_crackme_xor;

(* Output:
##
##  $ ./code_coverage_crackme_xor.py
##  Seed injected: {4096: 1}
##  Seed injected: {4096L: 101L}
##  Seed injected: {4096L: 0L}
##  Seed injected: {4096L: 101L, 4097L: 108L}
##  Seed injected: {4096L: 101L, 4097L: 0L}
##  Seed injected: {4096L: 101L, 4097L: 108L, 4098L: 105L}
##  Seed injected: {4096L: 101L, 4097L: 108L, 4098L: 0L}
##  Seed injected: {4096L: 101L, 4097L: 108L, 4098L: 105L, 4099L: 116L}
##  Seed injected: {4096L: 101L, 4097L: 108L, 4098L: 105L, 4099L: 0L}
##  Seed injected: {4096L: 101L, 4097L: 108L, 4098L: 105L, 4099L: 116L, 4100L: 101L}
##  Seed injected: {4096L: 101L, 4097L: 108L, 4098L: 105L, 4099L: 116L, 4100L: 0L}
*)

interface
      uses Winapi.Windows, Winapi.Messages, System.SysUtils,System.Generics.Collections,System.Generics.Defaults;
type
  op = record
    addr: UInt64;
    inst :TArray<Byte>;
  end;




const
(* Isolated function code which must be cover. The source code
#  of this function is basically:
#
#     /* Global serial */
#     char *serial = "\x31\x3e\x3d\x26\x31";
#
#     int check(char *ptr)
#     {
#       int i = 0;
#
#       while (i < 5){
#         if (((ptr[i] - 1) ^ 0x55) != serial[i])
#           return 1;
#         i++;
#       }
#       return 0;
#     }
#
# The objective is to cover this function and so return 1.
*)
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


        procedure main;

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
 test_count : Integer;
 Triton : TApi;
 dFunc   : TDictionary<UInt64,TArray<Byte>>;

// This function emulates the code.
procedure run(ip: UInt64);
var
 inst   : istruzione;
 node   : AbstractNode;
begin

    while dFunc.ContainsKey(ip) do
    begin
        // Build an instruction
        inst.Create;

        // Setup opcode
        inst.setOpcode(topcode( dFunc.Items[ip])) ;

        // Setup Address
        inst.setAddress(ip) ;

        // Process everything
        Triton.processing(inst) ;

        // Display instruction
        //Form1.Log(inst.ToStr);

        // Next instruction
        node := (Triton.getRegisterAst( Triton.getRegister(ID_REG_X86_RIP)));
        ip   := node.evaluate;
    end;
end;

// This function initializes the context memory.
procedure initContext;
begin
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

// This function returns a set of new inputs based on the last trace.
function getNewInput: TList< TDictionary<UInt64,UInt64> >;
var

  Pc      : PathConstraint;
  inputs  : TList< TDictionary<UInt64,UInt64> >;
  pco     : APathConstraint;
  astCtxt : AstContext;
  branches: ABranch;
  br      : Branch;
  symVar  : SymbolicVar;
  previousConstraints : AbstractNode;
  models  : TDictionary<UInt32,SolverModel> ;
  seed    : TDictionary<UInt64,UInt64>;
  n       : TPair<UInt32,SolverModel>;
begin
    // Set of new inputs
    inputs := TList< TDictionary<UInt64,UInt64> >.Create;

    // Get path constraints from the last execution
    pco := Triton.getPathConstraints;

    // Get the astContext
    astCtxt := Triton.getAstContext;

    // We start with any input. T (Top)
    previousConstraints := astCtxt.equal( astCtxt.bvtrue, astCtxt.bvtrue) ;

    // Go through the path constraints
    for pc in pco do
    begin
        // If there is a condition
        if pc.isMultipleBranches then
        begin
            // Get all branches
            branches := pc.BranchConstraints;
            for br in branches do
            begin
                // Get the constraint of the branch which has been not taken
                if br.taken= False then
                begin
                    // Ask for a model
                    models := Triton.getModel( astCtxt.land( previousConstraints, br.pc) );
                    seed   := TDictionary<UInt64,UInt64>.Create;
                    for n in models do
                    begin
                        // Get the symbolic variable assigned to the model
                        symVar := symbolicVar( Triton.getSymbolicVariableFromId(n.Key) );
                        // Save the new input as seed.
                        seed.AddOrSetValue(symVar.Origin,  n.Value.Value);
                    end;
                    if seed.Count > 0 then
                        inputs.Add(seed) ;
                end;
            end;
        end;
        // Update the previous constraints with true branch to keep a good path.
        previousConstraints := astCtxt.land(previousConstraints, pc.TakenPathConstraintAst)
    end;
    // Clear the path constraints to be clean at the next execution.
    Triton.clearPathConstraints;

    Result := inputs ;
end;

procedure symbolizeInputs(seed: TDictionary<UInt64,UInt64>);
var
 n       : TPair<UInt64,UInt64>;
 m1,m2   : ^MemAccess;

begin
    // Clean symbolic state
    Triton.concretizeAllRegister;
    Triton.concretizeAllMemory;

    for n in seed do
    begin
        New(m1);
        New(m2);

        m1^.Create(n.Key,   BYTE_SIZE);
        m2^.Create(n.Key+1, BYTE_SIZE);

        Triton.setConcreteMemoryValue(n.Key,n.Value);
        Triton.symbolizeMemory( m1^, 'm1' );
        Triton.symbolizeMemory( m2^ ,'m2');

    end;
end;



procedure main;
var
  uEntry     : UInt64;
  inputs     : TDictionary<UInt64,UInt64>;
  seed       : TDictionary<UInt64,UInt64>;
  lastInput  : TList< TDictionary<UInt64,UInt64> > ;
  worklist   : TList< TDictionary<UInt64,UInt64> > ;
  newInputs  : TList< TDictionary<UInt64,UInt64> >;
  Comparer   : IComparer<TDictionary<UInt64,UInt64>>;
  findItem   : Integer;
  function SeedToStr(dict : TDictionary<UInt64,UInt64>): string;
   var
     i    : integer;
     s,s1 : string;
     u    : TArray<Uint64> ;
  begin
      s  := 'Seed injected: {';
      s1 := '';
      u  := dict.Keys.ToArray;

      TArray.Sort<Uint64>(u);
      for i in u do
         s1 := s1 + i.ToString +' : '+ dict[i].ToString +',';
      s1 := Copy(s1,1,Length(s1)-1);

      Result := s + s1 +'}';
  end;

begin
    dFunc :=  TDictionary<UInt64,TArray<Byte>>.Create;
    for var i : Integer := 0to High(fun) do
     dFunc.Add(fun[i].addr,fun[i].inst);

    Comparer := TDelegatedComparer< TDictionary<UInt64,UInt64> >.Construct(
    function (const L, R: TDictionary<UInt64,UInt64>): Integer
    var
      k : Tarray<Uint64>;
      v : Tarray<Uint64>;
      cv,ck : uint64 ;
    begin
        k := L.Keys.ToArray;
        v := L.Values.ToArray;

        cv := 0; ck := 0;

        for var i := 0 to High(k) do
          ck := ck xor k[i];
        for var j := 0 to High(v) do
          cv := cv xor v[j];

        var a : uint64 := ck xor cv;

        k := R.Keys.ToArray;
        v := R.Values.ToArray;

        cv := 0; ck := 0;

        for var i := 0 to High(k) do
          ck := ck xor k[i];
        for var j := 0 to High(v) do
          cv := cv xor v[j];

        var b : uint64 := ck xor cv;

        Result :=a - b ;
    end
  );

    test_count := 0;

    Triton.Create;
    // Set the architecture
    Triton.setArchitecture(ARCH_X86_64);
    // Symbolic optimization
    Triton.SetMode(ALIGNED_MEMORY, True) ;

    // Define entry point
    uEntry := $40056d;

    // We start the execution with a random value located at 0x1000.
    lastInput := TList< TDictionary<UInt64,UInt64> >.Create;
    worklist  := TList< TDictionary<UInt64,UInt64> >.Create;

    worklist.Add(  TDictionary<UInt64,UInt64>.Create );
    worklist.Items[worklist.Count -1].Add($1000,1);

    while worklist.Count >0 do
    begin
        // Take the first seed
        seed := worklist[0] ;

        Form1.Log( SeedToStr(seed) );

        // Symbolize inputs
        symbolizeInputs(seed);

        // Init context memory
        initContext;

        // Emulate
        run(uEntry);

        lastInput.Add(seed);
        worklist.Delete(0);
        worklist.TrimExcess();
        lastInput.Sort(Comparer);
        worklist.Sort(Comparer);

        newInputs := getNewInput;

        for inputs in newInputs do
        begin
            if (not lastInput.BinarySearch(inputs,findItem,Comparer)) and (not worklist.BinarySearch(inputs,findItem,Comparer)) then
            begin
                worklist.Add( inputs );
            end;
        end;
        inc(test_count);

    end;

end;
end.
