unit test_path_constraint;

interface
      uses Winapi.Windows, Winapi.Messages, System.SysUtils,System.Generics.Collections;
type
  op = record
    inst : array of Byte;
  end;


const

demo_p : array[0..5] of op = (
    (inst:[$25,$ff,$ff,$ff,$3f]),     // and eax, 0x3fffffff
    (inst:[$81,$e3,$ff,$ff,$ff,$3f]), // and ebx, 0x3fffffff
    (inst:[$31,$d1]),                 // xor ecx, edx
    (inst:[$31,$fa]),                 // xor edx, edi
    (inst:[$01,$d8]),                 // add eax, ebx
    (inst:[$0f,$84,$55,$00,$00,$00])  // je 55
);



        procedure test_Path;

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

procedure test_trace(trace: array of op);
var
  i          : Integer;
  instruction: Istruzione;

begin
    Triton.Create;

    Triton.setArchitecture(ARCH_X86) ;

    Triton.convertRegisterToSymbolicVariable( Triton.getRegister(ID_REG_X86_EAX));
    Triton.convertRegisterToSymbolicVariable( Triton.getRegister(ID_REG_X86_EBX)) ;

    for i := 0 to Length(trace) -1 do
    begin
        instruction.Create;
        instruction.setOpcode(topcode( trace[i].inst) ) ;

        Triton.processing(instruction);
        Form1.Log(instruction.disassembly);


    end;
end;

procedure test_getPathConstraintsAst;
var
  astCtx  : AstContext;
  crst    : AbstractNode;
begin
        //Test getPathConstraintsAst//
        astCtx := Triton.getAstContext;
        crst := Triton.getPathConstraintsAst;
        assert(Triton.getModel(crst).Count <> 0);
        assert( Triton.getModel( astCtx.lnot(crst)).Count <> 0) ;
end;

procedure test_getPathConstraints;
var
  pco     : APathConstraint;
begin
        //Test getPathConstraints//
        pco := Triton.getPathConstraints;
        assert(Length(pco) = 1) ;
end;

procedure test_isMultipleBranches;
var
  Pc      : PathConstraint;
begin
        pc := Triton.getPathConstraints[0];
        assert(pc.isMultipleBranches = True)
end;

procedure test_getTakenPathConstraintAst;
var
  Pc      : PathConstraint;
begin
        pc := Triton.getPathConstraints[0];
        assert(pc.TakenPathConstraintAst.evaluate = 1);
end;

procedure test_getTakenAddress;
var
  Pc      : PathConstraint;
begin
        pc := Triton.getPathConstraints[0];
        assert(pc.TakenAddress = 91);
end;

procedure test_getBranchConstraints;
var
  Pc      : ABranch;
begin
        pc := Triton.getPathConstraints[0].BranchConstraints;

        assert(pc[0].taken = True) ;
        assert(pc[1].taken = False);

        assert(pc[0].srcAddr =  pc[1].srcAddr) ;

        assert(pc[0].dstAddr = 91);
        assert(pc[1].dstAddr = 23);
end;

procedure  test_Path;
begin
    test_trace(Demo_p);
    test_getPathConstraintsAst;
    test_getPathConstraints;
    test_isMultipleBranches;
    test_getTakenPathConstraintAst;
    test_getTakenAddress;
    test_getBranchConstraints;

    Triton.Free;
end;


end.
