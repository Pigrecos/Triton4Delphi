unit proving_opaque_predicates;

(*
## Example to detect opaque predicates. This example is based
## on the Tomislav Zubcic's blog post [0,1] =).
##
## Output:
##
##  $ python proving_opaque_predicates.py
##  xor eax, eax
##  jo 7
##  opaque predicate: never taken
##  ----------------------------------
##  xor eax, eax
##  je 7
##  opaque predicate: always taken
##  ----------------------------------
##  xor eax, ebx
##  je 7
##  not an opaque predicate
##  ----------------------------------
##  and eax, 0x3fffffff
##  and ebx, 0x3fffffff
##  xor ecx, edx
##  xor edx, edi
##  add eax, ebx
##  jo 0x16
##  opaque predicate: never taken
##  ----------------------------------
##  and eax, 0x3fffffff
##  and ebx, 0x3fffffff
##  xor ecx, edx
##  xor edx, edi
##  xor eax, ebx
##  je 0x16
##  not an opaque predicate
##  ----------------------------------
##
## [0] http://zubcic.re/blog/experimenting-with-z3-proving-opaque-predicates
## [1] https://www.reddit.com/r/ReverseEngineering/comments/4yf6tz/experimenting_with_z3_proving_opaque_predicates/
*)

interface
      uses Winapi.Windows, Winapi.Messages, System.SysUtils,System.Generics.Collections;
type
  op = record
    inst : array of Byte;
  end;


const
trace_1 : array[0..1] of op = (
    (inst:[$31,$C0]),                  // xor eax, eax
    (inst:[$0F,$80,$01,$00,$00,$00])   // jo 7
);

trace_2 : array[0..1] of op = (
    (inst:[$31,$C0]),                 // xor eax, eax
    (inst:[$0F,$84,$01,$00,$00,$00])  // je 7
);

trace_3 : array[0..1] of op = (
    (inst:[$31,$D8]),                 // xor eax, ebx
    (inst:[$0F,$84,$01,$00,$00,$00])  // je 7
);

trace_4 : array[0..5] of op = (
    (inst:[$25,$ff,$ff,$ff,$3f]),     // and eax, 0x3fffffff
    (inst:[$81,$e3,$ff,$ff,$ff,$3f]), // and ebx, 0x3fffffff
    (inst:[$31,$d1]),                 // xor ecx, edx
    (inst:[$31,$fa]),                 // xor edx, edi
    (inst:[$01,$d8]),                 // add eax, ebx
    (inst:[$0f,$80,$10,$00,$00,$00]) // jo 27
);

trace_5 : array[0..5] of op = (
    (inst:[$25,$ff,$ff,$ff,$3f]),     // and eax, 0x3fffffff
    (inst:[$81,$e3,$ff,$ff,$ff,$3f]), // and ebx, 0x3fffffff
    (inst:[$31,$d1]),                 // xor ecx, edx
    (inst:[$31,$fa]),                 // xor edx, edi
    (inst:[$31,$D8]),                 // xor eax, ebx
    (inst:[$0F,$84,$10,$00,$00,$00]) // je 16
);

        procedure test_trace(trace: array of op);

implementation
       uses Triton.Api,
            Triton.Core,
            Triton.Instruction,
            Triton.AstContext,
            Triton.AstNode,
            Triton.SymbolicExpression,
            Triton.SymbolicVariable,
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

procedure symbolization_init;
begin
    Triton.convertRegisterToSymbolicVariable( Triton.getRegister(ID_REG_X86_EAX) );
    Triton.convertRegisterToSymbolicVariable( Triton.getRegister(ID_REG_X86_EBX) );
    Triton.convertRegisterToSymbolicVariable( Triton.getRegister(ID_REG_X86_ECX) );
    Triton.convertRegisterToSymbolicVariable( Triton.getRegister(ID_REG_X86_EDX) );
end;

procedure test_trace(trace: array of op);
var
  i          : Integer;
  astCtxt    : AstContext;
  instruction: Istruzione;
  op_ast     : AbstractNode;
  model      : TDictionary<UInt32,SolverModel> ;
begin
    Triton.Create;

    Triton.setArchitecture(ARCH_X86) ;
    symbolization_init;

    astCtxt := Triton.getAstContext;

    for i := 0 to Length(trace) -1 do
    begin
        instruction.Create;
        instruction.setOpcode(topcode( trace[i].inst) ) ;
        Triton.processing(instruction);
        Form1.Log(instruction.disassembly);

        if instruction.branch then
        begin
            // Opaque Predicate AST
            op_ast := Triton.getPathConstraintsAst;
            // Try another model
            model := Triton.getModel(astCtxt.lnot(op_ast));
            if Assigned(model) and  (model.Count > 0) then
                Form1.Log('not an opaque predicate')
            else
                if instruction.conditionTaken then
                    Form1.Log('opaque predicate: always taken')
                else
                    Form1.Log('opaque predicate: never taken')
        end;
    end;
    Form1.Log('----------------------------------');
end;

end.
