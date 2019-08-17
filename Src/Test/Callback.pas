unit Callback;

interface
      uses Winapi.Windows, Winapi.Messages, System.SysUtils,System.Generics.Collections;
(*
## Output:
##
##  $ ./simplification.py
##  Expr:  (bvxor (_ bv1 8) (_ bv1 8))
##  Simp:  (_ bv0 8)
##
##  Expr:  (bvor (bvand (_ bv1 8) (bvnot (_ bv2 8))) (bvand (bvnot (_ bv1 8)) (_ bv2 8)))
##  Simp:  (bvxor (_ bv1 8) (_ bv2 8))
##
##  Expr:  (bvor (bvand (bvnot (_ bv2 8)) (_ bv1 8)) (bvand (bvnot (_ bv1 8)) (_ bv2 8)))
##  Simp:  (bvxor (_ bv1 8) (_ bv2 8))
##
##  Expr:  (bvor (bvand (bvnot (_ bv2 8)) (_ bv1 8)) (bvand (_ bv2 8) (bvnot (_ bv1 8))))
##  Simp:  (bvxor (_ bv1 8) (_ bv2 8))
##
##  Expr:  (bvor (bvand (_ bv2 8) (bvnot (_ bv1 8))) (bvand (bvnot (_ bv2 8)) (_ bv1 8)))
##  Simp:  (bvxor (_ bv2 8) (_ bv1 8))
##
*)


        procedure main_Callback;

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
 flag   : Boolean;

procedure cb_flag(aApi : HandleApi; reg_mem: Pointer);cdecl;
begin
    flag := True;
end;

procedure test_get_concrete_memory_value;
var
  Instruction : Istruzione;
begin
    Triton.Create;
    // Set arch to init engines
    Triton.setArchitecture(ARCH_X86_64);

    flag := False;
    Triton.addCallback(@cb_flag, GET_CONCRETE_MEMORY_VALUE) ;
    // movabs rax, qword ptr [0x1000]
    Instruction.Create;
    Instruction.setOpcode([$48,$a1,$00,$10,$00,$00,$00,$00,$00,$00]) ;
    Triton.processing(Instruction);
    assert(flag = True);

    flag := False;
    Triton.removeCallback(@cb_flag, GET_CONCRETE_MEMORY_VALUE) ;
    // movabs rax, qword ptr [0x1000]
    Instruction.Create;
    Instruction.setOpcode([$48,$a1,$00,$10,$00,$00,$00,$00,$00,$00]) ;
    Triton.processing(Instruction);
    assert(flag = False) ;

    Triton.Free;
end;

procedure test_get_concrete_register_value;
var
  Instruction : Istruzione;
begin
    Triton.Create;
    // Set arch to init engines
    Triton.setArchitecture(ARCH_X86_64);

    flag := False;
    Triton.addCallback(@cb_flag, GET_CONCRETE_REGISTER_VALUE);
    Instruction.Create;
    Instruction.setOpcode([$48,$89,$d8]);   // mov rax, rbx
    Triton.processing(Instruction) ;
    assert(flag = True);

    flag := False ;
    Triton.removeCallback(@cb_flag,GET_CONCRETE_REGISTER_VALUE);
    Instruction.Create;
    Instruction.setOpcode([$48,$89,$d8]);   // mov rax, rbx
    Triton.processing(Instruction) ;
    assert(flag = False);

    // Remove all callbacks
    flag := False ;
    Triton.addCallback(@cb_flag,GET_CONCRETE_REGISTER_VALUE);
    Instruction.Create;
    Instruction.setOpcode([$48,$89,$d8]); // mov rax, rbx
    Triton.processing(Instruction);
    assert(flag= True);

    flag := False;
    Triton.removeAllCallbacks;
    Instruction.Create;
    Instruction.setOpcode([$48,$89,$d8]); // mov rax, rbx
    Triton.processing(Instruction);
    assert(flag= false) ;

    Triton.Free;
end;

type
  op = record
     addr : UInt32;
     inst : array of Byte;
     size : UInt32;
  end;

var
  mem_symvars : TArray<SymbolicVar> ;
  reg_symvars : TArray<SymbolicVar> ;
  seen_regs   : TArray<uint32>;
  seen_mem    : TArray<uint64>;
procedure TestIssue789;
const code : array[0..2] of op  = (
                                    (addr:$400017; inst :[$48,$8b,$1d,$00,$01,$00,$00]; Size:3), //* mov rbx, [0x100] */
                                    (addr:$0;      inst :[$48,$01,$d8];                  Size:3), //* add rax, rbx */
                                    (addr:$0;      inst :[];                             Size:0)
                                   );

   procedure  setUp;
   begin
        Triton.Create;
        (* Set the arch *)
        Triton.setArchitecture(ARCH_X86_64);

        Triton.enableMode(ALIGNED_MEMORY, True);
   end;

   procedure handle_mem_read(ctx : HandleApi; ma: HandleMemAcc);
   var
     addr   : UInt64;
     comment: string;
     symvar : SymbolicVar;
     fIndex : Integer;
   begin
        // Keep track of seen registers to avoid recursion
        addr := MemAccess(ma).address;
        if TArray.BinarySearch<UInt64>(seen_mem, addr,fIndex) then Exit;
        seen_mem := seen_mem  + [ addr ] ;

        // Create symbolic var
        comment := Format('mem_{:#0x%x}',[MemAccess(ma).address]);
        symvar := Triton.convertMemoryToSymbolicVariable(MemAccess(ma),comment) ;
        mem_symvars := mem_symvars +  [ symvar ]
   end;

   Procedure handle_reg_read(ctx: HandleApi; reg: HandleReg);
   var
     reg_id : UInt32;
     comment: string;
     symvar : SymbolicVar;
     fIndex : Integer;
   begin
        // Keep track of seen registers to avoid recursion
        reg_id := Ord(Registro(reg).id);
        if TArray.BinarySearch<UInt32>(seen_regs, reg_id,fIndex) then  Exit;
        seen_regs := seen_regs +  [ reg_id ];

        // Create symbolic var
        comment := Format('sym_reg_{%s}',[Registro(reg).name] );
        symvar  := Triton.convertRegisterToSymbolicVariable(Registro(reg), comment);
        reg_symvars := reg_symvars + [ symvar ];
   end;

   procedure emulate(ip:UInt64);
   var
    inst : Istruzione;
    i    : Integer;
    node : AbstractNode;
   begin
        for i := 0 to Length(code)-2 do
        begin
            inst.Create;
            inst.setOpcode(TOpCode( code[i].inst));   // mov rax, rbx
            inst.setAddress(ip);
            Triton.processing(inst);
            node := (Triton.getRegisterAst( Triton.getRegister(ID_REG_X86_RIP)));
            ip   := node.evaluate;
        end;
   end;


var
 ast    : AstContext;
 rax    : symbolicExp;
 sUnRoll: string;
begin
    setUp;

    Triton.addCallback(@handle_mem_read, GET_CONCRETE_MEMORY_VALUE);
    Triton.addCallback(@handle_reg_read, GET_CONCRETE_REGISTER_VALUE);
    emulate($400000);
    ast := Triton.getAstContext;
    rax := Triton.getSymbolicRegister( Triton.getRegister(ID_REG_X86_RAX) );
    sUnRoll :=  (rax.Ast).unrollAst.ToStr;
    assert(sUnRoll = '(bvadd SymVar_2 SymVar_3)') ;

    Triton.Free;

end;


procedure main_Callback;
begin
    test_get_concrete_memory_value ;
    test_get_concrete_register_value ;
    TestIssue789
end;

end.
