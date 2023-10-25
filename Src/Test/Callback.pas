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
 Triton : TTritonCtx;
 flag   : Boolean;

procedure cb_flag(aApi : HandleContext; reg_mem: Pointer);cdecl;
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
                                    (addr:$400017; inst :[$48,$8b,$1d,$00,$01,$00,$00]; Size:7), //* mov rbx, [0x100] */
                                    (addr:$0;      inst :[$48,$01,$d8];                  Size:3), //* add rax, rbx */
                                    (addr:$0;      inst :[];                             Size:0)
                                   );

   procedure  setUp;
   begin
        Triton.Create;
        (* Set the arch *)
        Triton.setArchitecture(ARCH_X86_64);

        Triton.SetMode(ALIGNED_MEMORY, True);
   end;

   procedure handle_mem_read(ctx : HandleContext; ma: HandleMemAcc);
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
        symvar := Triton.symbolizeMemory(MemAccess(ma),AnsiString(comment)) ;
        mem_symvars := mem_symvars +  [ symvar ]
   end;

   Procedure handle_reg_read(ctx: HandleContext; reg: HandleReg);
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
        symvar  := Triton.symbolizeRegister(Registro(reg), AnsiString(comment));
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
    sUnRoll :=  ast.unroll(rax.Ast).ToStr;
    assert(sUnRoll = '(bvadd sym_reg_{rax} sym_reg_{rbx})') ;

    Triton.Free;

end;

procedure TestIssue792_795;

   procedure  setUp;
   begin
        Triton.Create;
        (* Set the arch *)
        Triton.setArchitecture(ARCH_X86_64);

        Triton.SetMode(ALIGNED_MEMORY, True);
   end;

var
 ast          : AstContext;
 var1,var2    : symbolicVar;
 ast_original,
 ast_duplicate,
 ast_unrolled,
 var1ast,
 var2ast,a1,b1,b2  : AbstractNode;
begin
    setUp;

    ast := Triton.getAstContext;

    var1 := Triton.newSymbolicVariable(64, 'var1');
    var2 := Triton.newSymbolicVariable(64, 'var2') ;

    ast_original  := ast.bvadd(ast.variable(var1), ast.variable(var2)) ;
    ast_duplicate := ast_original.duplicate;
    ast_unrolled  := ast.unroll(ast_original);

    Triton.setConcreteVariableValue(var1, 4);
    Triton.setConcreteVariableValue(var2, 2) ;

    assert(ast_original.evaluate = 6) ;
    assert(ast_duplicate.evaluate= 6);
    assert(ast_unrolled.evaluate= 6);

    ast_original.setChild(0, ast.bv(1, 64)) ;

    assert(ast_original.evaluate= 3) ;
    assert(ast_duplicate.evaluate= 6) ;
    assert(ast_unrolled.evaluate= 6);

    ast_duplicate.setChild(0, ast.bv(10, 64)) ;

    assert(ast_original.evaluate= 3) ;
    assert(ast_duplicate.evaluate= 12) ;
    assert(ast_unrolled.evaluate= 6) ;

    Triton.Free;

///Issue795
    setUp;

    ast := Triton.getAstContext;

    var1 := Triton.newSymbolicVariable(64, 'var1');
    var2 := Triton.newSymbolicVariable(64, 'var2') ;

    var1ast := ast.variable(var1);
    var2ast := ast.variable(var2);

    a1 := ast.bvadd(var1ast, var2ast) ;
    b1 := ast.bvnot(a1) ;
    b2 := b1.duplicate ;

    assert(Length(a1.Parents)= 1);
    assert(Length(b2.Parents)= 0);
    assert(Length(b2.Childrens[0].Parents)= 1);
    assert(Length(var1ast.Parents)= 2);
    assert(Length(var2ast.Parents)= 2);

    assert(b1.evaluate = b2.evaluate) ;
    Triton.setConcreteVariableValue(var1, 4) ;
    Triton.setConcreteVariableValue(var2, 2) ;
    assert(b1.evaluate = b2.evaluate);

    Triton.Free;

end;

procedure TestIssue673;

   procedure  setUp;
   begin
        Triton.Create;
        (* Set the arch *)
        Triton.setArchitecture(ARCH_X86_64);
   end;
var
  inst : Istruzione;

begin
    setUp;

    inst.Create;
    inst.setOpcode([$c0,$c0,$00]); // rol al, 0
    Triton.processing(inst);
    assert(inst.UndefinedRegisters.Count = 0);
    assert(inst.ReadRegisters.Count = 1) ;
    assert(inst.WrittenRegisters.Count = 2);

    inst.Create;
    inst.setOpcode([$c0,$c0,$01]); // rol al, 1
    Triton.processing(inst);
    assert(inst.UndefinedRegisters.Count = 0);
    assert(inst.ReadRegisters.Count= 2)  ;
    assert(inst.WrittenRegisters.Count= 4) ;

    inst.Create;
    inst.setOpcode([$c0,$c0,$07]); // rol al, 7
    Triton.processing(inst);
    assert(inst.UndefinedRegisters.Count= 1);
    assert(inst.ReadRegisters.Count= 2) ;
    assert(inst.WrittenRegisters.Count= 4) ;

    Triton.Free;

end;

procedure main_Callback;
begin
    test_get_concrete_memory_value ;
    test_get_concrete_register_value ;
    TestIssue789 ;
    TestIssue792_795  ;
    TestIssue673
end;

end.
