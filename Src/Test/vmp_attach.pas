unit vmp_attach;

interface
     uses System.SysUtils, System.IOUtils, System.Classes, System.Generics.Collections, Spring,
     Triton.Define;

     procedure analysis(trace1, trace2: string; symsize: Integer; vbraddr: UInt64; vbrflag: register_e= ID_REG_INVALID);

implementation
    uses  Triton.Api,
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
          triton.llvmToTriton,

          LLVM.Imports.Types,
          LLVM.Imports.Core,
          LLVM.Imports.Scalar,
          LLVM.Imports.IPO,

          UntMain;



var
  V_JMP : TList<AbstractNode>;

    var register_dil  : Registro;
    var register_sil  : Registro;

    var register_di   : Registro;
    var register_si   : Registro;

    var register_eax : Registro;
    var register_edi : Registro;
    var register_esi : Registro;

    var register_rax : Registro;
    var register_rbx : Registro;
    var register_rcx : Registro;
    var register_rdx : Registro;
    var register_rdi : Registro;
    var register_rsi : Registro;
    var register_rbp : Registro;
    var register_rsp : Registro;
    var register_r8  : Registro;
    var register_r9  : Registro;
    var register_r10 : Registro;
    var register_r11 : Registro;
    var register_r12 : Registro;
    var register_r13 : Registro;
    var register_r14 : Registro;
    var register_r15 : Registro;

    var register_cf  : Registro;
    var register_af  : Registro;

procedure SetMode(Ctx :  TTritonCtx );
begin
    //Define optimizations
    ctx.setMode(ALIGNED_MEMORY, True) ;
    ctx.setMode(AST_OPTIMIZATIONS, True);
    ctx.setMode(CONSTANT_FOLDING, True) ;
end;

procedure sync_memory(ctx: TTritonCtx; args: TArray<string>);
begin
    var addr := args[1].Replace('0x','$');
    var size := args[2].Replace('0x','$');
    var data := args[3].Replace('0x','$');
    var memory : MemAccess; memory.Create(StrToInt64(addr), StrToInt(size));
    var synch  := ctx.getConcreteMemoryValue(memory);
    if synch <> StrToInt64(data) then
        ctx.setConcreteMemoryValue(memory,StrToInt64(data))
end;

procedure sync_reg(ctx: TTritonCtx; regs: TArray<string>);
var
  mapping : TDictionary<Registro, Uint64>;
begin
    mapping := TDictionary<Registro, Uint64>.Create;

    mapping.Add( register_rax, StrToInt64(regs[0].Replace('0x','$')) );
    mapping.Add( register_rbx, StrToInt64(regs[1].Replace('0x','$')) );
    mapping.Add( register_rcx, StrToInt64(regs[2].Replace('0x','$')) );
    mapping.Add( register_rdx, StrToInt64(regs[3].Replace('0x','$')) );
    mapping.Add( register_rdi, StrToInt64(regs[4].Replace('0x','$')) );
    mapping.Add( register_rsi, StrToInt64(regs[5].Replace('0x','$')) );
    mapping.Add( register_rbp, StrToInt64(regs[6].Replace('0x','$')) );
    mapping.Add( register_rsp, StrToInt64(regs[7].Replace('0x','$')) );
    mapping.Add( register_r8,  StrToInt64(regs[8].Replace('0x','$')) );
    mapping.Add( register_r9,  StrToInt64(regs[9].Replace('0x','$')) );
    mapping.Add( register_r10, StrToInt64(regs[10].Replace('0x','$')) );
    mapping.Add( register_r11, StrToInt64(regs[11].Replace('0x','$')) );
    mapping.Add( register_r12, StrToInt64(regs[12].Replace('0x','$')) );
    mapping.Add( register_r13, StrToInt64(regs[13].Replace('0x','$')) );
    mapping.Add( register_r14, StrToInt64(regs[14].Replace('0x','$')) );
    mapping.Add( register_r15, StrToInt64(regs[15].Replace('0x','$')) );

    for var it in mapping do
    begin
        var tt_reg    := it.Key;
        var pin_value := it.Value;
        var tt_value := ctx.getConcreteRegisterValue(tt_reg);
        if tt_value <> pin_value then
            ctx.setConcreteRegisterValue(tt_reg, pin_value)
    end;
end;

procedure update_sym_var(ctx: TTritonCtx);
begin
    var ast := ctx.getAstContext() ;

    // Get concrete value of registers
    var x_val := ctx.getConcreteRegisterValue(register_rdi) ;
    var y_val := ctx.getConcreteRegisterValue(register_rsi) ;
    // Get symbolic variable
    var sym_x := SymbolicVar( ctx.getSymbolicVariable(0) );
    var sym_y := SymbolicVar( ctx.getSymbolicVariable(1) );
    // Set concrete value to symbolic variables
    ctx.setConcreteVariableValue(sym_x, x_val);
    ctx.setConcreteVariableValue(sym_y, y_val);
    // Create AST variables
    var x := ast.zx(64 - sym_x.getBitSize(), ast.variable(sym_x)) ;
    var y := ast.zx(64 - sym_y.getBitSize(), ast.variable(sym_y)) ;
    // Assign ASTs to symbolic reigsters
    ctx.assignSymbolicExpressionToRegister(ctx.newSymbolicExpression(x), register_rdi);
    ctx.assignSymbolicExpressionToRegister(ctx.newSymbolicExpression(y), register_rsi);
end;

function DictModelToStr(dModel : TDictionary<UInt32,SolverModel>): string;
begin
    Result := '';
    for var it in dModel do
    begin
        Result :=  Result + Format('%d : %s',[it.Key, it.Value.ToStr]) + ', '
    end;
    Result := Result.Substring(0,Length (Result) -2)
end;

procedure detecting_vjmp(execid: Integer; ctx: TTritonCtx; inst: Istruzione; vbraddr: UInt64 = 0; vbrflag: register_e = ID_REG_INVALID);
begin
    var ast := ctx.getAstContext;

    if V_JMP = nil then
      V_JMP := TList<AbstractNode>.Create;

    if (execid = 2) and (vbraddr <> 0) and (vbrflag <> ID_REG_INVALID) then
    begin
        if (inst.isSymbolized) and (inst.Address = vbraddr) then
        begin
            var flag := AbstractNode( ctx.getRegisterAst(ctx.getRegister(vbrflag)) );
            if Length(ast.search(flag, ast_e.VARIABLE_NODE)) = 2 then
            begin
                var tTupe := ctx.getModel(flag = flag.evaluate, SAT);
                V_JMP.Add(flag = flag.evaluate) ;
                Exit;
            end;
        end;
    end
    else if execid = 1 then
    begin
        // Virtual jmp marker 1
        if (inst.isSymbolized) and (inst.getType = ID_INS_POPFQ) then
        begin
            var cf := AbstractNode( ctx.getRegisterAst(register_cf) );
            if Length(ast.search(cf, ast_e.VARIABLE_NODE)) = 2 then
            begin
                var tTuple := ctx.getModel(cf <> cf.evaluate, SAT);
                var model := tTuple.Value1;
                var status:= tTuple.Value2;
                if status = status_e.SAT then
                     Form1.Log(Format('[+] A potential symbolic jump found on CF flag: %s - Model: {%s}',[inst.ToStr, DictModelToStr(model)]))
            end;
        end;
        
        // Virtual jmp marker 2
        if (inst.isSymbolized) and (inst.getType = ID_INS_CMP) then
        begin
            var op1 := inst.Operands[0] ;
            var op2 := inst.Operands[1] ;
            if (op1.Tipo = OP_REG) and (op2.Tipo = OP_REG) then
            begin
                var af := AbstractNode( ctx.getRegisterAst( register_af) );
                if Length(ast.search(af, ast_e.VARIABLE_NODE)) = 2 then
                begin
                    var tTuple := ctx.getModel(af <> af.evaluate, SAT);
                    var model := tTuple.Value1;
                    var status:= tTuple.Value2;
                    if status = status_e.SAT then
                         Form1.Log(Format('[+] A potential symbolic jump found on AF flag: %s - Model: %s',[inst.ToStr, DictModelToStr(model)]))
                end;
            end;
        end;
    end;
end;

function exec_instruction(execid: Integer; ctx: TTritonCtx; symsize: Integer; args: TArray<string>; fuse: Boolean; vbraddr: UInt64; vbrflag: register_e= ID_REG_INVALID): Boolean;
var
  map_size : TDictionary<UInt64, Tuple<Registro,Registro>>;
  Bytes    : TOpcode;
begin
    var addr := args[1].Replace('0x','$');
    var size := args[2].Replace('0x','$');
    var data := args[3].Replace('0x','');

    SetLength(Bytes, Length(data) div 2);
    HexToBin(PChar(data), Bytes[0], Length(Bytes)) ;

    // This fuse is burned after the first instruction
    if fuse then
    begin
        Form1.Log('[+] Symbolize inputs') ;
        map_size := TDictionary<UInt64, Tuple<Registro,Registro>>.Create;
        map_size.Add(1, Tuple.Create( register_dil, register_sil));
        map_size.Add(2, Tuple.Create( register_di,  register_si));
        map_size.Add(4, Tuple.Create( register_edi, register_esi));
        map_size.Add(8, Tuple.Create( register_rdi, register_rsi ));
        var  vars := ctx.getSymbolicVariables;
        // If symbolic variables do not exist, create them
        if (vars = nil) or (vars.Count = 0) then
        begin
            ctx.symbolizeRegister(map_size[symsize].Value1, 'x');
            ctx.symbolizeRegister(map_size[symsize].Value2, 'y') ;
        end else
        begin
            // If symbolic variables already exist, assign them to registers
            update_sym_var(ctx)
        end;
    end;
    var inst : Istruzione; inst.Create(StrToInt64(addr), Bytes);
    ctx.processing(inst);
    //Form1.Log('[+] Instruction : '+inst.ToStr+ '; Symbolized:' + BoolToStr(inst.isSymbolized, True));
    detecting_vjmp(execid, ctx, inst, vbraddr, vbrflag);
    Result := False;
end;

procedure emulate(execid: Integer; ctx: TTritonCtx; symsize: Integer; ffile: string; vbraddr: UInt64; vbrflag: register_e= ID_REG_INVALID);
var
  count : Integer;
  fuse  : Boolean;
begin
    count := 0;
    fuse := True;

    var cFile := TFile.ReadAllLines(ffile) ;
    for var i := 0 to Length(cFile)-1 do
    begin
        var fd : string := cfile[i];

        var args := fd.split([':']);
        var kind := args[0];

        // Synch memory read
        if kind = 'mr' then
            sync_memory(ctx, args);
        // Synch registers
        if kind = 'r' then
        begin
            var regs := Copy(args,1, Length(args) - 1);
            sync_reg(ctx, regs);
        end;
        // Execute instruction
        if kind = 'i' then
        begin
            fuse := exec_instruction(execid, ctx, symsize, args, fuse, vbraddr, vbrflag) ;
            count := count + 1;
            if count = 11936 then
                count := count;
        end;
    end;
    Form1.Log('[+] Instruction executed: '+ IntToStr(count));
end;

function one_path(execid: Integer; ctx: TTritonCtx; trace: string; symsize: Integer; vbraddr: UInt64; vbrflag: register_e= ID_REG_INVALID):AbstractNode;
begin
    Form1.Log('[+] Replaying the VMP trace') ;
    emulate(execid, ctx, symsize, trace, vbraddr, vbrflag);
    Form1.Log('[+] Emulation done') ;
    var eax := ctx.getRegisterAst( register_eax ) ;
    Result := eax ;
end;

procedure PrintResult(ctx: TTritonCtx; ret_expr: AbstractNode);
var
   FllvmCtx     : TLLVMContextRef;
   llvmToTriton : TllvmToTriton;
   llvmmodule   : TLLVMModuleRef;
   node         : AbstractNode;
begin
   FllvmCtx   := LLVMContextCreate;
   try
     llvmmodule   := loadModuleFromFile('C:\Users\Max\Documents\Embarcadero\Studio\Projects\LLVMDevirt\Test\bin\Max\MacroFunc_40156D\Handlers\H_40156D.ll',FllvmCtx);
     var nameFunc := LLVMGetValueName( LLVMGetFirstFunction(llvmmodule) );;

     var PassManager: TLLVMPassManagerRef;
     PassManager := LLVMCreatePassManager;

     LLVMAddCFGSimplificationPass(PassManager);
     LLVMAddCalledValuePropagationPass(PassManager);
     LLVMAddGlobalDCEPass(PassManager);

     LLVMRunPassManager(PassManager,llvmmodule);
     saveModuleToFile(llvmmodule,'passmager.II');

     llvmToTriton := TllvmToTriton.Create(ctx);

     Form1.Log('[+] llvm.Convert') ;

     var entryBB := LLVMGetEntryBasicBlock( LLVMGetNamedFunction(llvmmodule,nameFunc) );
     var terminator := LLVMDumpValueToStr( LLVMGetBasicBlockTerminator(entryBB) );

     node := AbstractNode( llvmToTriton.convert(llvmmodule,nameFunc) );

     var ssynth  := ctx.synthesize(node);
     if ssynth.IsNodeAssigned then
        Form1.log(ctx.liftToLLVM(ssynth)) ;
   finally
     LLVMContextDispose(FllvmCtx);
     llvmToTriton.Free;
   end;

    var ast    := ctx.getAstContext;
    var unro   := ast.unroll(ret_expr);
    var synth  := ctx.synthesize(ret_expr);
    var ppast1 := unro.ToStr;
    if ppast1.Length > 100 then
       ppast1 := ppast1.Substring(0,100);

    var ppast2 := synth.ToStr;
    if ppast2.Length > 100 then
       ppast2 := ppast2.Substring(0,100);

    Form1.Log('[+] Return value: ' + IntToHex(ret_expr.evaluate())) ;
    Form1.Log('[+] Devirt expr: '+ppast1);
    Form1.Log('[+] Synth expr: ' +ppast2) ;
    Form1.Log('[+] LLVM IR ==============================');
    if synth.IsNodeAssigned then
      Form1.log(ctx.liftToLLVM(synth))
    else
       Form1.Log(ctx.liftToLLVM(ret_expr)) ;

    Form1.Log('[+] EOF LLVM IR ============================== ') ;
end;

procedure analysis(trace1, trace2: string; symsize: Integer; vbraddr: UInt64; vbrflag: register_e= ID_REG_INVALID);
var
  Ctx :  TTritonCtx ;
begin
    Ctx.Create(ARCH_X86_64);
    SetMode(Ctx);

    register_dil := ctx.getRegister(ID_REG_X86_DIL);
    register_sil := ctx.getRegister(ID_REG_X86_SIL);
    register_di  := ctx.getRegister(ID_REG_X86_DI);
    register_si  := ctx.getRegister(ID_REG_X86_SI);


    register_eax := ctx.getRegister(ID_REG_X86_EAX);
    register_edi := ctx.getRegister(ID_REG_X86_EDI);
    register_esi := ctx.getRegister(ID_REG_X86_ESI);

    register_rax := ctx.getRegister(ID_REG_X86_RAX);
    register_rbx := ctx.getRegister(ID_REG_X86_RBX);
    register_rcx := ctx.getRegister(ID_REG_X86_RCX);
    register_rdx := ctx.getRegister(ID_REG_X86_RDX);
    register_rdi := ctx.getRegister(ID_REG_X86_RDI);
    register_rsi := ctx.getRegister(ID_REG_X86_RSI);
    register_rbp := ctx.getRegister(ID_REG_X86_RBP);
    register_rsp := ctx.getRegister(ID_REG_X86_RSP);
    register_r8  := ctx.getRegister(ID_REG_X86_R8);
    register_r9  := ctx.getRegister(ID_REG_X86_R9);
    register_r10 := ctx.getRegister(ID_REG_X86_R10);
    register_r11 := ctx.getRegister(ID_REG_X86_R11);
    register_r12 := ctx.getRegister(ID_REG_X86_R12);
    register_r13 := ctx.getRegister(ID_REG_X86_R13);
    register_r14 := ctx.getRegister(ID_REG_X86_R14);
    register_r15 := ctx.getRegister(ID_REG_X86_R15);

    register_cf  := ctx.getRegister(ID_REG_X86_CF);
    register_af  := ctx.getRegister(ID_REG_X86_AF);

    var ret_expr1 := one_path(1, ctx, trace1, symsize, vbraddr, vbrflag) ;
    if trace2 <> '' then
    begin
        Form1.Log('[+] A second trace has been provided') ;
        var ret_expr2 := one_path(2, ctx, trace2, symsize, vbraddr, vbrflag) ;
        var ast := ctx.getAstContext;
        Form1.Log('[+] Merging expressions from trace1 and trace2');
        var e1 := V_JMP[0];
        ret_expr2 := ast.ite(e1, ret_expr2, ret_expr1);
        PrintResult(ctx, ret_expr2)
    end else
        PrintResult(ctx, ret_expr1)
end;

end.
