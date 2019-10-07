unit Triton.Api;

{$Z4}
{$POINTERMATH ON}

interface
   uses System.SysUtils,Winapi.Windows,
        System.Generics.Collections,
        Triton.AstNode,
        Triton.SolverModel,
        Triton.Define,
        Triton.Register,
        Triton.Immediate,
        Triton.MemoryAccess,
        Triton.Instruction,
        Triton.OperandWrapper,
        Triton.AstContext,
        Triton.SymbolicExpression,
        Triton.SymbolicVariable,
        Triton.pathConstraint,
        Triton.Core;

type

  TApi = record
    private
       FHApi        : HandleApi;
       FArchitecture: architecture_e;
    public
       class operator Implicit(rApi: TApi): HandleApi;
       class Operator Explicit(hApi: HandleApi):TApi;
       class Operator Explicit(rApi: TApi):HandleApi;

       property Architecture : architecture_e read FArchitecture ;
  end;

  //ComparableFunctor<void(triton::API&, const triton::arch::Register&)>
  TGetRegVal = procedure(API : HandleApi; reg: HandleReg);  cdecl;
  //ComparableFunctor<void(triton::API&, const triton::arch::Register&,     const triton::uint512& value)>
  TSetRegVal = procedure(API : HandleApi; reg: HandleReg; value : uint64);  cdecl;

  //ComparableFunctor<void(triton::API&, const triton::arch::MemoryAccess&)>
  TGetMemVal  = procedure(API : HandleApi; mem: HandleMemAcc);  cdecl;
  //ComparableFunctor<void(triton::API&, const triton::arch::MemoryAccess&, const triton::uint512& value)>
  TSetMemVal  = procedure(API : HandleApi; mem: HandleMemAcc; value : uint64);  cdecl;

  //ComparableFunctor<triton::ast::SharedAbstractNode(triton::API&, const triton::ast::SharedAbstractNode&)>
  TSimplification    = function(API : HandleApi; snode: HandleAbstractNode): HandleAbstractNode ; cdecl;

  APathConstraint = array of PathConstraint;

  TApiHelper = record Helper for TApi
    private

    public
        procedure   Create;
        procedure   Free;
        (* Architecture API ============= *)
        procedure setArchitecture(arch: architecture_e);
        function  getArchitecture:architecture_e;
        function  isArchitectureValid: Boolean;
        procedure checkArchitecture;
        function  getCpuInstance: HandleCpuInterface;
        procedure clearArchitecture;
        function  getEndianness: endianness_e;
        function  isFlag(regId: register_e): Boolean; overload;
        function  isFlag(reg: Registro): Boolean;    overload;
        function  isRegister(regId : register_e ): Boolean;  overload;
        function  isRegister(reg: Registro): Boolean; overload;
        function  getRegister(id: register_e): Registro;
        function  getParentRegister(reg: Registro): Registro; overload;
        function  getParentRegister(id: register_e): Registro; overload;
        function  isRegisterValid(regId: register_e): Boolean ;  overload;
        function  isRegisterValid(reg: Registro): Boolean ;  overload;
        function  getGprBitSize: uint32;
        function  getGprSize: uint32;
        function  getNumberOfRegisters: uint32;
        function  getAllRegisters: TDictionary<register_e,Registro>;
        function  getParentRegisters:TArray<Registro>;
        function  getConcreteMemoryValue(addr: uint64; execCallbacks : Boolean = true):uint8; overload;
        function  getConcreteMemoryValue(mem: MemAccess; execCallbacks : Boolean = true): uint64; overload;
        function  getConcreteMemoryAreaValue(baseAddr: uint64; size: usize; execCallbacks : Boolean = true):TArray<Byte>;
        function  getConcreteRegisterValue(reg: Registro; execCallbacks : Boolean = true): uint64;
        procedure setConcreteMemoryValue(addr: uint64; value: uint8); overload;
        procedure setConcreteMemoryValue(mem: MemAccess; value: uint64); overload;
        procedure setConcreteMemoryAreaValue(baseAddr : uint64; values: array of Byte); overload;
        procedure setConcreteMemoryAreaValue(baseAddr: uint64; area : array of Byte;  size: usize); overload;
        procedure setConcreteRegisterValue(reg: Registro; value: uint64);
        function  isMemoryMapped(baseAddr: uint64;  size : usize = 1): Boolean;
        procedure unmapMemory(baseAddr: uint64;  size : usize = 1);
        procedure disassembly(var inst: Istruzione);
        (* Processing API ============ *)
        function   processing(var inst: Istruzione): Boolean;
        procedure  initEngines;
        procedure  removeEngines;
        procedure  reset;
        (* IR API ============== *)
        procedure  checkIrBuilder;
        function   buildSemantics(var inst: Istruzione): Boolean;
        function   getAstContext: AstContext;
        (* AST Representation API ========== *)
        function   getAstRepresentationMode:uint32 ;
        procedure  setAstRepresentationMode(mode: uint32);
        (* Callbacks API ========================= *)
        procedure addCallback(cb: Pointer; cbTipo: callback_e);
        procedure removeCallback(cb: Pointer; cbTipo: callback_e);
        procedure removeAllCallbacks;
        // si potrebbero eliminare  processCallbacks -- funzioni interne drante la fase di processing
        function  processCallbacks(kind: callback_e; node: HandleAbstractNode): HandleAbstractNode; overload;
        procedure processCallbacks(kind: callback_e; mem: MemAccess); overload;
        procedure processCallbacks(kind: callback_e; reg: Registro);  overload;
        (* Modes API============ *)
        procedure checkModes ;
        procedure enableMode(mode: mode_e; flag: Boolean);
        function  isModeEnabled(mode: mode_e): Boolean;
        (* Symbolic engine API ========= *)
        procedure checkSymbolic ;
        function  getSymbolicEngine: HandleSymbolicEngine;
        function  getSymbolicRegisters: TDictionary<register_e,symbolicExp>;
        function  getSymbolicRegister(reg: Registro):symbolicExp ;
        function  getSymbolicMemory: TDictionary<UInt64,symbolicExp>;  overload;
        function  getSymbolicMemory(addr: uint64 ):symbolicExp ; overload;
        function  getSymbolicMemoryValue(address: uint64): uint8; overload;
        function  getSymbolicMemoryValue(mem: MemAccess): uint64; overload;
        function  getSymbolicMemoryAreaValue(baseAddr: uint64;size: usize): TArray<Byte>;
        function  getSymbolicRegisterValue(reg: Registro): uint64;
        function  convertExpressionToSymbolicVariable(exprId: usize; symVarSize: uint32; symVarComment: AnsiString = ''): HandleSharedSymbolicVariable;
        function  convertMemoryToSymbolicVariable(mem: MemAccess;  symVarComment: AnsiString = ''): SymbolicVar;
        function  convertRegisterToSymbolicVariable(reg: Registro;  symVarComment : AnsiString = ''): SymbolicVar;
        function  getOperandAst(op: OpWrapper): HandleAbstractNode; overload;
        function  getOperandAst(inst: Istruzione ; op: OpWrapper):HandleAbstractNode; overload;
        function  getImmediateAst(imm: Immediate): HandleAbstractNode;overload;
        function  getImmediateAst(inst: Istruzione ; imm: Immediate):HandleAbstractNode;overload;
        function  getMemoryAst(mem: MemAccess):HandleAbstractNode;overload;
        function  getMemoryAst(inst: Istruzione ; mem: MemAccess):HandleAbstractNode;overload;
        function  getRegisterAst(reg: Registro):HandleAbstractNode; overload;
        function  getRegisterAst(inst: Istruzione ; reg: Registro):HandleAbstractNode; overload;
        function  newSymbolicExpression(node: HandleAbstractNode; comment: AnsiString = ''):symbolicExp;
        function  newSymbolicVariable(varSize: uint32; comment: AnsiString = ''):SymbolicVar;
        procedure removeSymbolicExpression(symExprId: usize);
        function  createSymbolicExpression(inst: Istruzione ; node: HandleAbstractNode; dst: OpWrapper; comment : AnsiString = ''):symbolicExp;
        function  createSymbolicMemoryExpression(inst: Istruzione ; node: HandleAbstractNode; mem: MemAccess; comment: AnsiString = ''):symbolicExp;
        function  createSymbolicRegisterExpression(inst: Istruzione ; node: HandleAbstractNode; reg: Registro; comment: AnsiString = ''):symbolicExp;
        function  createSymbolicFlagExpression(inst: Istruzione ; node: HandleAbstractNode; flag: Registro; comment: AnsiString = ''):symbolicExp;
        function  createSymbolicVolatileExpression(inst: Istruzione ; node: HandleAbstractNode; comment: AnsiString = ''):symbolicExp;
        procedure assignSymbolicExpressionToMemory(se:symbolicExp; mem: MemAccess);
        procedure assignSymbolicExpressionToRegister(se:symbolicExp; reg: Registro);
        function  simplify(node: HandleAbstractNode; z3: Boolean = false):HandleAbstractNode;
        function  getSymbolicExpressionFromId(symExprId: usize):symbolicExp ;
        function  getSymbolicVariableFromId(symVarId: usize):HandleSharedSymbolicVariable;
        function  getSymbolicVariableFromName(symVarName: AnsiString):HandleSharedSymbolicVariable;
        function  getPathConstraints:APathConstraint ;
        function  getPathConstraintsAst:HandleAbstractNode;
        procedure addPathConstraint(inst: Istruzione ; expr: symbolicExp);
        procedure clearPathConstraints;
        procedure enableSymbolicEngine(flag: Boolean);
        function  isSymbolicEngineEnabled: Boolean ;
        function  isSymbolicExpressionIdExists(symExprId: usize): Boolean ;
        function  isMemorySymbolized(mem: MemAccess): Boolean ; overload;
        function  isMemorySymbolized(addr: uint64; size: uint32 = 1): Boolean ;overload;
        function  isRegisterSymbolized(reg: Registro): Boolean ;
        procedure concretizeAllMemory;
        procedure concretizeAllRegister;
        procedure concretizeMemory(mem: MemAccess);overload;
        procedure concretizeMemory(addr: uint64); overload;
        procedure concretizeRegister(reg: Registro);
        function  sliceExpressions(expr: symbolicExp):TDictionary<usize,symbolicExp>;
        function  getTaintedSymbolicExpressions:TList<symbolicExp>;
        function  getSymbolicExpressions:TDictionary<usize,symbolicExp> ;
        function  getSymbolicVariables:TDictionary<usize,SymbolicVar> ;
        function  getConcreteVariableValue(symVar:SymbolicVar):uint64 ;
        procedure setConcreteVariableValue(symVar: SymbolicVar; value: UInt64);
        procedure initLeaAst(mem: MemAccess; force: Boolean = false);
        (* Solver engine API =========== *)
        procedure checkSolver ;
        function  getModel(node: HandleAbstractNode): TDictionary<UInt32,SolverModel> ;
        function  getModels(node: HandleAbstractNode;limit : uint32): TList< TDictionary<UInt32,SolverModel> >;
        function  isSat(node: HandleAbstractNode): Boolean ;
        function  getSolver: solver_e ;
        procedure setSolver(kind: solver_e);
        function  isSolverValid:Boolean ;
        function  evaluateAstViaZ3(node: HandleAbstractNode): uint64 ;
        function  processZ3Simplification(node: HandleAbstractNode): HandleAbstractNode ;
        (* Taint engine API==========*)

        procedure  checkTaint;
        function  getTaintEngine:HandleTaintEngine;
        function  getTaintedMemory:TArray<UInt64> ;
        function  getTaintedRegisters:TArray<Registro> ;
        procedure enableTaintEngine(flag: Boolean);
        function  isTaintEngineEnabled: Boolean ;
        function  isTainted(op:OpWrapper): Boolean ;
        function  isMemoryTainted(addr: uint64; size: uint32 = 1): Boolean ; overload;
        function  isMemoryTainted(mem: MemAccess ): Boolean ;overload;
        function  isRegisterTainted(reg: Registro): Boolean ;
        function  setTaint(op: OpWrapper; flag : Boolean): Boolean ;
        function  setTaintMemory(mem: MemAccess ;flag : Boolean): Boolean ;
        function  setTaintRegister(reg: Registro;flag : Boolean): Boolean ;
        function  taintMemory(addr: uint64): Boolean ;  overload;
        function  taintMemory(mem: MemAccess ): Boolean ; overload;
        function  taintRegister(reg: Registro): Boolean ;
        function  untaintMemory(addr: uint64): Boolean ;  overload;
        function  untaintMemory(mem: MemAccess ): Boolean ; overload;
        function  untaintRegister(reg: Registro): Boolean ;
        function  taintUnion(op1,op2: OpWrapper): Boolean ;
        function  taintAssignment(op1,op2: OpWrapper): Boolean ;
        function  taintUnionMemoryImmediate(Dst: MemAccess): Boolean ;
        function  taintUnionMemoryMemory(Dst: MemAccess; Src:MemAccess): Boolean ;
        function  taintUnionMemoryRegister(Dst: MemAccess; Src: Registro): Boolean ;
        function  taintUnionRegisterImmediate(Dst: Registro): Boolean ;
        function  taintUnionRegisterMemory(Dst: Registro; Src: MemAccess): Boolean ;
        function  taintUnionRegisterRegister(Dst:Registro; Src:Registro): Boolean ;
        function  taintAssignmentMemoryImmediate(Dst:MemAccess): Boolean ;
        function  taintAssignmentMemoryMemory(Dst: MemAccess; Src: MemAccess): Boolean ;
        function  taintAssignmentMemoryRegister(Dst: MemAccess; Src: Registro): Boolean ;
        function  taintAssignmentRegisterImmediate(Dst: Registro): Boolean ;
        function  taintAssignmentRegisterMemory(Dst: Registro; Src: MemAccess ): Boolean ;
        function  taintAssignmentRegisterRegister(Dst: Registro; Src: Registro): Boolean ;


        {class operator Implicit(rApi: TApi): HandleApi;
                class Operator Explicit(hApi: HandleApi):TApi;
                        class Operator Explicit(rApi: TApi):HandleApi;}
  end;

implementation

{ Api }

procedure TApiHelper.Create;
begin
    FHApi := CreateAPI();
end;

procedure TApiHelper.Free;
begin
    DeleteApi(FHApi)
end;

class operator TApi.Implicit(rApi: TApi): HandleApi;
begin
    Result := rApi.FHApi;
end;

class operator TApi.Explicit(rApi: TApi): HandleApi;
begin
    Result := rApi.FHApi;
end;

class operator TApi.Explicit(hApi: HandleApi): TApi;
begin
    ZeroMemory(@Result,SizeOf(TApi));
    Result.FHApi := hApi;

end;

procedure TApiHelper.addCallback(cb: Pointer; cbTipo: callback_e);
begin
    case callback_e(cbTipo) of
       GET_CONCRETE_MEMORY_VALUE :
       begin
           Triton.Core.addCallbackGetMem(FHApi,cb);
       end;
       SET_CONCRETE_MEMORY_VALUE:
       begin
           Triton.Core.addCallbackSetMem(FHApi,cb)
       end;
       GET_CONCRETE_REGISTER_VALUE:
       begin
           Triton.Core.addCallbackGetReg(FHApi,cb)
       end;
       SET_CONCRETE_REGISTER_VALUE:
       begin
           Triton.Core.addCallbackSetReg(FHApi,cb)
       end;
       SYMBOLIC_SIMPLIFICATION:
       begin
           Triton.Core.addCallbackSimplif(FHApi,cb)
       end;
    end;

end;

procedure TApiHelper.addPathConstraint(inst: Istruzione; expr: symbolicExp);
begin
    Triton.Core.addPathConstraint(FHApi, inst, expr)
end;

procedure TApiHelper.assignSymbolicExpressionToMemory(se: symbolicExp; mem: MemAccess);
begin
    Triton.Core.assignSymbolicExpressionToMemory(FHApi,se,mem)
end;

procedure TApiHelper.assignSymbolicExpressionToRegister(se: symbolicExp;  reg: Registro);
begin
    Triton.Core.assignSymbolicExpressionToRegister(FHApi,se,reg)
end;

function TApiHelper.buildSemantics(var inst: Istruzione): Boolean;
begin
    Result := Triton.Core.buildSemantics(FHApi,inst);
    inst.Reload;
end;

procedure TApiHelper.checkArchitecture;
begin
    Triton.Core.checkArchitecture(FHApi)
end;

procedure TApiHelper.checkIrBuilder;
begin
    Triton.Core.checkIrBuilder(FHApi)
end;

procedure TApiHelper.checkModes;
begin
    Triton.Core.checkModes(FHApi)
end;

procedure TApiHelper.checkSolver;
begin
    Triton.Core.checkSolver(FHApi)
end;

procedure TApiHelper.checkSymbolic;
begin
    Triton.Core.checkSymbolic(FHApi)
end;

procedure TApiHelper.checkTaint;
begin
    Triton.Core.checkTaint(FHApi)
end;

procedure TApiHelper.clearArchitecture;
begin
    Triton.Core.clearArchitecture(FHApi)
end;

procedure TApiHelper.clearPathConstraints;
begin
    Triton.Core.clearPathConstraints(FHApi)
end;

procedure TApiHelper.concretizeAllMemory;
begin
    Triton.Core.concretizeAllMemory(FHApi)
end;

procedure TApiHelper.concretizeAllRegister;
begin
    Triton.Core.concretizeAllRegister(FHApi)
end;

procedure TApiHelper.concretizeMemory(addr: uint64);
begin
    Triton.Core.concretizeMemory(FHApi,addr)
end;

procedure TApiHelper.concretizeMemory(mem: MemAccess);
begin
    Triton.Core.concretizeMemoryM(FHApi,mem)
end;

procedure TApiHelper.concretizeRegister(reg: Registro);
begin
    Triton.Core.concretizeRegister(FHApi,reg)
end;

function TApiHelper.convertExpressionToSymbolicVariable(exprId: usize; symVarSize: uint32; symVarComment: AnsiString): HandleSharedSymbolicVariable;
begin
    Result := Triton.Core.convertExpressionToSymbolicVariable(FHApi,exprId,symVarSize,PAnsiChar( symVarComment))
end;

function TApiHelper.convertMemoryToSymbolicVariable(mem: MemAccess; symVarComment: AnsiString): SymbolicVar;
begin
    Result := SymbolicVar( Triton.Core.convertMemoryToSymbolicVariable(FHApi,mem, PAnsiChar( symVarComment)) )
end;

function TApiHelper.convertRegisterToSymbolicVariable(reg: Registro; symVarComment: AnsiString ): SymbolicVar;
begin
    Result := SymbolicVar (Triton.Core.convertRegisterToSymbolicVariable(FHApi,reg, PAnsiChar(symVarComment)) )
end;

function TApiHelper.createSymbolicExpression(inst: Istruzione; node: HandleAbstractNode; dst: OpWrapper; comment: AnsiString): symbolicExp;
begin
    Result := symbolicExp (Triton.Core.createSymbolicExpression(FHApi,inst,node,dst,PAnsiChar (comment)) )
end;

function TApiHelper.createSymbolicFlagExpression(inst: Istruzione; node: HandleAbstractNode; flag: Registro; comment: AnsiString): symbolicExp;
begin
    Result := symbolicExp (Triton.Core.createSymbolicFlagExpression(FHApi,inst,node,flag,PAnsiChar( comment)) )
end;

function TApiHelper.createSymbolicMemoryExpression(inst: Istruzione; node: HandleAbstractNode; mem: MemAccess; comment: AnsiString): symbolicExp;
begin
    Result := symbolicExp (Triton.Core.createSymbolicMemoryExpression(FHApi,inst,node,mem,PAnsiChar (comment)) )
end;

function TApiHelper.createSymbolicRegisterExpression(inst: Istruzione;node: HandleAbstractNode; reg: Registro; comment: AnsiString): symbolicExp;
begin
    Result := symbolicExp (Triton.Core.createSymbolicRegisterExpression(FHApi,inst,node,reg,PAnsiChar( comment)) )
end;

function TApiHelper.createSymbolicVolatileExpression(inst: Istruzione; node: HandleAbstractNode; comment: AnsiString): symbolicExp;
begin
    Result := symbolicExp (Triton.Core.createSymbolicVolatileExpression(FHApi,inst,node,PAnsiChar( comment)) )
end;

procedure TApiHelper.disassembly(var inst: Istruzione);
begin
    Triton.Core.disassembly(FHApi,inst) ;
    inst.Reload;
end;

procedure TApiHelper.enableMode(mode: mode_e; flag: Boolean);
begin
    Triton.Core.enableMode(FHApi, mode, flag)
end;

procedure TApiHelper.enableSymbolicEngine(flag: Boolean);
begin
    Triton.Core.enableSymbolicEngine(FHApi,flag)
end;

procedure TApiHelper.enableTaintEngine(flag: Boolean);
begin
    Triton.Core.enableTaintEngine(FHApi,flag)
end;

function TApiHelper.evaluateAstViaZ3(node: HandleAbstractNode): uint64;
begin
    Result := Triton.Core.evaluateAstViaZ3(FHApi,node)
end;

function TApiHelper.getArchitecture: architecture_e;
begin
    Result := Triton.Core.getArchitecture(FHApi)
end;

function TApiHelper.getAstContext: AstContext;
begin
    Result := AstContext (Triton.Core.getAstContext(FHApi) )
end;

function TApiHelper.getAstRepresentationMode: uint32;
begin
    Result := Triton.Core.getAstRepresentationMode(FHApi)
end;

function TApiHelper.getConcreteMemoryAreaValue(baseAddr: uint64; size: usize; execCallbacks: Boolean): TArray<Byte>;
var
  res : PByte;
begin

    res := Triton.Core.getConcreteMemoryAreaValue(FHApi,baseAddr,size,execCallbacks);
    SetLength(Result,size);
    CopyMemory(@Result[0],res,size * SizeOf(uint8));
end;

function TApiHelper.getConcreteMemoryValue(mem: MemAccess; execCallbacks: Boolean): uint64;
begin
    Result := Triton.Core.getConcreteMemoryValue(FHApi,mem,execCallbacks)
end;

function TApiHelper.getConcreteMemoryValue(addr: uint64; execCallbacks: Boolean): uint8;
begin
    Result := Triton.Core.getConcreteMemoryValueByte(FHApi, addr,execCallbacks)
end;

function TApiHelper.getConcreteRegisterValue(reg: Registro;  execCallbacks: Boolean): uint64;
begin
    Result := Triton.Core.getConcreteRegisterValue(FHApi,reg, execCallbacks)
end;

function TApiHelper.getConcreteVariableValue(symVar: SymbolicVar): uint64;
begin
    Result := Triton.Core.getConcreteVariableValue(FHApi,HandleSharedSymbolicVariable( symVar) )
end;

function TApiHelper.getCpuInstance: HandleCpuInterface;
begin
    Result := Triton.Core.getCpuInstance(FHApi)
end;

function TApiHelper.getEndianness: endianness_e;
begin
    Result := Triton.Core.getEndianness(FHApi)
end;

function TApiHelper.getGprBitSize: uint32;
begin
    Result := Triton.Core.getGprBitSize(FHApi)
end;

function TApiHelper.getGprSize: uint32;
begin
   Result := Triton.Core.getGprSize (FHApi)
end;

function TApiHelper.getImmediateAst(imm: Immediate): HandleAbstractNode;
begin
    Result := Triton.Core.getImmediateAst(FHApi, imm)
end;

function TApiHelper.getImmediateAst(inst: Istruzione; imm: Immediate): HandleAbstractNode;
begin
    Result := Triton.Core.getImmediateAstIstruz(FHApi,inst,imm)
end;

function TApiHelper.getMemoryAst(mem: MemAccess): HandleAbstractNode;
begin
    Result := Triton.Core.getMemoryAst(FHApi,mem)
end;

function TApiHelper.getMemoryAst(inst: Istruzione; mem: MemAccess): HandleAbstractNode;
begin
    Result := Triton.Core.getMemoryAstIstruz(FHApi,inst,mem)
end;

function TApiHelper.getModel(node: HandleAbstractNode): TDictionary<UInt32,SolverModel>;
var
  n,i  : Integer;
  pOut : PAddrSolver ;

begin
    Result := nil;
    pOut := nil;
    n :=  Triton.Core.getModel(FHApi,node,@pOut );

    //if n > 0 then // Added by Max remove possibility of exception error when accessing count model 02/10/2019 18:48:56
    Result := TDictionary<UInt32,SolverModel>.Create;

    for i := 0 to n - 1 do
        Result.Add(pOut[i].id, SolverModel (pOut[i].Model) );
end;

function TApiHelper.getModels(node: HandleAbstractNode; limit: uint32): TList< TDictionary<UInt32,SolverModel> >;
var
  n,i,j : Integer;
  pOut  : PListSolver ;
  pAddS : PAddrSolver;
  dDict : TDictionary<UInt32,SolverModel>;
begin
    Result := nil;
    pOut := nil;
    n := Triton.Core.getModels(FHApi,node,limit,@pOut);

    if n > 0 then
       Result := TList< TDictionary<UInt32,SolverModel> >.Create;

     for i := 0 to n - 1 do
     begin
         pAddS := PAddrSolver(pOut[i]);
         dDict := TDictionary<UInt32,SolverModel>.Create;
         for j := 0 to pAddS[0].numEle -1 do
            dDict.Add(pAddS[j].id, SolverModel (pAddS[j].Model) );

         Result.Add(dDict);
     end;

end;

function TApiHelper.getNumberOfRegisters: uint32;
begin
    Result := Triton.Core.getNumberOfRegisters(FHApi)
end;

function TApiHelper.getOperandAst(op: OpWrapper): HandleAbstractNode;
begin
    Result :=  Triton.Core.getOperandAst(FHApi,op)
end;

function TApiHelper.getOperandAst(inst: Istruzione; op: OpWrapper): HandleAbstractNode;
begin
    Result := Triton.Core.getOperandAstIstruz(FHApi, inst, op)
end;

function TApiHelper.getParentRegister(id: register_e): Registro;
begin
    Result := Registro (Triton.Core.getParentRegister(FHApi,id) )
end;

function TApiHelper.getParentRegister(reg: Registro): Registro;
begin
    Result := Registro (Triton.Core.getParentRegisterR(FHApi,reg) )
end;

function TApiHelper.getPathConstraints: APathConstraint;
var
  n,i : Integer;
  p   : PHandlePathConstraint;
begin
    p := nil;
    n := Triton.Core.getPathConstraints(FHApi, @p );

    for i := 0 to n -1 do
      Result := Result + [ pathConstraint (p[i]) ]

end;

function TApiHelper.getPathConstraintsAst: HandleAbstractNode;
begin
    Result := Triton.Core.getPathConstraintsAst(FHApi)
end;

function TApiHelper.getRegister(id: register_e): Registro;
begin
    Result := Registro (Triton.Core.getRegister(FHApi, id) )
end;

function TApiHelper.getRegisterAst(reg: Registro): HandleAbstractNode;
begin
    Result := Triton.Core.getRegisterAst(FHApi, reg)
end;

function TApiHelper.getRegisterAst(inst: Istruzione; reg: Registro): HandleAbstractNode;
begin
    Result := Triton.Core.getRegisterAstIstruz(FHApi, inst,reg)
end;

function TApiHelper.getSolver: solver_e;
begin
    Result := Triton.Core.getSolver(FHApi)
end;

function TApiHelper.getSymbolicEngine: HandleSymbolicEngine;
begin
    Result := Triton.Core.getSymbolicEngine(FHApi)
end;

function TApiHelper.getSymbolicExpressionFromId(symExprId: usize): symbolicExp;
begin
    Result := symbolicExp (Triton.Core.getSymbolicExpressionFromId(FHApi, symExprId) )
end;

function TApiHelper.getSymbolicExpressions: TDictionary<usize,symbolicExp>;
var
  n,i  : Integer;
  pOut : PIdSymExpr ;
begin
    Result := nil;
    pOut   := nil;
    n := Triton.Core.getSymbolicExpressions(FHApi,@pOut);

    if n > 0 then
       Result := TDictionary<usize,symbolicExp>.Create;

    for i := 0 to n - 1 do
        Result.Add(pOut[i].id, symbolicExp (pOut[i].SymExpr) );
end;

function TApiHelper.getSymbolicMemory: TDictionary<UInt64,symbolicExp>;
var
  n,i  : Integer;
  pOut : PMemSymE ;
begin
    pOut   := nil;

    n  := Triton.Core.getSymbolicMemory(FHApi,@pOut);

    // remove possibility of memory access violation
    Result := TDictionary<UInt64,symbolicExp>.Create;

    for i := 0 to n - 1 do
        Result.Add(pOut[i].mem, symbolicExp (pOut[i].MemSym) )
end;

function TApiHelper.getSymbolicMemory(addr: uint64): symbolicExp;
begin
    Result := symbolicExp (Triton.Core.getSymbolicMemoryAddr(FHApi, addr) )
end;

function TApiHelper.getSymbolicMemoryAreaValue(baseAddr: uint64; size: usize): TArray<Byte>;
var
 res : PByte;
begin

    res := Triton.Core.getSymbolicMemoryAreaValue(FHApi,baseAddr,size)  ;

    SetLength(Result,size);
    CopyMemory(@Result[0],res,size * SizeOf(uint8));
end;

function TApiHelper.getSymbolicMemoryValue(address: uint64): uint8;
begin
    Result := Triton.Core.getSymbolicMemoryValue(FHApi, address)
end;

function TApiHelper.getSymbolicMemoryValue(mem: MemAccess): uint64;
begin
    Result := Triton.Core.getSymbolicMemoryValueM(FHApi, mem)
end;

function TApiHelper.getAllRegisters: TDictionary<register_e, Registro>;
var
  n,i  : Integer;
  pOut : PRegIdReg ;
begin
    Result := nil;
    pOut := nil;
    n := Triton.Core.getAllRegisters(FHApi,@pOut);

    if n > 0 then
       Result := TDictionary<register_e,Registro>.Create;

    for i := 0 to n - 1 do
        Result.Add(pOut[i].regId, Registro (pOut[i].Reg) );

end;

function TApiHelper.getSymbolicRegister(reg: Registro): symbolicExp;
begin
    Result := symbolicExp ( Triton.Core.getSymbolicRegister(FHApi, reg) )
end;

function TApiHelper.getSymbolicRegisters: TDictionary<register_e,symbolicExp>;
var
  n,i  : Integer;
  pOut : PRegSymE ;
begin
    Result := nil;
    pOut := nil;
    n := Triton.Core.getSymbolicRegisters(FHApi,@pOut);

    if n > 0 then
       Result := TDictionary<register_e,symbolicExp>.Create;

    for i := 0 to n - 1 do
        Result.Add(pOut[i].regId, symbolicExp (pOut[i].RegSym) );
end;

function TApiHelper.getSymbolicRegisterValue(reg: Registro): uint64;
begin
   Result := Triton.Core.getSymbolicRegisterValue(FHApi,reg)
end;

function TApiHelper.getSymbolicVariableFromId(symVarId: usize): HandleSharedSymbolicVariable;
begin
    Result := Triton.Core.getSymbolicVariableFromId(FHApi,symVarId)
end;

function TApiHelper.getSymbolicVariableFromName(symVarName: AnsiString): HandleSharedSymbolicVariable;
begin
    Result := Triton.Core.getSymbolicVariableFromName(FHApi, PAnsiChar(symVarName))
end;

function TApiHelper.getSymbolicVariables: TDictionary<usize,SymbolicVar>;
var
  n,i  : Integer;
  pOut : PIdSymVar ;
begin
    Result := nil;
    pOut := nil;
    n := Triton.Core.getSymbolicVariables(FHApi,@ pOut);

    if n > 0 then
       Result := TDictionary<usize,SymbolicVar>.Create;

    for i := 0 to n - 1 do
        Result.Add(pOut[i].id, SymbolicVar (pOut[i].SymVar) );
end;

function TApiHelper.getTaintedMemory: TArray<UInt64> ;
var
  n,i : Integer;
  uAddrs : PUInt64;
begin
   uAddrs := nil;
   n := Triton.Core.getTaintedMemory(FHApi,uAddrs);

   for i := 0 to n - 1 do
        Result := Result + [ uAddrs[i] ];
end;

function TApiHelper.getTaintedRegisters: TArray<Registro>;
var
  n,i     : Integer;
  rRegs   : PReg;
begin
    rRegs := nil;
    n := Triton.Core.getTaintedRegisters(FHApi,rRegs);

    for i := 0 to n - 1 do
        Result := Result + [ Registro (rRegs[i]) ];
end;

function TApiHelper.getParentRegisters:TArray<Registro>;
var
  n,i     : Integer;
  rRegs   : PReg;
begin
    rRegs := nil;
    n := Triton.Core.gettParentRegisters(FHApi,rRegs);

    for i := 0 to n - 1 do
        Result := Result + [ Registro (rRegs[i]) ];

end;

function TApiHelper.getTaintedSymbolicExpressions: TList<symbolicExp>;
var
  n,i        : Integer;
  rSimExpr   : PSimbolicExpr;
begin
    Result   := nil;
    rSimExpr := nil;
    n := Triton.Core.getTaintedSymbolicExpressions(FHApi,rSimExpr);

    if n > 0 then
       Result := TList<symbolicExp>.Create;

    for i := 0 to n - 1 do
        Result.Add( symbolicExp (rSimExpr[i]) );
end;

function TApiHelper.getTaintEngine: HandleTaintEngine;
begin
    Result := Triton.Core.getTaintEngine(FHApi)
end;

procedure TApiHelper.initEngines;
begin
    Triton.Core.initEngines(FHApi)
end;

procedure TApiHelper.initLeaAst(mem: MemAccess; force: Boolean);
begin
    Triton.Core.initLeaAst(FHApi,mem,force);
end;

function TApiHelper.isArchitectureValid: Boolean;
begin
    Result := Triton.Core.isArchitectureValid(FHApi)
end;

function TApiHelper.isFlag(regId: register_e): Boolean;
begin
    Result := Triton.Core.isFlag(FHApi, regId)
end;

function TApiHelper.isFlag(reg: Registro): Boolean;
begin
    Result := Triton.Core.isFlagR(FHApi,reg)
end;

function TApiHelper.isMemoryMapped(baseAddr: uint64; size: usize): Boolean;
begin
    Result := Triton.Core.isMemoryMapped(FHApi, baseAddr,size)
end;

function TApiHelper.isMemorySymbolized(mem: MemAccess): Boolean;
begin
    Result := Triton.Core.isMemorySymbolized(FHApi,mem)
end;

function TApiHelper.isMemorySymbolized(addr: uint64; size: uint32): Boolean;
begin
    Result := Triton.Core.isMemorySymbolizedSize(FHApi, addr,size)
end;

function TApiHelper.isMemoryTainted(addr: uint64; size: uint32): Boolean;
begin
    Result := Triton.Core.isMemoryTainted(FHApi, addr, size)
end;

function TApiHelper.isMemoryTainted(mem: MemAccess): Boolean;
begin
    Result := Triton.Core.isMemoryTaintedMem(FHApi,mem)
end;

function TApiHelper.isModeEnabled(mode: mode_e): Boolean;
begin
    Result := Triton.Core.isModeEnabled(FHApi,mode)
end;

function TApiHelper.isRegister(regId: register_e): Boolean;
begin
    Result := Triton.Core.isRegister(FHApi, regId)
end;

function TApiHelper.isRegister(reg: Registro): Boolean;
begin
    Result := Triton.Core.isRegisterR(FHApi, reg)
end;

function TApiHelper.isRegisterSymbolized(reg: Registro): Boolean;
begin
    Result := Triton.Core.isRegisterSymbolized(FHApi, reg)
end;

function TApiHelper.isRegisterTainted(reg: Registro): Boolean;
begin
    Result := Triton.Core.isRegisterTainted(FHApi, reg)
end;

function TApiHelper.isRegisterValid(regId: register_e): Boolean;
begin
    Result := Triton.Core.isRegisterValid(FHApi,regId)
end;

function TApiHelper.isRegisterValid(reg: Registro): Boolean;
begin
    Result := Triton.Core.isRegisterValidR(FHApi,reg)
end;

function TApiHelper.isSat(node: HandleAbstractNode): Boolean;
begin
    Result := Triton.Core.isSat(FHApi,node)
end;

function TApiHelper.isSolverValid: Boolean;
begin
    Result := Triton.Core.isSolverValid(FHApi)
end;

function TApiHelper.isSymbolicEngineEnabled: Boolean;
begin
    Result := Triton.Core.isSymbolicEngineEnabled(FHApi)
end;

function TApiHelper.isSymbolicExpressionIdExists(symExprId: usize): Boolean;
begin
    Result := Triton.Core.isSymbolicExpressionIdExists(FHApi,symExprId)
end;

function TApiHelper.isTainted(op: OpWrapper): Boolean;
begin
    Result := Triton.Core.isTainted(FHApi, op)
end;

function TApiHelper.isTaintEngineEnabled: Boolean;
begin
   Result := Triton.Core.isTaintEngineEnabled(FHApi)
end;

function TApiHelper.newSymbolicExpression(node: HandleAbstractNode;comment: AnsiString): symbolicExp;
begin
    Result := symbolicExp (Triton.Core.newSymbolicExpression(FHApi, node,PAnsiChar(comment)) )
end;

function TApiHelper.newSymbolicVariable(varSize: uint32;comment: AnsiString): SymbolicVar;
begin
    Result := SymbolicVar (Triton.Core.newSymbolicVariable(FHApi, varSize,PAnsiChar(comment)) )
end;

function TApiHelper.processCallbacks(kind: callback_e; node: HandleAbstractNode): HandleAbstractNode;
begin
    Result := Triton.Core.processCallbacks(FHApi, kind,node)
end;

procedure TApiHelper.processCallbacks(kind: callback_e; mem: MemAccess);
begin
    Triton.Core.processCallbacksMem(FHApi,kind,mem)
end;

procedure TApiHelper.processCallbacks(kind: callback_e; reg: Registro);
begin
    Triton.Core.processCallbacksReg(FHApi,kind,reg)
end;

function TApiHelper.processing(var inst: Istruzione): Boolean;
begin
    Result := Triton.Core.processing(FHApi, inst);
    inst.Reload;
end;

function TApiHelper.simplify(node: HandleAbstractNode; z3: Boolean): HandleAbstractNode;
begin
    Result := Triton.Core.processSimplification(FHApi,node,z3)
end;

function TApiHelper.processZ3Simplification(node: HandleAbstractNode): HandleAbstractNode;
begin
    Result := Triton.Core.processZ3Simplification(FHApi, node)
end;

procedure TApiHelper.removeAllCallbacks;
begin
    Triton.Core.removeAllCallbacks(FHApi)
end;

procedure TApiHelper.removeCallback(cb: Pointer; cbTipo: callback_e);
begin
    case callback_e(cbTipo) of
       GET_CONCRETE_MEMORY_VALUE :
       begin
           Triton.Core.removeCallbackGetMem(FHApi,cb);
       end;
       SET_CONCRETE_MEMORY_VALUE:
       begin
           Triton.Core.removeCallbackSetMem(FHApi,cb)
       end;
       GET_CONCRETE_REGISTER_VALUE:
       begin
           Triton.Core.removeCallbackGetReg(FHApi,cb)
       end;
       SET_CONCRETE_REGISTER_VALUE:
       begin
           Triton.Core.removeCallbackSetReg(FHApi,cb)
       end;
       SYMBOLIC_SIMPLIFICATION:
       begin
            Triton.Core.removeCallbackSimplif(FHApi,cb)
       end;
    end;

end;

procedure TApiHelper.removeEngines;
begin
   Triton.Core.removeEngines(FHApi)
end;

procedure TApiHelper.removeSymbolicExpression(symExprId: usize);
begin
    Triton.Core.removeSymbolicExpression(FHApi, symExprId)
end;

procedure TApiHelper.reset;
begin
    Triton.Core.reset(FHApi)
end;

procedure TApiHelper.setArchitecture(arch: architecture_e);
begin
   Triton.Core.setArchitecture(FHApi, arch);
   FArchitecture := arch;
end;

procedure TApiHelper.setAstRepresentationMode(mode: uint32);
begin
   Triton.Core.setAstRepresentationMode(FHApi, mode)
end;

procedure TApiHelper.setConcreteMemoryAreaValue(baseAddr: uint64; area: array of Byte; size: usize);
begin
    Triton.Core.setConcreteMemoryAreaValue(FHApi,baseAddr,@area[0], size)
end;

procedure TApiHelper.setConcreteMemoryAreaValue(baseAddr: uint64; values: array of Byte);
begin
    setConcreteMemoryAreaValue(baseAddr,values, Length(values))
end;

procedure TApiHelper.setConcreteMemoryValue(mem: MemAccess; value: uint64);
begin
    Triton.Core.setConcreteMemoryValue(FHApi, mem,value)
end;

procedure TApiHelper.setConcreteMemoryValue(addr: uint64; value: uint8);
begin
    Triton.Core.setConcreteMemoryValueByte(FHApi, addr ,value)
end;

procedure TApiHelper.setConcreteRegisterValue(reg: Registro; value: uint64);
begin
   Triton.Core.setConcreteRegisterValue(FHApi, reg,value)
end;

procedure TApiHelper.setConcreteVariableValue(symVar: SymbolicVar; value: UInt64);
begin
    Triton.Core.setConcreteVariableValue(FHApi, HandleSharedSymbolicVariable(symVar),value)
end;

procedure TApiHelper.setSolver(kind: solver_e);
begin
    Triton.Core.setSolver(FHApi,kind)
end;

function TApiHelper.setTaint(op: OpWrapper; flag: Boolean): Boolean;
begin
    Result := Triton.Core.setTaint(FHApi, op, flag)
end;

function TApiHelper.setTaintMemory(mem: MemAccess; flag: Boolean): Boolean;
begin
    Result := Triton.Core.setTaintMemory(FHApi, mem, flag)
end;

function TApiHelper.setTaintRegister(reg: Registro; flag: Boolean): Boolean;
begin
    Result := Triton.Core.setTaintRegister(FHApi, reg, flag)
end;

function TApiHelper.sliceExpressions(expr: symbolicExp): TDictionary<usize,symbolicExp>;
var
  n,i  : Integer;
  pOut : PIdSymExpr ;
begin
    Result := nil;
    pOut := nil;

    n := Triton.Core.sliceExpressions(FHApi, expr,@pOut);

    if n > 0 then
       Result := TDictionary<usize,symbolicExp>.Create;

    for i := 0 to n - 1 do
        Result.Add(pOut[i].id, symbolicExp (pOut[i].SymExpr) );
end;

function TApiHelper.taintAssignment(op1, op2: OpWrapper): Boolean;
begin
    Result := Triton.Core.taintAssignment(FHApi, op1,op2 )
end;

function TApiHelper.taintAssignmentMemoryImmediate(Dst: MemAccess): Boolean;
begin
    Result := Triton.Core.taintAssignmentMemoryImmediate(FHApi, Dst)
end;

function TApiHelper.taintAssignmentMemoryMemory(Dst, Src: MemAccess): Boolean;
begin
   Result := Triton.Core.taintAssignmentMemoryMemory(FHApi, Dst,Src)
end;

function TApiHelper.taintAssignmentMemoryRegister(Dst: MemAccess; Src: Registro): Boolean;
begin
    Result := Triton.Core.taintAssignmentMemoryRegister(FHApi,Dst,Src)
end;

function TApiHelper.taintAssignmentRegisterImmediate(Dst: Registro): Boolean;
begin
    Result := Triton.Core.taintAssignmentRegisterImmediate(FHApi, Dst)
end;

function TApiHelper.taintAssignmentRegisterMemory(Dst: Registro; Src: MemAccess): Boolean;
begin
    Result := Triton.Core.taintAssignmentRegisterMemory(FHApi,Dst,Src)
end;

function TApiHelper.taintAssignmentRegisterRegister(Dst, Src: Registro): Boolean;
begin
    Result := Triton.Core.taintAssignmentRegisterRegister(FHApi, Dst,Src)
end;

function TApiHelper.taintMemory(addr: uint64): Boolean;
begin
   Result := Triton.Core.taintMemory(FHApi, addr)
end;

function TApiHelper.taintMemory(mem: MemAccess): Boolean;
begin
    Result := Triton.Core.taintMemoryMem(FHApi, mem)
end;

function TApiHelper.taintRegister(reg: Registro): Boolean;
begin
   Result := Triton.Core.taintRegister(FHApi, reg)
end;

function TApiHelper.taintUnion(op1, op2: OpWrapper): Boolean;
begin
    Result := Triton.Core.taintUnion(FHApi, op1,op2)
end;

function TApiHelper.taintUnionMemoryImmediate(Dst: MemAccess): Boolean;
begin
    Result := Triton.Core.taintUnionMemoryImmediate(FHApi, Dst)
end;

function TApiHelper.taintUnionMemoryMemory(Dst, Src: MemAccess): Boolean;
begin
    Result := Triton.Core.taintUnionMemoryMemory(FHApi, Dst,Src)
end;

function TApiHelper.taintUnionMemoryRegister(Dst: MemAccess; Src: Registro): Boolean;
begin
    Result := Triton.Core.taintUnionMemoryRegister(FHApi,Dst,Src)
end;

function TApiHelper.taintUnionRegisterImmediate(Dst: Registro): Boolean;
begin
   Result := Triton.Core.taintUnionRegisterImmediate(FHApi, Dst)
end;

function TApiHelper.taintUnionRegisterMemory(Dst: Registro; Src: MemAccess): Boolean;
begin
   Result := Triton.Core.taintUnionRegisterMemory(FHApi, Dst,Src)
end;

function TApiHelper.taintUnionRegisterRegister(Dst, Src: Registro): Boolean;
begin
   Result := Triton.Core.taintUnionRegisterRegister(FHApi,Dst,Src)
end;

procedure TApiHelper.unmapMemory(baseAddr: uint64; size: usize);
begin
   Triton.Core.unmapMemory(FHApi, baseAddr,size)
end;

function TApiHelper.untaintMemory(addr: uint64): Boolean;
begin
    Result := Triton.Core.untaintMemory(FHApi,addr)
end;

function TApiHelper.untaintMemory(mem: MemAccess): Boolean;
begin
    Result := Triton.Core.untaintMemoryMem(FHApi, mem)
end;

function TApiHelper.untaintRegister(reg: Registro): Boolean;
begin
    Result := Triton.Core.untaintRegister(FHApi,reg)
end;

end.
