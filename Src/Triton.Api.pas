unit Triton.Api;

{$Z4}
{$POINTERMATH ON}

interface
   uses System.SysUtils,Winapi.Windows,
        System.Generics.Collections,
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

  APathConstraint = array of PathConstraint;

  TApi = record
    private
       FHApi : HandleApi;

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
        {  //! [**architecture api**] - Returns all registers. \sa triton::arch::x86::register_e.
        const std::unordered_map<triton::arch::register_e, const triton::arch::Register>& getAllRegisters(void) const;
        //! [**architecture api**] - Returns all parent registers. \sa triton::arch::x86::register_e.
        std::set<const triton::arch::Register*> getParentRegisters(void) const;   }
        function   getConcreteMemoryValue(addr: uint64; execCallbacks : Boolean = true):uint8; overload;
        function   getConcreteMemoryValue(mem: MemAccess; execCallbacks : Boolean = true): uint64; overload;
        function   getConcreteMemoryAreaValue(baseAddr: uint64; size: usize; execCallbacks : Boolean = true):TArray<Byte>;
        function   getConcreteRegisterValue(reg: Registro; execCallbacks : Boolean = true): uint64;
        procedure  setConcreteMemoryValue(addr: uint64; value: uint8); overload;
        procedure  setConcreteMemoryValue(mem: MemAccess; value: uint64); overload;
        procedure  setConcreteMemoryAreaValue(baseAddr : uint64; values: array of Byte); overload;
        procedure  setConcreteMemoryAreaValue(baseAddr: uint64; area : array of Byte;  size: usize); overload;
        procedure  setConcreteRegisterValue(reg: Registro; value: uint64);
        function   isMemoryMapped(baseAddr: uint64;  size : usize = 1): Boolean;
        procedure  unmapMemory(baseAddr: uint64;  size : usize = 1);
        procedure  disassembly(inst: Istruzione);
        (* Processing API ============ *)
        function   processing(var inst: Istruzione): Boolean;
        procedure  initEngines;
        procedure  removeEngines;
        procedure  reset;
        (* IR API ============== *)
        procedure  checkIrBuilder;
        function   buildSemantics(inst: Istruzione): Boolean;
        function   getAstContext: AstContext;
        (* AST Representation API ========== *)
        function   getAstRepresentationMode:uint32 ;
        procedure  setAstRepresentationMode(mode: uint32);
        (* Callbacks API ========================= *)
        procedure addCallback(cb: cbGetMemVal); overload;
        procedure addCallback(cb: cbGetRegVal); overload;
        procedure addCallback(cb: cbSetMemVal); overload;
        procedure addCallback(cb: cbSetRegVal); overload;
        procedure addCallback(cb: cbSimplification); overload;
        procedure removeAllCallbacks;
        procedure removeCallback(cb: cbGetMemVal);overload;
        procedure removeCallback(cb: cbGetRegVal);overload;
        procedure removeCallback(cb: cbSetMemVal); overload;
        procedure removeCallback(cb: cbSetRegVal);overload;
        procedure removeCallback(cb: cbSimplification); overload;
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
        function  convertMemoryToSymbolicVariable(mem: MemAccess;  symVarComment: AnsiString = ''): HandleSharedSymbolicVariable;
        function  convertRegisterToSymbolicVariable(reg: Registro;  symVarComment : AnsiString = ''): HandleSharedSymbolicVariable;
        function  getOperandAst(op: OpWrapper): HandleAbstractNode; overload;
        function  getOperandAst(inst: Istruzione ; op: OpWrapper):HandleAbstractNode; overload;
        function  getImmediateAst(imm: Immediate): HandleAbstractNode;overload;
        function  getImmediateAst(inst: Istruzione ; imm: Immediate):HandleAbstractNode;overload;
        function  getMemoryAst(mem: MemAccess):HandleAbstractNode;overload;
        function  getMemoryAst(inst: Istruzione ; mem: MemAccess):HandleAbstractNode;overload;
        function  getRegisterAst(reg: Registro):HandleAbstractNode; overload;
        function  getRegisterAst(inst: Istruzione ; reg: Registro):HandleAbstractNode; overload;
        function  newSymbolicExpression(node: HandleAbstractNode; comment: AnsiString = ''):symbolicExp;
        function  newSymbolicVariable(varSize: uint32; comment: AnsiString = ''):HandleSharedSymbolicVariable;
        procedure removeSymbolicExpression(symExprId: usize);
        function  createSymbolicExpression(inst: Istruzione ; node: HandleAbstractNode; dst: OpWrapper; comment : AnsiString = ''):symbolicExp;
        function  createSymbolicMemoryExpression(inst: Istruzione ; node: HandleAbstractNode; mem: MemAccess; comment: AnsiString = ''):symbolicExp;
        function  createSymbolicRegisterExpression(inst: Istruzione ; node: HandleAbstractNode; reg: Registro; comment: AnsiString = ''):symbolicExp;
        function  createSymbolicFlagExpression(inst: Istruzione ; node: HandleAbstractNode; flag: Registro; comment: AnsiString = ''):symbolicExp;
        function  createSymbolicVolatileExpression(inst: Istruzione ; node: HandleAbstractNode; comment: AnsiString = ''):symbolicExp;
        procedure assignSymbolicExpressionToMemory(se:symbolicExp; mem: MemAccess);
        procedure assignSymbolicExpressionToRegister(se:symbolicExp; reg: Registro);
        function  processSimplification(node: HandleAbstractNode; z3: Boolean = false):HandleAbstractNode;
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
        function  getTaintedSymbolicExpressions:ListExpr;
        function  getSymbolicExpressions:TDictionary<usize,symbolicExp> ;
        function  getSymbolicVariables:TDictionary<usize,SymbolicVar> ;
        function  getConcreteVariableValue(symVar:HandleSharedSymbolicVariable):uint64 ;
        procedure setConcreteVariableValue(symVar: HandleSharedSymbolicVariable; value: UInt64);
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
        function  untaintMemory(addr: uint64): Boolean ;
        function  untaintMemoryMem(mem: MemAccess ): Boolean ;
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

        class operator Implicit(rApi: TApi): HandleApi;
        class Operator Explicit(hApi: HandleApi):TApi;
        class Operator Explicit(rApi: TApi):HandleApi;
  end;

implementation

{ Api }

procedure TApi.Create;
begin
    FHApi := CreateAPI();
end;

procedure TApi.Free;
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

procedure TApi.addCallback(cb: cbGetMemVal);
begin
     Triton.Core.addCallbackGetMem(FHApi,cb)
end;

procedure TApi.addCallback(cb: cbGetRegVal);
begin
    Triton.Core.addCallbackGetReg(FHApi,cb)
end;

procedure TApi.addCallback(cb: cbSetMemVal);
begin
    Triton.Core.addCallbackSetMem(FHApi,cb)
end;

procedure TApi.addCallback(cb: cbSetRegVal);
begin
    Triton.Core.addCallbackSetReg(FHApi,cb)
end;

procedure TApi.addCallback(cb: cbSimplification);
begin
    Triton.Core.addCallbackSimplif(FHApi,cb)
end;

procedure TApi.addPathConstraint(inst: Istruzione; expr: symbolicExp);
begin
    Triton.Core.addPathConstraint(FHApi, inst, expr)
end;

procedure TApi.assignSymbolicExpressionToMemory(se: symbolicExp; mem: MemAccess);
begin
    Triton.Core.assignSymbolicExpressionToMemory(FHApi,se,mem)
end;

procedure TApi.assignSymbolicExpressionToRegister(se: symbolicExp;  reg: Registro);
begin
    Triton.Core.assignSymbolicExpressionToRegister(FHApi,se,reg)
end;

function TApi.buildSemantics(inst: Istruzione): Boolean;
begin
    Result := Triton.Core.buildSemantics(FHApi,inst)
end;

procedure TApi.checkArchitecture;
begin
    Triton.Core.checkArchitecture(FHApi)
end;

procedure TApi.checkIrBuilder;
begin
    Triton.Core.checkIrBuilder(FHApi)
end;

procedure TApi.checkModes;
begin
    Triton.Core.checkModes(FHApi)
end;

procedure TApi.checkSolver;
begin
    Triton.Core.checkSolver(FHApi)
end;

procedure TApi.checkSymbolic;
begin
    Triton.Core.checkSymbolic(FHApi)
end;

procedure TApi.checkTaint;
begin
    Triton.Core.checkTaint(FHApi)
end;

procedure TApi.clearArchitecture;
begin
    Triton.Core.clearArchitecture(FHApi)
end;

procedure TApi.clearPathConstraints;
begin
    Triton.Core.clearPathConstraints(FHApi)
end;

procedure TApi.concretizeAllMemory;
begin
    Triton.Core.concretizeAllMemory(FHApi)
end;

procedure TApi.concretizeAllRegister;
begin
    Triton.Core.concretizeAllRegister(FHApi)
end;

procedure TApi.concretizeMemory(addr: uint64);
begin
    Triton.Core.concretizeMemory(FHApi,addr)
end;

procedure TApi.concretizeMemory(mem: MemAccess);
begin
    Triton.Core.concretizeMemoryM(FHApi,mem)
end;

procedure TApi.concretizeRegister(reg: Registro);
begin
    Triton.Core.concretizeRegister(FHApi,reg)
end;

function TApi.convertExpressionToSymbolicVariable(exprId: usize; symVarSize: uint32; symVarComment: AnsiString): HandleSharedSymbolicVariable;
begin
    Result := Triton.Core.convertExpressionToSymbolicVariable(FHApi,exprId,symVarSize,PAnsiChar( symVarComment))
end;

function TApi.convertMemoryToSymbolicVariable(mem: MemAccess; symVarComment: AnsiString): HandleSharedSymbolicVariable;
begin
    Result := Triton.Core.convertMemoryToSymbolicVariable(FHApi,mem, PAnsiChar( symVarComment))
end;

function TApi.convertRegisterToSymbolicVariable(reg: Registro; symVarComment: AnsiString ): HandleSharedSymbolicVariable;
begin
    Result := Triton.Core.convertRegisterToSymbolicVariable(FHApi,reg, PAnsiChar(symVarComment))
end;

function TApi.createSymbolicExpression(inst: Istruzione; node: HandleAbstractNode; dst: OpWrapper; comment: AnsiString): symbolicExp;
begin
    Result := symbolicExp (Triton.Core.createSymbolicExpression(FHApi,inst,node,dst,PAnsiChar (comment)) )
end;

function TApi.createSymbolicFlagExpression(inst: Istruzione; node: HandleAbstractNode; flag: Registro; comment: AnsiString): symbolicExp;
begin
    Result := symbolicExp (Triton.Core.createSymbolicFlagExpression(FHApi,inst,node,flag,PAnsiChar( comment)) )
end;

function TApi.createSymbolicMemoryExpression(inst: Istruzione; node: HandleAbstractNode; mem: MemAccess; comment: AnsiString): symbolicExp;
begin
    Result := symbolicExp (Triton.Core.createSymbolicMemoryExpression(FHApi,inst,node,mem,PAnsiChar (comment)) )
end;

function TApi.createSymbolicRegisterExpression(inst: Istruzione;node: HandleAbstractNode; reg: Registro; comment: AnsiString): symbolicExp;
begin
    Result := symbolicExp (Triton.Core.createSymbolicRegisterExpression(FHApi,inst,node,reg,PAnsiChar( comment)) )
end;

function TApi.createSymbolicVolatileExpression(inst: Istruzione; node: HandleAbstractNode; comment: AnsiString): symbolicExp;
begin
    Result := symbolicExp (Triton.Core.createSymbolicVolatileExpression(FHApi,inst,node,PAnsiChar( comment)) )
end;

procedure TApi.disassembly(inst: Istruzione);
begin
    Triton.Core.disassembly(FHApi,inst)
end;

procedure TApi.enableMode(mode: mode_e; flag: Boolean);
begin
    Triton.Core.enableMode(FHApi, mode, flag)
end;

procedure TApi.enableSymbolicEngine(flag: Boolean);
begin
    Triton.Core.enableSymbolicEngine(FHApi,flag)
end;

procedure TApi.enableTaintEngine(flag: Boolean);
begin
    Triton.Core.enableTaintEngine(FHApi,flag)
end;

function TApi.evaluateAstViaZ3(node: HandleAbstractNode): uint64;
begin
    Result := Triton.Core.evaluateAstViaZ3(FHApi,node)
end;

function TApi.getArchitecture: architecture_e;
begin
    Result := Triton.Core.getArchitecture(FHApi)
end;

function TApi.getAstContext: AstContext;
begin
    Result := AstContext (Triton.Core.getAstContext(FHApi) )
end;

function TApi.getAstRepresentationMode: uint32;
begin
    Result := Triton.Core.getAstRepresentationMode(FHApi)
end;

function TApi.getConcreteMemoryAreaValue(baseAddr: uint64; size: usize; execCallbacks: Boolean): TArray<Byte>;
var
  res : PByte;
begin

    res := Triton.Core.getConcreteMemoryAreaValue(FHApi,baseAddr,size,execCallbacks);
    SetLength(Result,size);
    CopyMemory(@Result[0],res,size * SizeOf(uint8));
end;

function TApi.getConcreteMemoryValue(mem: MemAccess; execCallbacks: Boolean): uint64;
begin
    Result := Triton.Core.getConcreteMemoryValue(FHApi,mem,execCallbacks)
end;

function TApi.getConcreteMemoryValue(addr: uint64; execCallbacks: Boolean): uint8;
begin
    Result := Triton.Core.getConcreteMemoryValueByte(FHApi, addr,execCallbacks)
end;

function TApi.getConcreteRegisterValue(reg: Registro;  execCallbacks: Boolean): uint64;
begin
    Result := Triton.Core.getConcreteRegisterValue(FHApi,reg, execCallbacks)
end;

function TApi.getConcreteVariableValue(symVar: HandleSharedSymbolicVariable): uint64;
begin
    Result := Triton.Core.getConcreteVariableValue(FHApi, symVar)
end;

function TApi.getCpuInstance: HandleCpuInterface;
begin
    Result := Triton.Core.getCpuInstance(FHApi)
end;

function TApi.getEndianness: endianness_e;
begin
    Result := Triton.Core.getEndianness(FHApi)
end;

function TApi.getGprBitSize: uint32;
begin
    Result := Triton.Core.getGprBitSize(FHApi)
end;

function TApi.getGprSize: uint32;
begin
   Result := Triton.Core.getGprSize (FHApi)
end;

function TApi.getImmediateAst(imm: Immediate): HandleAbstractNode;
begin
    Result := Triton.Core.getImmediateAst(FHApi, imm)
end;

function TApi.getImmediateAst(inst: Istruzione; imm: Immediate): HandleAbstractNode;
begin
    Result := Triton.Core.getImmediateAstIstruz(FHApi,inst,imm)
end;

function TApi.getMemoryAst(mem: MemAccess): HandleAbstractNode;
begin
    Result := Triton.Core.getMemoryAst(FHApi,mem)
end;

function TApi.getMemoryAst(inst: Istruzione; mem: MemAccess): HandleAbstractNode;
begin
    Result := Triton.Core.getMemoryAstIstruz(FHApi,inst,mem)
end;

function TApi.getModel(node: HandleAbstractNode): TDictionary<UInt32,SolverModel>;
var
  n,i  : Integer;
  pOut : PAddrSolver ;

begin
    Result := nil;
    pOut := nil;
    n :=  Triton.Core.getModel(FHApi,node,@pOut );

    if n > 0 then
       Result := TDictionary<UInt32,SolverModel>.Create;

    for i := 0 to n - 1 do
        Result.Add(pOut[i].id, SolverModel (pOut[i].Model) );
end;

function TApi.getModels(node: HandleAbstractNode; limit: uint32): TList< TDictionary<UInt32,SolverModel> >;
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

function TApi.getNumberOfRegisters: uint32;
begin
    Result := Triton.Core.getNumberOfRegisters(FHApi)
end;

function TApi.getOperandAst(op: OpWrapper): HandleAbstractNode;
begin
    Result :=  Triton.Core.getOperandAst(FHApi,op)
end;

function TApi.getOperandAst(inst: Istruzione; op: OpWrapper): HandleAbstractNode;
begin
    Result := Triton.Core.getOperandAstIstruz(FHApi, inst, op)
end;

function TApi.getParentRegister(id: register_e): Registro;
begin
    Result := Registro (Triton.Core.getParentRegister(FHApi,id) )
end;

function TApi.getParentRegister(reg: Registro): Registro;
begin
    Result := Registro (Triton.Core.getParentRegisterR(FHApi,reg) )
end;

function TApi.getPathConstraints: APathConstraint;
var
  n,i : Integer;
  p   : PHandlePathConstraint;
begin
    p := nil;
    n := Triton.Core.getPathConstraints(FHApi, @p );

    for i := 0 to n -1 do
      Result := Result + [ pathConstraint (p[i]) ]

end;

function TApi.getPathConstraintsAst: HandleAbstractNode;
begin
    Result := Triton.Core.getPathConstraintsAst(FHApi)
end;

function TApi.getRegister(id: register_e): Registro;
begin
    Result := Registro (Triton.Core.getRegister(FHApi, id) )
end;

function TApi.getRegisterAst(reg: Registro): HandleAbstractNode;
begin
    Result := Triton.Core.getRegisterAst(FHApi, reg)
end;

function TApi.getRegisterAst(inst: Istruzione; reg: Registro): HandleAbstractNode;
begin
    Result := Triton.Core.getRegisterAstIstruz(FHApi, inst,reg)
end;

function TApi.getSolver: solver_e;
begin
    Result := Triton.Core.getSolver(FHApi)
end;

function TApi.getSymbolicEngine: HandleSymbolicEngine;
begin
    Result := Triton.Core.getSymbolicEngine(FHApi)
end;

function TApi.getSymbolicExpressionFromId(symExprId: usize): symbolicExp;
begin
    Result := symbolicExp (Triton.Core.getSymbolicExpressionFromId(FHApi, symExprId) )
end;

function TApi.getSymbolicExpressions: TDictionary<usize,symbolicExp>;
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

function TApi.getSymbolicMemory: TDictionary<UInt64,symbolicExp>;
var
  n,i  : Integer;
  pOut : PMemSymE ;
begin
    Result := nil;
    pOut   := nil;

    n  := Triton.Core.getSymbolicMemory(FHApi,@pOut);

    if n > 0 then
       Result := TDictionary<UInt64,symbolicExp>.Create;

    for i := 0 to n - 1 do
        Result.Add(pOut[i].mem, symbolicExp (pOut[i].MemSym) )
end;

function TApi.getSymbolicMemory(addr: uint64): symbolicExp;
begin
    Result := symbolicExp (Triton.Core.getSymbolicMemoryAddr(FHApi, addr) )
end;

function TApi.getSymbolicMemoryAreaValue(baseAddr: uint64; size: usize): TArray<Byte>;
var
 res : PByte;
begin

    res := Triton.Core.getSymbolicMemoryAreaValue(FHApi,baseAddr,size)  ;

    SetLength(Result,size);
    CopyMemory(@Result[0],res,size * SizeOf(uint8));
end;

function TApi.getSymbolicMemoryValue(address: uint64): uint8;
begin
    Result := Triton.Core.getSymbolicMemoryValue(FHApi, address)
end;

function TApi.getSymbolicMemoryValue(mem: MemAccess): uint64;
begin
    Result := Triton.Core.getSymbolicMemoryValueM(FHApi, mem)
end;

function TApi.getSymbolicRegister(reg: Registro): symbolicExp;
begin
   Result := symbolicExp (Triton.Core.getSymbolicRegister(FHApi, reg) )
end;

(*var
  n,i  : Integer;
  pOut : PAddrSolver ;

begin
    Result := nil;
    pOut := nil;
    n :=  Triton.Core.getModel(FHApi,node,@pOut );

    if n > 0 then
       Result := TDictionary<UInt32,SolverModel>.Create;

    for i := 0 to n - 1 do
        Result.Add(pOut[i].id, SolverModel (pOut[i].Model) );
*)
function TApi.getSymbolicRegisters: TDictionary<register_e,symbolicExp>;
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

function TApi.getSymbolicRegisterValue(reg: Registro): uint64;
begin
   Result := Triton.Core.getSymbolicRegisterValue(FHApi,reg)
end;

function TApi.getSymbolicVariableFromId(symVarId: usize): HandleSharedSymbolicVariable;
begin
    Result := Triton.Core.getSymbolicVariableFromId(FHApi,symVarId)
end;

function TApi.getSymbolicVariableFromName(symVarName: AnsiString): HandleSharedSymbolicVariable;
begin
    Result := Triton.Core.getSymbolicVariableFromName(FHApi, PAnsiChar(symVarName))
end;

function TApi.getSymbolicVariables: TDictionary<usize,SymbolicVar>;
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

function TApi.getTaintedMemory: TArray<UInt64> ;
var
  n,i : Integer;
  uAddrs : PUInt64;
begin
   uAddrs := nil;
   n := Triton.Core.getTaintedMemory(FHApi,uAddrs);

   for i := 0 to n - 1 do
        Result := Result + [ uAddrs[i] ];
end;

function TApi.getTaintedRegisters: TArray<Registro>;
var
  n,i     : Integer;
  rRegs   : PReg;
begin
    rRegs := nil;
    n := Triton.Core.getTaintedRegisters(FHApi,rRegs);

    for i := 0 to n - 1 do
        Result := Result + [ Registro (rRegs[i]) ];
end;

function TApi.getTaintedSymbolicExpressions: ListExpr;
begin
    Result := Triton.Core.getTaintedSymbolicExpressions(FHApi)
end;

function TApi.getTaintEngine: HandleTaintEngine;
begin
    Result := Triton.Core.getTaintEngine(FHApi)
end;

procedure TApi.initEngines;
begin
    Triton.Core.initEngines(FHApi)
end;

function TApi.isArchitectureValid: Boolean;
begin
    Result := Triton.Core.isArchitectureValid(FHApi)
end;

function TApi.isFlag(regId: register_e): Boolean;
begin
    Result := Triton.Core.isFlag(FHApi, regId)
end;

function TApi.isFlag(reg: Registro): Boolean;
begin
    Result := Triton.Core.isFlagR(FHApi,reg)
end;

function TApi.isMemoryMapped(baseAddr: uint64; size: usize): Boolean;
begin
    Result := Triton.Core.isMemoryMapped(FHApi, baseAddr,size)
end;

function TApi.isMemorySymbolized(mem: MemAccess): Boolean;
begin
    Result := Triton.Core.isMemorySymbolized(FHApi,mem)
end;

function TApi.isMemorySymbolized(addr: uint64; size: uint32): Boolean;
begin
    Result := Triton.Core.isMemorySymbolizedSize(FHApi, addr,size)
end;

function TApi.isMemoryTainted(addr: uint64; size: uint32): Boolean;
begin
    Result := Triton.Core.isMemoryTainted(FHApi, addr, size)
end;

function TApi.isMemoryTainted(mem: MemAccess): Boolean;
begin
    Result := Triton.Core.isMemoryTaintedMem(FHApi,mem)
end;

function TApi.isModeEnabled(mode: mode_e): Boolean;
begin
    Result := Triton.Core.isModeEnabled(FHApi,mode)
end;

function TApi.isRegister(regId: register_e): Boolean;
begin
    Result := Triton.Core.isRegister(FHApi, regId)
end;

function TApi.isRegister(reg: Registro): Boolean;
begin
    Result := Triton.Core.isRegisterR(FHApi, reg)
end;

function TApi.isRegisterSymbolized(reg: Registro): Boolean;
begin
    Result := Triton.Core.isRegisterSymbolized(FHApi, reg)
end;

function TApi.isRegisterTainted(reg: Registro): Boolean;
begin
    Result := Triton.Core.isRegisterTainted(FHApi, reg)
end;

function TApi.isRegisterValid(regId: register_e): Boolean;
begin
    Result := Triton.Core.isRegisterValid(FHApi,regId)
end;

function TApi.isRegisterValid(reg: Registro): Boolean;
begin
    Result := Triton.Core.isRegisterValidR(FHApi,reg)
end;

function TApi.isSat(node: HandleAbstractNode): Boolean;
begin
    Result := Triton.Core.isSat(FHApi,node)
end;

function TApi.isSolverValid: Boolean;
begin
    Result := Triton.Core.isSolverValid(FHApi)
end;

function TApi.isSymbolicEngineEnabled: Boolean;
begin
    Result := Triton.Core.isSymbolicEngineEnabled(FHApi)
end;

function TApi.isSymbolicExpressionIdExists(symExprId: usize): Boolean;
begin
    Result := Triton.Core.isSymbolicExpressionIdExists(FHApi,symExprId)
end;

function TApi.isTainted(op: OpWrapper): Boolean;
begin
    Result := Triton.Core.isTainted(FHApi, op)
end;

function TApi.isTaintEngineEnabled: Boolean;
begin
   Result := Triton.Core.isTaintEngineEnabled(FHApi)
end;

function TApi.newSymbolicExpression(node: HandleAbstractNode;comment: AnsiString): symbolicExp;
begin
    Result := symbolicExp (Triton.Core.newSymbolicExpression(FHApi, node,PAnsiChar(comment)) )
end;

function TApi.newSymbolicVariable(varSize: uint32;comment: AnsiString): HandleSharedSymbolicVariable;
begin
    Result := Triton.Core.newSymbolicVariable(FHApi, varSize,PAnsiChar(comment))
end;

function TApi.processCallbacks(kind: callback_e; node: HandleAbstractNode): HandleAbstractNode;
begin
    Result := Triton.Core.processCallbacks(FHApi, kind,node)
end;

procedure TApi.processCallbacks(kind: callback_e; mem: MemAccess);
begin
    Triton.Core.processCallbacksMem(FHApi,kind,mem)
end;

procedure TApi.processCallbacks(kind: callback_e; reg: Registro);
begin
    Triton.Core.processCallbacksReg(FHApi,kind,reg)
end;

function TApi.processing(var inst: Istruzione): Boolean;
begin
    Result := Triton.Core.processing(FHApi, inst);
    inst.Reload;
end;

function TApi.processSimplification(node: HandleAbstractNode; z3: Boolean): HandleAbstractNode;
begin
    Result := Triton.Core.processSimplification(FHApi,node,z3)
end;

function TApi.processZ3Simplification(node: HandleAbstractNode): HandleAbstractNode;
begin
    Result := Triton.Core.processZ3Simplification(FHApi, node)
end;

procedure TApi.removeAllCallbacks;
begin
    Triton.Core.removeAllCallbacks(FHApi)
end;

procedure TApi.removeCallback(cb: cbGetMemVal);
begin
    Triton.Core.removeCallbackGetMem(FHApi,  cb)
end;

procedure TApi.removeCallback(cb: cbGetRegVal);
begin
    Triton.Core.removeCallbackGetReg(FHApi, cb)
end;

procedure TApi.removeCallback(cb: cbSetMemVal);
begin
   Triton.Core.removeCallbackSetMem(FHApi, cb)
end;

procedure TApi.removeCallback(cb: cbSetRegVal);
begin
   Triton.Core.removeCallbackSetReg(FHApi, cb)
end;

procedure TApi.removeCallback(cb: cbSimplification);
begin
   Triton.Core.removeCallbackSimplif(FHApi, cb)
end;

procedure TApi.removeEngines;
begin
   Triton.Core.removeEngines(FHApi)
end;

procedure TApi.removeSymbolicExpression(symExprId: usize);
begin
    Triton.Core.removeSymbolicExpression(FHApi, symExprId)
end;

procedure TApi.reset;
begin
    Triton.Core.reset(FHApi)
end;

procedure TApi.setArchitecture(arch: architecture_e);
begin
   Triton.Core.setArchitecture(FHApi, arch)
end;

procedure TApi.setAstRepresentationMode(mode: uint32);
begin
   Triton.Core.setAstRepresentationMode(FHApi, mode)
end;

procedure TApi.setConcreteMemoryAreaValue(baseAddr: uint64; area: array of Byte; size: usize);
begin
    Triton.Core.setConcreteMemoryAreaValue(FHApi,baseAddr,@area[0], size)
end;

procedure TApi.setConcreteMemoryAreaValue(baseAddr: uint64; values: array of Byte);
begin
    setConcreteMemoryAreaValue(baseAddr,values, Length(values))
end;

procedure TApi.setConcreteMemoryValue(mem: MemAccess; value: uint64);
begin
    Triton.Core.setConcreteMemoryValue(FHApi, mem,value)
end;

procedure TApi.setConcreteMemoryValue(addr: uint64; value: uint8);
begin
    Triton.Core.setConcreteMemoryValueByte(FHApi, addr ,value)
end;

procedure TApi.setConcreteRegisterValue(reg: Registro; value: uint64);
begin
   Triton.Core.setConcreteRegisterValue(FHApi, reg,value)
end;

procedure TApi.setConcreteVariableValue(symVar: HandleSharedSymbolicVariable; value: UInt64);
begin
    Triton.Core.setConcreteVariableValue(FHApi, symVar,value)
end;

procedure TApi.setSolver(kind: solver_e);
begin
    Triton.Core.setSolver(FHApi,kind)
end;

function TApi.setTaint(op: OpWrapper; flag: Boolean): Boolean;
begin
    Result := Triton.Core.setTaint(FHApi, op, flag)
end;

function TApi.setTaintMemory(mem: MemAccess; flag: Boolean): Boolean;
begin
    Result := Triton.Core.setTaintMemory(FHApi, mem, flag)
end;

function TApi.setTaintRegister(reg: Registro; flag: Boolean): Boolean;
begin
    Result := Triton.Core.setTaintRegister(FHApi, reg, flag)
end;

function TApi.sliceExpressions(expr: symbolicExp): TDictionary<usize,symbolicExp>;
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

function TApi.taintAssignment(op1, op2: OpWrapper): Boolean;
begin
    Result := Triton.Core.taintAssignment(FHApi, op1,op2 )
end;

function TApi.taintAssignmentMemoryImmediate(Dst: MemAccess): Boolean;
begin
    Result := Triton.Core.taintAssignmentMemoryImmediate(FHApi, Dst)
end;

function TApi.taintAssignmentMemoryMemory(Dst, Src: MemAccess): Boolean;
begin
   Result := Triton.Core.taintAssignmentMemoryMemory(FHApi, Dst,Src)
end;

function TApi.taintAssignmentMemoryRegister(Dst: MemAccess; Src: Registro): Boolean;
begin
    Result := Triton.Core.taintAssignmentMemoryRegister(FHApi,Dst,Src)
end;

function TApi.taintAssignmentRegisterImmediate(Dst: Registro): Boolean;
begin
    Result := Triton.Core.taintAssignmentRegisterImmediate(FHApi, Dst)
end;

function TApi.taintAssignmentRegisterMemory(Dst: Registro; Src: MemAccess): Boolean;
begin
    Result := Triton.Core.taintAssignmentRegisterMemory(FHApi,Dst,Src)
end;

function TApi.taintAssignmentRegisterRegister(Dst, Src: Registro): Boolean;
begin
    Result := Triton.Core.taintAssignmentRegisterRegister(FHApi, Dst,Src)
end;

function TApi.taintMemory(addr: uint64): Boolean;
begin
   Result := Triton.Core.taintMemory(FHApi, addr)
end;

function TApi.taintMemory(mem: MemAccess): Boolean;
begin
    Result := Triton.Core.taintMemoryMem(FHApi, mem)
end;

function TApi.taintRegister(reg: Registro): Boolean;
begin
   Result := Triton.Core.taintRegister(FHApi, reg)
end;

function TApi.taintUnion(op1, op2: OpWrapper): Boolean;
begin
    Result := Triton.Core.taintUnion(FHApi, op1,op2)
end;

function TApi.taintUnionMemoryImmediate(Dst: MemAccess): Boolean;
begin
    Result := Triton.Core.taintUnionMemoryImmediate(FHApi, Dst)
end;

function TApi.taintUnionMemoryMemory(Dst, Src: MemAccess): Boolean;
begin
    Result := Triton.Core.taintUnionMemoryMemory(FHApi, Dst,Src)
end;

function TApi.taintUnionMemoryRegister(Dst: MemAccess; Src: Registro): Boolean;
begin
    Result := Triton.Core.taintUnionMemoryRegister(FHApi,Dst,Src)
end;

function TApi.taintUnionRegisterImmediate(Dst: Registro): Boolean;
begin
   Result := Triton.Core.taintUnionRegisterImmediate(FHApi, Dst)
end;

function TApi.taintUnionRegisterMemory(Dst: Registro; Src: MemAccess): Boolean;
begin
   Result := Triton.Core.taintUnionRegisterMemory(FHApi, Dst,Src)
end;

function TApi.taintUnionRegisterRegister(Dst, Src: Registro): Boolean;
begin
   Result := Triton.Core.taintUnionRegisterRegister(FHApi,Dst,Src)
end;

procedure TApi.unmapMemory(baseAddr: uint64; size: usize);
begin
   Triton.Core.unmapMemory(FHApi, baseAddr,size)
end;

function TApi.untaintMemory(addr: uint64): Boolean;
begin
    Result := Triton.Core.untaintMemory(FHApi,addr)
end;

function TApi.untaintMemoryMem(mem: MemAccess): Boolean;
begin
    Result := Triton.Core.untaintMemoryMem(FHApi, mem)
end;

function TApi.untaintRegister(reg: Registro): Boolean;
begin
    Result := Triton.Core.untaintRegister(FHApi,reg)
end;

end.
