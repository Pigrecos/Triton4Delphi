unit Triton.Api;

{$Z4}
{$POINTERMATH ON}

interface
   uses System.SysUtils,Winapi.Windows,
        System.Generics.Collections,
        Spring,
        triton.SyntResult,
        Triton.AstNode,
        Triton.SolverModel,
        Triton.Define,
        Triton.Register,
        Triton.Immediate,
        Triton.MemoryAccess,
        Triton.Instruction,
        Triton.BasicBlock,
        Triton.OperandWrapper,
        Triton.AstContext,
        Triton.SymbolicExpression,
        Triton.SymbolicVariable,
        Triton.pathConstraint,
        Triton.Core;

type

  TTritonCtx = record
    private
       FHApi        : HandleContext;
       FArchitecture: architecture_e;
    public
       class operator Implicit(rApi: TTritonCtx): HandleContext;
       class Operator Explicit(hApi: HandleContext):TTritonCtx;
       class Operator Explicit(rApi: TTritonCtx):HandleContext;

       property Architecture : architecture_e read FArchitecture ;
  end;

  //ComparableFunctor<void(triton::API&, const triton::arch::Register&)>
  TGetRegVal = procedure(API : HandleContext; reg: HandleReg);  cdecl;
  //ComparableFunctor<void(triton::API&, const triton::arch::Register&,     const triton::uint512& value)>
  TSetRegVal = procedure(API : HandleContext; reg: HandleReg; value : uint64);  cdecl;

  //ComparableFunctor<void(triton::API&, const triton::arch::MemoryAccess&)>
  TGetMemVal  = procedure(API : HandleContext; mem: HandleMemAcc);  cdecl;
  //ComparableFunctor<void(triton::API&, const triton::arch::MemoryAccess&, const triton::uint512& value)>
  TSetMemVal  = procedure(API : HandleContext; mem: HandleMemAcc; value : uint64);  cdecl;

  //ComparableFunctor<triton::ast::SharedAbstractNode(triton::API&, const triton::ast::SharedAbstractNode&)>
  TSimplification    = function(API : HandleContext; snode: HandleAbstractNode): HandleAbstractNode ; cdecl;

  APathConstraint = array of PathConstraint;

  TTTritonCtxHelper = record Helper for TTritonCtx
    public
        procedure   Create; overload;
        procedure   Create(arch: architecture_e); overload;
        procedure   Free;
        (* Architecture API ============= *)
        procedure setArchitecture(arch: architecture_e);
        function  getArchitecture:architecture_e;
        function  isArchitectureValid: Boolean;
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
        function  isConcreteMemoryValueDefined(baseAddr: uint64;  size : usize = 1): Boolean;
        procedure clearConcreteMemoryValue(baseAddr: uint64;  size : usize = 1);
        procedure disassembly(var inst: Istruzione); overload;
        procedure disassembly(block: TBasicBlock; addr: UInt64 = 0);overload;

        (* Processing API ============ *)
        function   processing(var inst: Istruzione): Boolean; overload;
        function   processing(var BasicBlock: TBasicBlock; addr : UInt64 = 0): Boolean; overload;
        procedure  initEngines;
        procedure  removeEngines;
        procedure  reset;
        (* IR API ============== *)
        function   buildSemantics(var inst: Istruzione): Boolean; overload;
        function   buildSemantics(var BasicBlock: TBasicBlock): Boolean; overload;
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
        procedure SetMode(mode: mode_e; flag: Boolean);
        function  isModeEnabled(mode: mode_e): Boolean;
        (* Symbolic engine API ========= *)
        function  getSymbolicEngine: HandleSymbolicEngine;
        function  getSymbolicRegisters: TDictionary<register_e,symbolicExp>;
        function  getSymbolicRegister(reg: Registro):symbolicExp ;
        function  getSymbolicMemory: TDictionary<UInt64,symbolicExp>;  overload;
        function  getSymbolicMemory(addr: uint64 ):symbolicExp ; overload;
        function  getSymbolicMemoryValue(address: uint64): uint8; overload;
        function  getSymbolicMemoryValue(mem: MemAccess): uint64; overload;
        function  getSymbolicMemoryAreaValue(baseAddr: uint64;size: usize): TArray<Byte>;
        function  getSymbolicRegisterValue(reg: Registro): uint64;
        function  symbolizeExpression(exprId: usize; symVarSize: uint32; symVarComment: AnsiString = ''): HandleSharedSymbolicVariable;
        function  symbolizeMemory(mem: MemAccess;  symVarComment: AnsiString = ''): SymbolicVar;
        function  symbolizeRegister(reg: Registro;  symVarComment : AnsiString = ''): SymbolicVar;
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
        function  createSymbolicVolatileExpression(inst: Istruzione ; node: HandleAbstractNode; comment: AnsiString = ''):symbolicExp;
        procedure assignSymbolicExpressionToMemory(se:symbolicExp; mem: MemAccess);
        procedure assignSymbolicExpressionToRegister(se:symbolicExp; reg: Registro);
        function  simplify(node: HandleAbstractNode; z3: Boolean = false):HandleAbstractNode; overload;
        function  simplify(BasicBlock: TBasicBlock; padding: Boolean = false):TBasicBlock; overload;
        function  getSymbolicExpressionFromId(symExprId: usize):symbolicExp ;
        function  getSymbolicVariable(symVarId: usize):HandleSharedSymbolicVariable; overload;
        function  getSymbolicVariable(symVarName: AnsiString):HandleSharedSymbolicVariable; overload;
        function  getPathConstraints:APathConstraint ;
        function  getPathPredicate:HandleAbstractNode;
        procedure pushPathConstraint(node: HandleAbstractNode);
        procedure pushPathConstraintPco(pco: HandlePathConstraint);
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
        function  getModel(node: HandleAbstractNode; status : status_e= UNKNOWN; timeout : UInt32 = 0; solvingTime : UInt32= 0): Tuple<TDictionary<UInt32,SolverModel>, status_e, UInt32> ;
        function  getModels(node: HandleAbstractNode;limit : uint32): TList< TDictionary<UInt32,SolverModel> >;
        function  isSat(node: HandleAbstractNode): Boolean ;
        function  getSolver: solver_e ;
        procedure setSolver(kind: solver_e);
        function  isSolverValid:Boolean ;
        function  evaluateAstViaSolver(node: HandleAbstractNode): uint64 ;
        function  simplifyAstViaSolver(node: HandleAbstractNode): HandleAbstractNode ;
        (* Taint engine API==========*)
        function  getTaintEngine:HandleTaintEngine;
        function  getTaintedMemory:TArray<UInt64> ;
        function  getTaintedRegisters:TArray<Registro> ;
        //procedure enableTaintEngine(flag: Boolean);
        //function  isTaintEngineEnabled: Boolean ;
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
        function  taintUnionMemoryImmediate(Dst: MemAccess; imm : Immediate): Boolean ;
        function  taintUnionMemoryMemory(Dst: MemAccess; Src:MemAccess): Boolean ;
        function  taintUnionMemoryRegister(Dst: MemAccess; Src: Registro): Boolean ;
        function  taintUnionRegisterImmediate(Dst: Registro; imm : Immediate): Boolean ;
        function  taintUnionRegisterMemory(Dst: Registro; Src: MemAccess): Boolean ;
        function  taintUnionRegisterRegister(Dst:Registro; Src:Registro): Boolean ;
        function  taintAssignmentMemoryImmediate(Dst:MemAccess; imm : Immediate): Boolean ;
        function  taintAssignmentMemoryMemory(Dst: MemAccess; Src: MemAccess): Boolean ;
        function  taintAssignmentMemoryRegister(Dst: MemAccess; Src: Registro): Boolean ;
        function  taintAssignmentRegisterImmediate(Dst: Registro; imm : Immediate): Boolean ;
        function  taintAssignmentRegisterMemory(Dst: Registro; Src: MemAccess ): Boolean ;
        function  taintAssignmentRegisterRegister(Dst: Registro; Src: Registro): Boolean ;
        (* Synthesizer engine Context ============================================================================= *)
        function synthesize(node: AbstractNode; constant: Boolean = True; subexpr: Boolean=True; opaque: Boolean= False): AbstractNode;
        (* Lifters engine Context ================================================================================= *)
        function liftToLLVM(node: AbstractNode; fname: PAnsiChar= nil; optimize: Boolean= False): string;overload;
        function liftToLLVM(expr: symbolicExp;  fname: PAnsiChar= nil; optimize: Boolean = False): String; overload;
        function liftToPython  (expr: symbolicExp; icomment: Boolean= False): String;
        function liftToSMT     (expr: symbolicExp; assert_: Boolean=False; icomment: Boolean= False): String;
        function NodeliftToDot (node: AbstractNode): string;
        function ExprliftToDot (expr: symbolicExp): string;
        function simplifyAstViaLLVM(node: AbstractNode): AbstractNode;

        {class operator Implicit(rApi: TApi): HandleApi;
                class Operator Explicit(hApi: HandleApi):TApi;
                        class Operator Explicit(rApi: TApi):HandleApi;}
  end;

implementation
           uses System.Classes;
{ Api }

procedure TTTritonCtxHelper.Create;
begin
    FHApi := CreateAPI();
end;

procedure TTTritonCtxHelper.Create(arch: architecture_e);
begin
    FHApi := CreateAPI();

    Triton.Core.setArchitecture(FHApi, arch);
    FArchitecture := arch;
end;

procedure TTTritonCtxHelper.Free;
begin
    DeleteApi(FHApi)
end;

class operator TTritonCtx.Implicit(rApi: TTritonCtx): HandleContext;
begin
    Result := rApi.FHApi;
end;

class operator TTritonCtx.Explicit(rApi: TTritonCtx): HandleContext;
begin
    Result := rApi.FHApi;
end;

class operator TTritonCtx.Explicit(hApi: HandleContext): TTritonCtx;
begin
    ZeroMemory(@Result,SizeOf(TTritonCtx));
    Result.FHApi := hApi;

end;

procedure TTTritonCtxHelper.addCallback(cb: Pointer; cbTipo: callback_e);
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

procedure TTTritonCtxHelper.pushPathConstraint(node: HandleAbstractNode);
begin
    Triton.Core.pushPathConstraint(FHApi, node)
end;

procedure TTTritonCtxHelper.pushPathConstraintPco(pco: HandlePathConstraint);
begin
    Triton.Core.pushPathConstraint(FHApi, pco)
end;

procedure TTTritonCtxHelper.assignSymbolicExpressionToMemory(se: symbolicExp; mem: MemAccess);
begin
    Triton.Core.assignSymbolicExpressionToMemory(FHApi,se,mem)
end;

procedure TTTritonCtxHelper.assignSymbolicExpressionToRegister(se: symbolicExp;  reg: Registro);
begin
    Triton.Core.assignSymbolicExpressionToRegister(FHApi,se,reg)
end;

function TTTritonCtxHelper.buildSemantics(var inst: Istruzione): Boolean;
begin
    Result := Triton.Core.buildSemantics(FHApi,inst) = exception_e.NO_FAULT;
    inst.Reload;
end;

function TTTritonCtxHelper.buildSemantics(var BasicBlock: TBasicBlock): Boolean;
begin
   Result := Triton.Core.buildSemantics_BB(FHApi,BasicBlock) = exception_e.NO_FAULT;
end;

procedure TTTritonCtxHelper.checkModes;
begin
    Triton.Core.checkModes(FHApi)
end;

procedure TTTritonCtxHelper.clearArchitecture;
begin
    Triton.Core.clearArchitecture(FHApi)
end;

procedure TTTritonCtxHelper.clearPathConstraints;
begin
    Triton.Core.clearPathConstraints(FHApi)
end;

procedure TTTritonCtxHelper.concretizeAllMemory;
begin
    Triton.Core.concretizeAllMemory(FHApi)
end;

procedure TTTritonCtxHelper.concretizeAllRegister;
begin
    Triton.Core.concretizeAllRegister(FHApi)
end;

procedure TTTritonCtxHelper.concretizeMemory(addr: uint64);
begin
    Triton.Core.concretizeMemory(FHApi,addr)
end;

procedure TTTritonCtxHelper.concretizeMemory(mem: MemAccess);
begin
    Triton.Core.concretizeMemoryM(FHApi,mem)
end;

procedure TTTritonCtxHelper.concretizeRegister(reg: Registro);
begin
    Triton.Core.concretizeRegister(FHApi,reg)
end;

function TTTritonCtxHelper.symbolizeExpression(exprId: usize; symVarSize: uint32; symVarComment: AnsiString): HandleSharedSymbolicVariable;
begin
    Result := Triton.Core.symbolizeExpression(FHApi,exprId,symVarSize,PAnsiChar( symVarComment))
end;

function TTTritonCtxHelper.symbolizeMemory(mem: MemAccess; symVarComment: AnsiString): SymbolicVar;
begin
    Result := SymbolicVar( Triton.Core.symbolizeMemory(FHApi,mem, PAnsiChar( symVarComment)) )
end;

function TTTritonCtxHelper.symbolizeRegister(reg: Registro; symVarComment: AnsiString ): SymbolicVar;
begin
    Result := SymbolicVar (Triton.Core.symbolizeRegister(FHApi,reg, PAnsiChar(symVarComment)) )
end;

function TTTritonCtxHelper.createSymbolicExpression(inst: Istruzione; node: HandleAbstractNode; dst: OpWrapper; comment: AnsiString): symbolicExp;
begin
    Result := symbolicExp (Triton.Core.createSymbolicExpression(FHApi,inst,node,dst,PAnsiChar (comment)) )
end;

function TTTritonCtxHelper.createSymbolicMemoryExpression(inst: Istruzione; node: HandleAbstractNode; mem: MemAccess; comment: AnsiString): symbolicExp;
begin
    Result := symbolicExp (Triton.Core.createSymbolicMemoryExpression(FHApi,inst,node,mem,PAnsiChar (comment)) )
end;

function TTTritonCtxHelper.createSymbolicRegisterExpression(inst: Istruzione;node: HandleAbstractNode; reg: Registro; comment: AnsiString): symbolicExp;
begin
    Result := symbolicExp (Triton.Core.createSymbolicRegisterExpression(FHApi,inst,node,reg,PAnsiChar( comment)) )
end;

function TTTritonCtxHelper.createSymbolicVolatileExpression(inst: Istruzione; node: HandleAbstractNode; comment: AnsiString): symbolicExp;
begin
    Result := symbolicExp (Triton.Core.createSymbolicVolatileExpression(FHApi,inst,node,PAnsiChar( comment)) )
end;

procedure TTTritonCtxHelper.disassembly(var inst: Istruzione);
begin
    Triton.Core.disassembly(FHApi,inst) ;
    inst.Reload;
end;

procedure TTTritonCtxHelper.disassembly(block: TBasicBlock; addr: UInt64);
begin
    Triton.Core.disassembly_BB(FHApi, block, addr) ;
end;

procedure TTTritonCtxHelper.SetMode(mode: mode_e; flag: Boolean);
begin
    Triton.Core.SetMode(FHApi, mode, flag)
end;

procedure TTTritonCtxHelper.enableSymbolicEngine(flag: Boolean);
begin
    Triton.Core.enableSymbolicEngine(FHApi,flag)
end;

function TTTritonCtxHelper.evaluateAstViaSolver(node: HandleAbstractNode): uint64;
begin
    Result := Triton.Core.evaluateAstViaSolver(FHApi,node)
end;

function TTTritonCtxHelper.getArchitecture: architecture_e;
begin
    Result := Triton.Core.getArchitecture(FHApi)
end;

function TTTritonCtxHelper.getAstContext: AstContext;
begin
    Result := AstContext (Triton.Core.getAstContext(FHApi) )
end;

function TTTritonCtxHelper.getAstRepresentationMode: uint32;
begin
    Result := Triton.Core.getAstRepresentationMode(FHApi)
end;

function TTTritonCtxHelper.getConcreteMemoryAreaValue(baseAddr: uint64; size: usize; execCallbacks: Boolean): TArray<Byte>;
var
  res : PByte;
begin

    res := Triton.Core.getConcreteMemoryAreaValue(FHApi,baseAddr,size,execCallbacks);
    SetLength(Result,size);
    CopyMemory(@Result[0],res,size * SizeOf(uint8));
end;

function TTTritonCtxHelper.getConcreteMemoryValue(mem: MemAccess; execCallbacks: Boolean): uint64;
begin
    Result := Triton.Core.getConcreteMemoryValue(FHApi,mem,execCallbacks)
end;

function TTTritonCtxHelper.getConcreteMemoryValue(addr: uint64; execCallbacks: Boolean): uint8;
begin
    Result := Triton.Core.getConcreteMemoryValueByte(FHApi, addr,execCallbacks)
end;

function TTTritonCtxHelper.getConcreteRegisterValue(reg: Registro;  execCallbacks: Boolean): uint64;
begin
    Result := Triton.Core.getConcreteRegisterValue(FHApi,reg, execCallbacks)
end;

function TTTritonCtxHelper.getConcreteVariableValue(symVar: SymbolicVar): uint64;
begin
    Result := Triton.Core.getConcreteVariableValue(FHApi,HandleSharedSymbolicVariable( symVar) )
end;

function TTTritonCtxHelper.getCpuInstance: HandleCpuInterface;
begin
    Result := Triton.Core.getCpuInstance(FHApi)
end;

function TTTritonCtxHelper.getEndianness: endianness_e;
begin
    Result := Triton.Core.getEndianness(FHApi)
end;

function TTTritonCtxHelper.getGprBitSize: uint32;
begin
    Result := Triton.Core.getGprBitSize(FHApi)
end;

function TTTritonCtxHelper.getGprSize: uint32;
begin
   Result := Triton.Core.getGprSize (FHApi)
end;

function TTTritonCtxHelper.getImmediateAst(imm: Immediate): HandleAbstractNode;
begin
    Result := Triton.Core.getImmediateAst(FHApi, imm)
end;

function TTTritonCtxHelper.getImmediateAst(inst: Istruzione; imm: Immediate): HandleAbstractNode;
begin
    Result := Triton.Core.getImmediateAstIstruz(FHApi,inst,imm)
end;

function TTTritonCtxHelper.getMemoryAst(mem: MemAccess): HandleAbstractNode;
begin
    Result := Triton.Core.getMemoryAst(FHApi,mem)
end;

function TTTritonCtxHelper.getMemoryAst(inst: Istruzione; mem: MemAccess): HandleAbstractNode;
begin
    Result := Triton.Core.getMemoryAstIstruz(FHApi,inst,mem)
end;

function TTTritonCtxHelper.getModel(node: HandleAbstractNode; status : status_e= UNKNOWN; timeout : UInt32 = 0; solvingTime : UInt32= 0): Tuple<TDictionary<UInt32,SolverModel>, status_e, UInt32> ;
var
  n,i    : Integer;
  pOut   : PAddrSolver ;
  Dict   : TDictionary<UInt32,SolverModel>;
begin
    pOut   := nil;

    var pStatus : pstatus_e := nil;
    if status <> UNKNOWN then
       pStatus :=  @status;

    var psolvingTime: PUInt32 := nil;
    if solvingTime <> 0 then
       psolvingTime := @solvingTime;

    n :=  Triton.Core.getModel(FHApi,node,@pOut, pStatus, timeout, psolvingTime);

    //if n > 0 then // Added by Max remove possibility of exception error when accessing count model 02/10/2019 18:48:56
    Dict := TDictionary<UInt32,SolverModel>.Create;

    for i := 0 to n - 1 do
        Dict.Add(pOut[i].id, SolverModel (pOut[i].Model) );

    Result := Tuple.Create(dict,status,solvingTime);
end;

function TTTritonCtxHelper.getModels(node: HandleAbstractNode; limit: uint32): TList< TDictionary<UInt32,SolverModel> >;
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

function TTTritonCtxHelper.getNumberOfRegisters: uint32;
begin
    Result := Triton.Core.getNumberOfRegisters(FHApi)
end;

function TTTritonCtxHelper.getOperandAst(op: OpWrapper): HandleAbstractNode;
begin
    Result :=  Triton.Core.getOperandAst(FHApi,op)
end;

function TTTritonCtxHelper.getOperandAst(inst: Istruzione; op: OpWrapper): HandleAbstractNode;
begin
    Result := Triton.Core.getOperandAstIstruz(FHApi, inst, op)
end;

function TTTritonCtxHelper.getParentRegister(id: register_e): Registro;
begin
    Result := Registro (Triton.Core.getParentRegister(FHApi,id) )
end;

function TTTritonCtxHelper.getParentRegister(reg: Registro): Registro;
begin
    Result := Registro (Triton.Core.getParentRegisterR(FHApi,reg) )
end;

function TTTritonCtxHelper.getPathConstraints: APathConstraint;
var
  n,i : Integer;
  p   : PHandlePathConstraint;
begin
    p := nil;
    n := Triton.Core.getPathConstraints(FHApi, @p );

    for i := 0 to n -1 do
      Result := Result + [ pathConstraint (p[i]) ]

end;

function TTTritonCtxHelper.getPathPredicate: HandleAbstractNode;
begin
    Result := Triton.Core.getPathPredicate(FHApi)
end;

function TTTritonCtxHelper.getRegister(id: register_e): Registro;
begin
    Result := Registro (Triton.Core.getRegister(FHApi, id) )
end;

function TTTritonCtxHelper.getRegisterAst(reg: Registro): HandleAbstractNode;
begin
    Result := Triton.Core.getRegisterAst(FHApi, reg)
end;

function TTTritonCtxHelper.getRegisterAst(inst: Istruzione; reg: Registro): HandleAbstractNode;
begin
    Result := Triton.Core.getRegisterAstIstruz(FHApi, inst,reg)
end;

function TTTritonCtxHelper.getSolver: solver_e;
begin
    Result := Triton.Core.getSolver(FHApi)
end;

function TTTritonCtxHelper.getSymbolicEngine: HandleSymbolicEngine;
begin
    Result := Triton.Core.getSymbolicEngine(FHApi)
end;

function TTTritonCtxHelper.getSymbolicExpressionFromId(symExprId: usize): symbolicExp;
begin
    Result := symbolicExp (Triton.Core.getSymbolicExpressionFromId(FHApi, symExprId) )
end;

function TTTritonCtxHelper.getSymbolicExpressions: TDictionary<usize,symbolicExp>;
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

function TTTritonCtxHelper.getSymbolicMemory: TDictionary<UInt64,symbolicExp>;
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

function TTTritonCtxHelper.getSymbolicMemory(addr: uint64): symbolicExp;
begin
    Result := symbolicExp (Triton.Core.getSymbolicMemoryAddr(FHApi, addr) )
end;

function TTTritonCtxHelper.getSymbolicMemoryAreaValue(baseAddr: uint64; size: usize): TArray<Byte>;
var
 res : PByte;
begin

    res := Triton.Core.getSymbolicMemoryAreaValue(FHApi,baseAddr,size)  ;

    SetLength(Result,size);
    CopyMemory(@Result[0],res,size * SizeOf(uint8));
end;

function TTTritonCtxHelper.getSymbolicMemoryValue(address: uint64): uint8;
begin
    Result := Triton.Core.getSymbolicMemoryValue(FHApi, address)
end;

function TTTritonCtxHelper.getSymbolicMemoryValue(mem: MemAccess): uint64;
begin
    Result := Triton.Core.getSymbolicMemoryValueM(FHApi, mem)
end;

function TTTritonCtxHelper.getAllRegisters: TDictionary<register_e, Registro>;
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

function TTTritonCtxHelper.getSymbolicRegister(reg: Registro): symbolicExp;
begin
    Result := symbolicExp ( Triton.Core.getSymbolicRegister(FHApi, reg) )
end;

function TTTritonCtxHelper.getSymbolicRegisters: TDictionary<register_e,symbolicExp>;
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

function TTTritonCtxHelper.getSymbolicRegisterValue(reg: Registro): uint64;
begin
   Result := Triton.Core.getSymbolicRegisterValue(FHApi,reg)
end;

function TTTritonCtxHelper.getSymbolicVariable(symVarId: usize): HandleSharedSymbolicVariable;
begin
    Result := Triton.Core.getSymbolicVariableFromId(FHApi,symVarId)
end;

function TTTritonCtxHelper.getSymbolicVariable(symVarName: AnsiString): HandleSharedSymbolicVariable;
begin
    Result := Triton.Core.getSymbolicVariableFromName(FHApi, PAnsiChar(symVarName))
end;

function TTTritonCtxHelper.getSymbolicVariables: TDictionary<usize,SymbolicVar>;
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

function TTTritonCtxHelper.getTaintedMemory: TArray<UInt64> ;
var
  n,i : Integer;
  uAddrs : PUInt64;
begin
   uAddrs := nil;
   n := Triton.Core.getTaintedMemory(FHApi,uAddrs);

   for i := 0 to n - 1 do
        Result := Result + [ uAddrs[i] ];
end;

function TTTritonCtxHelper.getTaintedRegisters: TArray<Registro>;
var
  n,i     : Integer;
  rRegs   : PReg;
begin
    rRegs := nil;
    n := Triton.Core.getTaintedRegisters(FHApi,rRegs);

    for i := 0 to n - 1 do
        Result := Result + [ Registro (rRegs[i]) ];
end;

function TTTritonCtxHelper.getParentRegisters:TArray<Registro>;
var
  n,i     : Integer;
  rRegs   : PReg;
begin
    rRegs := nil;
    n := Triton.Core.gettParentRegisters(FHApi,rRegs);

    for i := 0 to n - 1 do
        Result := Result + [ Registro (rRegs[i]) ];

end;

function TTTritonCtxHelper.getTaintedSymbolicExpressions: TList<symbolicExp>;
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

function TTTritonCtxHelper.getTaintEngine: HandleTaintEngine;
begin
    Result := Triton.Core.getTaintEngine(FHApi)
end;

procedure TTTritonCtxHelper.initEngines;
begin
    Triton.Core.initEngines(FHApi)
end;

procedure TTTritonCtxHelper.initLeaAst(mem: MemAccess; force: Boolean);
begin
    Triton.Core.initLeaAst(FHApi,mem,force);
end;

function TTTritonCtxHelper.isArchitectureValid: Boolean;
begin
    Result := Triton.Core.isArchitectureValid(FHApi)
end;

function TTTritonCtxHelper.isFlag(regId: register_e): Boolean;
begin
    Result := Triton.Core.isFlag(FHApi, regId)
end;

function TTTritonCtxHelper.isFlag(reg: Registro): Boolean;
begin
    Result := Triton.Core.isFlagR(FHApi,reg)
end;

function TTTritonCtxHelper.isConcreteMemoryValueDefined(baseAddr: uint64; size: usize): Boolean;
begin
    Result := Triton.Core.isConcreteMemoryValueDefined(FHApi, baseAddr,size)
end;

function TTTritonCtxHelper.isMemorySymbolized(mem: MemAccess): Boolean;
begin
    Result := Triton.Core.isMemorySymbolized(FHApi,mem)
end;

function TTTritonCtxHelper.isMemorySymbolized(addr: uint64; size: uint32): Boolean;
begin
    Result := Triton.Core.isMemorySymbolizedSize(FHApi, addr,size)
end;

function TTTritonCtxHelper.isMemoryTainted(addr: uint64; size: uint32): Boolean;
begin
    Result := Triton.Core.isMemoryTainted(FHApi, addr, size)
end;

function TTTritonCtxHelper.isMemoryTainted(mem: MemAccess): Boolean;
begin
    Result := Triton.Core.isMemoryTaintedMem(FHApi,mem)
end;

function TTTritonCtxHelper.isModeEnabled(mode: mode_e): Boolean;
begin
    Result := Triton.Core.isModeEnabled(FHApi,mode)
end;

function TTTritonCtxHelper.isRegister(regId: register_e): Boolean;
begin
    Result := Triton.Core.isRegister(FHApi, regId)
end;

function TTTritonCtxHelper.isRegister(reg: Registro): Boolean;
begin
    Result := Triton.Core.isRegisterR(FHApi, reg)
end;

function TTTritonCtxHelper.isRegisterSymbolized(reg: Registro): Boolean;
begin
    Result := Triton.Core.isRegisterSymbolized(FHApi, reg)
end;

function TTTritonCtxHelper.isRegisterTainted(reg: Registro): Boolean;
begin
    Result := Triton.Core.isRegisterTainted(FHApi, reg)
end;

function TTTritonCtxHelper.isRegisterValid(regId: register_e): Boolean;
begin
    Result := Triton.Core.isRegisterValid(FHApi,regId)
end;

function TTTritonCtxHelper.isRegisterValid(reg: Registro): Boolean;
begin
    Result := Triton.Core.isRegisterValidR(FHApi,reg)
end;

function TTTritonCtxHelper.isSat(node: HandleAbstractNode): Boolean;
begin
    Result := Triton.Core.isSat(FHApi,node)
end;

function TTTritonCtxHelper.isSolverValid: Boolean;
begin
    Result := Triton.Core.isSolverValid(FHApi)
end;

function TTTritonCtxHelper.isSymbolicEngineEnabled: Boolean;
begin
    Result := Triton.Core.isSymbolicEngineEnabled(FHApi)
end;

function TTTritonCtxHelper.isSymbolicExpressionIdExists(symExprId: usize): Boolean;
begin
    Result := Triton.Core.isSymbolicExpressionIdExists(FHApi,symExprId)
end;

function TTTritonCtxHelper.isTainted(op: OpWrapper): Boolean;
begin
    Result := Triton.Core.isTainted(FHApi, op)
end;

(*
procedure TTTritonCtxHelper.enableTaintEngine(flag: Boolean);
begin
    Triton.Core.enableTaintEngine(FHApi,flag)
end;

function TTTritonCtxHelper.isTaintEngineEnabled: Boolean;
begin
   Result := Triton.Core.isTaintEngineEnabled(FHApi)
end;
*)

function TTTritonCtxHelper.newSymbolicExpression(node: HandleAbstractNode;comment: AnsiString): symbolicExp;
begin
    Result := symbolicExp (Triton.Core.newSymbolicExpression(FHApi, node,PAnsiChar(comment)) )
end;

function TTTritonCtxHelper.newSymbolicVariable(varSize: uint32;comment: AnsiString): SymbolicVar;
begin
    Result := SymbolicVar (Triton.Core.newSymbolicVariable(FHApi, varSize,PAnsiChar(comment)) )
end;

function TTTritonCtxHelper.processCallbacks(kind: callback_e; node: HandleAbstractNode): HandleAbstractNode;
begin
    Result := Triton.Core.processCallbacks(FHApi, kind,node)
end;

procedure TTTritonCtxHelper.processCallbacks(kind: callback_e; mem: MemAccess);
begin
    Triton.Core.processCallbacksMem(FHApi,kind,mem)
end;

procedure TTTritonCtxHelper.processCallbacks(kind: callback_e; reg: Registro);
begin
    Triton.Core.processCallbacksReg(FHApi,kind,reg)
end;

function TTTritonCtxHelper.processing(var inst: Istruzione): Boolean;
begin
    var Res := Triton.Core.processing(FHApi, inst);
    Result := Res = NO_FAULT;

    inst.Reload;
end;

function TTTritonCtxHelper.processing(var BasicBlock: TBasicBlock; addr : UInt64): Boolean;
begin
     var Res := Triton.Core.processing_BB(FHApi, BasicBlock, addr);
     Result  := Res = NO_FAULT;
end;

function TTTritonCtxHelper.simplify(node: HandleAbstractNode; z3: Boolean): HandleAbstractNode;
begin
    Result := Triton.Core.simplify(FHApi,node,z3)
end;

function TTTritonCtxHelper.simplify(BasicBlock: TBasicBlock; padding: Boolean):TBasicBlock;
begin
    Result := TBasicBlock(Triton.Core.simplify_BB(FHApi,BasicBlock,padding))
end;

function TTTritonCtxHelper.simplifyAstViaSolver(node: HandleAbstractNode): HandleAbstractNode;
begin
    Result := Triton.Core.simplifyAstViaSolver(FHApi, node)
end;

procedure TTTritonCtxHelper.removeAllCallbacks;
begin
    Triton.Core.removeAllCallbacks(FHApi)
end;

procedure TTTritonCtxHelper.removeCallback(cb: Pointer; cbTipo: callback_e);
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

procedure TTTritonCtxHelper.removeEngines;
begin
   Triton.Core.removeEngines(FHApi)
end;

procedure TTTritonCtxHelper.removeSymbolicExpression(symExprId: usize);
begin
    Triton.Core.removeSymbolicExpression(FHApi, symExprId)
end;

procedure TTTritonCtxHelper.reset;
begin
    Triton.Core.reset(FHApi)
end;

procedure TTTritonCtxHelper.setArchitecture(arch: architecture_e);
begin
   Triton.Core.setArchitecture(FHApi, arch);
   FArchitecture := arch;
end;

procedure TTTritonCtxHelper.setAstRepresentationMode(mode: uint32);
begin
   Triton.Core.setAstRepresentationMode(FHApi, mode)
end;

procedure TTTritonCtxHelper.setConcreteMemoryAreaValue(baseAddr: uint64; area: array of Byte; size: usize);
begin
    Triton.Core.setConcreteMemoryAreaValue(FHApi,baseAddr,@area[0], size)
end;

procedure TTTritonCtxHelper.setConcreteMemoryAreaValue(baseAddr: uint64; values: array of Byte);
begin
    setConcreteMemoryAreaValue(baseAddr,values, Length(values))
end;

procedure TTTritonCtxHelper.setConcreteMemoryValue(mem: MemAccess; value: uint64);
begin
    Triton.Core.setConcreteMemoryValue(FHApi, mem,value)
end;

procedure TTTritonCtxHelper.setConcreteMemoryValue(addr: uint64; value: uint8);
begin
    Triton.Core.setConcreteMemoryValueByte(FHApi, addr ,value)
end;

procedure TTTritonCtxHelper.setConcreteRegisterValue(reg: Registro; value: uint64);
begin
   Triton.Core.setConcreteRegisterValue(FHApi, reg,value)
end;

procedure TTTritonCtxHelper.setConcreteVariableValue(symVar: SymbolicVar; value: UInt64);
begin
    Triton.Core.setConcreteVariableValue(FHApi, HandleSharedSymbolicVariable(symVar),value)
end;

procedure TTTritonCtxHelper.setSolver(kind: solver_e);
begin
    Triton.Core.setSolver(FHApi,kind)
end;

function TTTritonCtxHelper.setTaint(op: OpWrapper; flag: Boolean): Boolean;
begin
    Result := Triton.Core.setTaint(FHApi, op, flag)
end;

function TTTritonCtxHelper.setTaintMemory(mem: MemAccess; flag: Boolean): Boolean;
begin
    Result := Triton.Core.setTaintMemory(FHApi, mem, flag)
end;

function TTTritonCtxHelper.setTaintRegister(reg: Registro; flag: Boolean): Boolean;
begin
    Result := Triton.Core.setTaintRegister(FHApi, reg, flag)
end;

function TTTritonCtxHelper.sliceExpressions(expr: symbolicExp): TDictionary<usize,symbolicExp>;
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

function TTTritonCtxHelper.taintAssignment(op1, op2: OpWrapper): Boolean;
begin
    Result := Triton.Core.taintAssignment(FHApi, op1,op2 )
end;

function TTTritonCtxHelper.taintAssignmentMemoryImmediate(Dst: MemAccess; imm : Immediate): Boolean;
begin
    Result := Triton.Core.taintAssignmentMemoryImmediate(FHApi, Dst,imm)
end;

function TTTritonCtxHelper.taintAssignmentMemoryMemory(Dst, Src: MemAccess): Boolean;
begin
   Result := Triton.Core.taintAssignmentMemoryMemory(FHApi, Dst,Src)
end;

function TTTritonCtxHelper.taintAssignmentMemoryRegister(Dst: MemAccess; Src: Registro): Boolean;
begin
    Result := Triton.Core.taintAssignmentMemoryRegister(FHApi,Dst,Src)
end;

function TTTritonCtxHelper.taintAssignmentRegisterImmediate(Dst: Registro; imm : Immediate): Boolean;
begin
    Result := Triton.Core.taintAssignmentRegisterImmediate(FHApi, Dst,imm)
end;

function TTTritonCtxHelper.taintAssignmentRegisterMemory(Dst: Registro; Src: MemAccess): Boolean;
begin
    Result := Triton.Core.taintAssignmentRegisterMemory(FHApi,Dst,Src)
end;

function TTTritonCtxHelper.taintAssignmentRegisterRegister(Dst, Src: Registro): Boolean;
begin
    Result := Triton.Core.taintAssignmentRegisterRegister(FHApi, Dst,Src)
end;

function TTTritonCtxHelper.taintMemory(addr: uint64): Boolean;
begin
   Result := Triton.Core.taintMemory(FHApi, addr)
end;

function TTTritonCtxHelper.taintMemory(mem: MemAccess): Boolean;
begin
    Result := Triton.Core.taintMemoryMem(FHApi, mem)
end;

function TTTritonCtxHelper.taintRegister(reg: Registro): Boolean;
begin
   Result := Triton.Core.taintRegister(FHApi, reg)
end;

function TTTritonCtxHelper.taintUnion(op1, op2: OpWrapper): Boolean;
begin
    Result := Triton.Core.taintUnion(FHApi, op1,op2)
end;

function TTTritonCtxHelper.taintUnionMemoryImmediate(Dst: MemAccess; imm : Immediate): Boolean;
begin
    Result := Triton.Core.taintUnionMemoryImmediate(FHApi, Dst,imm)
end;

function TTTritonCtxHelper.taintUnionMemoryMemory(Dst, Src: MemAccess): Boolean;
begin
    Result := Triton.Core.taintUnionMemoryMemory(FHApi, Dst,Src)
end;

function TTTritonCtxHelper.taintUnionMemoryRegister(Dst: MemAccess; Src: Registro): Boolean;
begin
    Result := Triton.Core.taintUnionMemoryRegister(FHApi,Dst,Src)
end;

function TTTritonCtxHelper.taintUnionRegisterImmediate(Dst: Registro; imm : Immediate): Boolean;
begin
   Result := Triton.Core.taintUnionRegisterImmediate(FHApi, Dst,imm)
end;

function TTTritonCtxHelper.taintUnionRegisterMemory(Dst: Registro; Src: MemAccess): Boolean;
begin
   Result := Triton.Core.taintUnionRegisterMemory(FHApi, Dst,Src)
end;

function TTTritonCtxHelper.taintUnionRegisterRegister(Dst, Src: Registro): Boolean;
begin
   Result := Triton.Core.taintUnionRegisterRegister(FHApi,Dst,Src)
end;

procedure TTTritonCtxHelper.clearConcreteMemoryValue(baseAddr: uint64; size: usize);
begin
   Triton.Core.clearConcreteMemoryValue(FHApi, baseAddr,size)
end;

function TTTritonCtxHelper.untaintMemory(addr: uint64): Boolean;
begin
    Result := Triton.Core.untaintMemory(FHApi,addr)
end;

function TTTritonCtxHelper.untaintMemory(mem: MemAccess): Boolean;
begin
    Result := Triton.Core.untaintMemoryMem(FHApi, mem)
end;

function TTTritonCtxHelper.untaintRegister(reg: Registro): Boolean;
begin
    Result := Triton.Core.untaintRegister(FHApi,reg)
end;

function TTTritonCtxHelper.synthesize(node: AbstractNode; constant, subexpr, opaque: Boolean): AbstractNode;
begin
    var Res : SynthesisResult ;
    Res.Create(  SynthesisResult(Triton.Core.synthesize(FHApi, node, constant, subexpr, opaque)) );

    if res.successful then
      Result := res.getOutput;
end;

function TTTritonCtxHelper.liftToLLVM(node: AbstractNode; fname: PAnsiChar; optimize: Boolean): string;
begin
    if fname = nil then
      fname := '__Triton';

    var pStr := Triton.Core.NodeliftToLLVM(FHApi, node, fname, optimize);
    Result := string(AnsiString(pStr));

    var RPR := Result.Split([AnsiString(#10)]);
    Result := '';
    for var i := 0 to Length(RPR) - 1 do
      Result := Result + RPR[i] + sLineBreak;
end;

function TTTritonCtxHelper.liftToLLVM(expr: symbolicExp; fname: PAnsiChar; optimize: Boolean): String;
begin
    if fname = nil then
      fname := '__Triton';

    var pStr := Triton.Core.ExprliftToLLVM(FHApi, expr, fname, optimize);
    Result := string(AnsiString(pStr)) ;

    var RPR := Result.Split([AnsiString(#10)]);
    Result := '';
    for var i := 0 to Length(RPR) - 1 do
      Result := Result + RPR[i] + sLineBreak;
end;

function TTTritonCtxHelper.liftToPython(expr: symbolicExp; icomment: Boolean): String;
begin
    var pStr := Triton.Core.liftToPython(FHApi, expr, icomment);
    Result := string(AnsiString(pStr))
end;

function TTTritonCtxHelper.liftToSMT(expr: symbolicExp; assert_, icomment: Boolean): String;
begin
    var pStr := Triton.Core.liftToSMT(FHApi, expr, assert_, icomment);
    Result := string(AnsiString(pStr))
end;

function TTTritonCtxHelper.NodeliftToDot(node: AbstractNode): string;
begin
    var pStr := Triton.Core.NodeliftToDot(FHApi, node);
    Result := string(AnsiString(pStr))
end;

function TTTritonCtxHelper.ExprliftToDot(expr: symbolicExp): string;
begin
    var pStr := Triton.Core.ExprliftToDot(FHApi, expr);
    Result := string(AnsiString(pStr))
end;

function TTTritonCtxHelper.simplifyAstViaLLVM(node: AbstractNode): AbstractNode;
begin
    Result := Triton.Core.simplifyAstViaLLVM(FHApi, node)
end;

end.
