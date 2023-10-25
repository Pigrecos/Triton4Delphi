unit Triton.Instruction;

{$Z4}
{$POINTERMATH ON}

interface
    uses
       System.SysUtils, Winapi.Windows, System.Generics.Collections,
       Triton.Define,
       Triton.BitVector,
       Triton.Register,
       Triton.Immediate,
       Triton.MemoryAccess,
       Triton.OperandWrapper,
       Triton.SymbolicExpression,
       Triton.AstNode ;


 const
  MAXOPSIZE = 32;

type

 TOpcode = array of Byte;

 PMemNode = ^MemNode;
 MemNode = record
  	mem : HandleMemAcc;
	  node: HandleAbstractNode;
 end;

 PRegNode = ^RegNode;
 RegNode = record
	  reg : HandleReg;
	  node: HandleAbstractNode;
 end;

 PImmNode = ^ImmNode;
 ImmNode = record
  	imm : HandleImmediate;
	  node: HandleAbstractNode;
 end;

 PSymbolicEx = ^HandleSharedSymbolicExpression;
 POperand    = ^HandleOperandWrapper;

 _Istruz = record
    tid           : UInt32;
    address       : UInt64;
    nextAddress   : UInt64;
    disassembly   : PAnsiChar;
    opcode        : pByte;
    size          : UInt32;
    tipo          : UInt32;
    prefix        : prefix_e;
    branch        : Boolean;
    controlFlow   : Boolean;
    conditionTaken: Boolean;
    tainted       : Boolean;
 end;

 Istruzione = record
   private
     FHIstruz        : HandleInstruz;
     FInternalIstruz : _Istruz;
     procedure GetInternalData;
     function getLoadAccess:TDictionary<MemAccess,AbstractNode>;
     function getStoreAccess:TDictionary<MemAccess,AbstractNode>;
     function getReadRegisters:TDictionary<Registro,AbstractNode>;
     function getWrittenRegisters:TDictionary<Registro,AbstractNode>;
     function getReadImmediates:TDictionary<Immediate,AbstractNode>;
     function getUndefinedRegisters:TList<Registro>;
     function getsymbolicExpressions:TList<symbolicExp>;
     function getOperands: TArray<OpWrapper>;
   public
     tid           : UInt32;       //! The thread id of the instruction.
     address       : UInt64;       //! The address of the instruction.
     nextAddress   : UInt64;       //! The next address of the instruction.
     disassembly   : AnsiString;   //! The disassembly of the instruction. This field is set at the disassembly level.
     opcode        : TOpcode;      //! The opcode of the instruction.
     size          : UInt32;       //! The size of the instruction.
     tipo          : UInt32;       //! The type of the instruction. This field is set at the disassembly level.
     prefix        : prefix_e;     //! The prefix of the instruction. This field is set at the disassembly level. Mainly used for X86.
     codeCondition : condition_e;  //! The code condition of the instruction. This field is set at the disassembly level. Mainly used for AArch64.
     branch        : Boolean;      //! True if this instruction is a branch. This field is set at the disassembly level.
     controlFlow   : Boolean;      //! True if this instruction changes the control flow. This field is set at the disassembly level.
     conditionTaken: Boolean;      //! True if the condition is taken (i.g x86: jcc, cmocc, setcc, ...). This field is set at the semantics level.
     tainted       : Boolean;      //! True if this instruction is tainted. This field is set at the semantics level.
     Symbolized    : Boolean;      //! True if at least one of its expressions contains a symbolic variable.
     MemoryRead    : Boolean;      //! True if the instruction contains an expression which reads the memory.
     MemoryWrite   : Boolean;      //! True if the instruction contains an expression which writes into the memory.
     Prefixed      : Boolean;      //! True if the instruction has a prefix (mainly for X86).

     procedure Create; overload;
     procedure Create(addr: UInt64; opcode: TOpcode); overload;
     procedure Create(opcode: TOpcode;opSize: Uint32); overload;
     procedure Create(opcode: array of Byte); overload;
     function  CreateFrom(opcode: array of Byte): Istruzione;
     procedure Create(other: Istruzione); overload;
     procedure Free;
     procedure Reload;
     procedure Copy(other: Istruzione);
     procedure setThreadId(tid: UInt32);
     procedure setAddress(addr: UInt64);
     procedure setOpcode(aBuff: TOpcode;size: UInt32); overload;
     procedure setOpcode(aBuff: TOpcode); overload;
     procedure setOpcode(aBuff: array of Byte);overload;
     procedure setLoadAccess(mem : MemAccess; node: AbstractNode);
     procedure removeLoadAccess(mem:MemAccess);
     procedure setStoreAccess(mem : MemAccess; node: AbstractNode);
     procedure removeStoreAccess(mem:MemAccess);
     procedure setReadRegister(reg: Registro; node: AbstractNode);
     procedure removeReadRegister(reg: Registro);
     procedure setWrittenRegister(reg: Registro; node: AbstractNode);
     procedure removeWrittenRegister(reg: Registro);
     procedure setReadImmediate(imm: Immediate; node: AbstractNode);
     procedure removeReadImmediate(imm: Immediate);
     procedure setUndefinedRegister(reg: Registro);
     procedure removeUndefinedRegister(reg: Registro);
     procedure setSize(size: UInt32);
     procedure setType(tipo: UInt32);
     function  getType: instruction_e;
     procedure setPrefix(prefix: prefix_e);
     procedure setCodeCondition(codeCondition: condition_e);
     procedure setDisassembly(str: PAnsiChar);
     procedure setTaint(state: Boolean); overload;
     procedure setTaint; overload;
     function  isWriteTo(target: OpWrapper): Boolean ;
     function  isReadFrom(target: OpWrapper): Boolean;
     procedure setBranch(flag: Boolean);
     procedure setControlFlow(flag: Boolean);
     procedure setConditionTaken(flag: Boolean);
     function  isSymbolized: Boolean;

     procedure clear;
     function  ToStr: string;

     class Operator Explicit(hIstr: HandleInstruz):Istruzione;
     class Operator Explicit(rIstr: Istruzione):HandleInstruz;
     class Operator Implicit(rIstr: Istruzione):HandleInstruz;

     property Operands            : TArray<OpWrapper>                   read getOperands;
     property loadAccess          : TDictionary<MemAccess,AbstractNode> read getLoadAccess;
     property StoreAccess         : TDictionary<MemAccess,AbstractNode> read getStoreAccess;
     property readRegisters       : TDictionary<Registro,AbstractNode>  read getReadRegisters;
     property writtenRegisters    : TDictionary<Registro,AbstractNode>  read getWrittenRegisters;
     property readImmediates      : TDictionary<Immediate,AbstractNode> read getReadImmediates;
     property undefinedRegisters  : TList<Registro>                     read getUndefinedRegisters;
     property symbolicExpressions : TList<symbolicExp>                  read getsymbolicExpressions;
 end;


        function RefIstruzToIstruz(HOpW: HandleInstruz): _Istruz; cdecl;  external Triton_dll Name 'RefIstruzToIstruz';



    (*  Istruzione  =========================================================================== *)


        //! Constructor.

        function IInstruction: HandleInstruz; cdecl;  external Triton_dll Name 'IInstruction';
        //! Constructor with opcode.
        function IInstructionOP(opcode: pbyte;opSize: Uint32): HandleInstruz; cdecl;  external Triton_dll Name 'IInstructionOP';
        //! Constructor by copy.
        function IInstructionFrom(other: HandleInstruz): HandleInstruz; cdecl;  external Triton_dll Name 'IInstructionFrom';

        //! Destructor.

	      procedure  IDelete(Handle: HandleInstruz); cdecl;  external Triton_dll Name 'IDelete';

        //! Clears all instruction information.

        procedure Iclear(hIstr: HandleInstruz); cdecl;  external Triton_dll Name 'Iclear';
        //! Returns whether the instruction reads the specified operand.
        function IisReadFrom(hIstr: HandleInstruz; target: HandleOperandWrapper):Boolean ;cdecl;  external Triton_dll Name 'IisReadFrom';
        //! Returns whether the instruction writes the specified operand.
        function IisWriteTo(hIstr: HandleInstruz; target: HandleOperandWrapper):Boolean ; cdecl;  external Triton_dll Name 'IisWriteTo';
        //! Sets a load access.
        procedure IsetLoadAccess(hIstr: HandleInstruz; mem: HandleMemAcc; node: HandleAbstractNode);cdecl;  external Triton_dll Name 'IsetLoadAccess';
        //! Removes a load access.
        procedure IremoveLoadAccess(hIstr: HandleInstruz; mem: HandleMemAcc);cdecl;  external Triton_dll Name 'IremoveLoadAccess';
        //! Sets a store access.
        procedure IsetStoreAccess(hIstr: HandleInstruz; mem: HandleMemAcc; node: HandleAbstractNode);cdecl;  external Triton_dll Name 'IsetStoreAccess';
        //! Removes a store access.
        procedure IremoveStoreAccess(hIstr: HandleInstruz; mem: HandleMemAcc); cdecl;  external Triton_dll Name 'IremoveStoreAccess';
        //! Sets a read register.
        procedure IsetReadRegister(hIstr: HandleInstruz; reg: HandleReg; node: HandleAbstractNode);cdecl;  external Triton_dll Name 'IsetReadRegister';
        //! Removes a read register.
        procedure IremoveReadRegister(hIstr: HandleInstruz; reg: HandleReg); cdecl;  external Triton_dll Name 'IremoveReadRegister';
        //! Sets a written register.
        procedure IsetWrittenRegister(hIstr: HandleInstruz; reg: HandleReg; node: HandleAbstractNode);cdecl;  external Triton_dll Name 'IsetWrittenRegister';
        //! Removes a written register.
        procedure IremoveWrittenRegister(hIstr: HandleInstruz; reg: HandleReg);cdecl;  external Triton_dll Name 'IremoveWrittenRegister';
        //! Sets a read immediate.
        procedure IsetReadImmediate(hIstr: HandleInstruz; imm: HandleImmediate; node: HandleAbstractNode); cdecl;  external Triton_dll Name 'IsetReadImmediate';
        //! Removes a read immediate.
        procedure IremoveReadImmediate(hIstr: HandleInstruz; imm: HandleImmediate);cdecl;  external Triton_dll Name 'IremoveReadImmediate';
        //! Sets an undefined register.
        procedure IsetUndefinedRegister(hIstr: HandleInstruz; reg: HandleReg);cdecl;  external Triton_dll Name 'IsetUndefinedRegister';
        //! Removes an undefined register.
        procedure IremoveUndefinedRegister(hIstr: HandleInstruz; reg: HandleReg);cdecl;  external Triton_dll Name 'IremoveUndefinedRegister';
        //! Sets the address of the instruction.
        procedure IsetAddress(hIstr: HandleInstruz;addr: uint64);cdecl;  external Triton_dll Name 'IsetAddress';
        //! Sets flag to define this instruction as branch or not.
        procedure IsetBranch(hIstr: HandleInstruz; flag: Boolean);cdecl;  external Triton_dll Name 'IsetBranch';
        //! Sets the code condition of the instruction (mainly for AArch64).
        procedure IsetCodeCondition(hIstr: HandleInstruz; codeCondition: condition_e); cdecl;  external Triton_dll Name 'IsetCodeCondition';
        //! Sets flag to define if the condition is taken or not.
        procedure IsetConditionTaken(hIstr: HandleInstruz; flag: Boolean); cdecl;  external Triton_dll Name 'IsetConditionTaken';
        //! Sets flag to define this instruction changes the control flow or not.
        procedure IsetControlFlow(hIstr: HandleInstruz; flag: Boolean);cdecl;  external Triton_dll Name 'IsetControlFlow';
        //! Sets the disassembly of the instruction.
        procedure IsetDisassembly(hIstr: HandleInstruz; str: PAnsiChar); cdecl;  external Triton_dll Name 'IsetDisassembly';
        //! Sets the opcode of the instruction.
        procedure IsetOpcode(hIstr: HandleInstruz; opcode: pbyte; size: uint32); cdecl;  external Triton_dll Name 'IsetOpcode';
        //! Sets the prefix of the instruction (mainly for X86).
        procedure IsetPrefix(hIstr: HandleInstruz; prefix: prefix_e);cdecl;  external Triton_dll Name 'IsetPrefix';
        //! Sets the size of the instruction.
        procedure IsetSize(hIstr: HandleInstruz; size: uint32);cdecl;  external Triton_dll Name 'IsetSize';
        //! Sets the taint of the instruction.
        procedure IsetTaintBool(hIstr: HandleInstruz; state: Boolean); cdecl;  external Triton_dll Name 'IsetTaintBool';
        //! Sets the taint of the instruction based on its expressions.
        procedure IsetTaint(hIstr: HandleInstruz); cdecl;  external Triton_dll Name 'IsetTaint';
        //! Sets the thread id of the instruction.
        procedure IsetThreadId(hIstr: HandleInstruz; tid: uint32); cdecl;  external Triton_dll Name 'IsetThreadId';
        //! Sets the type of the instruction.
        procedure IsetType(hIstr: HandleInstruz; tipo: uint32);cdecl;  external Triton_dll Name 'IsetType';
        //! Returns the list of all implicit and explicit load access
	      function IgetLoadAccess(hIstr: HandleInstruz; var outArray: PMemNode): uint32; cdecl;  external Triton_dll Name 'IgetLoadAccess';
        //! Returns the list of all implicit and explicit store access
        function IgetStoreAccess(hIstr: HandleInstruz; var outArray:PMemNode): uint32;cdecl;  external Triton_dll Name 'IgetStoreAccess';
        //! Returns the list of all implicit and explicit register (flags includes) inputs (read)
        function IgetReadRegisters(hIstr: HandleInstruz; var outArray : PRegNode):uint32;cdecl;  external Triton_dll Name 'IgetReadRegisters';
        //! Returns the list of all implicit and explicit register (flags includes) outputs (write)
        function IgetWrittenRegisters(hIstr: HandleInstruz; var outArray: PRegNode):uint32;cdecl;  external Triton_dll Name 'IgetWrittenRegisters';
        //! Returns the list of all implicit and explicit immediate inputs (read)
        function IgetReadImmediates(hIstr: HandleInstruz; var outArray:PImmNode): uint32;cdecl;  external Triton_dll Name 'IgetReadImmediates';
        //! Returns the list of all implicit and explicit undefined registers.
        function IgetUndefinedRegisters(hIstr: HandleInstruz; var outArray: PReg): uint32;cdecl;  external Triton_dll Name 'IgetUndefinedRegisters';
        //!  The semantics set of the instruction.
        function IgetsymbolicExpressions(hIstr: HandleInstruz; var outArray: PSymbolicEx): uint32;cdecl;  external Triton_dll Name 'IgetsymbolicExpressions';
        //!  Get operands.
        function IGetOperand(hIstr: HandleInstruz; outArray: POperand): uint32;cdecl;  external Triton_dll Name 'IGetOperand';
        //! Returns true if at least one of its expressions contains a symbolic variable.
        function IisSymbolized(hIstr: HandleInstruz): Boolean;cdecl;  external Triton_dll Name 'IisSymbolized';

implementation
   uses  Triton.Core;

{ Istruzione }

procedure Istruzione.GetInternalData;
var
  i : Integer;
begin
    FInternalIstruz := RefIstruzToIstruz(FHIstruz) ;

    tid           := FInternalIstruz.tid;
    address       := FInternalIstruz.address;
    nextAddress   := FInternalIstruz.nextAddress;
    disassembly   := AnsiString(FInternalIstruz.disassembly);

    opcode := [];
    for i := 0 to FInternalIstruz.size - 1 do
           opcode := opcode + [FInternalIstruz.opcode[i]];

    size          := FInternalIstruz.size;
    tipo          := FInternalIstruz.tipo;
    prefix        := FInternalIstruz.prefix;
    branch        := FInternalIstruz.branch;
    controlFlow   := FInternalIstruz.controlFlow;
    conditionTaken:= FInternalIstruz.conditionTaken;
    tainted       := FInternalIstruz.tainted;
end;

function Istruzione.getLoadAccess: TDictionary<MemAccess, AbstractNode>;
var
  n,i       : Integer;
  outArray  : PMemNode ;
begin
    result := TDictionary<MemAccess, AbstractNode>.Create;

    n :=  IgetLoadAccess(FHIstruz,outArray);

    for i := 0 to n - 1 do
       result.add( MemAccess(outArray[i].mem), AbstractNode(outArray[i].node) );
end;


function Istruzione.getStoreAccess: TDictionary<MemAccess, AbstractNode>;
var
  n,i       : Integer;
  outArray  : PMemNode ;
begin
    result := TDictionary<MemAccess, AbstractNode>.Create;

    n :=  IgetStoreAccess(FHIstruz,outArray);

    for i := 0 to n - 1 do
       result.add( MemAccess(outArray[i].mem), AbstractNode(outArray[i].node) );

end;

function Istruzione.getReadRegisters: TDictionary<Registro, AbstractNode>;
var
  n,i       : Integer;
  outArray  : PRegNode ;

begin
    result := TDictionary<Registro, AbstractNode>.Create;

    n :=  IgetReadRegisters(FHIstruz,outArray);

    for i := 0 to n - 1 do
       result.add( Registro(outArray[i].reg), AbstractNode(outArray[i].node) );

end;

function Istruzione.getWrittenRegisters: TDictionary<Registro, AbstractNode>;
var
  n,i       : Integer;
  outArray  : PRegNode ;

begin
    result := TDictionary<Registro, AbstractNode>.Create;

    n :=  IgetWrittenRegisters(FHIstruz,outArray);

    for i := 0 to n - 1 do
       result.add( Registro(outArray[i].reg), AbstractNode(outArray[i].node) );

end;

function Istruzione.getReadImmediates: TDictionary<Immediate, AbstractNode>;
var
  n,i       : Integer;
  outArray  : PImmNode ;

begin
    result := TDictionary<Immediate, AbstractNode>.Create;

    n :=  IgetReadImmediates(FHIstruz,outArray);

    for i := 0 to n - 1 do
       result.add( Immediate(outArray[i].imm), AbstractNode(outArray[i].node) );

end;

function Istruzione.getUndefinedRegisters: TList<Registro>;
var
  n,i       : Integer;
  outArray  : PReg ;

begin
    result := TList<Registro>.Create;

    n :=  IgetUndefinedRegisters(FHIstruz,outArray);

    for i := 0 to n - 1 do
       result.add( Registro(outArray[i]) );

end;

function Istruzione.getsymbolicExpressions: TList<symbolicExp>;
var
  n,i       : Integer;
  outArray  : PSymbolicEx ;
  ret       : TList<symbolicExp>;
begin
    ret := TList<symbolicExp>.Create;
    outArray := nil;
    n :=  IgetsymbolicExpressions(FHIstruz,outArray);

    for i := 0 to n - 1 do
       ret.add( symbolicExp(outArray[i]) );

    Result := ret;
end;

function Istruzione.getType: instruction_e;
begin
    Result := instruction_e(tipo)
end;

function Istruzione.getOperands: TArray<OpWrapper>;
var
  n,i       : Integer;
  outArray  : POperand ;

begin
    Result := [];
    outArray := nil;
    n :=  IGetOperand(FHIstruz,@outArray);

    for i := 0 to n - 1 do
       Result := Result + [ OpWrapper( outArray[i] ) ];

end;

procedure Istruzione.Copy(other: Istruzione);
begin
    ZeroMemory(@self,SizeOf(Istruzione));
    FHIstruz := other.FHIstruz;

    GetInternalData;

    tid           := other.tid;
    address       := other.address;
    nextAddress   := other.nextAddress;
    disassembly   := other.disassembly;

    opcode := other.opcode;

    size          := other.size;
    tipo          := other.tipo;
    prefix        := other.prefix;
    branch        := other.branch;
    controlFlow   := other.controlFlow;
    conditionTaken:= other.conditionTaken;
    tainted       := other.tainted;

end;

procedure Istruzione.Create;
begin
    ZeroMemory(@self,SizeOf(Istruzione));
    FHIstruz := IInstruction;

    GetInternalData
end;

procedure Istruzione.Create(opcode: TOpcode; opSize: Uint32);
begin
    ZeroMemory(@self,SizeOf(Istruzione));
    FHIstruz := IInstructionOP(@opcode[0],opSize);

    GetInternalData ;

    {LoadAccess          := nil;
    StoreAccess         := nil;
    readRegisters       := nil;
    writtenRegisters    := nil;
    readImmediates      := nil;
    undefinedRegisters  := nil;
    symbolicExpressions := nil; }
end;

procedure Istruzione.Create(other: Istruzione);
begin
    ZeroMemory(@self,SizeOf(Istruzione));
    FHIstruz := IInstructionFrom(HandleInstruz(other));

    GetInternalData;

    {LoadAccess          := nil;
    StoreAccess         := nil;
    readRegisters       := nil;
    writtenRegisters    := nil;
    readImmediates      := nil;
    undefinedRegisters  := nil;
    symbolicExpressions := nil;}
end;

function Istruzione.CreateFrom(opcode: array of Byte): Istruzione;
begin
    ZeroMemory(@self,SizeOf(Istruzione));
    FHIstruz := IInstructionOP(@opcode[0],Length(opcode));

    GetInternalData ;

    Result := Self;
end;

procedure Istruzione.Create(opcode: array of Byte);
begin
    ZeroMemory(@self,SizeOf(Istruzione));
    FHIstruz := IInstructionOP(@opcode[0],Length(opcode));

    GetInternalData ;
end;

procedure Istruzione.Create(addr: UInt64; opcode: TOpcode);
begin
    ZeroMemory(@self,SizeOf(Istruzione));
    var size : UInt32 := Length(opcode) ;
    FHIstruz := IInstructionOP(@opcode[0],size);

    setAddress(addr);

    GetInternalData;
end;


procedure Istruzione.Free;
begin
    IDelete(FHIstruz);
    Self := default(Istruzione);

    if Assigned(LoadAccess)          then LoadAccess.Free;
    if Assigned(StoreAccess)         then StoreAccess.Free;
    if Assigned(readRegisters)       then readRegisters.Free;
    if Assigned(writtenRegisters)    then writtenRegisters.Free;
    if Assigned(readImmediates)      then readImmediates.Free;
    if Assigned(undefinedRegisters)  then undefinedRegisters.Free;
    if Assigned(symbolicExpressions) then symbolicExpressions.Free;
end;

class Operator Istruzione.Implicit(rIstr: Istruzione):HandleInstruz;
begin
    Result := rIstr.FHIstruz;
end;

class operator Istruzione.Explicit(rIstr: Istruzione): HandleInstruz;
begin
    Result := rIstr.FHIstruz;
end;

class operator Istruzione.Explicit(hIstr: HandleInstruz): Istruzione;
begin
    ZeroMemory(@Result,SizeOf(Istruzione));
    Result.FHIstruz  := hIstr ;

    Result.GetInternalData
end;

procedure Istruzione.Reload;
begin
    GetInternalData
end;

procedure Istruzione.clear;
begin
    Iclear(FHIstruz);

    GetInternalData
end;

function Istruzione.isReadFrom(target: OpWrapper): Boolean;
begin
     Result := IisReadFrom(FHIstruz,HandleOperandWrapper(target))
end;

function Istruzione.isSymbolized: Boolean;
begin
    Result := IisSymbolized(FHIstruz);
end;

function Istruzione.isWriteTo(target: OpWrapper): Boolean;
begin
    Result := IisWriteTo(FHIstruz,HandleOperandWrapper(target))
end;

procedure Istruzione.setLoadAccess(mem: MemAccess; node: AbstractNode);
begin
   IsetLoadAccess(FHIstruz,HandleMemAcc(mem),node );
   GetInternalData
end;

procedure Istruzione.removeLoadAccess(mem: MemAccess);
begin
   IremoveLoadAccess(FHIstruz,HandleMemAcc(mem));
   GetInternalData
end;

procedure Istruzione.setStoreAccess(mem: MemAccess; node: AbstractNode);
begin
  IsetStoreAccess(FHIstruz,HandleMemAcc(mem),node );
  GetInternalData
end;

procedure Istruzione.removeStoreAccess(mem: MemAccess);
begin
  IremoveStoreAccess(FHIstruz,HandleMemAcc(mem));
  GetInternalData
end;

procedure Istruzione.setReadRegister(reg: Registro; node: AbstractNode);
begin
  IsetReadRegister(FHIstruz,HandleReg(reg),node );
  GetInternalData
end;

procedure Istruzione.removeReadRegister(reg: Registro);
begin
  IremoveReadRegister(FHIstruz,HandleReg(reg));
  GetInternalData
end;

procedure Istruzione.setWrittenRegister(reg: Registro;  node: AbstractNode);
begin
  IsetWrittenRegister(FHIstruz,HandleReg(reg),node );
  GetInternalData
end;

procedure Istruzione.removeWrittenRegister(reg: Registro);
begin
   IremoveWrittenRegister(FHIstruz,HandleReg(reg));
   GetInternalData
end;

procedure Istruzione.setReadImmediate(imm: Immediate; node: AbstractNode);
begin
  IsetReadImmediate(FHIstruz,HandleImmediate(imm),node );
  GetInternalData
end;

procedure Istruzione.removeReadImmediate(imm: Immediate);
begin
   IremoveReadImmediate(FHIstruz,HandleImmediate(imm));
   GetInternalData
end;

procedure Istruzione.setUndefinedRegister(reg: Registro);
begin
  IsetUndefinedRegister(FHIstruz,HandleReg(reg));
  GetInternalData
end;

procedure Istruzione.removeUndefinedRegister(reg: Registro);
begin
  IremoveUndefinedRegister(FHIstruz,HandleReg(reg));
  GetInternalData
end;

procedure Istruzione.setAddress(addr: UInt64);
begin
  IsetAddress(FHIstruz,addr);
  GetInternalData
end;

procedure Istruzione.setBranch(flag: Boolean);
begin
  IsetBranch(FHIstruz,flag);
  GetInternalData
end;

procedure Istruzione.setCodeCondition(codeCondition: condition_e);
begin
  IsetCodeCondition(FHIstruz, codeCondition);
  GetInternalData
end;

procedure Istruzione.setConditionTaken(flag: Boolean);
begin
  IsetConditionTaken(FHIstruz, flag);
  GetInternalData
end;

procedure Istruzione.setControlFlow(flag: Boolean);
begin
  IsetControlFlow(FHIstruz, flag);
  GetInternalData
end;

procedure Istruzione.setDisassembly(str: PAnsiChar);
begin
  IsetDisassembly(FHIstruz,str);
  GetInternalData
end;

procedure Istruzione.setOpcode(aBuff: TOpcode; size: UInt32);
begin
  IsetOpcode(FHIstruz,@aBuff[0],size);
  GetInternalData
end;

procedure Istruzione.setOpcode(aBuff: TOpcode);
begin
    setOpcode(aBuff, Length(aBuff))
end;

procedure Istruzione.setOpcode(aBuff: array of Byte);
var
  A : TOpcode;
  i : Integer;
begin
    for i := 0 to High(aBuff) do
      A := A + [ aBuff[i] ];

    setOpcode(A, Length(aBuff))
end;


procedure Istruzione.setPrefix(prefix: prefix_e);
begin
  IsetPrefix(FHIstruz,prefix);
  GetInternalData
end;

procedure Istruzione.setSize(size: UInt32);
begin
  IsetSize(FHIstruz,size);
  GetInternalData
end;

procedure Istruzione.setTaint;
begin
  IsetTaint(FHIstruz);
  GetInternalData
end;

procedure Istruzione.setTaint(state: Boolean);
begin
  IsetTaintBool(FHIstruz,state);
  GetInternalData
end;

procedure Istruzione.setThreadId(tid: UInt32);
begin
  IsetThreadId(FHIstruz,tid);
  GetInternalData
end;

procedure Istruzione.setType(tipo: UInt32);
begin
  IsetType(FHIstruz,tipo);
  GetInternalData
end;

function Istruzione.ToStr: string;
begin
    Result := Format('0x%x: %s',[address,disassembly]);
end;

end.
