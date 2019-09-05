unit Triton.Core;

{$Z4}

interface
    uses Triton.Define;

    (* Architecture API ============================================================================== *)

        //! Constructor of the API.
        function CreateAPI:HandleApi;cdecl;  external Triton_dll Name 'CreateApi';

        //! Destructor of the API.
        procedure DeleteApi(vApi: HandleApi); cdecl;  external Triton_dll Name 'DeleteApi';

        //! [**architecture api**] - Initializes an architecture. \sa triton::arch::architecture_e.
        procedure setArchitecture(vApi: HandleApi;arch: architecture_e); cdecl;  external Triton_dll name 'setArchitecture';

        //! [**architecture api**] - Returns the architecture as triton::arch::architecture_e.
        function getArchitecture(vApi: HandleApi): architecture_e;  cdecl;  external Triton_dll name 'GetArchitecture';

        //! [**Architecture api**] - Returns true if the architecture is valid.
        function isArchitectureValid(vApi: HandleApi): Boolean; cdecl;  external Triton_dll Name 'isArchitectureValid';

        //! [**architecture api**] - Raises an exception if the architecture is not initialized.
        procedure checkArchitecture(vApi: HandleApi); external Triton_dll Name 'checkArchitecture';

        //! [**architecture api**] - Returns the instance of the current CPU used.
        function getCpuInstance(vApi: HandleApi): HandleCpuInterface; cdecl;  external Triton_dll Name 'getCpuInstance';

        //! [**architecture api**] - Clears the architecture states (registers and memory).
        procedure clearArchitecture(vApi: HandleApi); cdecl;  external Triton_dll Name 'clearArchitecture';

        //! [**architecture api**] - Returns the endianness as triton::arch::endianness_e.
        function getEndianness(vApi: HandleApi): endianness_e; cdecl;  external Triton_dll Name 'getEndianness';

        //! [**architecture api**] - Returns true if the register id is a flag. \sa triton::arch::x86::register_e.
        function isFlag(vApi: HandleApi;regId: register_e): Boolean; cdecl;  external Triton_dll Name 'isFlag';

        //! [**architecture api**] - Returns true if the register id is a flag.
        function isFlagR(vApi: HandleApi; reg: HandleReg): Boolean; cdecl;  external Triton_dll Name 'isFlagR';

        //! [**architecture api**] - Returns true if the regId is a register. \sa triton::arch::x86::register_e.
        function isRegister(vApi: HandleApi;regId : register_e ): Boolean; cdecl;  external Triton_dll Name 'isRegister';

        //! [**architecture api**] - Returns true if the regId is a register.
        function isRegisterR(vApi: HandleApi; reg: HandleReg): Boolean; cdecl;  external Triton_dll Name 'isRegisterR';

        //! [**architecture api**] - Returns Register from regId.
        function getRegister(vApi: HandleApi;  id: register_e): HandleReg; cdecl;  external Triton_dll Name 'getRegister';

        //! [**architecture api**] - Returns parent Register from a register.
        function getParentRegisterR(vApi: HandleApi; reg: HandleReg): HandleReg; cdecl;  external Triton_dll Name 'getParentRegisterR';

        //! [**architecture api**] - Returns parent Register from regId.
        function getParentRegister(vApi: HandleApi; id: register_e): HandleReg; cdecl;  external Triton_dll Name 'getParentRegister';

        //! [**architecture api**] - Returns true if the regId is a register or a flag. \sa triton::arch::x86::register_e.
        function isRegisterValid(vApi: HandleApi; regId: register_e): Boolean ;cdecl;  external Triton_dll Name 'isRegisterValid';

        //! [**architecture api**] - Returns true if the regId is a register or a flag.
        function isRegisterValidR(vApi: HandleApi; reg: HandleReg): Boolean ;cdecl;  external Triton_dll Name 'isRegisterValidR';

        //! [**architecture api**] - Returns the bit in byte of the General Purpose Registers.
        function getGprBitSize(vApi: HandleApi): uint32;cdecl;  external Triton_dll Name 'getGprBitSize';

        //! [**architecture api**] - Returns the size in byte of the General Purpose Registers.
        function getGprSize(vApi: HandleApi): uint32; cdecl;  external Triton_dll Name 'getGprSize';

        //! [**architecture api**] - Returns the number of registers according to the CPU architecture.
        function getNumberOfRegisters(vApi: HandleApi): uint32;cdecl;  external Triton_dll Name 'getNumberOfRegisters';

        //! [**architecture api**] - Returns all registers. \sa triton::arch::x86::register_e.
        function getAllRegisters(vApi: HandleApi; OutRegs: PRegIdReg): UInt32; cdecl;  external Triton_dll Name 'getAllRegisters';

        //! [**architecture api**] - Returns all parent registers. \sa triton::arch::x86::register_e.
        function gettParentRegisters(Handle: HandleApi; var outRegs:pReg):uint32; cdecl;  external Triton_dll Name 'gettParentRegisters';

        //! [**architecture api**] - Returns the concrete value of a memory cell.
        function  getConcreteMemoryValueByte(Handle: HandleApi; addr: uint64; execCallbacks : Boolean = true):uint8;cdecl;  external Triton_dll Name 'getConcreteMemoryValueByte';

        { TODO -oMax -c : Adding support uint512 23/07/2019 18:06:23 }
        //! [**architecture api**] - Returns the concrete value of memory cells.
        function  getConcreteMemoryValue(Handle: HandleApi; mem: HandleMemAcc; execCallbacks : Boolean = true): uint64;cdecl;  external Triton_dll Name 'getConcreteMemoryValue';

        //! [**architecture api**] - Returns the concrete value of a memory area.
        function  getConcreteMemoryAreaValue(Handle: HandleApi; baseAddr: uint64; size: usize; execCallbacks : Boolean = true):PByte; cdecl;  external Triton_dll Name 'getConcreteMemoryAreaValue';

        { TODO -oMax -c : Adding support uint512 23/07/2019 18:06:23 }
        //! [**architecture api**] - Returns the concrete value of a register. // not support 512
        function  getConcreteRegisterValue(Handle: HandleApi; reg: HandleReg; execCallbacks : Boolean = true): uint64;cdecl;  external Triton_dll Name 'getConcreteRegisterValue';

        procedure  setConcreteMemoryValueByte(Handle: HandleApi; addr: uint64; value: uint8);cdecl;  external Triton_dll Name 'setConcreteMemoryValueByte';

        { TODO -oMax -c : Adding support uint512 23/07/2019 18:06:23 }
        procedure  setConcreteMemoryValue(Handle: HandleApi; mem: HandleMemAcc; value: uint64);cdecl;  external Triton_dll Name 'setConcreteMemoryValue';

        procedure  setConcreteMemoryAreaValueByte(Handle: HandleApi; baseAddr : uint64; values: PByte);cdecl;  external Triton_dll Name 'setConcreteMemoryAreaValueByte';

        procedure  setConcreteMemoryAreaValue(Handle: HandleApi; baseAddr: uint64; area : PByte;  size: usize);cdecl;  external Triton_dll Name 'setConcreteMemoryAreaValue';

        { TODO -oMax -c : Adding support uint512 23/07/2019 18:06:23 }
        procedure  setConcreteRegisterValue(Handle: HandleApi; reg: HandleReg; value: uint64);cdecl;  external Triton_dll Name 'setConcreteRegisterValue';

        //! [**architecture api**] - Returns true if the range `[baseAddr:size]` is mapped into the internal memory representation. \sa getConcreteMemoryValue() and getConcreteMemoryAreaValue().
        function isMemoryMapped(Handle: HandleApi ;baseAddr: uint64;  size : usize = 1): Boolean; cdecl; external Triton_dll Name 'isMemoryMapped';

        //! [**architecture api**] - Removes the range `[baseAddr:size]` from the internal memory representation. \sa isMemoryMapped().
        procedure unmapMemory(Handle: HandleApi; baseAddr: uint64;  size : usize = 1);cdecl;  external Triton_dll Name 'unmapMemory';

        //! [**architecture api**] - Disassembles the instruction and setup operands. You must define an architecture before. \sa processing().
        procedure disassembly(vApi: HandleApi;inst: HandleInstruz);cdecl;  external Triton_dll Name 'disassembly';

    (* Processing API ================================================================================ *)

        //! [**proccesing api**] - Processes an instruction and updates engines according to the instruction semantics. Returns true if the instruction is supported.
        function  processing(Handle: HandleApi; inst: HandleInstruz): Boolean;cdecl;  external Triton_dll Name 'processing';

        //! [**proccesing api**] - Initializes everything.
        procedure   initEngines(Handle: HandleApi);cdecl;  external Triton_dll Name 'initEngines';

        //! [**proccesing api**] - Removes everything.
        procedure   removeEngines(Handle: HandleApi); cdecl;  external Triton_dll Name 'removeEngines';

        //! [**proccesing api**] - Resets everything.
        procedure  reset(Handle: HandleApi);cdecl;  external Triton_dll Name 'reset';


    (* IR API ======================================================================================== *)

        //! [**IR builder api**] - Raises an exception if the IR builder is not initialized.
        procedure   checkIrBuilder(Handle: HandleApi);cdecl;  external Triton_dll Name 'checkIrBuilder';

        //! [**IR builder api**] - Builds the instruction semantics. Returns true if the instruction is supported. You must define an architecture before. \sa processing().
        function   buildSemantics(Handle : HandleApi ; inst: HandleInstruz): Boolean;cdecl;  external Triton_dll Name 'buildSemantics';

        //! [**IR builder api**] - Returns the AST context. Used as AST builder.
        function   getAstContext(Handle: HandleApi): HandleAstContext; cdecl;  external Triton_dll Name 'getAstContext';

    (* AST Representation API ======================================================================== *)

        //! [**AST representation api**] - Returns the AST representation mode as triton::ast::representations::mode_e.
        function getAstRepresentationMode(Handle : HandleApi):uint32 ;cdecl;  external Triton_dll Name 'getAstRepresentationMode';

        //! [**AST representation api**] - Sets the AST representation mode.
        procedure  setAstRepresentationMode(Handle: HandleApi; mode: uint32);cdecl;  external Triton_dll Name 'setAstRepresentationMode';

    (* Callbacks API ================================================================================= *)

        //! [**callbacks api**] - Adds a GET_CONCRETE_MEMORY_VALUE callback (LOAD).
        procedure addCallbackGetMem(Handle: HandleApi; cb: pointer{cbGetMemVal}); cdecl;  external Triton_dll Name 'addCallbackGetMem';

        //! [**callbacks api**] - Adds a GET_CONCRETE_REGISTER_VALUE callback (GET).
        procedure  addCallbackGetReg(Handle: HandleApi; cb: pointer{cbGetRegVal}); cdecl;  external Triton_dll Name 'addCallbackGetReg';

        //! [**callbacks api**] - Adds a SET_CONCRETE_MEMORY_VALUE callback (STORE).
        procedure addCallbackSetMem(Handle: HandleApi; cb: pointer{cbSetMemVal}); cdecl;  external Triton_dll Name 'addCallbackSetMem';

        //! [**callbacks api**] - Adds a SET_CONCRETE_REGISTER_VALUE callback (PUT).
        procedure addCallbackSetReg(Handle: HandleApi; cb: pointer{cbSetRegVal});cdecl;  external Triton_dll Name 'addCallbackSetReg';

        //! [**callbacks api**] - Adds a SYMBOLIC_SIMPLIFICATION callback.
        procedure addCallbackSimplif(Handle: HandleApi; cb: pointer{cbSimplification}); cdecl;  external Triton_dll Name 'addCallbackSimplif';

        //! [**callbacks api**] - Removes all recorded callbacks.
        procedure removeAllCallbacks(Handle: HandleApi);cdecl;  external Triton_dll Name 'removeAllCallbacks';

        //! [**callbacks api**] - Deletes a GET_CONCRETE_MEMORY_VALUE callback (LOAD).
        procedure removeCallbackGetMem(Handle: HandleApi; cb: pointer{cbGetMemVal}); cdecl;  external Triton_dll Name 'removeCallbackGetMem';

        //! [**callbacks api**] - Deletes a GET_CONCRETE_REGISTER_VALUE callback (GET).
        procedure removeCallbackGetReg(Handle: HandleApi; cb: pointer{cbGetRegVal}); cdecl;  external Triton_dll Name 'removeCallbackGetReg';

        //! [**callbacks api**] - Deletes a SET_CONCRETE_MEMORY_VALUE callback (STORE).
        procedure removeCallbackSetMem(Handle: HandleApi; cb: pointer{cbSetMemVal}); cdecl;  external Triton_dll Name 'removeCallbackSetMem';

        //! [**callbacks api**] - Deletes a SET_CONCRETE_REGISTER_VALUE callback (PUT).
        procedure removeCallbackSetReg(Handle: HandleApi; cb: pointer{cbSetRegVal});  cdecl;  external Triton_dll Name 'removeCallbackSetReg';

        //! [**callbacks api**] - Deletes a SYMBOLIC_SIMPLIFICATION callback.
        procedure  removeCallbackSimplif(Handle: HandleApi; cb: pointer{cbSimplification}); cdecl;  external Triton_dll Name 'removeCallbackSimplif';

        //! [**callbacks api**] - Processes callbacks according to the kind and the C++ polymorphism.
        function processCallbacks(Handle: HandleApi; kind: callback_e; node: HandleAbstractNode): HandleAbstractNode; cdecl;  external Triton_dll Name 'processCallbacks';

        //! [**callbacks api**] - Processes callbacks according to the kind and the C++ polymorphism.
        procedure processCallbacksMem(Handle: HandleApi; kind: callback_e; mem: HandleMemAcc); cdecl;  external Triton_dll Name 'processCallbacksMem';

        //! [**callbacks api**] - Processes callbacks according to the kind and the C++ polymorphism.
        procedure processCallbacksReg(Handle: HandleApi; kind: callback_e; reg: HandleReg); cdecl;  external Triton_dll Name 'processCallbacksReg';

    (* Modes API====================================================================================== *)

        //! [**modes api**] - Raises an exception if modes interface is not initialized.
        procedure checkModes(Handle:HandleApi) ; cdecl;  external Triton_dll Name 'checkModes';

        //! [**modes api**] - Enables or disables a specific mode.
        procedure enableMode(Handle: HandleApi; mode: mode_e; flag: Boolean); cdecl;  external Triton_dll Name 'enableMode';

        //! [**modes api**] - Returns true if the mode is enabled.
        function isModeEnabled(Handle: HandleApi; mode: mode_e): Boolean; cdecl;  external Triton_dll Name 'isModeEnabled';

    (* Symbolic engine API =========================================================================== *)

        //! [**symbolic api**] - Raises an exception if the symbolic engine is not initialized.
        procedure  checkSymbolic(Handle: HandleApi ) ; cdecl;  external Triton_dll Name 'checkSymbolic';

        //! [**symbolic api**] - Returns the instance of the symbolic engine.
        function getSymbolicEngine(Handle: HandleApi): HandleSymbolicEngine; cdecl;  external Triton_dll Name 'getSymbolicEngine';

        //! [**symbolic api**] - Returns the map of symbolic registers defined.
        function getSymbolicRegisters(Handle : HandleApi; OutRegE: PRegSymE  ): UInt32; cdecl;  external Triton_dll Name 'getSymbolicRegisters';

        //! [**symbolic api**] - Returns the map (<Addr : SymExpr>) of symbolic memory defined.
        function  getSymbolicMemory(Handle: HandleApi;ouMemSym: PMemSymE): UInt32 ;  cdecl;  external Triton_dll Name 'getSymbolicMemory';

        //! [**symbolic api**] - Returns the shared symbolic expression corresponding to the memory address.
        function getSymbolicMemoryAddr(Handle: HandleApi; addr: uint64 ):HandleSharedSymbolicExpression ; cdecl;  external Triton_dll Name 'getSymbolicMemoryAddr';

        //! [**symbolic api**] - Returns the shared symbolic expression corresponding to the parent register.
        function  getSymbolicRegister(Handle: HandleApi; reg: HandleReg):HandleSharedSymbolicExpression ; cdecl;  external Triton_dll Name 'getSymbolicRegister';

        //! [**symbolic api**] - Returns the symbolic memory value.
        function getSymbolicMemoryValue(Handle: HandleApi; address: uint64): uint8; cdecl;  external Triton_dll Name 'getSymbolicMemoryValue';

        //! [**symbolic api**] - Returns the symbolic memory value.
        function getSymbolicMemoryValueM(Handle: HandleApi; mem: HandleMemAcc): uint64; cdecl;  external Triton_dll Name 'getSymbolicMemoryValueM';

        //! [**symbolic api**] - Returns the symbolic values of a memory area.
        function getSymbolicMemoryAreaValue(Handle: HandleApi; baseAddr: uint64;size: usize): PByte; cdecl;  external Triton_dll Name 'getSymbolicMemoryAreaValue';

        //! [**symbolic api**] - Returns the symbolic register value.
        function getSymbolicRegisterValue(Handle: HandleApi; reg: HandleReg): uint64; cdecl;  external Triton_dll Name 'getSymbolicRegisterValue';

        //! [**symbolic api**] - Converts a symbolic expression to a symbolic variable. `symVarSize` must be in bits.
        function convertExpressionToSymbolicVariable(Handle: HandleApi; exprId: usize; symVarSize: uint32; symVarComment: PAnsiChar = nil): HandleSharedSymbolicVariable; cdecl;  external Triton_dll Name 'convertExpressionToSymbolicVariable';

        //! [**symbolic api**] - Converts a symbolic memory expression to a symbolic variable.
        function convertMemoryToSymbolicVariable(Handle: HandleApi; mem: HandleMemAcc;  symVarComment: PAnsiChar = nil): HandleSharedSymbolicVariable; cdecl;  external Triton_dll Name 'convertMemoryToSymbolicVariable';

        //! [**symbolic api**] - Converts a symbolic register expression to a symbolic variable.
        function convertRegisterToSymbolicVariable(Handle: HandleApi; reg: HandleReg;  symVarComment : PAnsiChar = nil): HandleSharedSymbolicVariable; cdecl;  external Triton_dll Name 'convertRegisterToSymbolicVariable';

        //! [**symbolic api**] - Returns the AST corresponding to the operand.
        function getOperandAst(Handle: HandleApi; op: HandleOperandWrapper): HandleAbstractNode; cdecl;  external Triton_dll Name 'getOperandAst';

        //! [**symbolic api**] - Returns the AST corresponding to the operand.
        function getOperandAstIstruz(Handle: HandleApi; inst: HandleInstruz ; op: HandleOperandWrapper):HandleAbstractNode; cdecl;  external Triton_dll Name 'getOperandAstIstruz';

        //! [**symbolic api**] - Returns the AST corresponding to the immediate.
        function getImmediateAst(Handle: HandleApi; imm: HandleImmediate): HandleAbstractNode; cdecl;  external Triton_dll Name 'getImmediateAst';

        //! [**symbolic api**] - Returns the AST corresponding to the immediate and defines the immediate as input of the instruction..
        function getImmediateAstIstruz(Handle: HandleApi; inst: HandleInstruz ; imm: HandleImmediate):HandleAbstractNode; cdecl;  external Triton_dll Name 'getImmediateAstIstruz';

        //! [**symbolic api**] - Returns the AST corresponding to the memory.
        function getMemoryAst(Handle: HandleApi; mem: HandleMemAcc):HandleAbstractNode; cdecl;  external Triton_dll Name 'getMemoryAst';

        //! [**symbolic api**] - Returns the AST corresponding to the memory and defines the memory cell as input of the instruction.
        function getMemoryAstIstruz(Handle: HandleApi; inst: HandleInstruz ; mem: HandleMemAcc):HandleAbstractNode; cdecl;  external Triton_dll Name 'getMemoryAstIstruz';

        //! [**symbolic api**] - Returns the AST corresponding to the register.
        function getRegisterAst(Handle: HandleApi; reg: HandleReg):HandleAbstractNode; cdecl;  external Triton_dll Name 'getRegisterAst';

        //! [**symbolic api**] - Returns the AST corresponding to the register and defines the register as input of the instruction.
        function getRegisterAstIstruz(Handle: HandleApi; inst: HandleInstruz ; reg: HandleReg):HandleAbstractNode; cdecl;  external Triton_dll Name 'getRegisterAstIstruz';

        //! [**symbolic api**] - Returns a new shared symbolic expression. Note that if there are simplification passes recorded, simplification will be applied.
        function newSymbolicExpression(Handle: HandleApi; node: HandleAbstractNode; comment: PAnsiChar = nil):HandleSharedSymbolicExpression; cdecl;  external Triton_dll Name 'newSymbolicExpression';

        //! [**symbolic api**] - Returns a new symbolic variable.
        function newSymbolicVariable(Handle: HandleApi; varSize: uint32; comment: PAnsiChar = nil):HandleSharedSymbolicVariable; cdecl;  external Triton_dll Name 'newSymbolicVariable';

        //! [**symbolic api**] - Removes the symbolic expression corresponding to the id.
        procedure  removeSymbolicExpression(Handle: HandleApi; symExprId: usize); cdecl;  external Triton_dll Name 'removeSymbolicExpression';

        //! [**symbolic api**] - Returns the new shared symbolic abstract expression and links this expression to the instruction.
        function createSymbolicExpression(Handle: HandleApi; inst: HandleInstruz ; node: HandleAbstractNode; dst: HandleOperandWrapper; comment : PAnsiChar = nil):HandleSharedSymbolicExpression; cdecl;  external Triton_dll Name 'createSymbolicExpression';

        //! [**symbolic api**] - Returns the new shared symbolic memory expression and links this expression to the instruction.
        function createSymbolicMemoryExpression(Handle: HandleApi; inst: HandleInstruz ; node: HandleAbstractNode; mem: HandleMemAcc; comment: PAnsiChar = nil):HandleSharedSymbolicExpression;  cdecl;  external Triton_dll Name 'createSymbolicMemoryExpression';

        //! [**symbolic api**] - Returns the new shared symbolic register expression and links this expression to the instruction.
        function createSymbolicRegisterExpression(Handle: HandleApi; inst: HandleInstruz ; node: HandleAbstractNode; reg: HandleReg; comment: PAnsiChar = nil):HandleSharedSymbolicExpression; cdecl;  external Triton_dll Name 'createSymbolicRegisterExpression';

        //! [**symbolic api**] - Returns the new shared symbolic flag expression and links this expression to the instruction.
        function createSymbolicFlagExpression(Handle: HandleApi; inst: HandleInstruz ; node: HandleAbstractNode; flag: HandleReg; comment: PAnsiChar = nil):HandleSharedSymbolicExpression;  cdecl;  external Triton_dll Name 'createSymbolicFlagExpression';

        //! [**symbolic api**] - Returns the new shared symbolic volatile expression and links this expression to the instruction.
        function createSymbolicVolatileExpression(Handle: HandleApi; inst: HandleInstruz ; node: HandleAbstractNode; comment: PAnsiChar = nil):HandleSharedSymbolicExpression;  cdecl;  external Triton_dll Name 'createSymbolicVolatileExpression';

        //! [**symbolic api**] - Assigns a symbolic expression to a memory.
        procedure  assignSymbolicExpressionToMemory(Handle: HandleApi; se:HandleSharedSymbolicExpression; mem: HandleMemAcc); cdecl;  external Triton_dll Name 'assignSymbolicExpressionToMemory';

        //! [**symbolic api**] - Assigns a symbolic expression to a register.
        procedure  assignSymbolicExpressionToRegister(Handle: HandleApi; se:HandleSharedSymbolicExpression; reg: HandleReg); cdecl;  external Triton_dll Name 'assignSymbolicExpressionToRegister';

        //! [**symbolic api**] - Processes all recorded simplifications. Returns the simplified node.
        function processSimplification(Handle: HandleApi; node: HandleAbstractNode; z3: Boolean = false):HandleAbstractNode;  cdecl;  external Triton_dll Name 'processSimplification';

        //! [**symbolic api**] - Returns the shared symbolic expression corresponding to an id.
        function getSymbolicExpressionFromId(Handle: HandleApi; symExprId: usize):HandleSharedSymbolicExpression ; cdecl;  external Triton_dll Name 'getSymbolicExpressionFromId';

        //! [**symbolic api**] - Returns the symbolic variable corresponding to the symbolic variable id.
        function getSymbolicVariableFromId(Handle: HandleApi; symVarId: usize):HandleSharedSymbolicVariable; cdecl;  external Triton_dll Name 'getSymbolicVariableFromId';

        //! [**symbolic api**] - Returns the symbolic variable corresponding to the symbolic variable name.
        function getSymbolicVariableFromName(Handle: HandleApi; symVarName: PAnsiChar):HandleSharedSymbolicVariable;  cdecl;  external Triton_dll Name 'getSymbolicVariableFromName';

        //! [**symbolic api**] - Returns the logical conjunction vector of path constraints.
        function  getPathConstraints(Handle: HandleApi; outPath: PHandlePathConstraint):uint32 ;  cdecl;  external Triton_dll Name 'getPathConstraints';

        //! [**symbolic api**] - Returns the logical conjunction AST of path constraints.
        function getPathConstraintsAst(Handle:HandleApi):HandleAbstractNode;   cdecl;  external Triton_dll Name 'getPathConstraintsAst';

        //! [**symbolic api**] - Adds a path constraint.
        procedure  addPathConstraint(Handle: HandleApi; inst: HandleInstruz ; expr: HandleSharedSymbolicExpression); cdecl;  external Triton_dll Name 'addPathConstraint';

        //! [**symbolic api**] - Clears the logical conjunction vector of path constraints.
        procedure  clearPathConstraints(Handle: HandleApi);  cdecl;  external Triton_dll Name 'clearPathConstraints';

        //! [**symbolic api**] - Enables or disables the symbolic execution engine.
        procedure  enableSymbolicEngine(Handle: HandleApi; flag: Boolean); cdecl;  external Triton_dll Name 'enableSymbolicEngine';

        //! [**symbolic api**] - Returns true if the symbolic execution engine is enabled.
        function isSymbolicEngineEnabled(Handle: HandleApi): Boolean ; cdecl;  external Triton_dll Name 'isSymbolicEngineEnabled';

        //! [**symbolic api**] - Returns true if the symbolic expression ID exists.
        function isSymbolicExpressionIdExists(Handle: HandleApi; symExprId: usize): Boolean ;  cdecl;  external Triton_dll Name 'isSymbolicExpressionIdExists';

        //! [**symbolic api**] - Returns true if memory cell expressions contain symbolic variables.
        function isMemorySymbolized(Handle: HandleApi; mem: HandleMemAcc): Boolean ; cdecl;  external Triton_dll Name 'isMemorySymbolized';

        //! [**symbolic api**] - Returns true if memory cell expressions contain symbolic variables.
        function isMemorySymbolizedSize(Handle: HandleApi; addr: uint64; size: uint32 = 1): Boolean ;  cdecl;  external Triton_dll Name 'isMemorySymbolizedSize';

        //! [**symbolic api**] - Returns true if the register expression contains a symbolic variable.
        function isRegisterSymbolized(Handle: HandleApi; reg: HandleReg): Boolean ;  cdecl;  external Triton_dll Name 'isRegisterSymbolized';

        //! [**symbolic api**] - Concretizes all symbolic memory references.
        procedure  concretizeAllMemory(Handle: HandleApi); cdecl;  external Triton_dll Name 'concretizeAllMemory';

        //! [**symbolic api**] - Concretizes all symbolic register references.
        procedure  concretizeAllRegister(Handle: HandleApi);  cdecl;  external Triton_dll Name 'concretizeAllRegister';

        //! [**symbolic api**] - Concretizes a specific symbolic memory reference.
        procedure  concretizeMemoryM(Handle: HandleApi; mem: HandleMemAcc); cdecl;  external Triton_dll Name 'concretizeMemoryM';

        //! [**symbolic api**] - Concretizes a specific symbolic memory reference.
        procedure  concretizeMemory(Handle: HandleApi; addr: uint64);  cdecl;  external Triton_dll Name 'concretizeMemory';

        //! [**symbolic api**] - Concretizes a specific symbolic register reference.
        procedure  concretizeRegister(Handle: HandleApi; reg: HandleReg); cdecl;  external Triton_dll Name 'concretizeRegister';

        //! [**symbolic api**] - Slices all expressions from a given one.
        function sliceExpressions(Handle: HandleApi; expr: HandleSharedSymbolicExpression; outSlice: PIdSymExpr):uint32; cdecl;  external Triton_dll Name 'sliceExpressions';

        //! [**symbolic api**] - Returns the list of the tainted symbolic expressions.
        function getTaintedSymbolicExpressions(Handle:HandleApi; var outSimbolicExpr: PSimbolicExpr):uint32  cdecl;  external Triton_dll Name 'getTaintedSymbolicExpressions';

        //! [**symbolic api**] - Returns all symbolic expressions as a map of <SymExprId : SymExpr>
        function getSymbolicExpressions(Handle:HandleApi; outSymMap: PIdSymExpr):UInt32 ;  cdecl;  external Triton_dll Name 'getSymbolicExpressions';

        //! [**symbolic api**] - Returns all symbolic variables as a map of <SymVarId : SymVar>
        function getSymbolicVariables(Handle:HandleApi; outSymVar: PIdSymVar):uint32 ; cdecl;  external Triton_dll Name 'getSymbolicVariables';

        { TODO -oMax -c : Adding support uint512 23/07/2019 18:06:23 }
        //! [**symbolic api**] - Gets the concrete value of a symbolic variable.
        function getConcreteVariableValue(Handle: HandleApi; symVar:HandleSharedSymbolicVariable):uint64 ; cdecl;  external Triton_dll Name 'getConcreteVariableValue';

        { TODO -oMax -c : Adding support uint512 23/07/2019 18:06:23 }
        //! [**symbolic api**] - Sets the concrete value of a symbolic variable.
        procedure  setConcreteVariableValue(Handle: HandleApi; symVar: HandleSharedSymbolicVariable; value: UInt64);  cdecl;  external Triton_dll Name 'setConcreteVariableValue';

        //! Initializes the memory access AST (LOAD and STORE).
	      procedure initLeaAst(Handle: HandleApi; mem: HandleMemAcc; force: Boolean = false); cdecl;  external Triton_dll Name 'initLeaAst';

    	(* Solver engine API ============================================================================= *)

        //! [**solver api**] - Raises an exception if the solver engine is not initialized.
        procedure checkSolver(Handle: HandleApi) ; cdecl;  external Triton_dll Name 'checkSolver';

        (*!
        * \brief [**solver api**] - Computes and returns a model from a symbolic constraint.
        *
        * \details
        * **item1**: symbolic variable id<br>
        * **item2**: model
        *)
        function getModel(Handle: HandleApi; node: HandleAbstractNode; outModel: PAddrSolver): uint32 ; cdecl;  external Triton_dll Name 'getModel';

        (*!
        * \brief [**solver api**] - Computes and returns several models from a symbolic constraint. The `limit` is the number of models returned.
        *
        * \details
        * **item1**: symbolic variable id<br>
        * **item2**: model
        *)
        function getModels(Handle: HandleApi; node: HandleAbstractNode;limit : uint32; outModel: PListSolver): uint32; cdecl;  external Triton_dll Name 'getModels';

        //! Returns true if an expression is satisfiable.
        function isSat(Handle: HandleApi; node: HandleAbstractNode): Boolean ; cdecl;  external Triton_dll Name 'isSat';

        //! Returns the kind of solver as triton::engines::solver::solver_e.
        function getSolver(Handle: HandleApi): solver_e ; cdecl;  external Triton_dll Name 'getSolver';

        //! Returns the instance of the initialized solver
        //HandleSolverInterface EXPORTCALL getSolverInstance(HandleApi Handle) ;

        //! Initializes a predefined solver.
        procedure setSolver(Handle: HandleApi; kind: solver_e); cdecl;  external Triton_dll Name 'setSolver';

        //! Initializes a custom solver.
        //procedure setCustomSolver(Handle: HandleApi; customSolver: HandleSolverInterface);

        //! Returns true if the solver is valid.
        function isSolverValid(Handle: HandleApi):Boolean ;cdecl;  external Triton_dll Name 'isSolverValid';

        //! [**solver api**] - Evaluates a Triton's AST via Z3 and returns a concrete value.
        function evaluateAstViaZ3(Handle: HandleApi; node: HandleAbstractNode): uint64 ;cdecl;  external Triton_dll Name 'evaluateAstViaZ3';

        //! [**solver api**] - Converts a Triton's AST to a Z3's AST, perform a Z3 simplification and returns a Triton's AST.
        function processZ3Simplification(Handle: HandleApi; node: HandleAbstractNode): HandleAbstractNode ; cdecl;  external Triton_dll Name 'processZ3Simplification';

    (* Taint engine API
	   * ==============================================================================
	   *)

        //! [**taint api**] - Raises an exception if the taint engine is not
        //! initialized.
        procedure  checkTaint(Handle: HandleApi ); cdecl;  external Triton_dll Name 'checkTaint';

        //! [**taint api**] - Returns the instance of the taint engine.
        function getTaintEngine(Handle: HandleApi ):HandleTaintEngine;cdecl;  external Triton_dll Name 'getTaintEngine';

        //! [**taint api**] - Returns the tainted addresses.
        function getTaintedMemory(Handle: HandleApi; var outMemAddrs:pUInt64): uint32 ;cdecl;  external Triton_dll Name 'getTaintedMemory';

        //! [**taint api**] - Returns the tainted registers.
        function getTaintedRegisters(Handle: HandleApi; var outRegs:pReg):uint32 ;cdecl;  external Triton_dll Name 'getTaintedRegisters';

        //! [**taint api**] - Enables or disables the taint engine.
        procedure  enableTaintEngine(Handle: HandleApi ; flag: Boolean);cdecl;  external Triton_dll Name 'enableTaintEngine';

        //! [**taint api**] - Returns true if the taint engine is enabled.
        function isTaintEngineEnabled(Handle: HandleApi ): Boolean ;cdecl;  external Triton_dll Name 'isTaintEngineEnabled';

        //! [**taint api**] - Abstract taint verification. Returns true if the operand
        //! is tainted.
        function isTainted(Handle: HandleApi ; op:HandleOperandWrapper): Boolean ;cdecl;  external Triton_dll Name 'isTainted';

        //! [**taint api**] - Returns true if the address:size is tainted.
        function isMemoryTainted(Handle: HandleApi ;addr: uint64; size: uint32 = 1): Boolean ;cdecl;  external Triton_dll Name 'isMemoryTainted';

        //! [**taint api**] - Returns true if the memory is tainted.
        function isMemoryTaintedMem(Handle: HandleApi ;mem: HandleMemAcc ): Boolean ;cdecl;  external Triton_dll Name 'isMemoryTaintedMem';

        //! [**taint api**] - Returns true if the register is tainted.
        function isRegisterTainted(Handle: HandleApi ; reg: HandleReg): Boolean ;cdecl;  external Triton_dll Name 'isRegisterTainted';

        //! [**taint api**] - Sets the flag (taint or untaint) to an abstract operand
        //! (Register or Memory).
        function setTaint(Handle: HandleApi ; op: HandleOperandWrapper; flag : Boolean): Boolean ;cdecl;  external Triton_dll Name 'setTaint';

        //! [**taint api**] - Sets the flag (taint or untaint) to a memory.
        function setTaintMemory(Handle: HandleApi ; mem: HandleMemAcc ;flag : Boolean): Boolean ;cdecl;  external Triton_dll Name 'setTaintMemory';

        //! [**taint api**] - Sets the flag (taint or untaint) to a register.
        function setTaintRegister(Handle: HandleApi ; reg: HandleReg;flag : Boolean): Boolean ; cdecl;  external Triton_dll Name 'setTaintRegister';

        //! [**taint api**] - Taints an address. Returns TAINTED if the address has
        //! been tainted correctly. Otherwise it returns the last defined state.
        function taintMemory(Handle: HandleApi ; addr: uint64): Boolean ;cdecl;  external Triton_dll Name 'taintMemory';

        //! [**taint api**] - Taints a memory. Returns TAINTED if the memory has been
        //! tainted correctly. Otherwise it returns the last defined state.
        function taintMemoryMem(Handle: HandleApi ; mem: HandleMemAcc ): Boolean ;cdecl;  external Triton_dll Name 'taintMemoryMem';

        //! [**taint api**] - Taints a register. Returns TAINTED if the register has
        //! been tainted correctly. Otherwise it returns the last defined state.
        function taintRegister(Handle: HandleApi ; reg: HandleReg): Boolean ; cdecl;  external Triton_dll Name 'taintRegister';

        //! [**taint api**] - Untaints an address. Returns !TAINTED if the address has
        //! been untainted correctly. Otherwise it returns the last defined state.
        function untaintMemory(Handle: HandleApi ; addr: uint64): Boolean ; cdecl;  external Triton_dll Name 'untaintMemory';

        //! [**taint api**] - Untaints a memory. Returns !TAINTED if the memory has
        //! been untainted correctly. Otherwise it returns the last defined state.
        function untaintMemoryMem(Handle: HandleApi ; mem: HandleMemAcc ): Boolean ; cdecl;  external Triton_dll Name 'untaintMemoryMem';

        //! [**taint api**] - Untaints a register. Returns !TAINTED if the register
        //! has been untainted correctly. Otherwise it returns the last defined state.
        function untaintRegister(Handle: HandleApi ; reg: HandleReg): Boolean ;cdecl;  external Triton_dll Name 'untaintRegister';

        //! [**taint api**] - Abstract union tainting.
        function taintUnion(Handle: HandleApi ; op1,op2: HandleOperandWrapper): Boolean ;cdecl;  external Triton_dll Name 'taintUnion';

        //! [**taint api**] - Abstract assignment tainting.
        function taintAssignment(Handle: HandleApi; op1,op2: HandleOperandWrapper): Boolean ; cdecl;  external Triton_dll Name 'taintAssignment';

        //! [**taint api**] - Taints MemoryImmediate with union. Returns true if the
        //! memDst is TAINTED.
        function taintUnionMemoryImmediate(Handle: HandleApi ; Dst: HandleMemAcc): Boolean ;cdecl;  external Triton_dll Name 'taintUnionMemoryImmediate';

        //! [**taint api**] - Taints MemoryMemory with union. Returns true if the
        //! memDst or memSrc are TAINTED.
        function taintUnionMemoryMemory(Handle: HandleApi ; Dst: HandleMemAcc; Src:HandleMemAcc): Boolean ;cdecl;  external Triton_dll Name 'taintUnionMemoryMemory';

        //! [**taint api**] - Taints MemoryRegister with union. Returns true if the
        //! memDst or regSrc are TAINTED.
        function taintUnionMemoryRegister(Handle: HandleApi ; Dst: HandleMemAcc; Src: HandleReg): Boolean ;cdecl;  external Triton_dll Name 'taintUnionMemoryRegister';

        //! [**taint api**] - Taints RegisterImmediate with union. Returns true if the
        //! regDst is TAINTED.
        function taintUnionRegisterImmediate(Handle: HandleApi ; Dst: HandleReg): Boolean ;cdecl;  external Triton_dll Name 'taintUnionRegisterImmediate';

        //! [**taint api**] - Taints RegisterMemory with union. Returns true if the
        //! regDst or memSrc are TAINTED.
        function taintUnionRegisterMemory(Handle: HandleApi ; Dst: HandleReg; Src: HandleMemAcc): Boolean ; cdecl;  external Triton_dll Name 'taintUnionRegisterMemory';

        //! [**taint api**] - Taints RegisterRegister with union. Returns true if the
        //! regDst or regSrc are TAINTED.
        function taintUnionRegisterRegister(Handle: HandleApi ; Dst:HandleReg; Src:HandleReg): Boolean ;cdecl;  external Triton_dll Name 'taintUnionRegisterRegister';

        //! [**taint api**] - Taints MemoryImmediate with assignment. Returns always
        //! false.
        function taintAssignmentMemoryImmediate(Handle: HandleApi ; Dst:HandleMemAcc): Boolean ; cdecl;  external Triton_dll Name 'taintAssignmentMemoryImmediate';

        //! [**taint api**] - Taints MemoryMemory with assignment. Returns true if the
        //! memDst is tainted.
        function taintAssignmentMemoryMemory(Handle: HandleApi ; Dst: HandleMemAcc; Src: HandleMemAcc): Boolean ;cdecl;  external Triton_dll Name 'taintAssignmentMemoryMemory';

        //! [**taint api**] - Taints MemoryRegister with assignment. Returns true if
        //! the memDst is tainted.
        function taintAssignmentMemoryRegister(Handle: HandleApi ; Dst: HandleMemAcc; Src: HandleReg): Boolean ;cdecl;  external Triton_dll Name 'taintAssignmentMemoryRegister';

        //! [**taint api**] - Taints RegisterImmediate with assignment. Returns always
        //! false.
        function taintAssignmentRegisterImmediate(Handle: HandleApi ; Dst: HandleReg): Boolean ;cdecl;  external Triton_dll Name 'taintAssignmentRegisterImmediate';

        //! [**taint api**] - Taints RegisterMemory with assignment. Returns true if
        //! the regDst is tainted.
        function taintAssignmentRegisterMemory(Handle: HandleApi ; Dst: HandleReg; Src: HandleMemAcc ): Boolean ;cdecl;  external Triton_dll Name 'taintAssignmentRegisterMemory';

        //! [**taint api**] - Taints RegisterRegister with assignment. Returns true if
        //! the regDst is tainted.
        function taintAssignmentRegisterRegister(Handle: HandleApi ; Dst: HandleReg; Src: HandleReg): Boolean ;cdecl;  external Triton_dll Name 'taintAssignmentRegisterRegister';

implementation

end.
