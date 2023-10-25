unit Triton.Core;

{$Z4}

interface
    uses Triton.Define;

    (* Architecture API ============================================================================== *)

        //! Constructor of the API.
        function CreateAPI:HandleContext;cdecl;  external Triton_dll Name 'CreateApi';
        //! Destructor of the API.
        procedure DeleteApi(vApi: HandleContext); cdecl;  external Triton_dll Name 'DeleteApi';
        //! [**architecture api**] - Initializes an architecture. \sa triton::arch::architecture_e.
        procedure setArchitecture(vApi: HandleContext;arch: architecture_e); cdecl;  external Triton_dll name 'setArchitecture';
        //! [**architecture api**] - Returns the architecture as triton::arch::architecture_e.
        function getArchitecture(vApi: HandleContext): architecture_e;  cdecl;  external Triton_dll name 'GetArchitecture';
        //! [**Architecture api**] - Returns true if the architecture is valid.
        function isArchitectureValid(vApi: HandleContext): Boolean; cdecl;  external Triton_dll Name 'isArchitectureValid';
        //! [**architecture api**] - Returns the instance of the current CPU used.
        function getCpuInstance(vApi: HandleContext): HandleCpuInterface; cdecl;  external Triton_dll Name 'getCpuInstance';
        //! [**architecture api**] - Clears the architecture states (registers and memory).
        procedure clearArchitecture(vApi: HandleContext); cdecl;  external Triton_dll Name 'clearArchitecture';
        //! [**architecture api**] - Returns the endianness as triton::arch::endianness_e.
        function getEndianness(vApi: HandleContext): endianness_e; cdecl;  external Triton_dll Name 'getEndianness';
        //! [**architecture api**] - Returns true if the register id is a flag. \sa triton::arch::x86::register_e.
        function isFlag(vApi: HandleContext;regId: register_e): Boolean; cdecl;  external Triton_dll Name 'isFlag';
        //! [**architecture api**] - Returns true if the register id is a flag.
        function isFlagR(vApi: HandleContext; reg: HandleReg): Boolean; cdecl;  external Triton_dll Name 'isFlagR';
        //! [**architecture api**] - Returns true if the regId is a register. \sa triton::arch::x86::register_e.
        function isRegister(vApi: HandleContext;regId : register_e ): Boolean; cdecl;  external Triton_dll Name 'isRegister';
        //! [**architecture api**] - Returns true if the regId is a register.
        function isRegisterR(vApi: HandleContext; reg: HandleReg): Boolean; cdecl;  external Triton_dll Name 'isRegisterR';
        //! [**architecture api**] - Returns Register from regId.
        function getRegister(vApi: HandleContext;  id: register_e): HandleReg; cdecl;  external Triton_dll Name 'getRegister';
        //! [**architecture api**] - Returns parent Register from a register.
        function getParentRegisterR(vApi: HandleContext; reg: HandleReg): HandleReg; cdecl;  external Triton_dll Name 'getParentRegisterR';
        //! [**architecture api**] - Returns parent Register from regId.
        function getParentRegister(vApi: HandleContext; id: register_e): HandleReg; cdecl;  external Triton_dll Name 'getParentRegister';
        //! [**architecture api**] - Returns true if the regId is a register or a flag. \sa triton::arch::x86::register_e.
        function isRegisterValid(vApi: HandleContext; regId: register_e): Boolean ;cdecl;  external Triton_dll Name 'isRegisterValid';
        //! [**architecture api**] - Returns true if the regId is a register or a flag.
        function isRegisterValidR(vApi: HandleContext; reg: HandleReg): Boolean ;cdecl;  external Triton_dll Name 'isRegisterValidR';
        //! [**architecture api**] - Returns the bit in byte of the General Purpose Registers.
        function getGprBitSize(vApi: HandleContext): uint32;cdecl;  external Triton_dll Name 'getGprBitSize';
        //! [**architecture api**] - Returns the size in byte of the General Purpose Registers.
        function getGprSize(vApi: HandleContext): uint32; cdecl;  external Triton_dll Name 'getGprSize';
        //! [**architecture api**] - Returns the number of registers according to the CPU architecture.
        function getNumberOfRegisters(vApi: HandleContext): uint32;cdecl;  external Triton_dll Name 'getNumberOfRegisters';
        //! [**architecture api**] - Returns all registers. \sa triton::arch::x86::register_e.
        function getAllRegisters(vApi: HandleContext; OutRegs: PRegIdReg): UInt32; cdecl;  external Triton_dll Name 'getAllRegisters';
        //! [**architecture api**] - Returns all parent registers. \sa triton::arch::x86::register_e.
        function gettParentRegisters(Handle: HandleContext; var outRegs:pReg):uint32; cdecl;  external Triton_dll Name 'gettParentRegisters';
        //! [**architecture api**] - Returns the concrete value of a memory cell.
        function  getConcreteMemoryValueByte(Handle: HandleContext; addr: uint64; execCallbacks : Boolean = true):uint8;cdecl;  external Triton_dll Name 'getConcreteMemoryValueByte';

        { TODO -oMax -c : Adding support uint512 23/07/2019 18:06:23 }
        //! [**architecture api**] - Returns the concrete value of memory cells.
        function  getConcreteMemoryValue(Handle: HandleContext; mem: HandleMemAcc; execCallbacks : Boolean = true): uint64;cdecl;  external Triton_dll Name 'getConcreteMemoryValue';

        //! [**architecture api**] - Returns the concrete value of a memory area.
        function  getConcreteMemoryAreaValue(Handle: HandleContext; baseAddr: uint64; size: usize; execCallbacks : Boolean = true):PByte; cdecl;  external Triton_dll Name 'getConcreteMemoryAreaValue';

        { TODO -oMax -c : Adding support uint512 23/07/2019 18:06:23 }
        //! [**architecture api**] - Returns the concrete value of a register. // not support 512
        function  getConcreteRegisterValue(Handle: HandleContext; reg: HandleReg; execCallbacks : Boolean = true): uint64;cdecl;  external Triton_dll Name 'getConcreteRegisterValue';

        procedure  setConcreteMemoryValueByte(Handle: HandleContext; addr: uint64; value: uint8);cdecl;  external Triton_dll Name 'setConcreteMemoryValueByte';

        { TODO -oMax -c : Adding support uint512 23/07/2019 18:06:23 }
        procedure  setConcreteMemoryValue(Handle: HandleContext; mem: HandleMemAcc; value: uint64);cdecl;  external Triton_dll Name 'setConcreteMemoryValue';

        procedure  setConcreteMemoryAreaValueByte(Handle: HandleContext; baseAddr : uint64; values: PByte);cdecl;  external Triton_dll Name 'setConcreteMemoryAreaValueByte';

        procedure  setConcreteMemoryAreaValue(Handle: HandleContext; baseAddr: uint64; area : PByte;  size: usize);cdecl;  external Triton_dll Name 'setConcreteMemoryAreaValue';

        { TODO -oMax -c : Adding support uint512 23/07/2019 18:06:23 }
        procedure  setConcreteRegisterValue(Handle: HandleContext; reg: HandleReg; value: uint64);cdecl;  external Triton_dll Name 'setConcreteRegisterValue';

        //! [**architecture api**] - Returns true if the range `[baseAddr:size]` is mapped into the internal memory representation. \sa getConcreteMemoryValue() and getConcreteMemoryAreaValue().
        function isConcreteMemoryValueDefined(Handle: HandleContext ;baseAddr: uint64;  size : usize = 1): Boolean; cdecl; external Triton_dll Name 'isConcreteMemoryValueDefined';

        //! [**architecture api**] - Removes the range `[baseAddr:size]` from the internal memory representation. \sa isMemoryMapped().
        procedure clearConcreteMemoryValue(Handle: HandleContext; baseAddr: uint64;  size : usize = 1);cdecl;  external Triton_dll Name 'unmapMemory';

        //! [**architecture api**] - Disassembles the instruction and setup operands. You must define an architecture before. \sa processing().
        procedure disassembly(vApi: HandleContext;inst: HandleInstruz);cdecl;  external Triton_dll Name 'disassembly';

        //! [**architecture api**] - Disassembles a block of instructions. You must define an architecture before.
        procedure disassembly_BB(Handle: HandleContext; block: HandleBasicBlock; addr: UInt64 = 0); cdecl;  external Triton_dll Name 'disassembly_BB';

    (* Processing API ================================================================================ *)

        //! [**proccesing api**] - Processes an instruction and updates engines according to the instruction semantics. Returns true if the instruction is supported.
        function  processing(Handle: HandleContext; inst: HandleInstruz): exception_e;cdecl;  external Triton_dll Name 'processing';

        //! [**proccesing api**] - Processes a block of instructions and updates engines according to instructions semantics. Returns `triton::arch::NO_FAULT` if succeed.
        Function processing_BB(Handle: HandleContext; block: HandleBasicBlock; addr: UInt64 = 0): exception_e; cdecl;  external Triton_dll Name 'processing_BB';

        //! [**proccesing api**] - Initializes everything.
        procedure   initEngines(Handle: HandleContext);cdecl;  external Triton_dll Name 'initEngines';

        //! [**proccesing api**] - Removes everything.
        procedure   removeEngines(Handle: HandleContext); cdecl;  external Triton_dll Name 'removeEngines';

        //! [**proccesing api**] - Resets everything.
        procedure  reset(Handle: HandleContext);cdecl;  external Triton_dll Name 'reset';


    (* IR API ======================================================================================== *)

        //! [**IR builder api**] - Builds the instruction semantics. Returns true if the instruction is supported. You must define an architecture before. \sa processing().
        function   buildSemantics(Handle : HandleContext ; inst: HandleInstruz): exception_e;cdecl;  external Triton_dll Name 'buildSemantics';

        //! [**IR builder api**] - Builds the instructions semantics of a block. Returns `triton::arch::NO_FAULT` if succeed.
        function buildSemantics_BB(Handle: HandleContext; block: HandleBasicBlock): exception_e; cdecl;  external Triton_dll Name 'buildSemantics_BB';

        //! [**IR builder api**] - Returns the AST context. Used as AST builder.
        function   getAstContext(Handle: HandleContext): HandleAstContext; cdecl;  external Triton_dll Name 'getAstContext';
    (* AST Representation API ======================================================================== *)

        //! [**AST representation api**] - Returns the AST representation mode as triton::ast::representations::mode_e.
        function getAstRepresentationMode(Handle : HandleContext):uint32 ;cdecl;  external Triton_dll Name 'getAstRepresentationMode';

        //! [**AST representation api**] - Sets the AST representation mode.
        procedure  setAstRepresentationMode(Handle: HandleContext; mode: uint32);cdecl;  external Triton_dll Name 'setAstRepresentationMode';
    (* Callbacks API ================================================================================= *)

        //! [**callbacks api**] - Adds a GET_CONCRETE_MEMORY_VALUE callback (LOAD).
        procedure addCallbackGetMem(Handle: HandleContext; cb: pointer{cbGetMemVal}); cdecl;  external Triton_dll Name 'addCallbackGetMem';

        //! [**callbacks api**] - Adds a GET_CONCRETE_REGISTER_VALUE callback (GET).
        procedure  addCallbackGetReg(Handle: HandleContext; cb: pointer{cbGetRegVal}); cdecl;  external Triton_dll Name 'addCallbackGetReg';

        //! [**callbacks api**] - Adds a SET_CONCRETE_MEMORY_VALUE callback (STORE).
        procedure addCallbackSetMem(Handle: HandleContext; cb: pointer{cbSetMemVal}); cdecl;  external Triton_dll Name 'addCallbackSetMem';

        //! [**callbacks api**] - Adds a SET_CONCRETE_REGISTER_VALUE callback (PUT).
        procedure addCallbackSetReg(Handle: HandleContext; cb: pointer{cbSetRegVal});cdecl;  external Triton_dll Name 'addCallbackSetReg';

        //! [**callbacks api**] - Adds a SYMBOLIC_SIMPLIFICATION callback.
        procedure addCallbackSimplif(Handle: HandleContext; cb: pointer{cbSimplification}); cdecl;  external Triton_dll Name 'addCallbackSimplif';

        //! [**callbacks api**] - Removes all recorded callbacks.
        procedure removeAllCallbacks(Handle: HandleContext);cdecl;  external Triton_dll Name 'removeAllCallbacks';

        //! [**callbacks api**] - Deletes a GET_CONCRETE_MEMORY_VALUE callback (LOAD).
        procedure removeCallbackGetMem(Handle: HandleContext; cb: pointer{cbGetMemVal}); cdecl;  external Triton_dll Name 'removeCallbackGetMem';

        //! [**callbacks api**] - Deletes a GET_CONCRETE_REGISTER_VALUE callback (GET).
        procedure removeCallbackGetReg(Handle: HandleContext; cb: pointer{cbGetRegVal}); cdecl;  external Triton_dll Name 'removeCallbackGetReg';

        //! [**callbacks api**] - Deletes a SET_CONCRETE_MEMORY_VALUE callback (STORE).
        procedure removeCallbackSetMem(Handle: HandleContext; cb: pointer{cbSetMemVal}); cdecl;  external Triton_dll Name 'removeCallbackSetMem';

        //! [**callbacks api**] - Deletes a SET_CONCRETE_REGISTER_VALUE callback (PUT).
        procedure removeCallbackSetReg(Handle: HandleContext; cb: pointer{cbSetRegVal});  cdecl;  external Triton_dll Name 'removeCallbackSetReg';

        //! [**callbacks api**] - Deletes a SYMBOLIC_SIMPLIFICATION callback.
        procedure  removeCallbackSimplif(Handle: HandleContext; cb: pointer{cbSimplification}); cdecl;  external Triton_dll Name 'removeCallbackSimplif';

        //! [**callbacks api**] - Processes callbacks according to the kind and the C++ polymorphism.
        function processCallbacks(Handle: HandleContext; kind: callback_e; node: HandleAbstractNode): HandleAbstractNode; cdecl;  external Triton_dll Name 'processCallbacks';

        //! [**callbacks api**] - Processes callbacks according to the kind and the C++ polymorphism.
        procedure processCallbacksMem(Handle: HandleContext; kind: callback_e; mem: HandleMemAcc); cdecl;  external Triton_dll Name 'processCallbacksMem';

        //! [**callbacks api**] - Processes callbacks according to the kind and the C++ polymorphism.
        procedure processCallbacksReg(Handle: HandleContext; kind: callback_e; reg: HandleReg); cdecl;  external Triton_dll Name 'processCallbacksReg';
    (* Modes API====================================================================================== *)
        //! [**modes api**] - Raises an exception if modes interface is not initialized.
        procedure checkModes(Handle:HandleContext) ; cdecl;  external Triton_dll Name 'checkModes';

        //! [**modes api**] - Enables or disables a specific mode.
        procedure SetMode(Handle: HandleContext; mode: mode_e; flag: Boolean); cdecl;  external Triton_dll Name 'setMode';

        //! [**modes api**] - Returns true if the mode is enabled.
        function isModeEnabled(Handle: HandleContext; mode: mode_e): Boolean; cdecl;  external Triton_dll Name 'isModeEnabled';
    (* Symbolic engine API =========================================================================== *)

        //! [**symbolic api**] - Returns the instance of the symbolic engine.
        function getSymbolicEngine(Handle: HandleContext): HandleSymbolicEngine; cdecl;  external Triton_dll Name 'getSymbolicEngine';

        //! [**symbolic api**] - Returns the map of symbolic registers defined.
        function getSymbolicRegisters(Handle : HandleContext; OutRegE: PRegSymE  ): UInt32; cdecl;  external Triton_dll Name 'getSymbolicRegisters';

        //! [**symbolic api**] - Returns the map (<Addr : SymExpr>) of symbolic memory defined.
        function  getSymbolicMemory(Handle: HandleContext;ouMemSym: PMemSymE): UInt32 ;  cdecl;  external Triton_dll Name 'getSymbolicMemory';

        //! [**symbolic api**] - Returns the shared symbolic expression corresponding to the memory address.
        function getSymbolicMemoryAddr(Handle: HandleContext; addr: uint64 ):HandleSharedSymbolicExpression ; cdecl;  external Triton_dll Name 'getSymbolicMemoryAddr';

        //! [**symbolic api**] - Returns the shared symbolic expression corresponding to the parent register.
        function  getSymbolicRegister(Handle: HandleContext; reg: HandleReg):HandleSharedSymbolicExpression ; cdecl;  external Triton_dll Name 'getSymbolicRegister';

        //! [**symbolic api**] - Returns the symbolic memory value.
        function getSymbolicMemoryValue(Handle: HandleContext; address: uint64): uint8; cdecl;  external Triton_dll Name 'getSymbolicMemoryValue';

        //! [**symbolic api**] - Returns the symbolic memory value.
        function getSymbolicMemoryValueM(Handle: HandleContext; mem: HandleMemAcc): uint64; cdecl;  external Triton_dll Name 'getSymbolicMemoryValueM';

        //! [**symbolic api**] - Returns the symbolic values of a memory area.
        function getSymbolicMemoryAreaValue(Handle: HandleContext; baseAddr: uint64;size: usize): PByte; cdecl;  external Triton_dll Name 'getSymbolicMemoryAreaValue';

        //! [**symbolic api**] - Returns the symbolic register value.
        function getSymbolicRegisterValue(Handle: HandleContext; reg: HandleReg): uint64; cdecl;  external Triton_dll Name 'getSymbolicRegisterValue';

        //! [**symbolic api**] - Converts a symbolic expression to a symbolic variable. `symVarSize` must be in bits.
        function symbolizeExpression(Handle: HandleContext; exprId: usize; symVarSize: uint32; symVarComment: PAnsiChar = nil): HandleSharedSymbolicVariable; cdecl;  external Triton_dll Name 'symbolizeExpression';

        //! [**symbolic api**] - Converts a symbolic memory expression to a symbolic variable.
        function symbolizeMemory(Handle: HandleContext; mem: HandleMemAcc;  symVarComment: PAnsiChar = nil): HandleSharedSymbolicVariable; cdecl;  external Triton_dll Name 'symbolizeMemory';

        //! [**symbolic api**] - Converts a symbolic register expression to a symbolic variable.
        function symbolizeRegister(Handle: HandleContext; reg: HandleReg;  symVarComment : PAnsiChar = nil): HandleSharedSymbolicVariable; cdecl;  external Triton_dll Name 'symbolizeRegister';

        //! [**symbolic api**] - Returns the AST corresponding to the operand.
        function getOperandAst(Handle: HandleContext; op: HandleOperandWrapper): HandleAbstractNode; cdecl;  external Triton_dll Name 'getOperandAst';

        //! [**symbolic api**] - Returns the AST corresponding to the operand.
        function getOperandAstIstruz(Handle: HandleContext; inst: HandleInstruz ; op: HandleOperandWrapper):HandleAbstractNode; cdecl;  external Triton_dll Name 'getOperandAstIstruz';

        //! [**symbolic api**] - Returns the AST corresponding to the immediate.
        function getImmediateAst(Handle: HandleContext; imm: HandleImmediate): HandleAbstractNode; cdecl;  external Triton_dll Name 'getImmediateAst';

        //! [**symbolic api**] - Returns the AST corresponding to the immediate and defines the immediate as input of the instruction..
        function getImmediateAstIstruz(Handle: HandleContext; inst: HandleInstruz ; imm: HandleImmediate):HandleAbstractNode; cdecl;  external Triton_dll Name 'getImmediateAstIstruz';

        //! [**symbolic api**] - Returns the AST corresponding to the memory.
        function getMemoryAst(Handle: HandleContext; mem: HandleMemAcc):HandleAbstractNode; cdecl;  external Triton_dll Name 'getMemoryAst';

        //! [**symbolic api**] - Returns the AST corresponding to the memory and defines the memory cell as input of the instruction.
        function getMemoryAstIstruz(Handle: HandleContext; inst: HandleInstruz ; mem: HandleMemAcc):HandleAbstractNode; cdecl;  external Triton_dll Name 'getMemoryAstIstruz';

        //! [**symbolic api**] - Returns the AST corresponding to the register.
        function getRegisterAst(Handle: HandleContext; reg: HandleReg):HandleAbstractNode; cdecl;  external Triton_dll Name 'getRegisterAst';

        //! [**symbolic api**] - Returns the AST corresponding to the register and defines the register as input of the instruction.
        function getRegisterAstIstruz(Handle: HandleContext; inst: HandleInstruz ; reg: HandleReg):HandleAbstractNode; cdecl;  external Triton_dll Name 'getRegisterAstIstruz';

        //! [**symbolic api**] - Returns a new shared symbolic expression. Note that if there are simplification passes recorded, simplification will be applied.
        function newSymbolicExpression(Handle: HandleContext; node: HandleAbstractNode; comment: PAnsiChar = nil):HandleSharedSymbolicExpression; cdecl;  external Triton_dll Name 'newSymbolicExpression';

        //! [**symbolic api**] - Returns a new symbolic variable.
        function newSymbolicVariable(Handle: HandleContext; varSize: uint32; comment: PAnsiChar = nil):HandleSharedSymbolicVariable; cdecl;  external Triton_dll Name 'newSymbolicVariable';

        //! [**symbolic api**] - Removes the symbolic expression corresponding to the id.
        procedure  removeSymbolicExpression(Handle: HandleContext; symExprId: usize); cdecl;  external Triton_dll Name 'removeSymbolicExpression';

        //! [**symbolic api**] - Returns the new shared symbolic abstract expression and links this expression to the instruction.
        function createSymbolicExpression(Handle: HandleContext; inst: HandleInstruz ; node: HandleAbstractNode; dst: HandleOperandWrapper; comment : PAnsiChar = nil):HandleSharedSymbolicExpression; cdecl;  external Triton_dll Name 'createSymbolicExpression';

        //! [**symbolic api**] - Returns the new shared symbolic memory expression and links this expression to the instruction.
        function createSymbolicMemoryExpression(Handle: HandleContext; inst: HandleInstruz ; node: HandleAbstractNode; mem: HandleMemAcc; comment: PAnsiChar = nil):HandleSharedSymbolicExpression;  cdecl;  external Triton_dll Name 'createSymbolicMemoryExpression';

        //! [**symbolic api**] - Returns the new shared symbolic register expression and links this expression to the instruction.
        function createSymbolicRegisterExpression(Handle: HandleContext; inst: HandleInstruz ; node: HandleAbstractNode; reg: HandleReg; comment: PAnsiChar = nil):HandleSharedSymbolicExpression; cdecl;  external Triton_dll Name 'createSymbolicRegisterExpression';

        //! [**symbolic api**] - Returns the new shared symbolic volatile expression and links this expression to the instruction.
        function createSymbolicVolatileExpression(Handle: HandleContext; inst: HandleInstruz ; node: HandleAbstractNode; comment: PAnsiChar = nil):HandleSharedSymbolicExpression;  cdecl;  external Triton_dll Name 'createSymbolicVolatileExpression';

        //! [**symbolic api**] - Assigns a symbolic expression to a memory.
        procedure  assignSymbolicExpressionToMemory(Handle: HandleContext; se:HandleSharedSymbolicExpression; mem: HandleMemAcc); cdecl;  external Triton_dll Name 'assignSymbolicExpressionToMemory';

        //! [**symbolic api**] - Assigns a symbolic expression to a register.
        procedure  assignSymbolicExpressionToRegister(Handle: HandleContext; se:HandleSharedSymbolicExpression; reg: HandleReg); cdecl;  external Triton_dll Name 'assignSymbolicExpressionToRegister';

        //! [**symbolic api**] - Processes all recorded simplifications. Returns the simplified node.
        function simplify(Handle: HandleContext; node: HandleAbstractNode; z3: Boolean = false):HandleAbstractNode;  cdecl;  external Triton_dll Name 'simplify';

        //! [**symbolic api**] - Processes a dead store elimination simplification on a given basic block. If `padding` is true, keep addresses aligned and padds with NOP instructions.
        function simplify_BB(Handle: HandleContext; block: HandleBasicBlock; padding : Boolean= false): HandleBasicBlock; cdecl;  external Triton_dll Name 'simplify_BB';

        //! [**symbolic api**] - Returns the shared symbolic expression corresponding to an id.
        function getSymbolicExpressionFromId(Handle: HandleContext; symExprId: usize):HandleSharedSymbolicExpression ; cdecl;  external Triton_dll Name 'getSymbolicExpressionFromId';

        //! [**symbolic api**] - Returns the symbolic variable corresponding to the symbolic variable id.
        function getSymbolicVariableFromId(Handle: HandleContext; symVarId: usize):HandleSharedSymbolicVariable; cdecl;  external Triton_dll Name 'getSymbolicVariableFromId';

        //! [**symbolic api**] - Returns the symbolic variable corresponding to the symbolic variable name.
        function getSymbolicVariableFromName(Handle: HandleContext; symVarName: PAnsiChar):HandleSharedSymbolicVariable;  cdecl;  external Triton_dll Name 'getSymbolicVariableFromName';

        //! [**symbolic api**] - Returns the logical conjunction vector of path constraints.
        function  getPathConstraints(Handle: HandleContext; outPath: PHandlePathConstraint):uint32 ;  cdecl;  external Triton_dll Name 'getPathConstraints';

        //! [**symbolic api**] - Returns the logical conjunction AST of path constraints.
        function getPathPredicate(Handle:HandleContext):HandleAbstractNode;   cdecl;  external Triton_dll Name 'getPathPredicate';

        //! [**symbolic api**] - Adds a path constraint.
        procedure  pushPathConstraint(Handle: HandleContext; node: HandleAbstractNode); cdecl;  external Triton_dll Name 'pushPathConstraint';

        //! [**symbolic api**] - Adds a path constraint.
        procedure  pushPathConstraintPco(Handle: HandleContext; pco: HandlePathConstraint); cdecl;  external Triton_dll Name 'pushPathConstraintPco';

        //! [**symbolic api**] - Clears the logical conjunction vector of path constraints.
        procedure  clearPathConstraints(Handle: HandleContext);  cdecl;  external Triton_dll Name 'clearPathConstraints';

        //! [**symbolic api**] - Enables or disables the symbolic execution engine.
        procedure  enableSymbolicEngine(Handle: HandleContext; flag: Boolean); cdecl;  external Triton_dll Name 'enableSymbolicEngine';

        //! [**symbolic api**] - Returns true if the symbolic execution engine is enabled.
        function isSymbolicEngineEnabled(Handle: HandleContext): Boolean ; cdecl;  external Triton_dll Name 'isSymbolicEngineEnabled';

        //! [**symbolic api**] - Returns true if the symbolic expression ID exists.
        function isSymbolicExpressionIdExists(Handle: HandleContext; symExprId: usize): Boolean ;  cdecl;  external Triton_dll Name 'isSymbolicExpressionIdExists';

        //! [**symbolic api**] - Returns true if memory cell expressions contain symbolic variables.
        function isMemorySymbolized(Handle: HandleContext; mem: HandleMemAcc): Boolean ; cdecl;  external Triton_dll Name 'isMemorySymbolized';

        //! [**symbolic api**] - Returns true if memory cell expressions contain symbolic variables.
        function isMemorySymbolizedSize(Handle: HandleContext; addr: uint64; size: uint32 = 1): Boolean ;  cdecl;  external Triton_dll Name 'isMemorySymbolizedSize';

        //! [**symbolic api**] - Returns true if the register expression contains a symbolic variable.
        function isRegisterSymbolized(Handle: HandleContext; reg: HandleReg): Boolean ;  cdecl;  external Triton_dll Name 'isRegisterSymbolized';

        //! [**symbolic api**] - Concretizes all symbolic memory references.
        procedure  concretizeAllMemory(Handle: HandleContext); cdecl;  external Triton_dll Name 'concretizeAllMemory';

        //! [**symbolic api**] - Concretizes all symbolic register references.
        procedure  concretizeAllRegister(Handle: HandleContext);  cdecl;  external Triton_dll Name 'concretizeAllRegister';

        //! [**symbolic api**] - Concretizes a specific symbolic memory reference.
        procedure  concretizeMemoryM(Handle: HandleContext; mem: HandleMemAcc); cdecl;  external Triton_dll Name 'concretizeMemoryM';

        //! [**symbolic api**] - Concretizes a specific symbolic memory reference.
        procedure  concretizeMemory(Handle: HandleContext; addr: uint64);  cdecl;  external Triton_dll Name 'concretizeMemory';

        //! [**symbolic api**] - Concretizes a specific symbolic register reference.
        procedure  concretizeRegister(Handle: HandleContext; reg: HandleReg); cdecl;  external Triton_dll Name 'concretizeRegister';

        //! [**symbolic api**] - Slices all expressions from a given one.
        function sliceExpressions(Handle: HandleContext; expr: HandleSharedSymbolicExpression; outSlice: PIdSymExpr):uint32; cdecl;  external Triton_dll Name 'sliceExpressions';

        //! [**symbolic api**] - Returns the list of the tainted symbolic expressions.
        function getTaintedSymbolicExpressions(Handle:HandleContext; var outSimbolicExpr: PSimbolicExpr):uint32  cdecl;  external Triton_dll Name 'getTaintedSymbolicExpressions';

        //! [**symbolic api**] - Returns all symbolic expressions as a map of <SymExprId : SymExpr>
        function getSymbolicExpressions(Handle:HandleContext; outSymMap: PIdSymExpr):UInt32 ;  cdecl;  external Triton_dll Name 'getSymbolicExpressions';

        //! [**symbolic api**] - Returns all symbolic variables as a map of <SymVarId : SymVar>
        function getSymbolicVariables(Handle:HandleContext; outSymVar: PIdSymVar):uint32 ; cdecl;  external Triton_dll Name 'getSymbolicVariables';

        { TODO -oMax -c : Adding support uint512 23/07/2019 18:06:23 }
        //! [**symbolic api**] - Gets the concrete value of a symbolic variable.
        function getConcreteVariableValue(Handle: HandleContext; symVar:HandleSharedSymbolicVariable):uint64 ; cdecl;  external Triton_dll Name 'getConcreteVariableValue';

        { TODO -oMax -c : Adding support uint512 23/07/2019 18:06:23 }
        //! [**symbolic api**] - Sets the concrete value of a symbolic variable.
        procedure  setConcreteVariableValue(Handle: HandleContext; symVar: HandleSharedSymbolicVariable; value: UInt64);  cdecl;  external Triton_dll Name 'setConcreteVariableValue';


        //! Initializes the memory access AST (LOAD and STORE).

	      procedure initLeaAst(Handle: HandleContext; mem: HandleMemAcc; force: Boolean = false); cdecl;  external Triton_dll Name 'initLeaAst';


    	(* Solver engine API ============================================================================= *)


        (*!
        * \brief [**solver api**] - Computes and returns a model from a symbolic constraint.
        *
        * \details
        * **item1**: symbolic variable id<br>
        * **item2**: model
        *)
        function getModel(Handle: HandleContext; node: HandleAbstractNode; outModel: PAddrSolver; status: pstatus_e = nil; timeout: UInt32 = 0; solvingTime: PUInt32= nil): uint32 ; cdecl;  external Triton_dll Name 'getModel';

        (*!
        * \brief [**solver api**] - Computes and returns several models from a symbolic constraint. The `limit` is the number of models returned.
        *
        * \details
        * **item1**: symbolic variable id<br>
        * **item2**: model
        *)
        function getModels(Handle: HandleContext; node: HandleAbstractNode;limit : uint32; outModel: PListSolver): uint32; cdecl;  external Triton_dll Name 'getModels';

        //! Returns true if an expression is satisfiable.
        function isSat(Handle: HandleContext; node: HandleAbstractNode): Boolean ; cdecl;  external Triton_dll Name 'isSat';

        //! Returns the kind of solver as triton::engines::solver::solver_e.
        function getSolver(Handle: HandleContext): solver_e ; cdecl;  external Triton_dll Name 'getSolver';

        //! Returns the instance of the initialized solver
        //HandleSolverInterface EXPORTCALL getSolverInstance(HandleContext Handle) ;

        //! Initializes a predefined solver.
        procedure setSolver(Handle: HandleContext; kind: solver_e); cdecl;  external Triton_dll Name 'setSolver';

        //! Initializes a custom solver.
        //procedure setCustomSolver(Handle: HandleContext; customSolver: HandleSolverInterface);

        //! Returns true if the solver is valid.
        function isSolverValid(Handle: HandleContext):Boolean ;cdecl;  external Triton_dll Name 'isSolverValid';

        //! [**solver api**] - Evaluates a Triton's AST via Z3 and returns a concrete value.
        function evaluateAstViaSolver(Handle: HandleContext; node: HandleAbstractNode): uint64 ;cdecl;  external Triton_dll Name 'evaluateAstViaSolver';

        //! [**solver api**] - Converts a Triton's AST to a Z3's AST, perform a Z3 simplification and returns a Triton's AST.
        function simplifyAstViaSolver(Handle: HandleContext; node: HandleAbstractNode): HandleAbstractNode ; cdecl;  external Triton_dll Name 'simplifyAstViaSolver';


     (* Taint engine API

	   * ==============================================================================
	   *)

        //! [**taint api**] - Returns the instance of the taint engine.
        function getTaintEngine(Handle: HandleContext ):HandleTaintEngine;cdecl;  external Triton_dll Name 'getTaintEngine';

        //! [**taint api**] - Returns the tainted addresses.
        function getTaintedMemory(Handle: HandleContext; var outMemAddrs:pUInt64): uint32 ;cdecl;  external Triton_dll Name 'getTaintedMemory';

        //! [**taint api**] - Returns the tainted registers.
        function getTaintedRegisters(Handle: HandleContext; var outRegs:pReg):uint32 ;cdecl;  external Triton_dll Name 'getTaintedRegisters';

        //! [**taint api**] - Enables or disables the taint engine.
        //procedure  enableTaintEngine(Handle: HandleContext ; flag: Boolean);cdecl;  external Triton_dll Name 'enableTaintEngine';

        //! [**taint api**] - Returns true if the taint engine is enabled.
        //function isTaintEngineEnabled(Handle: HandleContext ): Boolean ;cdecl;  external Triton_dll Name 'isTaintEngineEnabled';

        //! [**taint api**] - Abstract taint verification. Returns true if the operand
        //! is tainted.
        function isTainted(Handle: HandleContext ; op:HandleOperandWrapper): Boolean ;cdecl;  external Triton_dll Name 'isTainted';

        //! [**taint api**] - Returns true if the address:size is tainted.
        function isMemoryTainted(Handle: HandleContext ;addr: uint64; size: uint32 = 1): Boolean ;cdecl;  external Triton_dll Name 'isMemoryTainted';

        //! [**taint api**] - Returns true if the memory is tainted.
        function isMemoryTaintedMem(Handle: HandleContext ;mem: HandleMemAcc ): Boolean ;cdecl;  external Triton_dll Name 'isMemoryTaintedMem';

        //! [**taint api**] - Returns true if the register is tainted.
        function isRegisterTainted(Handle: HandleContext ; reg: HandleReg): Boolean ;cdecl;  external Triton_dll Name 'isRegisterTainted';

        //! [**taint api**] - Sets the flag (taint or untaint) to an abstract operand
        //! (Register or Memory).
        function setTaint(Handle: HandleContext ; op: HandleOperandWrapper; flag : Boolean): Boolean ;cdecl;  external Triton_dll Name 'setTaint';

        //! [**taint api**] - Sets the flag (taint or untaint) to a memory.
        function setTaintMemory(Handle: HandleContext ; mem: HandleMemAcc ;flag : Boolean): Boolean ;cdecl;  external Triton_dll Name 'setTaintMemory';

        //! [**taint api**] - Sets the flag (taint or untaint) to a register.
        function setTaintRegister(Handle: HandleContext ; reg: HandleReg;flag : Boolean): Boolean ; cdecl;  external Triton_dll Name 'setTaintRegister';

        //! [**taint api**] - Taints an address. Returns TAINTED if the address has
        //! been tainted correctly. Otherwise it returns the last defined state.
        function taintMemory(Handle: HandleContext ; addr: uint64): Boolean ;cdecl;  external Triton_dll Name 'taintMemory';

        //! [**taint api**] - Taints a memory. Returns TAINTED if the memory has been
        //! tainted correctly. Otherwise it returns the last defined state.
        function taintMemoryMem(Handle: HandleContext ; mem: HandleMemAcc ): Boolean ;cdecl;  external Triton_dll Name 'taintMemoryMem';

        //! [**taint api**] - Taints a register. Returns TAINTED if the register has
        //! been tainted correctly. Otherwise it returns the last defined state.
        function taintRegister(Handle: HandleContext ; reg: HandleReg): Boolean ; cdecl;  external Triton_dll Name 'taintRegister';

        //! [**taint api**] - Untaints an address. Returns !TAINTED if the address has
        //! been untainted correctly. Otherwise it returns the last defined state.
        function untaintMemory(Handle: HandleContext ; addr: uint64): Boolean ; cdecl;  external Triton_dll Name 'untaintMemory';

        //! [**taint api**] - Untaints a memory. Returns !TAINTED if the memory has
        //! been untainted correctly. Otherwise it returns the last defined state.
        function untaintMemoryMem(Handle: HandleContext ; mem: HandleMemAcc ): Boolean ; cdecl;  external Triton_dll Name 'untaintMemoryMem';

        //! [**taint api**] - Untaints a register. Returns !TAINTED if the register
        //! has been untainted correctly. Otherwise it returns the last defined state.
        function untaintRegister(Handle: HandleContext ; reg: HandleReg): Boolean ;cdecl;  external Triton_dll Name 'untaintRegister';

        //! [**taint api**] - Abstract union tainting.
        function taintUnion(Handle: HandleContext ; op1,op2: HandleOperandWrapper): Boolean ;cdecl;  external Triton_dll Name 'taintUnion';

        //! [**taint api**] - Abstract assignment tainting.
        function taintAssignment(Handle: HandleContext; op1,op2: HandleOperandWrapper): Boolean ; cdecl;  external Triton_dll Name 'taintAssignment';

        //! [**taint api**] - Taints MemoryImmediate with union. Returns true if the
        //! memDst is TAINTED.
        function taintUnionMemoryImmediate(Handle: HandleContext ; Dst: HandleMemAcc; imm: HandleImmediate): Boolean ;cdecl;  external Triton_dll Name 'taintUnionMemoryImmediate';

        //! [**taint api**] - Taints MemoryMemory with union. Returns true if the
        //! memDst or memSrc are TAINTED.
        function taintUnionMemoryMemory(Handle: HandleContext ; Dst: HandleMemAcc; Src:HandleMemAcc): Boolean ;cdecl;  external Triton_dll Name 'taintUnionMemoryMemory';

        //! [**taint api**] - Taints MemoryRegister with union. Returns true if the
        //! memDst or regSrc are TAINTED.
        function taintUnionMemoryRegister(Handle: HandleContext ; Dst: HandleMemAcc; Src: HandleReg): Boolean ;cdecl;  external Triton_dll Name 'taintUnionMemoryRegister';

        //! [**taint api**] - Taints RegisterImmediate with union. Returns true if the
        //! regDst is TAINTED.
        function taintUnionRegisterImmediate(Handle: HandleContext ; Dst: HandleReg; imm: HandleImmediate): Boolean ;cdecl;  external Triton_dll Name 'taintUnionRegisterImmediate';

        //! [**taint api**] - Taints RegisterMemory with union. Returns true if the
        //! regDst or memSrc are TAINTED.
        function taintUnionRegisterMemory(Handle: HandleContext ; Dst: HandleReg; Src: HandleMemAcc): Boolean ; cdecl;  external Triton_dll Name 'taintUnionRegisterMemory';

        //! [**taint api**] - Taints RegisterRegister with union. Returns true if the
        //! regDst or regSrc are TAINTED.
        function taintUnionRegisterRegister(Handle: HandleContext ; Dst:HandleReg; Src:HandleReg): Boolean ;cdecl;  external Triton_dll Name 'taintUnionRegisterRegister';

        //! [**taint api**] - Taints MemoryImmediate with assignment. Returns always
        //! false.
        function taintAssignmentMemoryImmediate(Handle: HandleContext ; Dst:HandleMemAcc; imm: HandleImmediate): Boolean ; cdecl;  external Triton_dll Name 'taintAssignmentMemoryImmediate';

        //! [**taint api**] - Taints MemoryMemory with assignment. Returns true if the
        //! memDst is tainted.
        function taintAssignmentMemoryMemory(Handle: HandleContext ; Dst: HandleMemAcc; Src: HandleMemAcc): Boolean ;cdecl;  external Triton_dll Name 'taintAssignmentMemoryMemory';

        //! [**taint api**] - Taints MemoryRegister with assignment. Returns true if
        //! the memDst is tainted.
        function taintAssignmentMemoryRegister(Handle: HandleContext ; Dst: HandleMemAcc; Src: HandleReg): Boolean ;cdecl;  external Triton_dll Name 'taintAssignmentMemoryRegister';

        //! [**taint api**] - Taints RegisterImmediate with assignment. Returns always
        //! false.
        function taintAssignmentRegisterImmediate(Handle: HandleContext ; Dst: HandleReg; imm: HandleImmediate): Boolean ;cdecl;  external Triton_dll Name 'taintAssignmentRegisterImmediate';

        //! [**taint api**] - Taints RegisterMemory with assignment. Returns true if
        //! the regDst is tainted.
        function taintAssignmentRegisterMemory(Handle: HandleContext ; Dst: HandleReg; Src: HandleMemAcc ): Boolean ;cdecl;  external Triton_dll Name 'taintAssignmentRegisterMemory';

        //! [**taint api**] - Taints RegisterRegister with assignment. Returns true if
        //! the regDst is tainted.
        function taintAssignmentRegisterRegister(Handle: HandleContext ; Dst: HandleReg; Src: HandleReg): Boolean ;cdecl;  external Triton_dll Name 'taintAssignmentRegisterRegister';

     (* Synthesizer engine Context ============================================================================= *)

        function synthesize(Handle: HandleContext; node: HandleAbstractNode; constant: Boolean = True; subexpr: Boolean=True; opaque: Boolean= False): HandleSynthesisResult; external Triton_dll Name 'synthesize';

      (* Lifters engine Context ================================================================================= *)

        function NodeliftToLLVM(Handle: HandleContext; node: HandleAbstractNode; const fname: PAnsiChar; optimize: Boolean= False): PAnsiChar; cdecl;  external Triton_dll Name 'NodeliftToLLVM';

        function ExprliftToLLVM(Handle: HandleContext; expr: HandleSharedSymbolicExpression; const fname: PAnsiChar; optimize: Boolean = False): PAnsiChar; cdecl;  external Triton_dll Name 'ExprliftToLLVM';

        function liftToPython(Handle: HandleContext; expr: HandleSharedSymbolicExpression; icomment: Boolean= False): PAnsiChar; cdecl;  external Triton_dll Name 'liftToPython';

        function liftToSMT(Handle: HandleContext; expr: HandleSharedSymbolicExpression; assert_: Boolean=False; icomment: Boolean= False): PAnsiChar; cdecl;  external Triton_dll Name 'liftToSMT';

        function NodeliftToDot(Handle: HandleContext; node: HandleAbstractNode): PAnsiChar; cdecl;  external Triton_dll Name 'NodeliftToDot';

        function ExprliftToDot(Handle: HandleContext; expr: HandleSharedSymbolicExpression): PAnsiChar; cdecl;  external Triton_dll Name 'ExprliftToDot';

        function simplifyAstViaLLVM(Handle: HandleContext; node: HandleAbstractNode): HandleAbstractNode; cdecl;  external Triton_dll Name 'simplifyAstViaLLVM';


implementation

end.
