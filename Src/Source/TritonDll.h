#pragma once

#include <triton/register.hpp>
#include <triton/context.hpp>
#include <triton/ast.hpp>
#include <triton/astContext.hpp>
#include <triton/astRepresentation.hpp>
#include <triton/exceptions.hpp>
#include <triton/symbolicExpression.hpp>
#include <triton/symbolicVariable.hpp>
#include <triton/x8664Cpu.hpp>
#include <triton/x86Cpu.hpp>
#include <triton/archEnums.hpp>
#include <triton/llvmToTriton.hpp>
#include <llvm-c/Core.h>

using namespace triton::engines::symbolic;
using namespace triton::engines::taint;
using namespace triton::arch::x86;
using namespace triton::arch;
using namespace triton::ast;
using namespace triton;

typedef triton::Context               *HandleContext;
typedef triton::ast::AbstractNode     *hAbstractNode;

typedef triton::arch::BitsVector      *HandleBV;
typedef triton::arch::Register        *HandleReg;
typedef triton::arch::MemoryAccess    *HandleMemAcc;
typedef triton::arch::Immediate       *HandleImmediate;
typedef triton::arch::CpuInterface    *HandleCpuInterface;
typedef triton::arch::Instruction     *HandleInstruz;
typedef triton::arch::OperandWrapper  *HandleOperandWrapper;
typedef triton::ast::representations::AstRepresentation *hAstRepresentation;
typedef triton::arch::BasicBlock      *HandleBasicBlock;

typedef std::shared_ptr<triton::ast::AbstractNode>      *HandleAbstractNode;
typedef std::shared_ptr<triton::modes::Modes>           *hModes;
typedef SharedAstContext                                *HandleAstContext;
typedef triton::engines::synthesis::SynthesisResult    * HandleSynthesisResult;

typedef triton::engines::solver::SolverInterface                      *HandleSolverInterface;
typedef triton::engines::solver::SolverModel                          *HandleSolverModel;

typedef triton::engines::symbolic::SymbolicEngine                     *HandleSymbolicEngine;
typedef triton::engines::symbolic::SharedSymbolicExpression           *HandleSharedSymbolicExpression;
typedef triton::engines::symbolic::SharedSymbolicVariable             *HandleSharedSymbolicVariable;
typedef triton::engines::symbolic::PathConstraint                     *HandlePathConstraint;
typedef triton::engines::taint::TaintEngine                           *HandleTaintEngine;

typedef triton::ast::LLVMToTriton                                     *HLLVMToTriton;

typedef uint8 * retArray;

// Callback
typedef void(*cbGetMemVal) (HandleContext, HandleMemAcc);
typedef void(*cbGetRegVal) (HandleContext, HandleReg);
typedef void(*cbSetMemVal) (HandleContext, HandleMemAcc, uint64);
typedef void(*cbSetRegVal) (HandleContext, HandleReg,uint64);
typedef HandleAbstractNode(*cbSimplification) (HandleContext, HandleAbstractNode);


struct _BV {
	uint32    high;
	uint32    low;
	uint32    VectorSize;
	uint64    MaxValue;
};

struct _Imm {
	uint64    value;
	operand_e Tipo;
	uint32    Size;
	uint32    BitSize;
	_BV       BitVec;
};

struct _Reg {
	char * name;
	register_e id;
	register_e parent;
	operand_e  Tipo;
	uint32     Size;
	uint32     BitSize;
	_BV        BitVec;
};

struct _memAccess{
    uint64             address;
    uint64             pcRelative;
    HandleReg          segmentReg;
    HandleReg          baseReg;
    HandleReg          indexReg;
    HandleImmediate    displacement;
    HandleImmediate    scale;
    HandleAbstractNode leaAST;
    operand_e          Tipo;
    uint32             Size;
    uint32             BitSize;
    _BV                BitVec;
};

struct _OpWrapp {
	
   HandleImmediate imm;
   HandleMemAcc    mem;
   HandleReg       reg;
   operand_e       Tipo;
   uint32          Size;
   uint32          BitSize;
   uint32          high;
   uint32          low;
};

struct _Istruz {
	
   uint32  tid;
   uint64  address;
   uint64  nextAddress;
   char *  disassembly;
   const uint8 *  opcode;
   uint32  size;
   uint32  tipo;
   triton::arch::x86::prefix_e prefix;
   bool branch;
   bool controlFlow;
   bool conditionTaken;
   bool tainted;
};

struct  MemNode{
	HandleMemAcc  mem;
	HandleAbstractNode node;
};
struct RegNode {
	HandleReg  reg;
	HandleAbstractNode node;
};
struct  ImmNode {
	HandleImmediate  imm;
	HandleAbstractNode node;
};
struct  AddrSolver { //std::map<triton::uint32, triton::engines::solver::SolverModel>
	uint32            numEle;
	uint32            id;
	HandleSolverModel Model;
};
struct  IdSymExpr { //std::map<triton::usize, triton::engines::symbolic::SharedSymbolicExpression>
	usize                          id;
	HandleSharedSymbolicExpression SymExpr;
};
struct  IdSymVar { //std::unordered_map<triton::usize, triton::engines::symbolic::SharedSymbolicVariable>
	usize                          id;
	HandleSharedSymbolicVariable   SymVar;
};
struct  RegSymE { //std::map<triton::arch::register_e, triton::engines::symbolic::SharedSymbolicExpression> 
	register_e                     regId;
	HandleSharedSymbolicExpression RegSym;
};
struct  MemSymE { //std::map<triton::uint64, triton::engines::symbolic::SharedSymbolicExpression> 
	uint64                         mem;
	HandleSharedSymbolicExpression MemSym;
};
struct  RegIdReg { //	std::unordered_map<triton::arch::register_e, const triton::arch::Register>
	triton::arch::register_e       regId;
	HandleReg           	       Reg;
};
struct PathDat { //std::vector<std::tuple<bool, triton::uint64, triton::uint64, HandleAbstractNode>>
	bool   taken;
	uint64 srcAddr;
	uint64 dstAddr;
	HandleAbstractNode pc;
};

class Snapshot {
private:
  // Main Triton API
  Context* Api;
  // Snapshot of the symbolic engine
  SymbolicEngine* SnapshotSymEngine;
  // Snapshot of the taint engine
  TaintEngine* SnapshotTaintEngine;
  // AST Context
  AstContext* AstCtx;
  // Snapshot of triton CPU
  x8664Cpu* Cpu_x8664;
  x86Cpu* Cpu_x86;
  // Bitness
  bool Is64Bit;

public:
  // Constructor
  Snapshot(Context* Api, bool Is64Bit);
  // Destructor
  ~Snapshot();
  // Resets the snapshot engine
  void resetEngine();
  // Restores a snapshot
  void restoreSnapshot();
  // Takes a snapshot
  void takeSnapshot();
};

typedef Snapshot* HandleSnapshot;

// define a macro for the calling convention and export type
#define EXPORTCALL  __declspec(dllexport) 

extern "C"
{
	//! [**Max Function**] - 
	_Imm EXPORTCALL RefImmToImm(HandleImmediate HImm);

	//! [**Max Function 1**] - 
	_BV EXPORTCALL RefBVToBV(HandleBV HBv);

	//! [**Max Function 2**] - 
	_Reg EXPORTCALL RefRegToReg(HandleReg HReg);

	//! [**Max Function 3**] - 
	_memAccess EXPORTCALL RefMemToMem(HandleMemAcc HMem);

	//! [**Max Function 4**] - 
	_OpWrapp EXPORTCALL RefOpWrapToOpWrap(HandleOperandWrapper HOpW);

	// ![**Max Function 5 * *] -
	_Istruz EXPORTCALL RefIstruzToIstruz(HandleInstruz HOpW);

  /* Snapshot =======================================================================================*/

  // Constructor
  HandleSnapshot EXPORTCALL CreaSnapshot(HandleContext Handle, bool Is64Bit);

  // Destructor
  void EXPORTCALL DeleteSnapshot(HandleSnapshot Handle);

  // Resets the snapshot engine
  void EXPORTCALL snap_resetEngine(HandleSnapshot Handle);

  // Restores a snapshot
  void EXPORTCALL snap_restoreSnapshot(HandleSnapshot Handle);

  // Takes a snapshot
  void EXPORTCALL snap_takeSnapshot(HandleSnapshot Handle);

	/* Architecture API ============================================================================== */

    HandleContext  EXPORTCALL CreateApi(void);
	
	void  EXPORTCALL DeleteApi(HandleContext Handle);
	
	void  EXPORTCALL setArchitecture(HandleContext Handle, triton::arch::architecture_e arch);
	
	triton::arch::architecture_e  EXPORTCALL GetArchitecture(HandleContext Handle);
	
	bool  EXPORTCALL isArchitectureValid(HandleContext Handle);
	
	void  EXPORTCALL clearArchitecture(HandleContext Handle);
	
	triton::arch::endianness_e  EXPORTCALL getEndianness(HandleContext Handle);
	
	HandleCpuInterface  EXPORTCALL getCpuInstance(HandleContext Handle);
	
	bool  EXPORTCALL isFlag(HandleContext Handle, triton::arch::register_e regId);
	
	bool  EXPORTCALL isFlagR(HandleContext Handle, HandleReg reg);
	
	bool  EXPORTCALL isRegister(HandleContext Handle, triton::arch::register_e regId);
	
	bool  EXPORTCALL isRegisterR(HandleContext Handle, HandleReg reg);
	
	HandleReg  EXPORTCALL getRegister(HandleContext Handle, register_e id);
	
	HandleReg  EXPORTCALL getParentRegisterR(HandleContext Handle, HandleReg reg);
	
	HandleReg  EXPORTCALL getParentRegister(HandleContext Handle, triton::arch::register_e id);
	
	bool  EXPORTCALL isRegisterValid(HandleContext Handle, triton::arch::register_e id);
	
	bool  EXPORTCALL isRegisterValidR(HandleContext Handle, HandleReg reg);
	
	triton::uint32  EXPORTCALL getGprBitSize(HandleContext Handle);
	
	triton::uint32  EXPORTCALL getGprSize(HandleContext Handle);
	
	triton::uint32  EXPORTCALL getNumberOfRegisters(HandleContext Handle);
	
	//! [**architecture api**] - Returns the concrete value of a memory cell.
	triton::uint8   EXPORTCALL getConcreteMemoryValueByte(HandleContext Handle, triton::uint64 addr, bool execCallbacks = true);
	
	//! [**architecture api**] - Returns the concrete value of memory cells.// not support 512
	triton::uint64   EXPORTCALL getConcreteMemoryValue(HandleContext Handle, HandleMemAcc mem, bool execCallbacks = true);
	
	//! [**architecture api**] - Returns the concrete value of a memory area.
	retArray   EXPORTCALL getConcreteMemoryAreaValue(HandleContext Handle, triton::uint64 baseAddr, triton::usize size, bool execCallbacks = true);
	
	//! [**architecture api**] - Returns the concrete value of a register. // not support 512
	triton::uint64   EXPORTCALL getConcreteRegisterValue(HandleContext Handle, HandleReg reg, bool execCallbacks = true);
	
	void  EXPORTCALL setConcreteMemoryValueByte(HandleContext Handle, triton::uint64 addr, triton::uint8 value);
	
	// not support 512
	void  EXPORTCALL setConcreteMemoryValue(HandleContext Handle, HandleMemAcc mem, triton::uint64 value);
	
	// eliminata -utilizzo setConcreteMemoryAreaValue(HandleContext Handle, triton::uint64 baseAddr, triton::uint8* area, triton::usize size)
	//void  EXPORTCALL setConcreteMemoryAreaValueByte(HandleContext Handle, triton::uint64 baseAddr, triton::uint8* values);
	
	void  EXPORTCALL setConcreteMemoryAreaValue(HandleContext Handle, triton::uint64 baseAddr, triton::uint8* area, triton::usize size);
	
	// not support 512
	void  EXPORTCALL setConcreteRegisterValue(HandleContext Handle, HandleReg reg, triton::uint64 value);
	
	//! [**architecture api**] - Returns true if the range `[baseAddr:size]` is mapped into the internal memory representation. \sa getConcreteMemoryValue() and getConcreteMemoryAreaValue().
	bool  EXPORTCALL isConcreteMemoryValueDefined(HandleContext Handle, triton::uint64 baseAddr, triton::usize size = 1);
	
	//! [**architecture api**] - Removes the range `[baseAddr:size]` from the internal memory representation. \sa isMemoryMapped().
	void   EXPORTCALL clearConcreteMemoryValue(HandleContext Handle, triton::uint64 baseAddr, triton::usize size = 1);
	
  //! [**architecture api**] - Disassembles the instruction and setup operands.
	void  EXPORTCALL disassembly(HandleContext Handle, HandleInstruz inst);
	
  //! [**architecture api**] - Disassembles a block of instructions. You must define an architecture before.
  void EXPORTCALL disassembly_BB(HandleContext Handle, HandleBasicBlock block, triton::uint64 addr = 0);
	
	//! [**architecture api**] - Returns all registers. \sa triton::arch::x86::register_e.
	uint32 EXPORTCALL getAllRegisters(HandleContext Handle, RegIdReg **Regs) ;

	//! [**architecture api**] - Returns all parent registers. \sa triton::arch::x86::register_e.
	uint32 EXPORTCALL getParentRegisters(HandleContext Handle, HandleReg*& outRegs) ;
	

	/* Processing API ================================================================================ */

	//! [**proccesing api**] - Processes an instruction and updates engines according to the instruction semantics. Returns true if the instruction is supported.
  exception_e EXPORTCALL processing(HandleContext Handle, HandleInstruz inst);

  //! [**proccesing api**] - Processes a block of instructions and updates engines according to instructions semantics. Returns `triton::arch::NO_FAULT` if succeed.
  exception_e EXPORTCALL processing_BB(HandleContext Handle, HandleBasicBlock block, triton::uint64 addr = 0);
	
	//! [**proccesing api**] - Initializes everything.
	void   EXPORTCALL initEngines(HandleContext Handle);
	
	//! [**proccesing api**] - Removes everything.
	void   EXPORTCALL removeEngines(HandleContext Handle);
	
	//! [**proccesing api**] - Resets everything.
	void  EXPORTCALL reset(HandleContext Handle);
	
	
	/* IR API ======================================================================================== */
  	
	//! [**IR builder api**] - Builds the instruction semantics. Returns true if the instruction is supported. You must define an architecture before. \sa processing().
  exception_e EXPORTCALL buildSemantics(HandleContext Handle, HandleInstruz inst);
	
  //! [**IR builder api**] - Builds the instructions semantics of a block. Returns `triton::arch::NO_FAULT` if succeed.
  exception_e EXPORTCALL buildSemantics_BB(HandleContext Handle, HandleBasicBlock block);

	//! [**IR builder api**] - Returns the AST context. Used as AST builder.
	HandleAstContext   EXPORTCALL getAstContext(HandleContext Handle);

	/* AST Representation API ======================================================================== */

	//! [**AST representation api**] - Returns the AST representation mode as triton::ast::representations::mode_e.
	triton::uint32 EXPORTCALL getAstRepresentationMode(HandleContext Handle) ;

	//! [**AST representation api**] - Sets the AST representation mode.
	void EXPORTCALL setAstRepresentationMode(HandleContext Handle, triton::uint32 mode);

	/* Callbacks API ================================================================================= */
	
	//! [**callbacks api**] - Adds a GET_CONCRETE_MEMORY_VALUE callback (LOAD).
	void EXPORTCALL addCallbackGetMem(HandleContext Handle, cbGetMemVal cb);

	//! [**callbacks api**] - Adds a GET_CONCRETE_REGISTER_VALUE callback (GET).
	void EXPORTCALL  addCallbackGetReg(HandleContext Handle, cbGetRegVal cb);

	//! [**callbacks api**] - Adds a SET_CONCRETE_MEMORY_VALUE callback (STORE).
	void EXPORTCALL addCallbackSetMem(HandleContext Handle, cbSetMemVal cb);

	//! [**callbacks api**] - Adds a SET_CONCRETE_REGISTER_VALUE callback (PUT).
	void EXPORTCALL addCallbackSetReg(HandleContext Handle, cbSetRegVal cb);

	//! [**callbacks api**] - Adds a SYMBOLIC_SIMPLIFICATION callback.
	void EXPORTCALL addCallbackSimplif(HandleContext Handle, cbSimplification cb);

	//! [**callbacks api**] - Removes all recorded callbacks.
	void EXPORTCALL removeAllCallbacks(HandleContext Handle);

	//! [**callbacks api**] - Deletes a GET_CONCRETE_MEMORY_VALUE callback (LOAD).
	void EXPORTCALL removeCallbackGetMem(HandleContext Handle, cbGetMemVal cb);

	//! [**callbacks api**] - Deletes a GET_CONCRETE_REGISTER_VALUE callback (GET).
	void EXPORTCALL removeCallbackGetReg(HandleContext Handle, cbGetRegVal cb);

	//! [**callbacks api**] - Deletes a SET_CONCRETE_MEMORY_VALUE callback (STORE).
	void EXPORTCALL removeCallbackSetMem(HandleContext Handle, cbSetMemVal cb);

	//! [**callbacks api**] - Deletes a SET_CONCRETE_REGISTER_VALUE callback (PUT).
	void EXPORTCALL removeCallbackSetReg(HandleContext Handle, cbSetRegVal cb);

	//! [**callbacks api**] - Deletes a SYMBOLIC_SIMPLIFICATION callback.
	void EXPORTCALL removeCallbackSimplif(HandleContext Handle, cbSimplification cb);

	//! [**callbacks api**] - Processes callbacks according to the kind and the C++ polymorphism.
	HandleAbstractNode EXPORTCALL processCallbacks(HandleContext Handle, triton::callbacks::callback_e kind, HandleAbstractNode node);

	//! [**callbacks api**] - Processes callbacks according to the kind and the C++ polymorphism.
	void EXPORTCALL processCallbacksMem(HandleContext Handle, triton::callbacks::callback_e kind, HandleMemAcc mem);

	//! [**callbacks api**] - Processes callbacks according to the kind and the C++ polymorphism.
	void EXPORTCALL processCallbacksReg(HandleContext Handle, triton::callbacks::callback_e kind, HandleReg reg);

	/* Modes API====================================================================================== */

	//! [**modes api**] - Raises an exception if modes interface is not initialized.
	void EXPORTCALL checkModes(HandleContext Handle) ;

	//! [**modes api**] - Enables or disables a specific mode.
	void EXPORTCALL setMode(HandleContext Handle, triton::modes::mode_e mode, bool flag);

	//! [**modes api**] - Returns true if the mode is enabled.
	bool EXPORTCALL isModeEnabled(HandleContext Handle, triton::modes::mode_e mode) ;


	/* Symbolic engine API =========================================================================== */
  
	//! [**symbolic api**] - Returns the instance of the symbolic engine.
	HandleSymbolicEngine EXPORTCALL getSymbolicEngine(HandleContext Handle);

	//! [**symbolic api**] - Returns the map of symbolic registers defined.
	uint32 EXPORTCALL getSymbolicRegisters(HandleContext Handle,RegSymE **OutRegE);

	//! [**symbolic api**] - Returns the map (<Addr : SymExpr>) of symbolic memory defined.
	uint32 EXPORTCALL  getSymbolicMemory(HandleContext Handle, MemSymE **ouMemSym) ;

	//! [**symbolic api**] - Returns the shared symbolic expression corresponding to the memory address.
	HandleSharedSymbolicExpression EXPORTCALL getSymbolicMemoryAddr(HandleContext Handle, triton::uint64 addr) ;

	//! [**symbolic api**] - Returns the shared symbolic expression corresponding to the parent register.
	HandleSharedSymbolicExpression EXPORTCALL getSymbolicRegister(HandleContext Handle, HandleReg reg) ;

	//! [**symbolic api**] - Returns the symbolic memory value.
	triton::uint8 EXPORTCALL getSymbolicMemoryValue(HandleContext Handle, triton::uint64 address);

	//! [**symbolic api**] - Returns the symbolic memory value.
	uint64 EXPORTCALL getSymbolicMemoryValueM(HandleContext Handle, HandleMemAcc mem);

	//! [**symbolic api**] - Returns the symbolic values of a memory area.
	retArray EXPORTCALL getSymbolicMemoryAreaValue(HandleContext Handle, triton::uint64 baseAddr, triton::usize size);

	//! [**symbolic api**] - Returns the symbolic register value.
	uint64 EXPORTCALL getSymbolicRegisterValue(HandleContext Handle, HandleReg reg);

	//! [**symbolic api**] - Converts a symbolic expression to a symbolic variable. `symVarSize` must be in bits.
	HandleSharedSymbolicVariable EXPORTCALL symbolizeExpression(HandleContext Handle, triton::usize exprId, triton::uint32 symVarSize, char* symVarComment = NULL);

	//! [**symbolic api**] - Converts a symbolic memory expression to a symbolic variable.
	HandleSharedSymbolicVariable EXPORTCALL symbolizeMemory(HandleContext Handle, HandleMemAcc mem, char* symVarComment = NULL);

	//! [**symbolic api**] - Converts a symbolic register expression to a symbolic variable.
	HandleSharedSymbolicVariable EXPORTCALL symbolizeRegister(HandleContext Handle, HandleReg reg, char* symVarComment = NULL);

	//! [**symbolic api**] - Returns the AST corresponding to the operand.
	HandleAbstractNode EXPORTCALL getOperandAst(HandleContext Handle, HandleOperandWrapper op);

	//! [**symbolic api**] - Returns the AST corresponding to the operand.
	HandleAbstractNode EXPORTCALL getOperandAstIstruz(HandleContext Handle, HandleInstruz inst, HandleOperandWrapper op);

	//! [**symbolic api**] - Returns the AST corresponding to the immediate.
	HandleAbstractNode EXPORTCALL getImmediateAst(HandleContext Handle, HandleImmediate imm);

	//! [**symbolic api**] - Returns the AST corresponding to the immediate and defines the immediate as input of the instruction..
	HandleAbstractNode EXPORTCALL getImmediateAstIstruz(HandleContext Handle, HandleInstruz inst, HandleImmediate imm);

	//! [**symbolic api**] - Returns the AST corresponding to the memory.
	HandleAbstractNode EXPORTCALL getMemoryAst(HandleContext Handle, HandleMemAcc mem);

	//! [**symbolic api**] - Returns the AST corresponding to the memory and defines the memory cell as input of the instruction.
	HandleAbstractNode EXPORTCALL getMemoryAstIstruz(HandleContext Handle, HandleInstruz inst, HandleMemAcc mem);

	//! [**symbolic api**] - Returns the AST corresponding to the register.
	HandleAbstractNode EXPORTCALL getRegisterAst(HandleContext Handle, HandleReg reg);

	//! [**symbolic api**] - Returns the AST corresponding to the register and defines the register as input of the instruction.
	HandleAbstractNode EXPORTCALL getRegisterAstIstruz(HandleContext Handle, HandleInstruz inst, HandleReg reg);

	//! [**symbolic api**] - Returns a new shared symbolic expression. Note that if there are simplification passes recorded, simplification will be applied.
	HandleSharedSymbolicExpression EXPORTCALL newSymbolicExpression(HandleContext Handle, HandleAbstractNode node, char* comment = NULL);

	//! [**symbolic api**] - Returns a new symbolic variable.
	HandleSharedSymbolicVariable EXPORTCALL newSymbolicVariable(HandleContext Handle, triton::uint32 varSize, char* comment = NULL);

	//! [**symbolic api**] - Removes the symbolic expression corresponding to the id.
	void EXPORTCALL removeSymbolicExpression(HandleContext Handle, HandleSharedSymbolicExpression symExpr);

	//! [**symbolic api**] - Returns the new shared symbolic abstract expression and links this expression to the instruction.
	HandleSharedSymbolicExpression EXPORTCALL createSymbolicExpression(HandleContext Handle, HandleInstruz inst, HandleAbstractNode node, HandleOperandWrapper dst, char* comment = NULL);

	//! [**symbolic api**] - Returns the new shared symbolic memory expression and links this expression to the instruction.
	HandleSharedSymbolicExpression EXPORTCALL createSymbolicMemoryExpression(HandleContext Handle, HandleInstruz inst, HandleAbstractNode node, HandleMemAcc mem, char* comment = NULL);

	//! [**symbolic api**] - Returns the new shared symbolic register expression and links this expression to the instruction.
	HandleSharedSymbolicExpression EXPORTCALL createSymbolicRegisterExpression(HandleContext Handle, HandleInstruz inst, HandleAbstractNode node, HandleReg reg, char* comment = NULL);

	//! [**symbolic api**] - Returns the new shared symbolic volatile expression and links this expression to the instruction.
	HandleSharedSymbolicExpression EXPORTCALL createSymbolicVolatileExpression(HandleContext Handle, HandleInstruz inst, HandleAbstractNode node, char* comment = NULL);

	//! [**symbolic api**] - Assigns a symbolic expression to a memory.
	void EXPORTCALL assignSymbolicExpressionToMemory(HandleContext Handle, HandleSharedSymbolicExpression se, HandleMemAcc mem);

	//! [**symbolic api**] - Assigns a symbolic expression to a register.
	void EXPORTCALL assignSymbolicExpressionToRegister(HandleContext Handle, HandleSharedSymbolicExpression se, HandleReg reg);

	//! [**symbolic api**] - Processes all recorded simplifications. Returns the simplified node.
  HandleAbstractNode EXPORTCALL simplify(HandleContext Handle, HandleAbstractNode node, bool z3 = false);

  //! [**symbolic api**] - Processes a dead store elimination simplification on a given basic block. If `padding` is true, keep addresses aligned and padds with NOP instructions.
  HandleBasicBlock EXPORTCALL simplify_BB(HandleContext Handle, HandleBasicBlock block, bool padding = false);
 
	//! [**symbolic api**] - Returns the shared symbolic expression corresponding to an id.
	HandleSharedSymbolicExpression EXPORTCALL getSymbolicExpressionFromId(HandleContext Handle, triton::usize symExprId) ;

	//! [**symbolic api**] - Returns the symbolic variable corresponding to the symbolic variable id.
	HandleSharedSymbolicVariable EXPORTCALL getSymbolicVariableFromId(HandleContext Handle, triton::usize symVarId);

	//! [**symbolic api**] - Returns the symbolic variable corresponding to the symbolic variable name.
	HandleSharedSymbolicVariable EXPORTCALL getSymbolicVariableFromName(HandleContext Handle, char* symVarName);

	//! [**symbolic api**] - Returns the logical conjunction vector of path constraints.
	uint32 EXPORTCALL getPathConstraints(HandleContext Handle, HandlePathConstraint *& outPath);

	//! [**symbolic api**] - Returns the logical conjunction AST of path constraints.
	HandleAbstractNode EXPORTCALL getPathPredicate(HandleContext Handle);

	//! [**symbolic api**] - Adds a path constraint.
	void EXPORTCALL pushPathConstraint(HandleContext Handle, HandleAbstractNode node);

  //! [**symbolic api**] - Adds a path constraint.
  void EXPORTCALL pushPathConstraintPco(HandleContext Handle, HandlePathConstraint pco);

	//! [**symbolic api**] - Clears the logical conjunction vector of path constraints.
	void EXPORTCALL clearPathConstraints(HandleContext Handle);
	
	//! [**symbolic api**] - Returns true if the symbolic expression ID exists.
	bool EXPORTCALL isSymbolicExpressionIdExists(HandleContext Handle, triton::usize symExprId) ;

	//! [**symbolic api**] - Returns true if memory cell expressions contain symbolic variables.
	bool EXPORTCALL isMemorySymbolized(HandleContext Handle, HandleMemAcc mem) ;

	//! [**symbolic api**] - Returns true if memory cell expressions contain symbolic variables.
	bool EXPORTCALL isMemorySymbolizedSize(HandleContext Handle, triton::uint64 addr, triton::uint32 size = 1) ;

	//! [**symbolic api**] - Returns true if the register expression contains a symbolic variable.
	bool EXPORTCALL isRegisterSymbolized(HandleContext Handle, HandleReg reg) ;

	//! [**symbolic api**] - Concretizes all symbolic memory references.
	void EXPORTCALL concretizeAllMemory(HandleContext Handle);

	//! [**symbolic api**] - Concretizes all symbolic register references.
	void EXPORTCALL concretizeAllRegister(HandleContext Handle);

	//! [**symbolic api**] - Concretizes a specific symbolic memory reference.
	void EXPORTCALL concretizeMemoryM(HandleContext Handle, HandleMemAcc mem);

	//! [**symbolic api**] - Concretizes a specific symbolic memory reference.
	void EXPORTCALL concretizeMemory(HandleContext Handle, triton::uint64 addr);

	//! [**symbolic api**] - Concretizes a specific symbolic register reference.
	void EXPORTCALL concretizeRegister(HandleContext Handle, HandleReg reg);

	//! [**symbolic api**] - Slices all expressions from a given one.
	uint32 EXPORTCALL sliceExpressions(HandleContext Handle, HandleSharedSymbolicExpression expr, IdSymExpr **outSlice);

	//! [**symbolic api**] - Returns the list of the tainted symbolic expressions.
	uint32 EXPORTCALL  getTaintedSymbolicExpressions(HandleContext Handle, HandleSharedSymbolicExpression *& outSimbolicExp);

	//! [**symbolic api**] - Returns all symbolic expressions as a map of <SymExprId : SymExpr>
	uint32 EXPORTCALL getSymbolicExpressions(HandleContext Handle, IdSymExpr **outSymMap) ;

	//! [**symbolic api**] - Returns all symbolic variables as a map of <SymVarId : SymVar>
	uint32 EXPORTCALL getSymbolicVariables(HandleContext Handle, IdSymVar **outSymVar) ;

	//! [**symbolic api**] - Gets the concrete value of a symbolic variable.
	uint64 EXPORTCALL getConcreteVariableValue(HandleContext Handle, HandleSharedSymbolicVariable symVar) ;

	//! [**symbolic api**] - Sets the concrete value of a symbolic variable.
	void EXPORTCALL setConcreteVariableValue(HandleContext Handle, HandleSharedSymbolicVariable symVar, const triton::uint64 value);

	//! Initializes the memory access AST (LOAD and STORE).
	void EXPORTCALL initLeaAst(HandleContext Handle, HandleMemAcc mem, bool force = false);

	/* Solver engine API ============================================================================= */
  	
	/*!
	* \brief [**solver api**] - Computes and returns a model from a symbolic constraint.
	*
	* \details
	* **item1**: symbolic variable id<br>
	* **item2**: model
	*/
	uint32 EXPORTCALL getModel(HandleContext Handle,HandleAbstractNode node, AddrSolver** outModel, triton::uint32* status= nullptr, triton::uint32 timeout= 0, triton::uint32* solvingTime = nullptr) ;

	/*!
	* \brief [**solver api**] - Computes and returns several models from a symbolic constraint. The `limit` is the number of models returned.
	*
	* \details
	* **item1**: symbolic variable id<br>
	* **item2**: model
	*/
	uint32 EXPORTCALL getModels(HandleContext Handle, HandleAbstractNode node, triton::uint32 limit, AddrSolver ***outModels);

	//! Returns true if an expression is satisfiable.
	bool EXPORTCALL isSat(HandleContext Handle, HandleAbstractNode node) ;

	//! Returns the kind of solver as triton::engines::solver::solver_e.
	triton::engines::solver::solver_e EXPORTCALL getSolver(HandleContext Handle) ;

	//! Returns the instance of the initialized solver
	//HandleSolverInterface EXPORTCALL getSolverInstance(HandleContext Handle) ;

	//! Initializes a predefined solver.
	void EXPORTCALL setSolver(HandleContext Handle, triton::engines::solver::solver_e kind);

	//! Initializes a custom solver.
	void EXPORTCALL setCustomSolver(HandleContext Handle, HandleSolverInterface customSolver);

	//! Returns true if the solver is valid.
	bool EXPORTCALL isSolverValid(HandleContext Handle) ;

	//! [**solver api**] - Evaluates a Triton's AST via Z3 and returns a concrete value.
	triton::uint64 EXPORTCALL evaluateAstViaSolver(HandleContext Handle, HandleAbstractNode node) ;

	//! [**solver api**] - Converts a Triton's AST to a Z3's AST, perform a Z3 simplification and returns a Triton's AST.
	HandleAbstractNode EXPORTCALL simplifyAstViaSolver(HandleContext Handle, HandleAbstractNode node) ;

	/* Taint engine API
	* ==============================================================================
	*/
  
	//! [**taint api**] - Returns the instance of the taint engine.
	HandleTaintEngine EXPORTCALL getTaintEngine(HandleContext Handle);

	//! [**taint api**] - Returns the tainted addresses.
	uint32 EXPORTCALL getTaintedMemory(HandleContext Handle, uint64 *& outMemAddrs);

	//! [**taint api**] - Returns the tainted registers.
	uint32 EXPORTCALL getTaintedRegisters(HandleContext Handle, HandleReg*& outRegs);
		
	//! [**taint api**] - Abstract taint verification. Returns true if the operand
	//! is tainted.
	bool EXPORTCALL isTainted(HandleContext Handle, HandleOperandWrapper op) ;

	//! [**taint api**] - Returns true if the address:size is tainted.
	bool EXPORTCALL isMemoryTainted(HandleContext Handle,triton::uint64 addr,triton::uint32 size = 1) ;

	//! [**taint api**] - Returns true if the memory is tainted.
	bool EXPORTCALL isMemoryTaintedMem(HandleContext Handle,HandleMemAcc mem) ;

	//! [**taint api**] - Returns true if the register is tainted.
	bool EXPORTCALL isRegisterTainted(HandleContext Handle, HandleReg reg) ;

	//! [**taint api**] - Sets the flag (taint or untaint) to an abstract operand
	//! (Register or Memory).
	bool EXPORTCALL setTaint(HandleContext Handle, HandleOperandWrapper op,bool flag);

	//! [**taint api**] - Sets the flag (taint or untaint) to a memory.
	bool EXPORTCALL setTaintMemory(HandleContext Handle, HandleMemAcc mem,bool flag);

	//! [**taint api**] - Sets the flag (taint or untaint) to a register.
	bool EXPORTCALL setTaintRegister(HandleContext Handle, HandleReg reg,bool flag);

	//! [**taint api**] - Taints an address. Returns TAINTED if the address has
	//! been tainted correctly. Otherwise it returns the last defined state.
	bool EXPORTCALL taintMemory(HandleContext Handle, triton::uint64 addr);

	//! [**taint api**] - Taints a memory. Returns TAINTED if the memory has been
	//! tainted correctly. Otherwise it returns the last defined state.
	bool EXPORTCALL taintMemoryMem(HandleContext Handle, HandleMemAcc mem);

	//! [**taint api**] - Taints a register. Returns TAINTED if the register has
	//! been tainted correctly. Otherwise it returns the last defined state.
	bool EXPORTCALL taintRegister(HandleContext Handle, HandleReg reg);

	//! [**taint api**] - Untaints an address. Returns !TAINTED if the address has
	//! been untainted correctly. Otherwise it returns the last defined state.
	bool EXPORTCALL untaintMemory(HandleContext Handle, triton::uint64 addr);

	//! [**taint api**] - Untaints a memory. Returns !TAINTED if the memory has
	//! been untainted correctly. Otherwise it returns the last defined state.
	bool EXPORTCALL untaintMemoryMem(HandleContext Handle, HandleMemAcc mem);

	//! [**taint api**] - Untaints a register. Returns !TAINTED if the register
	//! has been untainted correctly. Otherwise it returns the last defined state.
	bool EXPORTCALL untaintRegister(HandleContext Handle, HandleReg reg);

	//! [**taint api**] - Abstract union tainting.
	bool EXPORTCALL taintUnion(HandleContext Handle, HandleOperandWrapper op1, HandleOperandWrapper op2);

	//! [**taint api**] - Abstract assignment tainting.
	bool EXPORTCALL taintAssignment(HandleContext Handle, HandleOperandWrapper op1, HandleOperandWrapper op2);

  //! [**taint api**] - Taints MemoryImmediate with union. Returns true if the memDst is TAINTED.
	bool EXPORTCALL taintUnionMemoryImmediate(HandleContext Handle, HandleMemAcc memDst, HandleImmediate imm);

	//! [**taint api**] - Taints MemoryMemory with union. Returns true if the
	//! memDst or memSrc are TAINTED.
	bool EXPORTCALL taintUnionMemoryMemory(HandleContext Handle, HandleMemAcc memDst, HandleMemAcc memSrc);

	//! [**taint api**] - Taints MemoryRegister with union. Returns true if the
	//! memDst or regSrc are TAINTED.
	bool EXPORTCALL taintUnionMemoryRegister(HandleContext Handle, HandleMemAcc memDst, HandleReg regSrc);

	//! [**taint api**] - Taints RegisterImmediate with union. Returns true if the
	//! regDst is TAINTED.
	bool EXPORTCALL taintUnionRegisterImmediate(HandleContext Handle, HandleReg regDst, HandleImmediate imm);

	//! [**taint api**] - Taints RegisterMemory with union. Returns true if the
	//! regDst or memSrc are TAINTED.
	bool EXPORTCALL taintUnionRegisterMemory(HandleContext Handle, HandleReg regDst, HandleMemAcc memSrc);

	//! [**taint api**] - Taints RegisterRegister with union. Returns true if the
	//! regDst or regSrc are TAINTED.
	bool EXPORTCALL taintUnionRegisterRegister(HandleContext Handle, HandleReg regDst, HandleReg regSrc);

	//! [**taint api**] - Taints MemoryImmediate with assignment. Returns always
	//! false.
	bool EXPORTCALL taintAssignmentMemoryImmediate(HandleContext Handle, HandleMemAcc memDst, HandleImmediate imm);

	//! [**taint api**] - Taints MemoryMemory with assignment. Returns true if the
	//! memDst is tainted.
	bool EXPORTCALL taintAssignmentMemoryMemory(HandleContext Handle, HandleMemAcc memDst, HandleMemAcc memSrc);

	//! [**taint api**] - Taints MemoryRegister with assignment. Returns true if
	//! the memDst is tainted.
	bool EXPORTCALL taintAssignmentMemoryRegister(HandleContext Handle, HandleMemAcc memDst, HandleReg regSrc);

	//! [**taint api**] - Taints RegisterImmediate with assignment. Returns always
	//! false.
	bool EXPORTCALL taintAssignmentRegisterImmediate(HandleContext Handle, HandleReg regDst, HandleImmediate imm);

	//! [**taint api**] - Taints RegisterMemory with assignment. Returns true if
	//! the regDst is tainted.
	bool EXPORTCALL taintAssignmentRegisterMemory(HandleContext Handle, HandleReg regDst, HandleMemAcc memSrc);

	//! [**taint api**] - Taints RegisterRegister with assignment. Returns true if
	//! the regDst is tainted.
	bool EXPORTCALL taintAssignmentRegisterRegister(HandleContext Handle, HandleReg regDst, HandleReg regSrc);

	/* Synthesizer engine Context ============================================================================= */

	HandleSynthesisResult EXPORTCALL synthesize(HandleContext Handle, HandleAbstractNode node, bool constant, bool subexpr, bool opaque);

	/* Lifters engine Context ================================================================================= */

	EXPORTCALL char *NodeliftToLLVM(HandleContext Handle, HandleAbstractNode node, const char* fname, bool optimize);
	
	EXPORTCALL char *ExprliftToLLVM(HandleContext Handle, HandleSharedSymbolicExpression expr, const char* fname, bool optimize);

	EXPORTCALL char *liftToPython(HandleContext Handle, HandleSharedSymbolicExpression expr, bool icomment);

	EXPORTCALL char *liftToSMT(HandleContext Handle, HandleSharedSymbolicExpression expr, bool assert_, bool icomment);

	EXPORTCALL char *NodeliftToDot(HandleContext Handle, HandleAbstractNode node);

	EXPORTCALL char *ExprliftToDot(HandleContext Handle, HandleSharedSymbolicExpression expr);

	HandleAbstractNode EXPORTCALL simplifyAstViaLLVM(HandleContext Handle, HandleAbstractNode node);

	/*  BitsVector ========================================================================== */

	//! Constructor.
	HandleBV EXPORTCALL BVCreateBitsVector();

	//! Constructor.
	HandleBV EXPORTCALL BVCreateBitsVectorHL(triton::uint32 high, triton::uint32 low);

	//! Constructor by copy.
	HandleBV EXPORTCALL BVCreateBitsVectorFrom(HandleBV other);

	//! Destructor.
	void  EXPORTCALL BVDelete(HandleBV Handle);

	//! Sets the highest bit position
	void EXPORTCALL  BVsetHigh(HandleBV bv,triton::uint32 v);

	//! Sets the lower bit position
	void EXPORTCALL  BVsetLow(HandleBV bv, triton::uint32 v);


	/*  Immediate =========================================================================== */

	//! Constructor.
	HandleImmediate EXPORTCALL IMMCreateImmediate();

	//! Constructor.
	HandleImmediate EXPORTCALL IMMCreateImmediateS(triton::uint64 value, triton::uint32 size /* bytes*/);

	//! Constructor by copy.
	HandleImmediate EXPORTCALL IMMCreateImmediateFrom(HandleImmediate other);

	//! Destructor.
	void  EXPORTCALL IMMDelete(HandleImmediate Handle);

	//! Sets the value of the operand.
	void EXPORTCALL  IMMsetValue(HandleImmediate vImm, triton::uint64 v, triton::uint32 size /* bytes*/);

	/* Register =============================================================================*/

	//! Constructor.
	HandleReg EXPORTCALL REGCreateRegister();

	//! Constructor.
	HandleReg EXPORTCALL REGCreateRegisterS(triton::arch::register_e regId, char * name, triton::arch::register_e parent, triton::uint32 high, triton::uint32 low, bool vmutable);

	//! Constructor.
	HandleReg EXPORTCALL RegCreateRegisterC(HandleCpuInterface, triton::arch::register_e regId);

	//! Constructor.
	HandleReg EXPORTCALL REGCreateRegisterFrom(HandleReg other);

  bool EXPORTCALL REGisOverlapWith(HandleReg r1, HandleReg other);

	//! Destructor.
	void  EXPORTCALL REGDelete(HandleReg Handle);

	/* MemoryAccess =============================================================================*/

	//! Constructor.
	HandleMemAcc EXPORTCALL MEMCreateMemory();

	//! Constructor.
	HandleMemAcc EXPORTCALL MEMCreateMemoryS(triton::uint64 address, triton::uint32 size /* bytes */);

	//! Constructor by copy.
	HandleMemAcc EXPORTCALL MEMCreateMemoryFrom(const HandleMemAcc other);

	//! Destructor.
	void  EXPORTCALL MEMDelete(HandleMemAcc Handle);

	//! Sets the address of the memory access.
	void EXPORTCALL MEMsetAddress(HandleMemAcc hMem, triton::uint64 addr);

	//! LEA - Sets pc relative.
	void EXPORTCALL MEMsetPcRelative(HandleMemAcc hMem, triton::uint64 addr);

	//! LEA - Sets the segment register operand.
	void EXPORTCALL MEMsetSegmentRegister(HandleMemAcc hMem, HandleReg segment);

	//! LEA - Sets the base register operand.
	void EXPORTCALL MEMsetBaseRegister(HandleMemAcc hMem, HandleReg base);

	//! LEA - Sets the index register operand.
	void EXPORTCALL MEMsetIndexRegister(HandleMemAcc hMem, HandleReg index);

	//! LEA - Sets the displacement operand.
	void EXPORTCALL MEMsetDisplacement(HandleMemAcc hMem, HandleImmediate displacement);

	//! LEA - Sets the scale operand.
	void EXPORTCALL MEMsetScale(HandleMemAcc hMem, HandleImmediate scale);

	//! Sets the AST of the memory access (LEA).
	void EXPORTCALL MEMsetLeaAst(HandleMemAcc hMem, HandleAbstractNode ast);

	/* OperandWrapper =============================================================================*/

	//! Immediate constructor.
	HandleOperandWrapper EXPORTCALL OPOperandWrapperI(HandleImmediate imm);

	//! Memory constructor.
	HandleOperandWrapper EXPORTCALL OPOperandWrapperM(HandleMemAcc mem);

	//! Register constructor.
	HandleOperandWrapper EXPORTCALL OPOperandWrapperR(HandleReg reg);

	//! Constructor by copy.
	HandleOperandWrapper EXPORTCALL OPOperandWrapperFrom(HandleOperandWrapper other);

	//! Destructor. 
	void  EXPORTCALL OPDelete(HandleOperandWrapper Handle);

	//! Sets the immediate operand.
	void EXPORTCALL OPsetImmediate(HandleOperandWrapper hOp, HandleImmediate imm);

	//! Sets the memory operand.
	void EXPORTCALL OPsetMemory(HandleOperandWrapper hOp, HandleMemAcc mem);

	//! Sets the register operand.
	void EXPORTCALL OPsetRegister(HandleOperandWrapper hOp, HandleReg reg);

  /*  BasicBlock ============================================================================== */

  //! Constructor.
  HandleBasicBlock EXPORTCALL BB_BasicBlock();

  //! Constructor.
  HandleBasicBlock EXPORTCALL BB_BasicBlockFromArray(HandleInstruz *instructions, uint32 num_instructions);

  //! Constructor by copy.
  HandleBasicBlock EXPORTCALL BB_BasicBlockFrom(HandleBasicBlock other);
  
  //! Destructor.
  void EXPORTCALL DeleteBasicBlock(HandleBasicBlock H);

  //! Add an instruction to the block
  void EXPORTCALL BB_add(HandleBasicBlock handle, HandleInstruz instruction);

  //! Remove an instruction from the block at the given position. Returns
  //! true if success.
  bool EXPORTCALL BB_remove(HandleBasicBlock handle, triton::uint32 position);

  //! Gets all instructions of the block
  uint32 EXPORTCALL BB_getInstructions(HandleBasicBlock handle, HandleInstruz **instructions);

  //! Returns the number of instructions in the block
  usize EXPORTCALL BB_getSize(HandleBasicBlock handle); 

  //! Returns the first instruction's address
  uint64 EXPORTCALL BB_getFirstAddress(HandleBasicBlock handle);

  //! Returns the last instruction's address
  uint64 EXPORTCALL BB_getLastAddress(HandleBasicBlock handle);

	/*  Istruction ============================================================================== */
    
	//! Constructor.
	HandleInstruz EXPORTCALL IInstruction();

	//! Constructor with opcode.
	HandleInstruz EXPORTCALL IInstructionOP(triton::uint8* opcode, triton::uint32 opSize);

	//! Constructor by copy.
	HandleInstruz EXPORTCALL IInstructionFrom(HandleInstruz other);

	//! Destructor. 
	void  EXPORTCALL IDelete(HandleInstruz Handle);

	//! Clears all instruction information.
	void EXPORTCALL Iclear(HandleInstruz hIstr);

	//! Returns whether the instruction reads the specified operand.
	bool EXPORTCALL IisReadFrom(HandleInstruz hIstr,HandleOperandWrapper target) ;

	//! Returns whether the instruction writes the specified operand.
	bool EXPORTCALL IisWriteTo(HandleInstruz hIstr, HandleOperandWrapper target) ;

	//! Sets a load access.
	void EXPORTCALL IsetLoadAccess(HandleInstruz hIstr, HandleMemAcc mem, HandleAbstractNode node);

	//! Removes a load access.
	void EXPORTCALL IremoveLoadAccess(HandleInstruz hIstr, HandleMemAcc mem);

	//! Sets a store access.
	void EXPORTCALL IsetStoreAccess(HandleInstruz hIstr, HandleMemAcc mem, HandleAbstractNode node);

	//! Removes a store access.
	void EXPORTCALL IremoveStoreAccess(HandleInstruz hIstr, HandleMemAcc mem);

	//! Sets a read register.
	void EXPORTCALL IsetReadRegister(HandleInstruz hIstr, HandleReg reg, HandleAbstractNode node);

	//! Removes a read register.
	void EXPORTCALL IremoveReadRegister(HandleInstruz hIstr, HandleReg reg);

	//! Sets a written register.
	void EXPORTCALL IsetWrittenRegister(HandleInstruz hIstr, HandleReg reg, HandleAbstractNode node);

	//! Removes a written register.
	void EXPORTCALL IremoveWrittenRegister(HandleInstruz hIstr, HandleReg reg);

	//! Sets a read immediate.
	void EXPORTCALL IsetReadImmediate(HandleInstruz hIstr, HandleImmediate imm, HandleAbstractNode node);

	//! Removes a read immediate.
	void EXPORTCALL IremoveReadImmediate(HandleInstruz hIstr, HandleImmediate imm);

	//! Sets an undefined register.
	void EXPORTCALL IsetUndefinedRegister(HandleInstruz hIstr, HandleReg reg);

	//! Removes an undefined register.
	void EXPORTCALL IremoveUndefinedRegister(HandleInstruz hIstr, HandleReg reg);

	//! Sets the address of the instruction.
	void EXPORTCALL IsetAddress(HandleInstruz hIstr, triton::uint64 addr);

	//! Sets flag to define this instruction as branch or not.
	void EXPORTCALL IsetBranch(HandleInstruz hIstr, bool flag);

	//! Sets the code condition of the instruction (mainly for AArch64).
	void EXPORTCALL IsetCodeCondition(HandleInstruz hIstr, triton::arch::arm::condition_e codeCondition);

	//! Sets flag to define if the condition is taken or not.
	void EXPORTCALL IsetConditionTaken(HandleInstruz hIstr, bool flag);

	//! Sets flag to define this instruction changes the control flow or not.
	void EXPORTCALL IsetControlFlow(HandleInstruz hIstr, bool flag);

	//! Sets the disassembly of the instruction.
	void EXPORTCALL IsetDisassembly(HandleInstruz hIstr, char* str);

	//! Sets the opcode of the instruction.
	void EXPORTCALL IsetOpcode(HandleInstruz hIstr, const triton::uint8* opcode, triton::uint32 size);

	//! Sets the prefix of the instruction (mainly for X86).
	void EXPORTCALL IsetPrefix(HandleInstruz hIstr, triton::arch::x86::prefix_e prefix);

	//! Sets the size of the instruction.
	void EXPORTCALL IsetSize(HandleInstruz hIstr, triton::uint32 size);

	//! Sets the taint of the instruction.
	void EXPORTCALL IsetTaintBool(HandleInstruz hIstr, bool state);

	//! Sets the taint of the instruction based on its expressions.
	void EXPORTCALL IsetTaint(HandleInstruz hIstr);

	//! Sets the thread id of the instruction.
	void EXPORTCALL IsetThreadId(HandleInstruz hIstr, triton::uint32 tid);

	//! Sets the type of the instruction.
	void EXPORTCALL IsetType(HandleInstruz hIstr, triton::uint32 type);
	
	//! Returns the list of all implicit and explicit load access
	uint32 EXPORTCALL IgetLoadAccess(HandleInstruz hIstr, MemNode * &outArray);

	//! Returns the list of all implicit and explicit store access
	uint32 EXPORTCALL IgetStoreAccess(HandleInstruz hIstr, MemNode * &outArray);

	//! Returns the list of all implicit and explicit register (flags includes) inputs (read)
	uint32 EXPORTCALL IgetReadRegisters(HandleInstruz hIstr, RegNode * &outArray);

	//! Returns the list of all implicit and explicit register (flags includes) outputs (write)
	uint32 EXPORTCALL IgetWrittenRegisters(HandleInstruz hIstr, RegNode * &outArray);

	//! Returns the list of all implicit and explicit immediate inputs (read)
	uint32 EXPORTCALL IgetReadImmediates(HandleInstruz hIstr, ImmNode * &outArray);

	//! Returns the list of all implicit and explicit undefined registers.
	uint32 EXPORTCALL IgetUndefinedRegisters(HandleInstruz hIstr, HandleReg * &outArray);

	//!  The semantics set of the instruction.
	uint32 EXPORTCALL IgetsymbolicExpressions(HandleInstruz hIstr, HandleSharedSymbolicExpression * &outArray);

	uint32 EXPORTCALL IGetOperand(HandleInstruz hIstr, HandleOperandWrapper ** outArray);

  bool EXPORTCALL IisSymbolized(HandleInstruz hIstr);

	/*  SymbolicExpression ============================================================================== */

	//! Constructor.
	HandleSharedSymbolicExpression EXPORTCALL SECreateSymbolicExpression(HandleAbstractNode node,triton::usize id,triton::engines::symbolic::expression_e type, char * comment = NULL);

	//! Constructor by copy.
	HandleSharedSymbolicExpression EXPORTCALL SECreateSymbolicExpressionFrom(HandleSharedSymbolicExpression other);

	//! Destructor. 
	void  EXPORTCALL SEDelete(HandleSharedSymbolicExpression Handle);

	//! Returns the symbolic expression id.
	triton::usize EXPORTCALL SEgetId(HandleSharedSymbolicExpression Handle) ;

	//! Returns true if the symbolic expression is assigned to a memory.
	bool EXPORTCALL SEisMemory(HandleSharedSymbolicExpression Handle) ;

	//! Returns true if the symbolic expression is assigned to a register.
	bool EXPORTCALL SEisRegister(HandleSharedSymbolicExpression Handle) ;

	//! Returns true if the expression contains a symbolic variable.
	bool EXPORTCALL SEisSymbolized(HandleSharedSymbolicExpression Handle) ;

	//! Returns the type of the symbolic expression assignment.
	triton::engines::symbolic::expression_e EXPORTCALL SEgetType(HandleSharedSymbolicExpression Handle) ;

	//! Returns the SMT AST root node of the symbolic expression. This is the semantics.
    HandleAbstractNode EXPORTCALL SEgetAst(HandleSharedSymbolicExpression Handle) ;

	//! Returns a new SMT AST root node of the symbolic expression. This new instance is a duplicate of the original node and may be changed without changing the original semantics.
	HandleAbstractNode EXPORTCALL SEgetNewAst(HandleSharedSymbolicExpression Handle) ;
	
	//! Returns the comment of the symbolic expression.
	void EXPORTCALL  SEgetComment(HandleSharedSymbolicExpression Handle, char* &comment) ;

	//! Returns the id as string of the symbolic expression according the mode of the AST representation.
	void EXPORTCALL  SEgetFormattedId(HandleSharedSymbolicExpression Handle, char* &frmId) ;

	//! Returns the comment as string of the symbolic expression according the mode of the AST representation.
	void EXPORTCALL  SEgetFormattedComment(HandleSharedSymbolicExpression Handle, char* &frmcomment) ;

	//! Returns the symbolic expression representation as string according the mode of the AST representation.
	void EXPORTCALL  SEgetFormattedExpression(HandleSharedSymbolicExpression Handle, char* &frmExp) ;

	//! Returns the origin memory access if `kind` is equal to `triton::engines::symbolic::MEM`, invalid memory otherwise.
	HandleMemAcc EXPORTCALL SEgetOriginMemory(HandleSharedSymbolicExpression Handle) ;

	//! Returns the origin register if `kind` is equal to `triton::engines::symbolic::REG`, `REG_INVALID` otherwise.
	HandleReg EXPORTCALL SEgetOriginRegister(HandleSharedSymbolicExpression Handle) ;

	//! Sets a root node.
	void EXPORTCALL SEsetAst(HandleSharedSymbolicExpression Handle,HandleAbstractNode node);

	//! Sets a comment to the symbolic expression.
	void EXPORTCALL SEsetComment(HandleSharedSymbolicExpression Handle,char * comment);

	//! Sets the kind of the symbolic expression.
	void EXPORTCALL SEsetType(HandleSharedSymbolicExpression Handle,triton::engines::symbolic::expression_e type);

	//! Sets the origin memory acccess.
	void EXPORTCALL SEsetOriginMemory(HandleSharedSymbolicExpression Handle,HandleMemAcc mem);

	//! Sets the origin register.
	void EXPORTCALL SEsetOriginRegister(HandleSharedSymbolicExpression Handle,HandleReg reg);

	//! Get is taint expression.
	bool EXPORTCALL SEisTainted(HandleSharedSymbolicExpression Handle);

	/*  Symbolic Variable =================================================================== */

	//! Constructor.
	HandleSharedSymbolicVariable EXPORTCALL SVCreateSymbolicVariable(triton::engines::symbolic::variable_e type,triton::uint64 origin,triton::usize id,triton::uint32 size,	char * comment);

	//! Constructor by copy.
	HandleSharedSymbolicVariable EXPORTCALL SVCreateSymbolicVariableFrom(HandleSharedSymbolicVariable other);

	//! Destructor. 
	void  EXPORTCALL SVDelete(HandleSharedSymbolicVariable Handle);

	//! Returns the symbolic variable type assignment.
	triton::engines::symbolic::variable_e EXPORTCALL SVgetType(HandleSharedSymbolicVariable Handle) ;

	//! Returns the alias of the symbolic variable.
	void EXPORTCALL SVgetAlias(HandleSharedSymbolicVariable Handle, char* &sAlias) ;

	//! Returns the comment of the symbolic variable.
	void EXPORTCALL SVgetComment(HandleSharedSymbolicVariable Handle, char* &sComment) ;

	//! Returns the name of the symbolic variable.
	void EXPORTCALL SVgetName(HandleSharedSymbolicVariable Handle,char* &sName) ;

	//! Returns the id of the symbolic variable. This id is unique.
	triton::usize EXPORTCALL SVgetId(HandleSharedSymbolicVariable Handle) ;

	//! Returns the source value of the symbolic variable.
	triton::uint64 EXPORTCALL SVgetOrigin(HandleSharedSymbolicVariable Handle) ;

	//! Returns the size (in bits) of the symbolic variable.
	triton::uint32 EXPORTCALL SVgetSize(HandleSharedSymbolicVariable Handle) ;

	//! Sets the alias of the symbolic variable.
	void EXPORTCALL SVsetAlias(HandleSharedSymbolicVariable Handle, char * sAlias);

	//! Sets the comment of the symbolic variable.
	void EXPORTCALL SVsetComment(HandleSharedSymbolicVariable Handle,char * comment);


	/*  Modes ============================================================================== */

	//! Constructor.
	hModes EXPORTCALL MCreateModes();

	//! Constructor.
	hModes EXPORTCALL MCreateModesFrom(hModes hMode);

	//! Destructor. 
	void EXPORTCALL MDeleteModes(hModes hMode);

	//! Returns true if the mode is enabled.
	bool EXPORTCALL MisModeEnabled(hModes hMode,triton::modes::mode_e mode);

	//! Enables or disables a specific mode.
	void EXPORTCALL MenableMode(hModes hMode,triton::modes::mode_e mode, bool flag);

	/*  AstRapresentation ============================================================================== */
    
	//! Constructor.
	hAstRepresentation EXPORTCALL RAPCreate(void);

	//! Constructor.
	hAstRepresentation EXPORTCALL RAPCreateFrom(hAstRepresentation other);

	//! Destructor. 
	void EXPORTCALL RAPDelete(hAstRepresentation handle);
	
	//! Returns the representation mode.
	triton::uint32 EXPORTCALL RAPgetMode(hAstRepresentation handle) ;

	//! Sets the representation mode.
	void EXPORTCALL RAPsetMode(hAstRepresentation handle,triton::uint32 mode);


	/* ASTContext ============================================================================== */

	//! Constructor
	HandleAstContext EXPORTCALL CtxCreate(hModes modes);

	//! Constructor by copy
	HandleAstContext EXPORTCALL CtxCreateFrom(HandleAstContext other);

	//! Destructor
	void EXPORTCALL CtxDelete(HandleAstContext hCtx);
		
	//! AST C++ API - assert node builder
	HandleAbstractNode EXPORTCALL assert_(HandleAstContext hCtx, HandleAbstractNode expr);

	//! AST C++ API - bv node builder //todo support uint512
	HandleAbstractNode EXPORTCALL bv(HandleAstContext hCtx, triton::uint64 value, triton::uint32 size);

	//! AST C++ API - bvadd node builder
	HandleAbstractNode EXPORTCALL bvadd(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - bvand node builder
	HandleAbstractNode EXPORTCALL bvand(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - bvashr node builder
	HandleAbstractNode EXPORTCALL bvashr(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - bvfalse node builder
	HandleAbstractNode EXPORTCALL bvfalse(HandleAstContext hCtx);

	//! AST C++ API - bvlshr node builder
	HandleAbstractNode EXPORTCALL bvlshr(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - bvmul node builder
	HandleAbstractNode EXPORTCALL bvmul(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - bvnand node builder
	HandleAbstractNode EXPORTCALL bvnand(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - bvneg node builder
	HandleAbstractNode EXPORTCALL bvneg(HandleAstContext hCtx, HandleAbstractNode expr);

	//! AST C++ API - bvnor node builder
	HandleAbstractNode EXPORTCALL bvnor(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - bvnot node builder
	HandleAbstractNode EXPORTCALL bvnot(HandleAstContext hCtx, HandleAbstractNode expr);

	//! AST C++ API - bvor node builder
	HandleAbstractNode EXPORTCALL bvor(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - bvrol node builder
	HandleAbstractNode EXPORTCALL bvrol_u(HandleAstContext hCtx, HandleAbstractNode expr, triton::uint32 rot);

	//! AST C++ API - bvrol node builder
	HandleAbstractNode EXPORTCALL bvrol(HandleAstContext hCtx, HandleAbstractNode expr, HandleAbstractNode rot);

	//! AST C++ API - bvror node builder
	HandleAbstractNode EXPORTCALL bvror_u(HandleAstContext hCtx, HandleAbstractNode expr, triton::uint32 rot);

	//! AST C++ API - bvror node builder
	HandleAbstractNode EXPORTCALL bvror(HandleAstContext hCtx, HandleAbstractNode expr, HandleAbstractNode rot);

	//! AST C++ API - bvsdiv node builder
	HandleAbstractNode EXPORTCALL bvsdiv(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - bvsge node builder
	HandleAbstractNode EXPORTCALL bvsge(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - bvsgt node builder
	HandleAbstractNode EXPORTCALL bvsgt(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - bvshl node builder
	HandleAbstractNode EXPORTCALL bvshl(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - bvsle node builder
	HandleAbstractNode EXPORTCALL bvsle(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - bvslt node builder
	HandleAbstractNode EXPORTCALL bvslt(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - bvsmod node builder
	HandleAbstractNode EXPORTCALL bvsmod(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - bvsrem node builder
	HandleAbstractNode EXPORTCALL bvsrem(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - bvsub node builder
	HandleAbstractNode EXPORTCALL bvsub(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - bvtrue node builder
	HandleAbstractNode EXPORTCALL bvtrue(HandleAstContext hCtx);

	//! AST C++ API - bvudiv node builder
	HandleAbstractNode EXPORTCALL bvudiv(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - bvuge node builder
	HandleAbstractNode EXPORTCALL bvuge(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - bvugt node builder
	HandleAbstractNode EXPORTCALL bvugt(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - bvule node builder
	HandleAbstractNode EXPORTCALL bvule(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - bvult node builder
	HandleAbstractNode EXPORTCALL bvult(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - bvurem node builder
	HandleAbstractNode EXPORTCALL bvurem(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - bvxnor node builder
	HandleAbstractNode EXPORTCALL bvxnor(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - bvxor node builder
	HandleAbstractNode EXPORTCALL bvxor(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - concat node builder
	HandleAbstractNode EXPORTCALL concat(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - declare node builder
	HandleAbstractNode EXPORTCALL declare(HandleAstContext hCtx, HandleAbstractNode var);

	//! AST C++ API - distinct node builder
	HandleAbstractNode EXPORTCALL distinct(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - equal node builder
	HandleAbstractNode EXPORTCALL equal(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - extract node builder
	HandleAbstractNode EXPORTCALL extract(HandleAstContext hCtx, triton::uint32 high, triton::uint32 low, HandleAbstractNode expr);

	//! AST C++ API - iff node builder
	HandleAbstractNode EXPORTCALL iff(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - integer node builder //todo Support uint512
	HandleAbstractNode EXPORTCALL integer(HandleAstContext hCtx, triton::uint64 value);

	//! AST C++ API - ite node builder
	HandleAbstractNode EXPORTCALL ite(HandleAstContext hCtx, HandleAbstractNode ifExpr, HandleAbstractNode thenExpr, HandleAbstractNode elseExpr);

	//! AST C++ API - land node builder
	HandleAbstractNode EXPORTCALL land(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - let node builder
	HandleAbstractNode EXPORTCALL let(HandleAstContext hCtx, char * alias, HandleAbstractNode expr2, HandleAbstractNode expr3);

	//! AST C++ API - lnot node builder
	HandleAbstractNode EXPORTCALL lnot(HandleAstContext hCtx, HandleAbstractNode expr);

	//! AST C++ API - lor node builder
	HandleAbstractNode EXPORTCALL lor(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2);

	//! AST C++ API - reference node builder
	HandleAbstractNode EXPORTCALL reference(HandleAstContext hCtx, HandleSharedSymbolicExpression expr);

	//! AST C++ API - string node builder
	HandleAbstractNode EXPORTCALL string(HandleAstContext hCtx, char * value);

	//! AST C++ API - sx node builder
	HandleAbstractNode EXPORTCALL sx(HandleAstContext hCtx, triton::uint32 sizeExt, HandleAbstractNode expr);

	//! AST C++ API - variable node builder
	HandleAbstractNode EXPORTCALL variable(HandleAstContext hCtx, HandleSharedSymbolicVariable symVar);

	//! AST C++ API - zx node builder
	HandleAbstractNode EXPORTCALL zx(HandleAstContext hCtx, triton::uint32 sizeExt, HandleAbstractNode expr);

	//! Initializes a variable in the context //todo support uint512
	void EXPORTCALL initVariable(HandleAstContext hCtx, char * name, triton::uint64 value, HandleAbstractNode node);

	//! Updates a variable value in this context //todo support uint512
	void EXPORTCALL updateVariable(HandleAstContext hCtx, char * name, triton::uint64 value);

	//! Gets a variable node from its name.
	HandleAbstractNode EXPORTCALL getVariableNode(HandleAstContext hCtx, char * name);

	//! Gets a variable value from its name. //todo support uint512
	triton::uint64 EXPORTCALL getVariableValue(HandleAstContext hCtx, char * varName) ;

	//! Sets the representation mode for this astContext
	void EXPORTCALL setRepresentationMode(HandleAstContext hCtx, triton::uint32 mode);

	//! Gets the representations mode of this astContext
	triton::uint32 EXPORTCALL getRepresentationMode(HandleAstContext hCtx) ;

	/*  Ast Node ============================================================================== */

	//! Constructor.
	HandleAbstractNode EXPORTCALL Node_Create(triton::ast::ast_e type, HandleAstContext ctxt);

	//! Destructor.
	void EXPORTCALL Node_Delete(HandleAbstractNode Handle);

	//! Access to its context
	HandleAstContext EXPORTCALL Node_getContext(HandleAbstractNode Handle) ;

	//! Returns the type of the node.
	triton::ast::ast_e EXPORTCALL Node_getType(HandleAbstractNode Handle) ;

	//! Returns the size of the node.
	triton::uint32 EXPORTCALL Node_getBitvectorSize(HandleAbstractNode Handle) ;

	//! Returns the vector mask according the size of the node.//todo add support uint512
	triton::uint64 EXPORTCALL Node_getBitvectorMask(HandleAbstractNode Handle) ;

	//! According to the size of the expression, returns true if the MSB is 1.
	bool EXPORTCALL Node_isSigned(HandleAbstractNode Handle) ;

	//! Returns true if the tree contains a symbolic variable.
	bool EXPORTCALL Node_isSymbolized(HandleAbstractNode Handle) ;

	//! Returns true if it's a logical node.
	bool EXPORTCALL Node_isLogical(HandleAbstractNode Handle) ;

	//! Returns true if the current tree is equal to the second one.
	bool EXPORTCALL Node_equalTo(HandleAbstractNode Handle, HandleAbstractNode other) ;

	//! Evaluates the tree.// todo support uint512
	triton::uint64 EXPORTCALL Node_evaluate(HandleAbstractNode Handle) ;

	//! Returns the children of the node.
	uint32 EXPORTCALL Node_getChildren(HandleAbstractNode Handle, HandleAbstractNode * &outArray);

	//! Returns the parents of node or an empty set if there is still no parent defined.
	uint32 EXPORTCALL Node_getParents(HandleAbstractNode Handle, HandleAbstractNode * &outArray);

	//! Removes a parent node.
	void EXPORTCALL Node_removeParent(HandleAbstractNode Handle,HandleAbstractNode p);

	//! Sets a parent node.
	void EXPORTCALL Node_setParent(HandleAbstractNode Handle,HandleAbstractNode p);

	//! Sets the parent nodes.
	void EXPORTCALL Node_setParents(HandleAbstractNode Handle,HandleAbstractNode* p, uint32 size);

	//! Sets the size of the node.
	void EXPORTCALL Node_setBitvectorSize(HandleAbstractNode Handle, triton::uint32 size);

	//! Adds a child.
	void EXPORTCALL Node_addChild(HandleAbstractNode Handle, HandleAbstractNode child);

	//! Sets a child at an index.
	void EXPORTCALL Node_setChild(HandleAbstractNode Handle,triton::uint32 index, HandleAbstractNode child);

	//! Returns the string representation of the node.
	void EXPORTCALL Node_str(HandleAbstractNode Handle, char* &sSt) ;

	//! Init stuffs like size and eval.
	void EXPORTCALL Node_init(HandleAbstractNode Handle) ;

	//! AST C++ API - Unrolls the SSA form of a given AST.
	HandleAbstractNode EXPORTCALL  Node_unroll(HandleAbstractNode node);

	//! Gets a duplicate.
	HandleAbstractNode EXPORTCALL  Node_duplicate(HandleAbstractNode node);

	// !Displays the node in ast representation.
	void EXPORTCALL AstToStr(HandleAbstractNode node, char * &sOut);

	//! Returns a deque of collected matched nodes via a depth-first pre order traversal.
	uint32 EXPORTCALL Node_search(HandleAbstractNode node, HandleAbstractNode * &outArray,triton::ast::ast_e match = ANY_NODE);

	//! Returns the has of the tree. The hash is computed recursively on the whole tree.
	triton::uint64 EXPORTCALL Node_hash(HandleAbstractNode node) ;

  //! Returns the deep level of the tree.
  triton::uint32 EXPORTCALL Node_Level(HandleAbstractNode node);

	//! descendig node particular procedure //todo support uint512 
	triton::uint64 EXPORTCALL  NodeInteger_getInteger(HandleAbstractNode node);

	//! descendig node particular procedure //todo support uint512 
	HandleSharedSymbolicExpression EXPORTCALL  NodeRef_getSymbolicExpression(HandleAbstractNode node);

	//! descendig node particular procedure //todo support uint512 
	HandleSharedSymbolicVariable EXPORTCALL  NodeRef_getSymbolicVariable(HandleAbstractNode node);

	/*  Solver Model ======================================================================== */

	//! Constructor.
	HandleSolverModel EXPORTCALL SMCreate();

	//! Constructor.// todo adding support uint512
	HandleSolverModel EXPORTCALL SMCreateD(HandleSharedSymbolicVariable variable, uint64 value);

	//! Constructor by copy.
	HandleSolverModel EXPORTCALL SMCreateFrom(HandleSolverModel other);
	
	//! Destructor.
	void EXPORTCALL SMDelete(HandleSolverModel Handle);

	//! Returns the id of the variable.
	triton::usize EXPORTCALL SMgetId(HandleSolverModel Handle);

	//! Returns the value of the model.// todo adding support uint512
	uint64 EXPORTCALL SMgetValue(HandleSolverModel Handle) ;

	//! Returns the symbolic variable.
	HandleSharedSymbolicVariable EXPORTCALL SMgetVariable(HandleSolverModel Handle) ;

	/*  PathConstraint ======================================================================== */

	//! Constructor.
	HandlePathConstraint EXPORTCALL PCPathConstraint();

	//! Constructor by copy.
	HandlePathConstraint EXPORTCALL PCPathConstraintFrom(HandlePathConstraint other);

	//! Destructor.
	void EXPORTCALL PCDelete(HandlePathConstraint Handle);

	//! Adds a branch to the path constraint.
	void EXPORTCALL PCaddBranchConstraint(HandlePathConstraint Handle,bool taken, triton::uint64 srcdAddr, triton::uint64 dstAddr, HandleAbstractNode pc);

	//! Returns the branch constraints.
	uint32 EXPORTCALL PCgetBranchConstraints(HandlePathConstraint Handle, PathDat **OutArray) ;

	//! Returns the address of the taken branch.
	triton::uint64 EXPORTCALL PCgetTakenAddress(HandlePathConstraint Handle) ;

	//! Returns the path constraint AST of the taken branch.
	HandleAbstractNode EXPORTCALL PCgetTakenPathConstraintAst(HandlePathConstraint Handle) ;

	//! Returns true if it is not a direct jump.
	bool EXPORTCALL PCisMultipleBranches(HandlePathConstraint Handle);

  /*  SynthesisResult ======================================================================== */
  
  //! Constructor.
  HandleSynthesisResult EXPORTCALL SyntResult();

  //! Constructor by copy.
  HandleSynthesisResult EXPORTCALL SyntResultFrom(HandleSynthesisResult other);

  //! Destructor.
  void EXPORTCALL SyntResultDelete(HandleSynthesisResult Handle);

  //! Gets the input node
  HandleAbstractNode EXPORTCALL SyntResultgetInput(HandleSynthesisResult Handle);

  //! Gets the output node
  HandleAbstractNode EXPORTCALL SyntResultgetOutput(HandleSynthesisResult Handle);
  
  //! Returns True the input node has been synthesized successfully.
  bool EXPORTCALL SyntResultsuccessful(HandleSynthesisResult Handle);

  /*  LLVMToTriton ======================================================================== */

  //! Constructor.
  HLLVMToTriton EXPORTCALL LLVMToTritonCtx(HandleContext ctx);

  //! Constructor.
  HLLVMToTriton EXPORTCALL LLVMToTritonNode(HandleAstContext actx);

  //! Destructor.
  void EXPORTCALL LLVMToTritonDelete(HLLVMToTriton Handle);

  //! Converts a given function from an LLVM module to a Triton AST.
  HandleAbstractNode EXPORTCALL convertModule(HLLVMToTriton Handle, LLVMModuleRef llvmModule, char* fname );

  //! Converts an LLVM instruction function to a Triton AST.
  HandleAbstractNode EXPORTCALL convertValue(HLLVMToTriton Handle, LLVMValueRef instruction);

 

} // extern "C"