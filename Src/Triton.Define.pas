unit Triton.Define;

{$Z4}
{$DEFINE Z3_INTERFACE}

interface

const
  {$IF Defined(WIN32)}
  Triton_dll = 'triton_x32.dll';
  {$ELSEIF Defined(WIN64)}
  Triton_dll = 'triton_x64.dll';
  {$ELSE}
    {$MESSAGE Error 'Unsupported platform'}
  {$ENDIF}
   reg2 = 1;
const

//*! Returns the FLAG size in bit. */
FLAG_SIZE_BIT = 1 ;

//*! Returns the BYTE size in byte. */
BYTE_SIZE = 1 ;

//*! Returns the BYTE size in bit. */
BYTE_SIZE_BIT = 8 ;

//*! Returns the WORD size in byte. */
WORD_SIZE = 2 ;

//*! Returns the WORD size in bit. */
WORD_SIZE_BIT = 16;

//*! Returns the DWORD size in byte. */
DWORD_SIZE = 4 ;

//*! Returns the DWORD size in bit. */
DWORD_SIZE_BIT = 32;

//*! Returns the QWORD size in byte. */
QWORD_SIZE = 8;

//*! Returns the QWORD size in bit. */
QWORD_SIZE_BIT = 64 ;

//*! Returns the DQWORD size in byte. */
DQWORD_SIZE = 16 ;

//*! Returns the DQWORD size in bit. */
DQWORD_SIZE_BIT = 128 ;

//*! Returns the QQWORD size in byte. */
QQWORD_SIZE = 32 ;

//*! Returns the QQWORD size in bit. */
QQWORD_SIZE_BIT = 256  ;

//*! Returns the DQQWORD size in byte. */
DQQWORD_SIZE = 64  ;

//*! Returns the DQQWORD size in bit. */
DQQWORD_SIZE_BIT = 512 ;

//*! Returns the max bits supported */
MAX_BITS_SUPPORTED = DQQWORD_SIZE_BIT ;
type
(*                         ---------Start tritonTypes.hpp---------            *)
(**********************************************************************)
    //! unisgned 8-bits
    uint8 = Byte;
   (* //! unsigned 128-bits
    typedef boost::multiprecision::uint128_t uint128;
    //! unsigned 256-bits
    typedef boost::multiprecision::uint256_t uint256;
    //! unsigned 512-bits
    typedef boost::multiprecision::uint512_t uint512;   *)
    //! signed 8-bits
    sint8 = Int8;
    //! signed 16-bits
    sint16 = Int16;
    //! signed 32-bits
    sint32 = Int32;
    //! signed 64-bits
    sint64 = Int64;
    (*)/! signed 128-bits
    typedef boost::multiprecision::int128_t sint128;
    //! signed 256-bits
    typedef boost::multiprecision::int256_t sint256;
    //! signed 512-bits
    typedef boost::multiprecision::int512_t sint512; *)
    //! unsigned MAX_INT 32 or 64 bits according to the CPU.
    usize = NativeUInt;
    {$IFDEF  CPUX64}
    //! unsigned long long if the arch is 64-bits.
    __uint = UInt64 ;
    {$ENDIF}
    //! signed long long if the arch is 64-bits.
    __sint = Int64;
(*                         ---------End tritonTypes.hpp-----------            *)
(**********************************************************************)

(*                         ---------Start archEnums.hpp-----------            *)
(**********************************************************************)

(*! Types of architecture *)
    architecture_e = (
      ARCH_INVALID = 0, (*!< Invalid architecture.   *)
      ARCH_AARCH64,     (*!< AArch64 architecture.   *)
      ARCH_ARM32,       (*!< ARM32 architecture.     *)
      ARCH_X86,         (*!< X86 architecture.       *)
      ARCH_X86_64);     (*!< X86_64 architecture.    *)


    (*! Types of endianness *)
    endianness_e = (
      LE_ENDIANNESS, (*!< Little endian.     *)
      BE_ENDIANNESS);(*!< Big endian.        *)


    (*! Types of operand *)
    operand_e = (
      OP_INVALID = 0, //!< invalid operand
      OP_IMM,         //!< immediate operand
      OP_MEM,         //!< memory operand
      OP_REG);        //!< register operand

   (*! Types of exceptions *)
   exception_e = (
      NO_FAULT = 0,     (*!< Succeed, no fault.                       *)
      FAULT_DE,         (*!< Fault raised: Divide-by-zero.            *)
      FAULT_BP,         (*!< Fault raised: Breakpoint.                *)
      FAULT_UD,         (*!< Fault raised: Instruction not supported. *)
      FAULT_GP          (*!< Fault raised: General Protection Fault.  *)
    );


    //! Types of register.
    register_e = (
      ID_REG_INVALID = 0, //!< invalid = 0
      {$I x86._Reg_spec.inc}
      (* Must be the last item *)
      ID_REG_LAST_ITEM ); //!< must be the last item

    //! The list of opcodes.
    instruction_e = (
       ID_INS_INVALID = 0 , //!< invalid = 0
       {$I x86._Ins_spec.inc}
       (* Must be the last item *)
        ID_INS_LAST_ITEM);//!< must be the last item

    (*! \brief Types of prefix.
       *
       *  \details
       *  Note that `REP` and `REPE` have the some opcode. The `REP`
       *  prefix becomes a `REPE` if the instruction modifies `ZF`.
       *)
      prefix_e = (
        ID_PREFIX_INVALID = 0,  //!< invalid
        ID_PREFIX_LOCK,         //!< LOCK
        ID_PREFIX_REP,          //!< REP
        ID_PREFIX_REPE,         //!< REPE
        ID_PREFIX_REPNE,        //!< REPNE
        (* Must be the last item *)
        ID_PREFIX_LAST_ITEM) ;    //!< must be the last item

      //! Types of shift
      shift_e = (
        ID_SHIFT_INVALID = 0, //!< invalid
        ID_SHIFT_ASR,         //!< Arithmetic Shift Right (immediate)
        ID_SHIFT_LSL,         //!< Logical Shift Left (immediate)
        ID_SHIFT_LSR,         //!< Logical Shift Right (immediate)
        ID_SHIFT_ROR,         //!< Rotate Right (immediate)
        ID_SHIFT_RRX,         //!< Rotate Right with Extend (immediate)
        ID_SHIFT_ASR_REG,     //!< Arithmetic Shift Right (register)
        ID_SHIFT_LSL_REG,     //!< Logical Shift Left (register)
        ID_SHIFT_LSR_REG,     //!< Logical Shift Right (register)
        ID_SHIFT_ROR_REG,     //!< Rotate Right (register)
        ID_SHIFT_RRX_REG,     //!< Rotate Right with Extend (register)
        ID_SHIFT_LAST_ITEM);  //!< Must be the last item

      //! Types of extend
      extend_e = (
        ID_EXTEND_INVALID = 0,   //!< invalid
        ID_EXTEND_UXTB,          //!< Extracts a byte (8-bit) value from a register and zero extends it to the size of the register
        ID_EXTEND_UXTH,          //!< Extracts a halfword (16-bit) value from a register and zero extends it to the size of the register
        ID_EXTEND_UXTW,          //!< Extracts a word (32-bit) value from a register and zero extends it to the size of the register
        ID_EXTEND_UXTX,          //!< Use the whole 64-bit register
        ID_EXTEND_SXTB,          //!< Extracts a byte (8-bit) value from a register and zero extends it to the size of the register
        ID_EXTEND_SXTH,          //!< Extracts a halfword (16-bit) value from a register and zero extends it to the size of the register
        ID_EXTEND_SXTW,          //!< Extracts a word (32-bit) value from a register and zero extends it to the size of the register
        ID_EXTEND_SXTX,          //!< Use the whole 64-bit register
        ID_EXTEND_LAST_ITEM);    //!< Must be the last item
      //! Types of condition
      condition_e = (
        ID_CONDITION_INVALID = 0, //!< invalid
        ID_CONDITION_AL,          //!< Always. Any flags. This suffix is normally omitted.
        ID_CONDITION_EQ,          //!< Equal. Z set.
        ID_CONDITION_GE,          //!< Signed >=. N and V the same.
        ID_CONDITION_GT,          //!< Signed >. Z clear, N and V the same.
        ID_CONDITION_HI,          //!< Higher (unsigned >). C set and Z clear.
        ID_CONDITION_HS,          //!< Higher or same (unsigned >=). C set.
        ID_CONDITION_LE,          //!< Signed <=. Z set, N and V differ.
        ID_CONDITION_LO,          //!< Lower (unsigned <). C clear.
        ID_CONDITION_LS,          //!< Lower or same (unsigned <=). C clear or Z set.
        ID_CONDITION_LT,          //!< Signed <. N and V differ.
        ID_CONDITION_MI,          //!< Negative. N set.
        ID_CONDITION_NE,          //!< Not equal. Z clear.
        ID_CONDITION_PL,          //!< Positive or zero. N clear.
        ID_CONDITION_VC,          //!< No overflow. V clear.
        ID_CONDITION_VS,          //!< Overflow. V set.
        ID_CONDITION_LAST_ITEM);  //!< must be the last item.
(*                         ---------End archEnums.hpp---------            *)
(*                         ---------ModesEnums.hpp-------                 *)
      mode_e = (
        ALIGNED_MEMORY,                 //!< [symbolic] Keep a map of aligned memory.
        AST_OPTIMIZATIONS,              //!< [AST] Classical arithmetic optimisations to reduce the depth of the trees.
        CONCRETIZE_UNDEFINED_REGISTERS, //!< [symbolic] Concretize every registers tagged as undefined (see #750).
        CONSTANT_FOLDING,               //!< [symbolic] Perform a constant folding optimization of sub ASTs which do not contain symbolic variables.
        MEMORY_ARRAY,                   //!< [symbolic] Enable memory symbolic array
        ONLY_ON_SYMBOLIZED,             //!< [symbolic] Perform symbolic execution only on symbolized expressions.
        ONLY_ON_TAINTED,                //!< [symbolic] Perform symbolic execution only on tainted instructions.
        PC_TRACKING_SYMBOLIC,           //!< [symbolic] Track path constraints only if they are symbolized.
        SYMBOLIZE_INDEX_ROTATION,       //!< [symbolic] Symbolize index rotation for bvrol and bvror (see #751). This mode increases the complexity of solving.
        SYMBOLIZE_LOAD,                 //!< [symbolic] Symbolize memory load if memory array is enabled
        SYMBOLIZE_STORE,                //!< [symbolic] Symbolize memory store if memory array is enabled
        TAINT_THROUGH_POINTERS);        //!< [taint] Spread the taint if an index pointer is already tainted (see #725).
(*                         ---------End modesEnums.hpp---------            *)

(*                         ---------callbacksEnums.hpp---------            *)
      (*! Enumerates all kinds callbacks. *)
      callback_e = (
        GET_CONCRETE_MEMORY_VALUE,    ///*!< LOAD concrete memory value callback */
        GET_CONCRETE_REGISTER_VALUE,  ///*!< GET concrete register value callback */
        SET_CONCRETE_MEMORY_VALUE,    ///*!< STORE concrete memory value callback */
        SET_CONCRETE_REGISTER_VALUE,  ///*!< PUT concrete register value callback */
        SYMBOLIC_SIMPLIFICATION);     ///*!< Symbolic simplification callback */
(*                         ---------End callbacksEnums.hpp---------            *)

(*                         ---------SolverEnums.hpp---------            *)
      (*! The different kind of solvers *)
      solver_e = (
        SOLVER_INVALID = 0, (*!< invalid solver. *)
        SOLVER_CUSTOM       (*!< custom solver. *)
        {$IFDEF Z3_INTERFACE}
        ,SOLVER_Z3          (*!< z3 solver. *)
        {$ENDIF}
        {$IFDEF TRITON_BITWUZLA_INTERFACE}
        ,SOLVER_BITWUZLA          (*!< bitwuzla solver. *)
        {$ENDIF}
      );

      (*! The different kind of status *)
      status_e = (
        UNSAT = 0, (*!< UNSAT *)
        SAT = 1,   (*!< SAT *)
        TIMEOUT,   (*!< TIMEOUT *)
        OUTOFMEM,  (*!< MEMORY LIMIT REACHED *)
        UNKNOWN    (*!< UNKNOWN *)
      );
      pstatus_e = ^status_e;
(*                         ---------End SolverEnums.hpp---------            *)

(*                         ---------SymbolicExpression.hpp---------            *)
      //! Type of symbolic expressions.
      expression_e = (
        MEMORY_EXPRESSION,      //!< Assigned to a memory expression.
        REGISTER_EXPRESSION,    //!< Assigned to a register expression.
        VOLATILE_EXPRESSION);   //!< Assigned to a volatile expression.

(*                         ---------End SymbolicExpression.hpp---------            *)

(*                         ---------SymbolicVariable.hpp---------            *)
      //! Type of symbolic variable.
      variable_e = (
        MEMORY_VARIABLE,       //!< Variable assigned to a memory.
        REGISTER_VARIABLE,     //!< Variable assigned to a register.
       UNDEFINED_VARIABLE);   //!< Undefined assignment.
(*                         ---------End SymbolicVariable.hpp---------            *)
(*                         ---------AstEnum.hpp---------            *)
     //! All types of representation mode.
     Rap_mode_e = (
        SMT_REPRESENTATION,     ///*!< SMT representation */
        PYTHON_REPRESENTATION,  ///*!< Python representation */
        LAST_REPRESENTATION);   ///*!< Must be the last item */
    ///*! Enumerates all types of node. Must be prime numbers. */
    ast_e = (
      INVALID_NODE = 0,               (*!< Invalid node *)
      ANY_NODE = 0,                   (*!< Any node *)
      ASSERT_NODE = 3,                (*!< (assert x) *)
      BSWAP_NODE = 5,                 (*!< (bswap x) *)
      BVADD_NODE = 7,                 (*!< (bvadd x y) *)
      BVAND_NODE = 13,                (*!< (bvand x y) *)
      BVASHR_NODE = 17,               (*!< (bvashr x y) *)
      BVLSHR_NODE = 19,               (*!< (bvlshr x y) *)
      BVMUL_NODE = 23,                (*!< (bvmul x y) *)
      BVNAND_NODE = 29,               (*!< (bvnand x y) *)
      BVNEG_NODE = 31,                (*!< (bvneg x) *)
      BVNOR_NODE = 37,                (*!< (bvnor x y) *)
      BVNOT_NODE = 41,                (*!< (bvnot x) *)
      BVOR_NODE = 43,                 (*!< (bvor x y) *)
      BVROL_NODE = 47,                (*!< ((_ rotate_left x) y) *)
      BVROR_NODE = 53,                (*!< ((_ rotate_right x) y) *)
      BVSDIV_NODE = 59,               (*!< (bvsdiv x y) *)
      BVSGE_NODE = 61,                (*!< (bvsge x y) *)
      BVSGT_NODE = 67,                (*!< (bvsgt x y) *)
      BVSHL_NODE = 71,                (*!< (bvshl x y) *)
      BVSLE_NODE = 73,                (*!< (bvsle x y) *)
      BVSLT_NODE = 79,                (*!< (bvslt x y) *)
      BVSMOD_NODE = 83,               (*!< (bvsmod x y) *)
      BVSREM_NODE = 89,               (*!< (bvsrem x y) *)
      BVSUB_NODE = 97,                (*!< (bvsub x y) *)
      BVUDIV_NODE = 101,              (*!< (bvudiv x y) *)
      BVUGE_NODE = 103,               (*!< (bvuge x y) *)
      BVUGT_NODE = 107,               (*!< (bvugt x y) *)
      BVULE_NODE = 109,               (*!< (bvule x y) *)
      BVULT_NODE = 113,               (*!< (bvult x y) *)
      BVUREM_NODE = 127,              (*!< (bvurem x y) *)
      BVXNOR_NODE = 131,              (*!< (bvxnor x y) *)
      BVXOR_NODE = 137,               (*!< (bvxor x y) *)
      BV_NODE = 139,                  (*!< (_ bvx y) *)
      COMPOUND_NODE = 149,            (*!< A compound of nodes *)
      CONCAT_NODE = 151,              (*!< (concat x y z ...) *)
      DECLARE_NODE = 157,             (*!< (declare-fun <var_name> () (_ BitVec <var_size>)) *)
      DISTINCT_NODE = 163,            (*!< (distinct x y) *)
      EQUAL_NODE = 167,               (*!< (= x y) *)
      EXTRACT_NODE = 173,             (*!< ((_ extract x y) z) *)
      FORALL_NODE = 179,              (*!< (forall ((x (_ BitVec <size>)), ...) body) *)
      IFF_NODE = 181,                 (*!< (iff x y) *)
      INTEGER_NODE = 191,             (*!< Integer node *)
      ITE_NODE = 193,                 (*!< (ite x y z) *)
      LAND_NODE = 197,                (*!< (and x y) *)
      LET_NODE = 199,                 (*!< (let ((x y)) z) *)
      LNOT_NODE = 211,                (*!< (and x y) *)
      LOR_NODE = 223,                 (*!< (or x y) *)
      LXOR_NODE = 227,                (*!< (xor x y) *)
      REFERENCE_NODE = 229,           (*!< Reference node *)
      STRING_NODE = 233,              (*!< String node *)
      SX_NODE = 239,                  (*!< ((_ sign_extend x) y) *)
      VARIABLE_NODE = 241,            (*!< Variable node *)
      ZX_NODE = 251,                  (*!< ((_ zero_extend x) y) *)
      ARRAY_NODE = 257,               (*!< (Array (_ BitVec addrSize) (_ BitVec 8)) *)
      SELECT_NODE = 263,              (*!< (select array index) *)
      STORE_NODE = 269);              (*!< (store array index expr) *)

(*                     ---------End AstEnum.hpp---------            *)
HandleContext       = Pointer;   //triton::Context
HandleSnapshot      = Pointer;   //Snapshot
HandleBV            = Pointer;   //triton::arch::BitsVector
HandleCpuInterface  = Pointer;   //triton::arch::CpuInterface
HandleOperandWrapper= Pointer;   //triton::arch::OperandWrapper
HandleInstruz       = Pointer;   //triton::arch::Instruction
HandleReg           = Pointer;   //triton::arch::Register
HandleMemAcc        = Pointer;   //triton::arch::MemoryAccess
HandleImmediate     = Pointer;   //triton::arch::Immediate
HandleAstContext    = Pointer;   //triton::ast::AstContext
HandleSolverModel   = pointer;   //triton::engines::solver::SolverModel
HandleTaintEngine   = Pointer;   //triton::engines::taint::TaintEngine
HandlePathConstraint= Pointer;   //triton::engines::symbolic::PathConstraint
HandleSynthesisResult= Pointer;  //triton::engines::synthesis::SynthesisResult
HandleBasicBlock     = Pointer;  //triton::arch::BasicBlock

AHandleInstruz = array of HandleInstruz;

HLLVMToTriton       = Pointer;   //typedef triton::ast::LLVMToTriton

HandleSymbolicEngine           = Pointer;  //triton::engines::symbolic::SymbolicEngine
HandleSharedSymbolicExpression = Pointer;  //triton::engines::symbolic::SharedSymbolicExpression
HandleSharedSymbolicVariable   = Pointer;  //triton::engines::symbolic::SharedSymbolicVariable
HandleAbstractNode  = Pointer;   //std::shared_ptr<triton::ast::AbstractNode>

PAddrSolver = ^AddrSolver;

AddrSolver = record { //std::map<triton::uint32, triton::engines::solver::SolverModel>  }
  numEle: uint32;
	id    : uint32;
	Model : HandleSolverModel;
end;
AAddrSolver = array of PAddrSolver;
PListSolver = ^AAddrSolver;
PIdSymExpr = ^IdSymExpr;
IdSymExpr = record { //std::map<triton::usize, triton::engines::symbolic::SharedSymbolicExpression> }
	id      : usize;
	SymExpr : HandleSharedSymbolicExpression ;
end;
PIdSymVar = ^IdSymVar;
IdSymVar = record //std::unordered_map<triton::usize, triton::engines::symbolic::SharedSymbolicVariable>
	id     : usize;
	SymVar : HandleSharedSymbolicVariable;
end;
PRegSymE = ^RegSymE;
RegSymE = record  //std::map<triton::arch::register_e, triton::engines::symbolic::SharedSymbolicExpression>
	regId  : register_e;
	RegSym : HandleSharedSymbolicExpression;
end;
PMemSymE = ^MemSymE;
MemSymE = record { //std::map<triton::uint64, triton::engines::symbolic::SharedSymbolicExpression> }
	mem    : uint64;
	MemSym : HandleSharedSymbolicExpression;
end;
PRegIdReg = ^RegIdReg;
RegIdReg = record { //std::map<triton::uint64, triton::engines::symbolic::SharedSymbolicExpression> }
	regId  : register_e;
	Reg    : HandleReg;
end;
PBranch = ^Branch;
Branch = record  //std::vector<std::tuple<bool, triton::uint64, triton::uint64, HandleAbstractNode>>
	taken  : Boolean;
	srcAddr: uint64;
	dstAddr: uint64;
	pc     : HandleAbstractNode;
end;
ABranch = array of Branch;
PHandlePathConstraint = ^HandlePathConstraint;
PReg          = ^HandleReg;
PSimbolicExpr = ^HandleSharedSymbolicExpression;
PHandleInstruz= ^AHandleInstruz;
(**********************************************************************)

implementation

end.
