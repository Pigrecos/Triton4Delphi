//! \file
/*
**  Copyright (C) - Triton
**
**  This program is under the terms of the BSD License.
*/

#include <algorithm>
#include <cmath>
#include <new>
#include <stack>
#include <unordered_map>
#include <utility>

#include <triton/register.hpp>
#include <triton/api.hpp>
#include <triton/ast.hpp>
#include <triton/astContext.hpp>
#include <triton/astRepresentation.hpp>
#include <triton/exceptions.hpp>
#include <triton/symbolicExpression.hpp>
#include <triton/symbolicVariable.hpp>

#include <triton/TritonDll.h>


/* Architecture API ============================================================================== */

_BV RefBVToBV(HandleBV HBv)
{
	_BV bv;
	bv.high       = HBv->getHigh();
	bv.low        = HBv->getLow();
	bv.VectorSize = HBv->getVectorSize();
	bv.MaxValue   = (uint64)HBv->getMaxValue();

	return bv;
};

_Imm RefImmToImm(HandleImmediate HImm)
{
	_Imm vImm;
	vImm.value        = HImm->getValue();
	vImm.Tipo         = HImm->getType();
	vImm.Size         = HImm->getSize();
	vImm.BitSize      = HImm->getBitSize();
	//BitVector		
	vImm.BitVec.high  = HImm->getHigh();
	vImm.BitVec.low   = HImm->getLow();
	vImm.BitVec.VectorSize = HImm->getVectorSize();
	vImm.BitVec.MaxValue = (uint64)HImm->getMaxValue();

	return vImm;
};

_Reg RefRegToReg(HandleReg HReg)
{
	_Reg vReg;
		
	std::string name = HReg->getName();
	vReg.name = (char *)malloc(sizeof(char) * name.size() + 1u);
	
	std::vector<char> chars(name.c_str(), name.c_str() + name.size() + 1u);
	strcpy_s(vReg.name, name.size()+1, &chars[0]);
	

	vReg.id          = HReg->getId();
	vReg.parent      = HReg->getParent();
	vReg.Tipo        = HReg->getType();
	vReg.Size        = HReg->getSize();
	vReg.BitSize     = HReg->getBitSize();
	//BitVector	
	vReg.BitVec.high = HReg->getHigh();
	vReg.BitVec.low  = HReg->getLow();
	vReg.BitVec.VectorSize = HReg->getVectorSize();
	vReg.BitVec.MaxValue = (uint64)HReg->getMaxValue();

	return vReg;
}
_memAccess RefMemToMem(HandleMemAcc HMem)
{
	_memAccess mem;

		mem.address       = HMem->getAddress();
		mem.pcRelative = HMem->getPcRelative();
		mem.segmentReg = &HMem->getSegmentRegister();
		mem.baseReg = &HMem->getBaseRegister();
		mem.indexReg = &HMem->getIndexRegister();
		mem.displacement = &HMem->getDisplacement();
		mem.scale = &HMem->getScale();
		mem.leaAST = &HMem->getLeaAst();
		mem.Tipo = HMem->getType();
		mem.Size = HMem->getSize();
		mem.BitSize = HMem->getBitSize();
		//BitVector	
		mem.BitVec.high = HMem->getHigh();
		mem.BitVec.low = HMem->getLow();
		mem.BitVec.VectorSize = HMem->getVectorSize();
		mem.BitVec.MaxValue = (uint64)HMem->getMaxValue();

		return mem;
}
_OpWrapp RefOpWrapToOpWrap(HandleOperandWrapper HOpW)
{
	_OpWrapp op;

	op.imm = &HOpW->getImmediate();
	op.mem = &HOpW->getMemory();
	op.reg = &HOpW->getRegister();
	op.Tipo = HOpW->getType();
	op.Size = HOpW->getSize();
	op.BitSize = HOpW->getBitSize();
	op.high = HOpW->getHigh();
	op.low = HOpW->getLow();

	return op;
}
_Istruz RefIstruzToIstruz(HandleInstruz HOpW)
{
	_Istruz istr;

	istr.tid = HOpW->getThreadId();
	istr.address = HOpW->getAddress();
	istr.nextAddress = HOpW->getNextAddress();

	std::string dis = HOpW->getDisassembly();
	istr.disassembly = (char *)malloc(sizeof(char) * dis.size() + 1u);

	std::vector<char> chars(dis.c_str(), dis.c_str() + dis.size() + 1u);
	strcpy_s(istr.disassembly, dis.size() + 1, &chars[0]);

	
	istr.opcode =  HOpW->getOpcode();
	istr.size = HOpW->getSize();
	istr.tipo = HOpW->getType();
	istr.prefix = HOpW->getPrefix();
	istr.branch = HOpW->isBranch();
	istr.controlFlow = HOpW->isControlFlow();
	istr.conditionTaken = HOpW->isConditionTaken();
	istr.tainted = HOpW->isTainted();

	return istr;

};


/* Architecture API ============================================================================== */

/* Callbacs Procedure         -- very bad implementation but it works ...:):) */

HandleAbstractNode(*cs)(HandleApi, HandleAbstractNode);
HandleAbstractNode(*cs1)(HandleApi, HandleAbstractNode);
HandleAbstractNode(*cs2)(HandleApi, HandleAbstractNode);

void(*cSRVal)  (HandleApi, HandleReg, uint64);
void(*cSRVal1) (HandleApi, HandleReg, uint64);
void(*cSRVal2) (HandleApi, HandleReg, uint64);

void(*cSMVal)  (HandleApi, HandleMemAcc, uint64);
void(*cSMVal1) (HandleApi, HandleMemAcc, uint64);
void(*cSMVal2) (HandleApi, HandleMemAcc, uint64);

void(*cGRVal)  (HandleApi, HandleReg);
void(*cGRVal1) (HandleApi, HandleReg);
void(*cGRVal2) (HandleApi, HandleReg);

void(*cGMVal)  (HandleApi, HandleMemAcc);
void(*cGMVal1) (HandleApi, HandleMemAcc);
void(*cGMVal2) (HandleApi, HandleMemAcc);

HandleApi  CreateApi(void)
{
	return new triton::API();
};


void  DeleteApi(HandleApi Handle)
{
	delete Handle;
	cs = NULL;
	cs1 = NULL;
	cs2 = NULL;

	cSRVal = NULL;
	cSRVal1 = NULL;
	cSRVal2 = NULL;

	cSMVal = NULL;
	cSMVal1 = NULL;
	cSMVal2 = NULL;

	cGRVal = NULL;
	cGRVal1 = NULL;
	cGRVal2 = NULL;

	cGMVal = NULL;
	cGMVal1 = NULL;
	cGMVal2 = NULL;
}

void  setArchitecture(HandleApi Handle, triton::arch::architecture_e arch)
{
	Handle->setArchitecture(arch);
}

triton::arch::architecture_e  GetArchitecture(HandleApi Handle)
{
	return Handle->getArchitecture();
}

bool  isArchitectureValid(HandleApi Handle)
{
	return Handle->isArchitectureValid();
}

void  checkArchitecture(HandleApi Handle)
{
	Handle->checkArchitecture();
}

void  clearArchitecture(HandleApi Handle)
{
	Handle->clearArchitecture();
}

triton::arch::endianness_e  getEndianness(HandleApi Handle)
{
	return  Handle->getEndianness();
}

HandleCpuInterface  getCpuInstance(HandleApi Handle)
{
	return  Handle->getCpuInstance();
}

bool  isFlag(HandleApi Handle, triton::arch::register_e regId)
{
	return  Handle->isFlag(regId);
}

bool  isFlagR(HandleApi Handle, HandleReg reg)
{
	return  Handle->isFlag(*reg);
}

bool  isRegister(HandleApi Handle, triton::arch::register_e regId)
{
	return  Handle->isRegister(regId);
}

bool  isRegisterR(HandleApi Handle, HandleReg reg)
{
	return Handle->isRegister(*reg);
}

HandleReg  getRegister(HandleApi Handle, register_e id)
{

	return new Register(Handle->getRegister(id));;

}

HandleReg  getParentRegisterR(HandleApi Handle, HandleReg reg)
{

	Register r = Handle->getParentRegister(*reg);
	return new Register(r);

}

HandleReg  getParentRegister(HandleApi Handle, triton::arch::register_e id)
{

	Register r = Handle->getParentRegister(id);
	return new Register(r);

}

bool  isRegisterValid(HandleApi Handle, triton::arch::register_e id)
{

	return Handle->isRegisterValid(id);

}

bool  isRegisterValidR(HandleApi Handle, HandleReg reg)
{

	return Handle->isRegisterValid(*reg);

}

uint32  getGprBitSize(HandleApi Handle)
{

	return Handle->getGprBitSize();

}

triton::uint32  getGprSize(HandleApi Handle)
{

	return Handle->getGprSize();

}

triton::uint32  getNumberOfRegisters(HandleApi Handle)
{

	return Handle->getNumberOfRegisters();

}

//! [**architecture api**] - Returns the concrete value of a memory cell.
triton::uint8   getConcreteMemoryValueByte(HandleApi Handle, triton::uint64 addr, bool execCallbacks )
{
	return Handle->getConcreteMemoryValue(addr, execCallbacks);
}

//! [**architecture api**] - Returns the concrete value of memory cells.// not support 512
uint64   getConcreteMemoryValue(HandleApi Handle, HandleMemAcc mem, bool execCallbacks )
{
	return (uint64)Handle->getConcreteMemoryValue(*mem, execCallbacks);
}

//! [**architecture api**] - Returns the concrete value of a memory area.
retArray   getConcreteMemoryAreaValue(HandleApi Handle, triton::uint64 baseAddr, triton::usize size, bool execCallbacks )
{	
	std::vector<triton::uint8> v = Handle->getConcreteMemoryAreaValue(baseAddr, size, execCallbacks);

	uint8* result = new uint8[size];

	memcpy(result, &v.front(), v.size() * sizeof(uint8));
	
	
	return result;
}

//! [**architecture api**] - Returns the concrete value of a register. // not support 512
triton::uint64   getConcreteRegisterValue(HandleApi Handle, HandleReg reg, bool execCallbacks )
{
	return (uint64)Handle->getConcreteRegisterValue(Register(*reg), execCallbacks);
}

void  setConcreteMemoryValueByte(HandleApi Handle, triton::uint64 addr, triton::uint8 value)
{
	Handle->setConcreteMemoryValue(addr, value);
}

// not support 512
void  setConcreteMemoryValue(HandleApi Handle, HandleMemAcc mem, triton::uint64 value)
{
	Handle->setConcreteMemoryValue(*mem, value);
}

void  setConcreteMemoryAreaValue(HandleApi Handle, triton::uint64 baseAddr, triton::uint8* area, triton::usize size)
{
	Handle->setConcreteMemoryAreaValue(baseAddr, area, size);
}

// not support 512
void  setConcreteRegisterValue(HandleApi Handle, HandleReg reg, triton::uint64 value)
{
	Handle->setConcreteRegisterValue(*reg, value);
}

//! [**architecture api**] - Returns true if the range `[baseAddr:size]` is mapped into the internal memory representation. \sa getConcreteMemoryValue() and getConcreteMemoryAreaValue().
bool  isMemoryMapped(HandleApi Handle, triton::uint64 baseAddr, triton::usize size )
{
	return Handle->isMemoryMapped(baseAddr, size);
}

//! [**architecture api**] - Removes the range `[baseAddr:size]` from the internal memory representation. \sa isMemoryMapped().
void   unmapMemory(HandleApi Handle, triton::uint64 baseAddr, triton::usize size )
{
	Handle->unmapMemory(baseAddr, size);
}

void  disassembly(HandleApi Handle, HandleInstruz inst)
{
	Handle->disassembly(*inst);
}

uint32 mapRegsToArray(std::unordered_map<triton::arch::register_e, const triton::arch::Register> mregs, RegIdReg **OutRegs)
{	
	auto   n = mregs.size();

	if (n < 1) return 0;

	RegIdReg* a = new RegIdReg[n];
	RegIdReg* newA = NULL;

	auto i = 0;
	for (auto elem : mregs) {
		a[i].regId  = elem.first;
		a[i].Reg	= HandleReg(&elem.second);

		triton::arch::Register* x = new triton::arch::Register;
		memcpy(x, &elem.second, sizeof(triton::arch::Register));

		a[i].Reg = *&x;
		i++;
	}

	newA = (RegIdReg*)realloc(*OutRegs, sizeof(RegSymE)*n);

	if (newA) {
		memcpy(newA, a, sizeof(RegSymE)*n);

		*OutRegs = newA;
	}
	else {
		memcpy(*OutRegs, a, sizeof(RegSymE)*n);
	}

	delete a;

	return (uint32)n;
}

//! [**architecture api**] - Returns all registers. \sa triton::arch::x86::register_e.
uint32 getAllRegisters(HandleApi Handle, RegIdReg **Regs)
{
	std::unordered_map<triton::arch::register_e, const triton::arch::Register> s = Handle->getAllRegisters();
	auto   n = mapRegsToArray(s, Regs);

	return n;
}

//! [**architecture api**] - Returns all parent registers. \sa triton::arch::x86::register_e.
uint32 getParentRegisters(HandleApi Handle, HandleReg*& outRegs)
{
	std::set<const triton::arch::Register*> r = Handle->getParentRegisters();
	size_t n = r.size();

	if (r.size() < 1) return 0;

	outRegs = new HandleReg[n];

	auto i = 0;
	for (auto elem : r)
	{
		triton::arch::Register* reg = new triton::arch::Register;
		memcpy(&reg, &elem, sizeof(triton::arch::Register));

		outRegs[i] = &*reg;
		i++;
	};
	return (uint32)n;
}

/* Processing API ================================================================================ */

//! [**proccesing api**] - Processes an instruction and updates engines according to the instruction semantics. Returns true if the instruction is supported.
bool   processing(HandleApi Handle, HandleInstruz inst)
{
	return Handle->processing(*inst);
}

//! [**proccesing api**] - Initializes everything.
void   initEngines(HandleApi Handle)
{
	Handle->initEngines();
}

//! [**proccesing api**] - Removes everything.
void   removeEngines(HandleApi Handle)
{
	Handle->removeEngines();
}
//! [**proccesing api**] - Resets everything.
void  reset(HandleApi Handle)
{
	Handle->reset();
}


/* IR API ======================================================================================== */

//! [**IR builder api**] - Raises an exception if the IR builder is not initialized.
void  checkIrBuilder(HandleApi Handle)
{
	Handle->checkIrBuilder();
}
//! [**IR builder api**] - Builds the instruction semantics. Returns true if the instruction is supported. You must define an architecture before. \sa processing().
bool   buildSemantics(HandleApi Handle, HandleInstruz inst)
{
	return Handle->buildSemantics(*inst);
}

//! [**IR builder api**] - Returns the AST context. Used as AST builder.
HandleAstContext   getAstContext(HandleApi Handle)
{
   HandleAstContext cc = new std::shared_ptr<triton::ast::AstContext>;
   *cc = Handle->getAstContext();

	return cc;
}

/* AST Representation API ======================================================================== */

triton::uint32 getAstRepresentationMode(HandleApi Handle)
{
	return  Handle->getAstRepresentationMode();
}

void setAstRepresentationMode(HandleApi Handle,triton::uint32 mode)
{
	Handle->setAstRepresentationMode(mode);
}

/* Callbacks API ================================================================================= */

/*addCallbackGetMem Callbacs Procedure         -- very bad implementation but it works ...:):) */

void cbGetMem(triton::API& ctx, const triton::arch::MemoryAccess& mem)
{
	cGMVal(&ctx, HandleMemAcc(&mem));
}
void cbGetMem1(triton::API& ctx, const triton::arch::MemoryAccess& mem)
{
	cGMVal1(&ctx, HandleMemAcc(&mem));
}
void cbGetMem2(triton::API& ctx, const triton::arch::MemoryAccess& mem)
{
	cGMVal2(&ctx, HandleMemAcc(&mem));
}

void addCallbackGetMem(HandleApi Handle, cbGetMemVal cb)
{
	if (cGMVal == NULL)
	{
		cGMVal = *cb;
		Handle->addCallback(cbGetMem);
	}
	else if (cGMVal1 == NULL)
	{
		cGMVal1 = *cb;
		Handle->addCallback(cbGetMem1);
	}
	else if (cGMVal2 == NULL)
	{
		cGMVal2 = *cb;
		Handle->addCallback(cbGetMem2);
	}
	else
	{
		std::cout << "API For C::addCallbackGetMem(): reached callback limit." << std::endl;
	}
}

/*addCallbackGetReg Callbacs Procedure         -- very bad implementation but it works ...:):) */

void cbGetReg(triton::API& ctx, const triton::arch::Register& reg)
{
	cGRVal(&ctx, HandleReg(&reg));
}
void cbGetReg1(triton::API& ctx, const triton::arch::Register& reg)
{
	cGRVal1(&ctx, HandleReg(&reg));
}
void cbGetReg2(triton::API& ctx, const triton::arch::Register& reg)
{
	cGRVal2(&ctx, HandleReg(&reg));
}

void  addCallbackGetReg(HandleApi Handle, cbGetRegVal cb)
{
	if (cGRVal == NULL)
	{
		cGRVal = *cb;
		Handle->addCallback(cbGetReg);
	}
	else if (cGRVal1 == NULL)
	{
		cGRVal1 = *cb;
		Handle->addCallback(cbGetReg1);
	}
	else if (cGRVal2 == NULL)
	{
		cGRVal2 = *cb;
		Handle->addCallback(cbGetReg2);
	}
	else
	{
		std::cout << "API For C::addCallbackGetReg(): reached callback limit." << std::endl;
	}
}

/*addCallbackSetMem Callbacs Procedure         -- very bad implementation but it works ...:):) */

void cbSetMem(triton::API& ctx, const triton::arch::MemoryAccess& mem, const triton::uint512& value)
{
	cSMVal(&ctx, HandleMemAcc(&mem), (uint64)value);
}
void cbSetMem1(triton::API& ctx, const triton::arch::MemoryAccess& mem, const triton::uint512& value)
{
	cSMVal1(&ctx, HandleMemAcc(&mem), (uint64)value);
}
void cbSetMem2(triton::API& ctx, const triton::arch::MemoryAccess& mem, const triton::uint512& value)
{
	cSMVal2(&ctx, HandleMemAcc(&mem), (uint64)value);
}

void addCallbackSetMem(HandleApi Handle, cbSetMemVal cb)
{
	if (cSMVal == NULL)
	{
		cSMVal = *cb;
		Handle->addCallback(cbSetMem);
	}
	else if (cSMVal1 == NULL)
	{
		cSMVal1 = *cb;
		Handle->addCallback(cbSetMem1);
	}
	else if (cSMVal2 == NULL)
	{
		cSMVal2 = *cb;
		Handle->addCallback(cbSetMem2);
	}
	else
	{
		std::cout << "API For C::addCallbackSetMem(): reached callback limit." << std::endl;
	}
	
}

/*addCallbackSetReg Callbacs Procedure         -- very bad implementation but it works ...:):) */

void cbSetReg(triton::API& ctx , const triton::arch::Register& reg , const triton::uint512& value)
{
	cSRVal(&ctx, HandleReg(&reg), (uint64)value);
}
void cbSetReg1(triton::API& ctx, const triton::arch::Register& reg, const triton::uint512& value)
{
	cSRVal1(&ctx, HandleReg(&reg), (uint64)value);
}
void cbSetReg2(triton::API& ctx, const triton::arch::Register& reg, const triton::uint512& value)
{
	cSRVal2(&ctx, HandleReg(&reg), (uint64)value);
}

void addCallbackSetReg(HandleApi Handle, cbSetRegVal cb)
{
	
	if (cSRVal == NULL)
	{
		cSRVal = *cb;
		Handle->addCallback(cbSetReg);
	}
	else if (cSRVal1 == NULL)
	{
		cSRVal1 = *cb;
		Handle->addCallback(cbSetReg1);
	}
	else if (cSRVal2 == NULL)
	{
		cSRVal2 = *cb;
		Handle->addCallback(cbSetReg2);
	}
	else
	{
		std::cout << "API For C::addCallbackSetReg(): reached callback limit." << std::endl;
	}
}

/*addCallbackSimplif Callbacs Procedure         -- very bad implementation but it works ...:):) */

triton::ast::SharedAbstractNode simplification(triton::API& ctx, const triton::ast::SharedAbstractNode& node)
{	
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = node;

	cc = cs(&ctx, cc);
	
	return *cc;
}
triton::ast::SharedAbstractNode simplification1(triton::API& ctx, const triton::ast::SharedAbstractNode& node)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = node;

	cc = cs1(&ctx, cc);

	return *cc;
}
triton::ast::SharedAbstractNode simplification2(triton::API& ctx, const triton::ast::SharedAbstractNode& node)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = node;

	cc = cs2(&ctx, cc);

	return *cc;
}
void addCallbackSimplif(HandleApi Handle, cbSimplification cb)
{
	if (cs == NULL)
	{ 
		cs = *cb;
		Handle->addCallback(simplification);
	}
	else if (cs1 == NULL)
	{
		cs1 = *cb;
		Handle->addCallback(simplification1);
	}
	else if (cs1 == NULL)
	{
		cs2 = *cb;
		Handle->addCallback(simplification2);
	}
	else 
	{
		std::cout << "API For C::addCallbackSimplif(): reached callback limit."  << std::endl;
	}
	
}

void removeAllCallbacks(HandleApi Handle)
{
	Handle->removeAllCallbacks();
	cs = NULL;
	cs1 = NULL;
	cs2 = NULL;

	cSRVal = NULL;
	cSRVal1 = NULL;
	cSRVal2 = NULL;

	cSMVal = NULL;
	cSMVal1 = NULL;
	cSMVal2 = NULL;

	cGRVal = NULL;
	cGRVal1 = NULL;
	cGRVal2 = NULL;

	cGMVal = NULL;
	cGMVal1 = NULL;
	cGMVal2 = NULL;
}

void removeCallbackGetMem(HandleApi Handle, cbGetMemVal cb)
{
	if (cGMVal == *cb)
	{
		Handle->removeCallback(cbGetMem);
		cGMVal = NULL;
	}
	else if (cGMVal1 == *cb)
	{
		Handle->removeCallback(cbGetMem1);
		cGMVal1 = NULL;
	}
	else if (cGMVal2 == *cb)
	{
		Handle->removeCallback(cbGetMem2);
		cGMVal2 = NULL;
	}
	else
	{
		std::cout << "API For C::removeCallbackGetMem(): callback not found." << std::endl;
	}
}

void removeCallbackGetReg(HandleApi Handle, cbGetRegVal cb)
{
	if (cGRVal == *cb)
	{
		Handle->removeCallback(cbGetReg);
		cGRVal = NULL;
	}
	else if (cGRVal1 == *cb)
	{
		Handle->removeCallback(cbGetReg1);
		cGRVal1 = NULL;
	}
	else if (cGRVal2 == *cb)
	{
		Handle->removeCallback(cbGetReg2);
		cGRVal2 = NULL;
	}
	else
	{
		std::cout << "API For C::removeCallbackGetReg(): callback not found." << std::endl;
	}
}

void removeCallbackSetMem(HandleApi Handle, cbSetMemVal cb)
{
	if (cSMVal == *cb)
	{
		Handle->removeCallback(cbSetMem);
		cSMVal = NULL;
	}
	else if (cSMVal1 == *cb)
	{
		Handle->removeCallback(cbSetMem1);
		cSMVal1 = NULL;
	}
	else if (cSMVal2 == *cb)
	{
		Handle->removeCallback(cbSetMem2);
		cSMVal2 = NULL;
	}
	else
	{
		std::cout << "API For C::removeCallbackSetMem(): callback not found." << std::endl;
	}
}

void removeCallbackSetReg(HandleApi Handle, cbSetRegVal cb)
{
	if (cSRVal == *cb)
	{
		Handle->removeCallback(cbSetReg);
		cSRVal = NULL;
	}
	else if (cSRVal1 == *cb)
	{
		Handle->removeCallback(cbSetReg1);
		cSRVal1 = NULL;
	}
	else if (cSRVal2 == *cb)
	{
		Handle->removeCallback(cbSetReg2);
		cSRVal2 = NULL;
	}
	else
	{
		std::cout << "API For C::removeCallbackSetReg(): callback not found." << std::endl;
	}
}

void removeCallbackSimplif(HandleApi Handle, cbSimplification cb)
{
	if (cs == *cb)
	{
		Handle->removeCallback(simplification);
		cs = NULL;
	}
	else if (cs1 == *cb)
	{
		Handle->removeCallback(simplification1);
		cs1 = NULL;
	}
	else if (cs2 == *cb)
	{
		Handle->removeCallback(simplification2);
		cs2 = NULL;
	}
	else
	{
		std::cout <<"API For C::removeCallbackSimplif(): callback not found." << std::endl;
	}
	
}

HandleAbstractNode processCallbacks(HandleApi Handle, triton::callbacks::callback_e kind, HandleAbstractNode node)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = Handle->processCallbacks(kind, *node);

	return cc;

}

void processCallbacksMem(HandleApi Handle, triton::callbacks::callback_e kind, HandleMemAcc mem)
{
	Handle->processCallbacks(kind,*mem);
}

void processCallbacksReg(HandleApi Handle, triton::callbacks::callback_e kind, HandleReg reg)
{
	Handle->processCallbacks(kind, *reg);
}


/* Modes API====================================================================================== */

void checkModes(HandleApi Handle)
{
	//Handle->checkModes();
}

void enableMode(HandleApi Handle,triton::modes::mode_e mode, bool flag)
{
	Handle->enableMode(mode,flag);
}

bool isModeEnabled(HandleApi Handle,triton::modes::mode_e mode)
{
	return Handle->isModeEnabled(mode);
}

/* Symbolic engine API =========================================================================== */

void checkSymbolic(HandleApi Handle)
{
	Handle->checkSymbolic();
}

HandleSymbolicEngine getSymbolicEngine(HandleApi Handle)
{
	return Handle->getSymbolicEngine();
}

uint32 mapRegEToArray(std::map<triton::arch::register_e, triton::engines::symbolic::SharedSymbolicExpression> mregE, RegSymE **OutRegE)
{	
	auto   n = mregE.size();

	if (n < 1) return 0;

	RegSymE* a = new RegSymE[n];
	RegSymE* newA = NULL;

	auto i = 0;
	for (auto elem : mregE) {
		a[i].regId  = elem.first;
		a[i].RegSym = &elem.second;

		triton::engines::symbolic::SharedSymbolicExpression* x = new triton::engines::symbolic::SharedSymbolicExpression;
		memcpy(x, &elem.second, sizeof(triton::engines::symbolic::SharedSymbolicExpression));

		a[i].RegSym = *&x;
		i++;
	}

	newA = (RegSymE*)realloc(*OutRegE, sizeof(RegSymE)*n);

	if (newA) {
		memcpy(newA, a, sizeof(RegSymE)*n);

		*OutRegE = newA;
	}
	else {
		memcpy(*OutRegE, a, sizeof(RegSymE)*n);
	}

	delete a;

	return (uint32)n;
}

uint32 getSymbolicRegisters(HandleApi Handle, RegSymE **OutRegE)
{
	std::map<triton::arch::register_e, triton::engines::symbolic::SharedSymbolicExpression> s = Handle->getSymbolicRegisters();
	auto   n = mapRegEToArray(s, OutRegE);

	return n;
}

uint32 mMemToArray(std::map<triton::uint64, triton::engines::symbolic::SharedSymbolicExpression> mMemSym, MemSymE **ouMemSym)
{	
	auto   n = mMemSym.size();

	if (n < 1) return 0;

	MemSymE* a = new MemSymE[n];
	MemSymE* newA = NULL;

	auto i = 0;
	for (auto elem : mMemSym) {
		a[i].mem    = elem.first;
		a[i].MemSym = &elem.second;

		triton::engines::symbolic::SharedSymbolicExpression* x = new triton::engines::symbolic::SharedSymbolicExpression;
		memcpy(x, &elem.second, sizeof(triton::engines::symbolic::SharedSymbolicExpression));

		a[i].MemSym = *&x;
		i++;
	}

	newA = (MemSymE*)realloc(*ouMemSym, sizeof(MemSymE)*n);

	if (newA) {
		memcpy(newA, a, sizeof(MemSymE)*n);

		*ouMemSym = newA;
	}
	else {
		memcpy(*ouMemSym, a, sizeof(MemSymE)*n);
	}

	delete a;

	return (uint32)n;
}

uint32 EXPORTCALL  getSymbolicMemory(HandleApi Handle, MemSymE **ouMemSym)
{
	auto s = Handle->getSymbolicMemory();
	auto   n = mMemToArray(s, ouMemSym);

	return n;
}

HandleSharedSymbolicExpression getSymbolicMemoryAddr(HandleApi Handle, triton::uint64 addr)
{
	HandleSharedSymbolicExpression cc = new triton::engines::symbolic::SharedSymbolicExpression;
	*cc = Handle->getSymbolicMemory(addr);

	return cc;
}

HandleSharedSymbolicExpression getSymbolicRegister(HandleApi Handle, HandleReg reg)
{
	HandleSharedSymbolicExpression cc = new triton::engines::symbolic::SharedSymbolicExpression;
	*cc = Handle->getSymbolicRegister(*reg);

	return cc;
}

triton::uint8 getSymbolicMemoryValue(HandleApi Handle, triton::uint64 address)
{
	return Handle->getSymbolicMemoryValue(address);
}

uint64 getSymbolicMemoryValueM(HandleApi Handle, HandleMemAcc mem)
{
	return (uint64) Handle->getSymbolicMemoryValue(*mem);
}

retArray getSymbolicMemoryAreaValue(HandleApi Handle, triton::uint64 baseAddr, triton::usize size)
{
	std::vector<triton::uint8> v = Handle->getSymbolicMemoryAreaValue(baseAddr, size);

	uint8* result = new uint8[size];

	memcpy(result, &v.front(), v.size() * sizeof(uint8));


	return result;
}

uint64 getSymbolicRegisterValue(HandleApi Handle, HandleReg reg)
{
	return (uint64)Handle->getSymbolicRegisterValue(*reg);
}

HandleSharedSymbolicVariable convertExpressionToSymbolicVariable(HandleApi Handle, triton::usize exprId, triton::uint32 symVarSize, char* symVarComment)
{
	if (!symVarComment)
	{
		symVarComment = const_cast<char*> ("");
	}
	HandleSharedSymbolicVariable cc = new triton::engines::symbolic::SharedSymbolicVariable;
	*cc = Handle->convertExpressionToSymbolicVariable(exprId, symVarSize, symVarComment);

	return cc;
}

HandleSharedSymbolicVariable convertMemoryToSymbolicVariable(HandleApi Handle, HandleMemAcc mem, char* symVarComment)
{
	if (!symVarComment)
	{
		symVarComment = const_cast<char*> ("");
	}
	HandleSharedSymbolicVariable cc = new triton::engines::symbolic::SharedSymbolicVariable;
	*cc = Handle->convertMemoryToSymbolicVariable(*mem, symVarComment);

	return cc;
}

HandleSharedSymbolicVariable convertRegisterToSymbolicVariable(HandleApi Handle, HandleReg reg, char* symVarComment)
{
	if (!symVarComment)
	{
		symVarComment = const_cast<char*> ("");
	}
	HandleSharedSymbolicVariable cc = new triton::engines::symbolic::SharedSymbolicVariable;
	*cc = Handle->convertRegisterToSymbolicVariable(*reg, symVarComment);

	return cc;
}

HandleAbstractNode getOperandAst(HandleApi Handle, HandleOperandWrapper op)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = Handle->getOperandAst(*op);

	return cc;
}

HandleAbstractNode getOperandAstIstruz(HandleApi Handle, HandleInstruz inst, HandleOperandWrapper op)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = Handle->getOperandAst(*inst, *op);

	return cc;
}

HandleAbstractNode getImmediateAst(HandleApi Handle, HandleImmediate imm)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = Handle->getImmediateAst(*imm);

	return cc;
}

HandleAbstractNode getImmediateAstIstruz(HandleApi Handle, HandleInstruz inst, HandleImmediate imm)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = Handle->getImmediateAst(*inst, *imm);

	return cc;
}

HandleAbstractNode getMemoryAst(HandleApi Handle, HandleMemAcc mem)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = Handle->getMemoryAst(*mem);

	return cc;
}

HandleAbstractNode getMemoryAstIstruz(HandleApi Handle, HandleInstruz inst, HandleMemAcc mem)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = Handle->getMemoryAst(*inst, *mem);

	return cc;
}

HandleAbstractNode getRegisterAst(HandleApi Handle, HandleReg reg)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;

	*cc = Handle->getRegisterAst(*reg);
	return cc;
}

HandleAbstractNode getRegisterAstIstruz(HandleApi Handle, HandleInstruz inst, HandleReg reg)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = Handle->getRegisterAst(*inst, *reg);

	return cc;
}

HandleSharedSymbolicExpression newSymbolicExpression(HandleApi Handle, HandleAbstractNode node, char* comment)
{
	if (!comment)
	{
		comment = const_cast<char*> ("");
	}
	HandleSharedSymbolicExpression cc = new triton::engines::symbolic::SharedSymbolicExpression;
	*cc = Handle->newSymbolicExpression(*node, comment);

	return cc;
}

HandleSharedSymbolicVariable newSymbolicVariable(HandleApi Handle, triton::uint32 varSize, char* comment)
{
	if (!comment)
	{
		comment = const_cast<char*> ("");
	}
	HandleSharedSymbolicVariable cc = new triton::engines::symbolic::SharedSymbolicVariable;
	*cc = Handle->newSymbolicVariable(varSize, comment);

	return cc;
}

void removeSymbolicExpression(HandleApi Handle, triton::usize symExprId)
{
	Handle->removeSymbolicExpression(symExprId);
}

HandleSharedSymbolicExpression createSymbolicExpression(HandleApi Handle, HandleInstruz inst, HandleAbstractNode node, HandleOperandWrapper dst, char* comment)
{
	if (!comment)
	{
		comment = const_cast<char*> ("");
	}
	HandleSharedSymbolicExpression cc = new triton::engines::symbolic::SharedSymbolicExpression;
	*cc = Handle->createSymbolicExpression(*inst, *node, *dst, comment);

	return cc;
}

HandleSharedSymbolicExpression createSymbolicMemoryExpression(HandleApi Handle, HandleInstruz inst, HandleAbstractNode node, HandleMemAcc mem, char* comment)
{
	if (!comment)
	{
		comment = const_cast<char*> ("");
	}
	HandleSharedSymbolicExpression cc = new triton::engines::symbolic::SharedSymbolicExpression;
	*cc = Handle->createSymbolicMemoryExpression(*inst, *node, *mem, comment);

	return cc;
}

HandleSharedSymbolicExpression createSymbolicRegisterExpression(HandleApi Handle, HandleInstruz inst, HandleAbstractNode node, HandleReg reg, char* comment)
{
	if (!comment)
	{
		comment = const_cast<char*> ("");
	}
	HandleSharedSymbolicExpression cc = new triton::engines::symbolic::SharedSymbolicExpression;
	*cc = Handle->createSymbolicRegisterExpression(*inst, *node, *reg, comment);

	return cc;
}

HandleSharedSymbolicExpression createSymbolicFlagExpression(HandleApi Handle, HandleInstruz inst, HandleAbstractNode node, HandleReg flag, char* comment)
{
	if (!comment)
	{
		comment = const_cast<char*> ("");
	}
	HandleSharedSymbolicExpression cc = new triton::engines::symbolic::SharedSymbolicExpression;
	*cc = Handle->createSymbolicFlagExpression(*inst, *node, *flag, comment);

	return cc;
}

HandleSharedSymbolicExpression createSymbolicVolatileExpression(HandleApi Handle, HandleInstruz inst, HandleAbstractNode node, char* comment)
{
	if (!comment)
	{
		comment = const_cast<char*> ("");
	}
	HandleSharedSymbolicExpression cc = new triton::engines::symbolic::SharedSymbolicExpression;
	*cc = Handle->createSymbolicVolatileExpression(*inst, *node, comment);

	return cc;
}

void assignSymbolicExpressionToMemory(HandleApi Handle, HandleSharedSymbolicExpression se, HandleMemAcc mem)
{
	Handle->assignSymbolicExpressionToMemory(*se,*mem);
}

void assignSymbolicExpressionToRegister(HandleApi Handle, HandleSharedSymbolicExpression se, HandleReg reg)
{
	Handle->assignSymbolicExpressionToRegister(*se, *reg);
}

HandleAbstractNode processSimplification(HandleApi Handle, HandleAbstractNode node, bool z3)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = Handle->processSimplification(*node, z3);

	return cc;
}

HandleSharedSymbolicExpression getSymbolicExpressionFromId(HandleApi Handle, triton::usize symExprId)
{
	HandleSharedSymbolicExpression cc = new triton::engines::symbolic::SharedSymbolicExpression;
	*cc = Handle->getSymbolicExpressionFromId(symExprId);

	return cc;
}

HandleSharedSymbolicVariable getSymbolicVariableFromId(HandleApi Handle, triton::usize symVarId)
{
	HandleSharedSymbolicVariable cc = new triton::engines::symbolic::SharedSymbolicVariable;
	*cc = Handle->getSymbolicVariableFromId(symVarId);

	return cc;
}

HandleSharedSymbolicVariable getSymbolicVariableFromName(HandleApi Handle, char* symVarName)
{
	if (!symVarName)
	{
		symVarName = const_cast<char*> ("");
	}
	HandleSharedSymbolicVariable cc = new triton::engines::symbolic::SharedSymbolicVariable;
	*cc = Handle->getSymbolicVariableFromName(symVarName);

	return cc;
}

uint32 getPathConstraints(HandleApi Handle, HandlePathConstraint *& outPath)
{
	std::vector<triton::engines::symbolic::PathConstraint> p = Handle->getPathConstraints();

	auto n = p.size();
	auto i = 0;

	if (n < 1) return 0;

	outPath = new HandlePathConstraint[n];

	for (size_t i = 0; i < n; i++)
	{
		outPath[i] = (HandlePathConstraint)&Handle->getPathConstraints()[i];
	};

	return (uint32)n;
}

HandleAbstractNode getPathConstraintsAst(HandleApi Handle)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = Handle->getPathConstraintsAst();

	return cc;
}

void addPathConstraint(HandleApi Handle, HandleInstruz inst, HandleSharedSymbolicExpression expr)
{
	Handle->addPathConstraint(*inst,*expr);
}

void clearPathConstraints(HandleApi Handle)
{
	Handle->clearPathConstraints();
}

void enableSymbolicEngine(HandleApi Handle, bool flag)
{
	Handle->enableSymbolicEngine(flag);
}

bool isSymbolicEngineEnabled(HandleApi Handle)
{
	return Handle->isSymbolicEngineEnabled();
}

bool isSymbolicExpressionIdExists(HandleApi Handle, triton::usize symExprId)
{
	return Handle->isSymbolicExpressionIdExists(symExprId);
}

bool isMemorySymbolized(HandleApi Handle, HandleMemAcc mem)
{
	return Handle->isMemorySymbolized(*mem);
}

bool isMemorySymbolizedSize(HandleApi Handle, triton::uint64 addr, triton::uint32 size)
{
	return Handle->isMemorySymbolized(addr, size);
}

bool isRegisterSymbolized(HandleApi Handle, HandleReg reg)
{
	return Handle->isRegisterSymbolized(*reg);
}

void concretizeAllMemory(HandleApi Handle)
{
	Handle->concretizeAllMemory();
}

void concretizeAllRegister(HandleApi Handle)
{
	Handle->concretizeAllRegister();
}

void concretizeMemoryM(HandleApi Handle, HandleMemAcc mem)
{
	Handle->concretizeMemory(*mem);
}

void concretizeMemory(HandleApi Handle, triton::uint64 addr)
{
	Handle->concretizeMemory(addr);
}

void concretizeRegister(HandleApi Handle, HandleReg reg)
{
	Handle->concretizeRegister(*reg);
}

uint32 mSliceToArray(std::map<triton::usize, triton::engines::symbolic::SharedSymbolicExpression> mslice, IdSymExpr **outSlice)
{	
	auto   n = mslice.size();

	if (n < 1) return 0;

	IdSymExpr* a = new IdSymExpr[n];
	IdSymExpr* newA = NULL;

	auto i = 0;
	for (auto elem : mslice) {
		a[i].id = elem.first;
		a[i].SymExpr = &elem.second;

		triton::engines::symbolic::SharedSymbolicExpression* x = new triton::engines::symbolic::SharedSymbolicExpression;
		memcpy(x, &elem.second, sizeof(triton::engines::symbolic::SharedSymbolicExpression));

		a[i].SymExpr = *&x;
		i++;
	}

	newA = (IdSymExpr*)realloc(*outSlice, sizeof(IdSymExpr)*n);

	if (newA) {
		memcpy(newA, a, sizeof(IdSymExpr)*n);

		*outSlice = newA;
	}
	else {
		memcpy(*outSlice, a, sizeof(IdSymExpr)*n);
	}

	delete a;

	return (uint32)n;
}

uint32 sliceExpressions(HandleApi Handle, HandleSharedSymbolicExpression expr, IdSymExpr **outSlice)
{
	auto s = Handle->sliceExpressions(*expr);
	auto   n = mSliceToArray(s, outSlice);

	return n;
}

uint32 getTaintedSymbolicExpressions(HandleApi Handle, HandleSharedSymbolicExpression *& outSimbolicExp)
{
	std::list<triton::engines::symbolic::SharedSymbolicExpression>  r = Handle->getTaintedSymbolicExpressions();
	size_t n = r.size();

	if (r.size() < 1) return 0;

	outSimbolicExp = new HandleSharedSymbolicExpression[n];

	auto i = 0;
	for (auto elem : r)
	{
		triton::engines::symbolic::SharedSymbolicExpression* SimExp = new triton::engines::symbolic::SharedSymbolicExpression;
		memcpy(&SimExp, &elem, sizeof(triton::engines::symbolic::SharedSymbolicExpression));

		outSimbolicExp[i] = &*SimExp;
		i++;
	};


	return (uint32)n;
}

uint32 mSymToArray(std::unordered_map<triton::usize, triton::engines::symbolic::SharedSymbolicExpression> mslice, IdSymExpr **outSlice)
{
	auto   n = mslice.size();

	if (n < 1) return 0;

	IdSymExpr* a = new IdSymExpr[n];
	IdSymExpr* newA = NULL;

	auto i = 0;
	for (auto elem : mslice) {
		a[i].id = elem.first;
		a[i].SymExpr = &elem.second;

		triton::engines::symbolic::SharedSymbolicExpression* x = new triton::engines::symbolic::SharedSymbolicExpression;
		memcpy(x, &elem.second, sizeof(triton::engines::symbolic::SharedSymbolicExpression));

		a[i].SymExpr = *&x;
		i++;
	}

	newA = (IdSymExpr*)realloc(*outSlice, sizeof(IdSymExpr)*n);

	if (newA) {
		memcpy(newA, a, sizeof(IdSymExpr)*n);

		*outSlice = newA;
	}
	else {
		memcpy(*outSlice, a, sizeof(IdSymExpr)*n);
	}

	delete a;

	return (uint32)n;
}

uint32 getSymbolicExpressions(HandleApi Handle, IdSymExpr **outSymMap)
{
	auto s = Handle->getSymbolicExpressions();
	auto   n = mSymToArray(s, outSymMap);

	return n;
}

uint32 mSymVarToArray(std::unordered_map<triton::usize, triton::engines::symbolic::SharedSymbolicVariable> mSymVar, IdSymVar **outSymVar)
{
	auto   n = mSymVar.size();

	if (n < 1) return 0;

	IdSymVar* a = new IdSymVar[n];
	IdSymVar* newA = NULL;

	auto i = 0;
	for (auto elem : mSymVar) {
		a[i].id = elem.first;
		a[i].SymVar = &elem.second;

		triton::engines::symbolic::SharedSymbolicVariable* x = new triton::engines::symbolic::SharedSymbolicVariable;
		memcpy(x, &elem.second, sizeof(triton::engines::symbolic::SharedSymbolicVariable));

		a[i].SymVar = *&x;
		i++;
	}

	newA = (IdSymVar*)realloc(*outSymVar, sizeof(IdSymVar)*n);

	if (newA) {
		memcpy(newA, a, sizeof(IdSymVar)*n);

		*outSymVar = newA;
	}
	else {
		memcpy(*outSymVar, a, sizeof(IdSymVar)*n);
	}

	delete a;

	return (uint32)n;
}

uint32  getSymbolicVariables(HandleApi Handle, IdSymVar **outSymVar)
{
	std::unordered_map<triton::usize, triton::engines::symbolic::SharedSymbolicVariable> m = Handle->getSymbolicVariables();
	auto   n = mSymVarToArray(m, outSymVar);

	return n;
}

uint64 getConcreteVariableValue(HandleApi Handle, HandleSharedSymbolicVariable symVar)
{
	return (uint64) Handle->getConcreteVariableValue(*symVar);
}

void setConcreteVariableValue(HandleApi Handle, HandleSharedSymbolicVariable symVar, const uint64 value)
{
	Handle->setConcreteVariableValue(*symVar, value);
}

void initLeaAst(HandleApi Handle, HandleMemAcc mem, bool force)
{
	Handle->getSymbolicEngine()->initLeaAst(*mem, force);
}


/* Solver engine API ============================================================================= */

void checkSolver(HandleApi Handle)
{
	Handle->checkSolver();
}

uint32 ModelToArray(std::map<triton::uint32, triton::engines::solver::SolverModel> model, AddrSolver **outModel)
{	
	auto   n = model.size();

	if (n < 1) return 0;

	AddrSolver* a = new AddrSolver[n];
	AddrSolver* newA = NULL;

	auto i = 0;
	for (auto elem : model) {
		a[i].numEle = (uint32)n; // mi serve per le liste
		a[i].id = elem.first;
		a[i].Model = &elem.second;

		triton::engines::solver::SolverModel* x = new triton::engines::solver::SolverModel;
		memcpy(x, &elem.second, sizeof(triton::engines::solver::SolverModel));

		a[i].Model = *&x;
		i++;
	}

	newA = (AddrSolver*)realloc(*outModel, sizeof(AddrSolver)*n);

	if (newA) {
		memcpy(newA, a, sizeof(AddrSolver)*n);

		*outModel = newA;
	}
	else {
		memcpy(*outModel, a, sizeof(AddrSolver)*n);
	}

	delete a;

	return (uint32)n;
}

uint32 getModel(HandleApi Handle, HandleAbstractNode node, AddrSolver **outModel)
{
	auto   s = Handle->getModel(*node);
	auto   n = ModelToArray(s, outModel);

	return n;
}

uint32 getModels(HandleApi Handle, HandleAbstractNode node, triton::uint32 limit, AddrSolver ***outModels)
{

	auto s = Handle->getModels(*node, limit);
	auto len = s.size();

	// riga
	AddrSolver ** lista = (AddrSolver**) malloc( len * sizeof(AddrSolver *) );
	
	

	auto i = 0;
	for (auto const& it : s) {
		AddrSolver* outModel = new AddrSolver;

		auto   n = ModelToArray(it, &outModel);
		
		lista[i] = (AddrSolver *)malloc(n * sizeof(AddrSolver));

		memcpy(lista[i], outModel, sizeof(AddrSolver)*n);
		

		i++;
	}
	
	*outModels = lista;

	return (uint32)len;

}

bool isSat(HandleApi Handle, HandleAbstractNode node)
{
	return Handle->isSat(*node);
}

triton::engines::solver::solver_e getSolver(HandleApi Handle)
{
	return Handle->getSolver();
}


HandleSolverInterface getSolverInstance(HandleApi Handle)
{
	//triton::engines::solver::SolverInterface s = new (triton::engines::solver::SolverInterface);
	return NULL;
}


void setSolver(HandleApi Handle, triton::engines::solver::solver_e kind)
{
	Handle->setSolver(kind);
}

void setCustomSolver(HandleApi Handle, HandleSolverInterface customSolver)
{
	Handle->setCustomSolver(customSolver);
}

bool isSolverValid(HandleApi Handle)
{
	return Handle->isSolverValid();
}

uint64 evaluateAstViaZ3(HandleApi Handle, HandleAbstractNode node)
{
	return (uint64)Handle->evaluateAstViaZ3(*node);
}

HandleAbstractNode processZ3Simplification(HandleApi Handle, HandleAbstractNode node)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = Handle->processZ3Simplification(*node);

	return cc;
}


/* Taint engine API
* ==============================================================================
*/

void checkTaint(HandleApi Handle)
{
	Handle->checkTaint();
}

HandleTaintEngine getTaintEngine(HandleApi Handle)
{
	return Handle->getTaintEngine();
}

uint32 getTaintedMemory(HandleApi Handle, uint64 *& outMemAddrs)
{
	std::set<triton::uint64> s = Handle->getTaintedMemory();
	size_t n = s.size();
		
	if (s.size() < 1) return 0;

	outMemAddrs = new uint64[n];

	std::vector<uint64> v(n);
	std::copy(s.begin(), s.end(), v.begin()); ;
	
	for (int i = 0; i < n; i++)
	{
		outMemAddrs[i] = v[i];
	};

	
	return (uint32)n;
}

uint32 getTaintedRegisters(HandleApi Handle, HandleReg*& outRegs)
{
	std::set<const triton::arch::Register*> r = Handle->getTaintedRegisters();
	size_t n = r.size();

	if (r.size() < 1) return 0;

	outRegs = new HandleReg[n];
	
	auto i = 0;
	for (auto elem: r)
	{
		triton::arch::Register* reg = new triton::arch::Register;
		memcpy(&reg, &elem, sizeof(triton::arch::Register));

		outRegs[i] = &*reg; 
		i++;
	};


	return (uint32)n;
}

void enableTaintEngine(HandleApi Handle, bool flag)
{
	Handle->enableTaintEngine(flag);
}

bool isTaintEngineEnabled(HandleApi Handle)
{
	return Handle->isTaintEngineEnabled();
}

bool isTainted(HandleApi Handle, HandleOperandWrapper op)
{
	return Handle->isTainted(*op);
}

bool isMemoryTainted(HandleApi Handle, triton::uint64 addr, triton::uint32 size)
{
	return Handle->isMemoryTainted(addr, size);
}

bool isMemoryTaintedMem(HandleApi Handle, HandleMemAcc mem)
{
	return Handle->isMemoryTainted(*mem);
}

bool isRegisterTainted(HandleApi Handle, HandleReg reg)
{
	return Handle->isRegisterTainted(*reg);
}

bool setTaint(HandleApi Handle, HandleOperandWrapper op, bool flag)
{
	return Handle->setTaint(*op, flag);
}

bool setTaintMemory(HandleApi Handle, HandleMemAcc mem, bool flag)
{
	return Handle->setTaintMemory(*mem, flag);
}

bool setTaintRegister(HandleApi Handle, HandleReg reg, bool flag)
{
	return Handle->setTaintRegister(*reg, flag);
}

bool taintMemory(HandleApi Handle, triton::uint64 addr)
{
	return Handle->taintMemory(addr);
}

bool taintMemoryMem(HandleApi Handle, HandleMemAcc mem)
{
	return Handle->taintMemory(*mem);
}

bool taintRegister(HandleApi Handle, HandleReg reg)
{
	return Handle->taintRegister(*reg);
}

bool untaintMemory(HandleApi Handle, triton::uint64 addr)
{
	return Handle->untaintMemory(addr);
}

bool untaintMemoryMem(HandleApi Handle, HandleMemAcc mem)
{
	return Handle->untaintMemory(*mem);
}

bool untaintRegister(HandleApi Handle, HandleReg reg)
{
	return Handle->untaintRegister(*reg);
}

bool taintUnion(HandleApi Handle, HandleOperandWrapper op1, HandleOperandWrapper op2)
{
	return Handle->taintUnion(*op1, *op2);
}

bool taintAssignment(HandleApi Handle, HandleOperandWrapper op1, HandleOperandWrapper op2)
{
	return Handle->taintAssignment(*op1, *op2);
}

bool taintUnionMemoryImmediate(HandleApi Handle, HandleMemAcc memDst)
{
	return Handle->taintUnionMemoryImmediate(*memDst);
}

bool taintUnionMemoryMemory(HandleApi Handle, HandleMemAcc memDst, HandleMemAcc memSrc)
{
	return Handle->taintUnionMemoryMemory(*memDst,*memSrc);
}

bool taintUnionMemoryRegister(HandleApi Handle, HandleMemAcc memDst, HandleReg regSrc)
{
	return Handle->taintUnionMemoryRegister(*memDst,*regSrc);
}

bool taintUnionRegisterImmediate(HandleApi Handle, HandleReg regDst)
{
	return Handle->taintUnionRegisterImmediate(*regDst);
}

bool taintUnionRegisterMemory(HandleApi Handle, HandleReg regDst, HandleMemAcc memSrc)
{
	return Handle->taintUnionRegisterMemory(*regDst,*memSrc);
}

bool taintUnionRegisterRegister(HandleApi Handle, HandleReg regDst, HandleReg regSrc)
{
	return Handle->taintUnionRegisterRegister(*regDst,*regSrc);
}

bool taintAssignmentMemoryImmediate(HandleApi Handle, HandleMemAcc memDst)
{
	return Handle->taintAssignmentMemoryImmediate(*memDst);
}

bool taintAssignmentMemoryMemory(HandleApi Handle, HandleMemAcc memDst, HandleMemAcc memSrc)
{
	return Handle->taintAssignmentMemoryMemory(*memDst,*memSrc);
}

bool taintAssignmentMemoryRegister(HandleApi Handle, HandleMemAcc memDst, HandleReg regSrc)
{
	return Handle->taintAssignmentMemoryRegister(*memDst,*regSrc);
}

bool taintAssignmentRegisterImmediate(HandleApi Handle, HandleReg regDst)
{
	return Handle->taintAssignmentRegisterImmediate(*regDst);
}

bool taintAssignmentRegisterMemory(HandleApi Handle, HandleReg regDst, HandleMemAcc memSrc)
{
	return Handle->taintAssignmentRegisterMemory(*regDst,*memSrc);
}

bool taintAssignmentRegisterRegister(HandleApi Handle, HandleReg regDst, HandleReg regSrc)
{
	return Handle->taintAssignmentRegisterRegister(*regDst,*regSrc);
}


/*  BitsVector ========================================================================== */

HandleBV BVCreateBitsVector()
{
	return new BitsVector();
}

HandleBV BVCreateBitsVectorHL(triton::uint32 high, triton::uint32 low)
{
	return new BitsVector(high, low);
}

HandleBV BVCreateBitsVectorFrom(HandleBV other)
{
	return new BitsVector(*other);
}

void  BVDelete(HandleBV Handle)
{
	delete Handle;
}

void BVsetHigh(HandleBV bv, triton::uint32 v)
{
	bv->setHigh(v);
}

void BVsetLow(HandleBV bv, triton::uint32 v)
{
	bv->setLow(v);
}


/*  Immediate =========================================================================== */


HandleImmediate IMMCreateImmediate()
{
	return new Immediate();
}

HandleImmediate IMMCreateImmediateS(triton::uint64 value, triton::uint32 size)
{
	return new Immediate(value, size);
}

HandleImmediate IMMCreateImmediateFrom(HandleImmediate other)
{
	return new Immediate(*other);
}

void  IMMDelete(HandleImmediate Handle)
{
	delete Handle;
}

void IMMsetValue(HandleImmediate vImm, triton::uint64 v, triton::uint32 size)
{
	vImm->setValue(v, size);
}


/* Register =============================================================================*/

HandleReg REGCreateRegister()
{
	return new Register();
}

HandleReg REGCreateRegisterS(triton::arch::register_e regId, char * name, triton::arch::register_e parent, triton::uint32 high, triton::uint32 low, bool vmutable)
{
	return new Register(regId,name,parent,high,low,vmutable);
}

HandleReg RegCreateRegisterC(HandleCpuInterface cpu, triton::arch::register_e regId)
{
	return new Register(*cpu,regId);
}

HandleReg REGCreateRegisterFrom(HandleReg other)
{
	return new Register(*other);
}

void  REGDelete(HandleReg Handle)
{
	delete Handle;
}

/* MemoryAccess =============================================================================*/

HandleMemAcc MEMCreateMemory()
{
	return new MemoryAccess();
}

HandleMemAcc MEMCreateMemoryS(triton::uint64 address, triton::uint32 size)
{
	return new MemoryAccess(address,size);
}

HandleMemAcc MEMCreateMemoryFrom(const HandleMemAcc other)
{
	return new MemoryAccess(*other);
}

void  MEMDelete(HandleMemAcc Handle)
{
	delete Handle;
}

void MEMsetAddress(HandleMemAcc hMem, triton::uint64 addr)
{
	hMem->setAddress(addr);
}

void MEMsetPcRelative(HandleMemAcc hMem, triton::uint64 addr)
{
	hMem->setPcRelative(addr);
}

void MEMsetSegmentRegister(HandleMemAcc hMem, HandleReg segment)
{
	hMem->setSegmentRegister(*segment);
}

void MEMsetBaseRegister(HandleMemAcc hMem, HandleReg base)
{
	hMem->setBaseRegister(*base);
}

void MEMsetIndexRegister(HandleMemAcc hMem, HandleReg index)
{
	hMem->setIndexRegister(*index);
}

void MEMsetDisplacement(HandleMemAcc hMem, HandleImmediate displacement)
{
	hMem->setDisplacement(*displacement);
}

void MEMsetScale(HandleMemAcc hMem, HandleImmediate scale)
{
	hMem->setScale(*scale);
}

void MEMsetLeaAst(HandleMemAcc hMem, HandleAbstractNode ast)
{
	hMem->setLeaAst(*ast);
}


/* OperandWrapper =============================================================================*/

HandleOperandWrapper OPOperandWrapperI(HandleImmediate imm)
{
	return new OperandWrapper(*imm);
}

HandleOperandWrapper OPOperandWrapperM(HandleMemAcc mem)
{
	return new OperandWrapper(*mem);
}

HandleOperandWrapper OPOperandWrapperR(HandleReg reg)
{
	return new OperandWrapper(*reg);
}

HandleOperandWrapper OPOperandWrapperFrom(HandleOperandWrapper other)
{
	return new OperandWrapper(*other);
}

void  OPDelete(HandleOperandWrapper Handle)
{
	delete Handle;
}

void OPsetImmediate(HandleOperandWrapper hOp, HandleImmediate imm)
{
	hOp->setImmediate(*imm);
}

void OPsetMemory(HandleOperandWrapper hOp, HandleMemAcc mem)
{
	hOp->setMemory(*mem);
}

void OPsetRegister(HandleOperandWrapper hOp, HandleReg reg)
{
	hOp->setRegister(*reg);
}

/*  Istruction ============================================================================== */

HandleInstruz IInstruction()
{
	return new Instruction();
}

HandleInstruz IInstructionOP(triton::uint8 * opcode, triton::uint32 opSize)
{
	return new Instruction(opcode, opSize);
}

HandleInstruz IInstructionFrom(HandleInstruz other)
{
	return new Instruction(*other);
}

void  IDelete(HandleInstruz Handle)
{
	delete Handle;
}

void Iclear(HandleInstruz hIstr)
{
	hIstr->clear();
}

bool IisReadFrom(HandleInstruz hIstr, HandleOperandWrapper target)
{
	return hIstr->isReadFrom(*target);
}

bool IisWriteTo(HandleInstruz hIstr, HandleOperandWrapper target)
{
	return hIstr->isWriteTo(*target);
}

void IsetLoadAccess(HandleInstruz hIstr, HandleMemAcc mem, HandleAbstractNode node)
{
	hIstr->setLoadAccess(*mem,*node);
}

void IremoveLoadAccess(HandleInstruz hIstr, HandleMemAcc mem)
{
	hIstr->removeLoadAccess(*mem);
}

void IsetStoreAccess(HandleInstruz hIstr, HandleMemAcc mem, HandleAbstractNode node)
{
	hIstr->setStoreAccess(*mem,*node);
}

void IremoveStoreAccess(HandleInstruz hIstr, HandleMemAcc mem)
{
	hIstr->removeStoreAccess(*mem);
}

void IsetReadRegister(HandleInstruz hIstr, HandleReg reg, HandleAbstractNode node)
{
	hIstr->setReadRegister(*reg,*node);
}

void IremoveReadRegister(HandleInstruz hIstr, HandleReg reg)
{
	hIstr->removeReadRegister(*reg);
}

void IsetWrittenRegister(HandleInstruz hIstr, HandleReg reg, HandleAbstractNode node)
{
	hIstr->setWrittenRegister(*reg,*node);
}

void IremoveWrittenRegister(HandleInstruz hIstr, HandleReg reg)
{
	hIstr->removeWrittenRegister(*reg);
}

void IsetReadImmediate(HandleInstruz hIstr, HandleImmediate imm, HandleAbstractNode node)
{
	hIstr->setReadImmediate(*imm,*node);
}

void IremoveReadImmediate(HandleInstruz hIstr, HandleImmediate imm)
{
	hIstr->removeReadImmediate(*imm);
}

void IsetUndefinedRegister(HandleInstruz hIstr, HandleReg reg)
{
	hIstr->setUndefinedRegister(*reg);
}

void IremoveUndefinedRegister(HandleInstruz hIstr, HandleReg reg)
{
	hIstr->removeUndefinedRegister(*reg);
}

void IsetAddress(HandleInstruz hIstr, triton::uint64 addr)
{
	hIstr->setAddress(addr);
}

void IsetBranch(HandleInstruz hIstr, bool flag)
{
	hIstr->setBranch(flag);
}

void IsetCodeCondition(HandleInstruz hIstr, triton::arch::aarch64::condition_e codeCondition)
{
	hIstr->setCodeCondition(codeCondition);
}

void IsetConditionTaken(HandleInstruz hIstr, bool flag)
{
	hIstr->setConditionTaken(flag);
}

void IsetControlFlow(HandleInstruz hIstr, bool flag)
{
	hIstr->setControlFlow(flag);
}

void IsetDisassembly(HandleInstruz hIstr, char * str)
{
	hIstr->setDisassembly(str);
}

void IsetOpcode(HandleInstruz hIstr, const triton::uint8 * opcode, triton::uint32 size)
{
	hIstr->setOpcode(opcode,size);
}

void IsetPrefix(HandleInstruz hIstr, triton::arch::x86::prefix_e prefix)
{
	hIstr->setPrefix(prefix);
}

void IsetSize(HandleInstruz hIstr, triton::uint32 size)
{
	hIstr->setSize(size);
}

void IsetTaintBool(HandleInstruz hIstr, bool state)
{
	hIstr->setTaint(state);
}

void IsetTaint(HandleInstruz hIstr)
{
	hIstr->setTaint();
}

void IsetThreadId(HandleInstruz hIstr, triton::uint32 tid)
{
	hIstr->setThreadId(tid);
}

void IsetType(HandleInstruz hIstr, triton::uint32 type)
{
	hIstr->setType(type);
}

uint32 IgetLoadAccess(HandleInstruz hIstr, MemNode *& outArray)
{
	const auto& loadAccess = hIstr->getLoadAccess();
	size_t n = loadAccess.size();

	outArray = new MemNode[n];

	triton::uint32 index = 0;
	for (auto&& it : loadAccess) {
		{
			outArray[index].mem  = HandleMemAcc(&it.first);
			outArray[index].node = HandleAbstractNode(&it.second);
			index++;
		};
		
	}
	return (uint32)n;
}

uint32 IgetStoreAccess(HandleInstruz hIstr, MemNode *& outArray)
{
	const auto& storeAccess = hIstr->getStoreAccess();
	size_t n = storeAccess.size();

	outArray = new MemNode[n];

	triton::uint32 index = 0;
	for (auto&& it : storeAccess) {
		{
			outArray[index].mem  = HandleMemAcc(&it.first);
			outArray[index].node = HandleAbstractNode(&it.second);
			index++;
		};
				
	}
	return (uint32)n;
}

uint32 EXPORTCALL IgetReadRegisters(HandleInstruz hIstr, RegNode *& outArray)
{
	const auto& ReadRegisters = hIstr->getReadRegisters();
	size_t n = ReadRegisters.size();

	outArray = new RegNode[n];

	triton::uint32 index = 0;
	for (auto&& it : ReadRegisters) {
		{
			outArray[index].reg  = HandleReg(&it.first);
			outArray[index].node = HandleAbstractNode(&it.second);
			index++;
		};
				
	}
	return (uint32)n;
}

uint32 EXPORTCALL IgetWrittenRegisters(HandleInstruz hIstr, RegNode *& outArray)
{
	const auto& WrittenRegisters = hIstr->getWrittenRegisters();
	size_t n = WrittenRegisters.size();

	outArray = new RegNode[n];
	
	triton::uint32 index = 0;
	for (auto&& it: WrittenRegisters) {
		{
			outArray[index].reg  = HandleReg(&it.first);
			outArray[index].node = HandleAbstractNode(&it.second);
			index++;
		};
				
	}
	
	return (uint32)n;
}

uint32 EXPORTCALL IgetReadImmediates(HandleInstruz hIstr, ImmNode *& outArray)
{
	const auto& ReadImm = hIstr->getReadImmediates();
	size_t n = ReadImm.size();

	outArray = new ImmNode[n];

	triton::uint32 index = 0;
	for (auto&& it : ReadImm) {
		{
			outArray[index].imm  = HandleImmediate(&it.first);
			outArray[index].node = HandleAbstractNode(&it.second);
			index++;
		};
				
	}
	return (uint32)n;
}

uint32 EXPORTCALL IgetUndefinedRegisters(HandleInstruz hIstr, HandleReg *& outArray)
{
	auto ureg = hIstr->getUndefinedRegisters();
	size_t n = ureg.size();

	outArray = new HandleReg[n];
	
	uint32 i = 0;
	for ( auto& it: hIstr->getUndefinedRegisters())
	{
		outArray[i] = (HandleReg)&it;
		i++;
	};

	return (uint32)n;
}

uint32 EXPORTCALL IgetsymbolicExpressions(HandleInstruz hIstr, HandleSharedSymbolicExpression *& outArray)
{
	size_t n = hIstr->symbolicExpressions.size();

	outArray = new HandleSharedSymbolicExpression[n];

	auto i = 0;
	for ( auto& it : hIstr->symbolicExpressions)
	{
		outArray[i] = (HandleSharedSymbolicExpression)&it;
		i++;
	};
	
	return (uint32)n;
}

uint32 EXPORTCALL IGetOperand(HandleInstruz hIstr, HandleOperandWrapper ** outArray)
{
	size_t n = hIstr->operands.size();

	HandleOperandWrapper* a = new HandleOperandWrapper[n];
	HandleOperandWrapper* newA = NULL;
		
	for (int i = 0; i < n; i++)
	{
		a[i] = &hIstr->operands[i];
	};

	newA = (HandleOperandWrapper*)realloc(*outArray, sizeof(HandleOperandWrapper)*n);

	if (newA) {
		memcpy(newA, a, sizeof(HandleOperandWrapper)*n);
		*outArray = newA;
	}
	else {
		memcpy(*outArray, a, sizeof(HandleOperandWrapper)*n);
	}

	delete a;

	return (uint32)n;
}



/*  SymbolicExpression ============================================================================== */

HandleSharedSymbolicExpression SECreateSymbolicExpression(HandleAbstractNode node, triton::usize id, triton::engines::symbolic::expression_e type, char * comment)
{
	if (!comment)
	{
		comment = const_cast<char*> ("");
	}
	return new triton::engines::symbolic::SharedSymbolicExpression(new triton::engines::symbolic::SymbolicExpression(*node,id,type,comment));
}

HandleSharedSymbolicExpression SECreateSymbolicExpressionFrom(HandleSharedSymbolicExpression other)
{
	triton::engines::symbolic::SharedSymbolicExpression ii = *other ;
	triton::engines::symbolic::SymbolicExpression oo = triton::engines::symbolic::SymbolicExpression(*ii);

	return new triton::engines::symbolic::SharedSymbolicExpression(new triton::engines::symbolic::SymbolicExpression(oo));
	
}

void SEDelete(HandleSharedSymbolicExpression Handle)
{
	delete Handle;
}

triton::usize SEgetId(HandleSharedSymbolicExpression Handle)
{
	triton::engines::symbolic::SharedSymbolicExpression ii = *Handle;
	
	return ii->getId();
}

bool SEisMemory(HandleSharedSymbolicExpression Handle)
{
	triton::engines::symbolic::SharedSymbolicExpression ii = *Handle;
	
	return ii->isMemory();
}

bool SEisRegister(HandleSharedSymbolicExpression Handle)
{
	triton::engines::symbolic::SharedSymbolicExpression ii = *Handle;
	
	return ii->isRegister();
}

bool SEisSymbolized(HandleSharedSymbolicExpression Handle)
{
	triton::engines::symbolic::SharedSymbolicExpression ii = *Handle;
	
	return ii->isSymbolized();
}

triton::engines::symbolic::expression_e SEgetType(HandleSharedSymbolicExpression Handle)
{
	triton::engines::symbolic::SharedSymbolicExpression ii = *Handle;
	
	return ii->getType();
}

HandleAbstractNode SEgetAst(HandleSharedSymbolicExpression Handle)
{
	triton::engines::symbolic::SharedSymbolicExpression ii = *Handle;
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = ii->getAst();
	
	return cc;
}

HandleAbstractNode SEgetNewAst(HandleSharedSymbolicExpression Handle)
{
	triton::engines::symbolic::SharedSymbolicExpression ii = *Handle;
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = ii->getNewAst();

	return cc;
}

void SEgetComment(HandleSharedSymbolicExpression Handle, char* &comment)
{
	triton::engines::symbolic::SharedSymbolicExpression ii = *Handle;
	
	std::string name = ii->getComment();
	
	comment = (char *)malloc(sizeof(char) * name.size() + 1u);

	std::vector<char> chars(name.c_str(), name.c_str() + name.size() + 1u);
	strcpy_s(comment, name.size() + 1, &chars[0]);
		
}

void SEgetFormattedId(HandleSharedSymbolicExpression Handle, char* &frmId)
{
	triton::engines::symbolic::SharedSymbolicExpression ii = *Handle;

	std::string name = ii->getFormattedId();

	frmId = (char *)malloc(sizeof(char) * name.size() + 1u);

	std::vector<char> chars(name.c_str(), name.c_str() + name.size() + 1u);
	strcpy_s(frmId, name.size() + 1, &chars[0]);
}

void SEgetFormattedComment(HandleSharedSymbolicExpression Handle, char* &frmcomment)
{
	triton::engines::symbolic::SharedSymbolicExpression ii = *Handle;

	std::string name = ii->getFormattedComment();

	frmcomment = (char *)malloc(sizeof(char) * name.size() + 1u);

	std::vector<char> chars(name.c_str(), name.c_str() + name.size() + 1u);
	strcpy_s(frmcomment, name.size() + 1, &chars[0]);
}

void SEgetFormattedExpression(HandleSharedSymbolicExpression Handle, char* &frmExp)
{
	triton::engines::symbolic::SharedSymbolicExpression ii = *Handle;

	std::string name = ii->getFormattedExpression();

	frmExp = (char *)malloc(sizeof(char) * name.size() + 1u);

	std::vector<char> chars(name.c_str(), name.c_str() + name.size() + 1u);
	strcpy_s(frmExp, name.size() + 1, &chars[0]);
}

HandleMemAcc  SEgetOriginMemory(HandleSharedSymbolicExpression Handle)
{
	triton::engines::symbolic::SharedSymbolicExpression ii = *Handle;
	triton::engines::symbolic::SymbolicExpression oo = triton::engines::symbolic::SymbolicExpression(*ii);

	return (HandleMemAcc)&ii->getOriginMemory();
}

HandleReg  SEgetOriginRegister(HandleSharedSymbolicExpression Handle)
{
	triton::engines::symbolic::SharedSymbolicExpression ii = *Handle;
	triton::engines::symbolic::SymbolicExpression oo = triton::engines::symbolic::SymbolicExpression(*ii);

	return (HandleReg)&ii->getOriginRegister();
}

void  SEsetAst(HandleSharedSymbolicExpression Handle, HandleAbstractNode node)
{
	triton::engines::symbolic::SharedSymbolicExpression ii = *Handle;
	triton::engines::symbolic::SymbolicExpression oo = triton::engines::symbolic::SymbolicExpression(*ii);

	ii->setAst(*node);
}

void SEsetComment(HandleSharedSymbolicExpression Handle, char * comment)
{
	triton::engines::symbolic::SharedSymbolicExpression ii = *Handle;
	triton::engines::symbolic::SymbolicExpression oo = triton::engines::symbolic::SymbolicExpression(*ii);

	ii->setComment(comment);
}

void SEsetType(HandleSharedSymbolicExpression Handle, triton::engines::symbolic::expression_e type)
{
	triton::engines::symbolic::SharedSymbolicExpression ii = *Handle;
	triton::engines::symbolic::SymbolicExpression oo = triton::engines::symbolic::SymbolicExpression(*ii);

	ii->setType(type);
}

void SEsetOriginMemory(HandleSharedSymbolicExpression Handle, HandleMemAcc mem)
{
	triton::engines::symbolic::SharedSymbolicExpression ii = *Handle;
	triton::engines::symbolic::SymbolicExpression oo = triton::engines::symbolic::SymbolicExpression(*ii);

	ii->setOriginMemory(*mem);
}

void SEsetOriginRegister(HandleSharedSymbolicExpression Handle, HandleReg reg)
{
	triton::engines::symbolic::SharedSymbolicExpression ii = *Handle;
	triton::engines::symbolic::SymbolicExpression oo = triton::engines::symbolic::SymbolicExpression(*ii);

	ii->setOriginRegister(*reg);
}


/*  Symbolic Variable ===================================================================*/ 

HandleSharedSymbolicVariable EXPORTCALL SVCreateSymbolicVariable(triton::engines::symbolic::variable_e type, triton::uint64 origin, triton::usize id, triton::uint32 size, char * comment)
{
	if (!comment)
	{
		comment = const_cast<char*> ("");
	}
	return new   triton::engines::symbolic::SharedSymbolicVariable( new triton::engines::symbolic::SymbolicVariable(type, origin, id, size, comment) );
}

HandleSharedSymbolicVariable EXPORTCALL SVCreateSymbolicVariableFrom(HandleSharedSymbolicVariable other)
{
	triton::engines::symbolic::SharedSymbolicVariable ii = *other;
	triton::engines::symbolic::SymbolicVariable oo = triton::engines::symbolic::SymbolicVariable(*ii);

	return new triton::engines::symbolic::SharedSymbolicVariable(new triton::engines::symbolic::SymbolicVariable(oo));

}

void EXPORTCALL SVDelete(HandleSharedSymbolicVariable Handle)
{
	delete Handle;
}

triton::engines::symbolic::variable_e EXPORTCALL SVgetType(HandleSharedSymbolicVariable Handle)
{
	triton::engines::symbolic::SharedSymbolicVariable ii = *Handle;

	return ii->getType();
}

void EXPORTCALL SVgetAlias(HandleSharedSymbolicVariable Handle, char *& sAlias)
{
	triton::engines::symbolic::SharedSymbolicVariable ii = *Handle;

	std::string name = ii->getAlias();

	sAlias = (char *)malloc(sizeof(char) * name.size() + 1u);

	std::vector<char> chars(name.c_str(), name.c_str() + name.size() + 1u);
	strcpy_s(sAlias, name.size() + 1, &chars[0]);
}

void EXPORTCALL SVgetComment(HandleSharedSymbolicVariable Handle, char *& sComment)
{
	triton::engines::symbolic::SharedSymbolicVariable ii = *Handle;

	std::string name = ii->getComment();

	sComment = (char *)malloc(sizeof(char) * name.size() + 1u);

	std::vector<char> chars(name.c_str(), name.c_str() + name.size() + 1u);
	strcpy_s(sComment, name.size() + 1, &chars[0]);
}

void EXPORTCALL SVgetName(HandleSharedSymbolicVariable Handle, char *& sName)
{
	triton::engines::symbolic::SharedSymbolicVariable ii = *Handle;

	std::string name = ii->getName();

	sName = (char *)malloc(sizeof(char) * name.size() + 1u);

	std::vector<char> chars(name.c_str(), name.c_str() + name.size() + 1u);
	strcpy_s(sName, name.size() + 1, &chars[0]);
}

triton::usize EXPORTCALL SVgetId(HandleSharedSymbolicVariable Handle)
{
	triton::engines::symbolic::SharedSymbolicVariable ii = *Handle;

	return ii->getId();
}

triton::uint64 EXPORTCALL SVgetOrigin(HandleSharedSymbolicVariable Handle)
{
	triton::engines::symbolic::SharedSymbolicVariable ii = *Handle;

	return ii->getOrigin();
}

triton::uint32 EXPORTCALL SVgetSize(HandleSharedSymbolicVariable Handle)
{
	triton::engines::symbolic::SharedSymbolicVariable ii = *Handle;

	return ii->getSize();
}

void EXPORTCALL SVsetAlias(HandleSharedSymbolicVariable Handle, char * sAlias)
{
	triton::engines::symbolic::SharedSymbolicVariable ii = *Handle;
	triton::engines::symbolic::SymbolicVariable oo = triton::engines::symbolic::SymbolicVariable(*ii);

	ii->setAlias(sAlias);
}

void EXPORTCALL SVsetComment(HandleSharedSymbolicVariable Handle, char * comment)
{
	triton::engines::symbolic::SharedSymbolicVariable ii = *Handle;
	triton::engines::symbolic::SymbolicVariable oo = triton::engines::symbolic::SymbolicVariable(*ii);

	ii->setComment(comment);
}


/*  Modes ============================================================================== */

hModes MCreateModes(void)
{
	hModes ss = new std::shared_ptr<triton::modes::Modes>;

	auto cc = std::make_shared<triton::modes::Modes>();

	*ss = cc;

	return ss;
}

hModes MCreateModesFrom(hModes hMode)
{	
	hModes ss = new std::shared_ptr<triton::modes::Modes>;

	auto cc = std::make_shared<triton::modes::Modes>(*hMode->get());

	*ss = cc;

	return ss;
}

void MDeleteModes(hModes hMode)
{
	delete hMode;
}

bool MisModeEnabled(hModes hMode,triton::modes::mode_e mode)
{
	return hMode->get()->isModeEnabled(mode);
}

void MenableMode(hModes hMode,triton::modes::mode_e mode, bool flag)
{
	hMode->get()->enableMode(mode,flag);
}


/*  AstRapresentation ============================================================================== */

hAstRepresentation RAPCreate(void)
{
	return new triton::ast::representations::AstRepresentation();
}

hAstRepresentation RAPCreateFrom(hAstRepresentation other)
{
	return new triton::ast::representations::AstRepresentation(*other);
}

void RAPDelete(hAstRepresentation handle)
{
	delete handle;
}

triton::uint32 RAPgetMode(hAstRepresentation handle)
{
	return handle->getMode();
}

void RAPsetMode(hAstRepresentation handle, triton::uint32 mode)
{
	handle->setMode(mode);
}


/* ASTContext ============================================================================== */

HandleAstContext CtxCreate(hModes modes)
{
	HandleAstContext ss = new std::shared_ptr<triton::ast::AstContext>;
		
	return ss;
	
}

HandleAstContext CtxCreateFrom(HandleAstContext other)
{
	HandleAstContext ss = new std::shared_ptr<triton::ast::AstContext>;
	
	return ss;

}

void CtxDelete(HandleAstContext hCtx)
{
	delete hCtx;
}

HandleAbstractNode assert_(HandleAstContext hCtx, HandleAbstractNode expr)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->assert_(*expr);

	return cc;
}

HandleAbstractNode bv(HandleAstContext hCtx, triton::uint64 value, triton::uint32 size) 
{	
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bv(value, size);
	
	return cc;
}

HandleAbstractNode bvand(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvand(*expr1,*expr2);

	return cc;
}

HandleAbstractNode EXPORTCALL bvadd(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvadd(*expr1, *expr2);

	return cc;
}

HandleAbstractNode bvashr(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvashr(*expr1, *expr2);

	return cc;
}

HandleAbstractNode bvfalse(HandleAstContext hCtx)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvfalse();

	return cc;
}

HandleAbstractNode bvlshr(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvlshr(*expr1,*expr2);

	return cc;
}

HandleAbstractNode bvmul(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvmul(*expr1, *expr2);

	return cc;
}

HandleAbstractNode bvnand(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvnand(*expr1, *expr2);

	return cc;
}

HandleAbstractNode bvneg(HandleAstContext hCtx, HandleAbstractNode expr)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvneg(*expr);

	return cc;
}

HandleAbstractNode bvnor(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvnor(*expr1, *expr2);

	return cc;
}

HandleAbstractNode bvnot(HandleAstContext hCtx, HandleAbstractNode expr)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvnot(*expr);

	return cc;
}

HandleAbstractNode bvor(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvor(*expr1,*expr2);

	return cc;
}

HandleAbstractNode bvrol_u(HandleAstContext hCtx, HandleAbstractNode expr, triton::uint32 rot)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvrol(*expr,rot);

	return cc;
}

HandleAbstractNode bvrol(HandleAstContext hCtx, HandleAbstractNode expr, HandleAbstractNode rot)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvrol(*expr,*rot);

	return cc;
}

HandleAbstractNode bvror_u(HandleAstContext hCtx, HandleAbstractNode expr, triton::uint32 rot)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvror(*expr,rot);

	return cc;
}

HandleAbstractNode bvror(HandleAstContext hCtx, HandleAbstractNode expr, HandleAbstractNode rot)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvror(*expr,*rot);

	return cc;
}

HandleAbstractNode bvsdiv(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvsdiv(*expr1,*expr2);

	return cc;
}

HandleAbstractNode bvsge(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvsge(*expr1, *expr2);

	return cc;
}

HandleAbstractNode bvsgt(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvsgt(*expr1, *expr2);

	return cc;
}

HandleAbstractNode bvshl(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvshl(*expr1, *expr2);

	return cc;
}

HandleAbstractNode bvsle(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvsle(*expr1, *expr2);

	return cc;
}

HandleAbstractNode bvslt(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvslt(*expr1, *expr2);

	return cc;
}

HandleAbstractNode bvsmod(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvsmod(*expr1, *expr2);

	return cc;
}

HandleAbstractNode bvsrem(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvsrem(*expr1, *expr2);

	return cc;
}

HandleAbstractNode bvsub(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvsub(*expr1, *expr2);

	return cc;
}

HandleAbstractNode bvtrue(HandleAstContext hCtx)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvtrue();

	return cc;
}

HandleAbstractNode bvudiv(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvudiv(*expr1, *expr2);

	return cc;
}

HandleAbstractNode bvuge(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvuge(*expr1, *expr2);

	return cc;
}

HandleAbstractNode bvugt(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvugt(*expr1, *expr2);

	return cc;
}

HandleAbstractNode bvule(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvule(*expr1, *expr2);

	return cc;
}

HandleAbstractNode bvult(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvult(*expr1, *expr2);

	return cc;
}

HandleAbstractNode bvurem(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvurem(*expr1, *expr2);

	return cc;
}

HandleAbstractNode bvxnor(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvxnor(*expr1, *expr2);

	return cc;
}

HandleAbstractNode bvxor(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->bvxor(*expr1, *expr2);

	return cc;
}

HandleAbstractNode concat(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->concat(*expr1, *expr2);

	return cc;
}

HandleAbstractNode declare(HandleAstContext hCtx, HandleAbstractNode var)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->declare(*var);

	return cc;
}

HandleAbstractNode distinct(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->distinct(*expr1, *expr2);

	return cc;
}

HandleAbstractNode equal(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->equal(*expr1, *expr2);

	return cc;
}

HandleAbstractNode extract(HandleAstContext hCtx, triton::uint32 high, triton::uint32 low, HandleAbstractNode expr)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->extract(high, low, *expr);

	return cc;
}

HandleAbstractNode iff(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->iff(*expr1, *expr2);

	return cc;
}

HandleAbstractNode integer(HandleAstContext hCtx, triton::uint64 value)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->integer(value);

	return cc;
}

HandleAbstractNode ite(HandleAstContext hCtx, HandleAbstractNode ifExpr, HandleAbstractNode thenExpr, HandleAbstractNode elseExpr)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->ite(*ifExpr,*thenExpr,*elseExpr);

	return cc;
}

HandleAbstractNode land(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->land(*expr1, *expr2);

	return cc;
}

HandleAbstractNode let(HandleAstContext hCtx, char * alias, HandleAbstractNode expr2, HandleAbstractNode expr3)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->let(alias, *expr2, *expr3);

	return cc;
}

HandleAbstractNode lnot(HandleAstContext hCtx, HandleAbstractNode expr)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->lnot(*expr);

	return cc;
}

HandleAbstractNode lor(HandleAstContext hCtx, HandleAbstractNode expr1, HandleAbstractNode expr2)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->lor(*expr1, *expr2);

	return cc;
}

HandleAbstractNode reference(HandleAstContext hCtx, HandleSharedSymbolicExpression expr)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->reference(*expr);

	return cc;
}

HandleAbstractNode string(HandleAstContext hCtx, char * value)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->string(value);

	return cc;
}

HandleAbstractNode sx(HandleAstContext hCtx, triton::uint32 sizeExt, HandleAbstractNode expr)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->sx(sizeExt,*expr);

	return cc;
}

HandleAbstractNode variable(HandleAstContext hCtx, HandleSharedSymbolicVariable symVar)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->variable(*symVar);

	return cc;
}

HandleAbstractNode zx(HandleAstContext hCtx, triton::uint32 sizeExt, HandleAbstractNode expr)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->zx(sizeExt, *expr);

	return cc;
}

void initVariable(HandleAstContext hCtx, char * name, triton::uint64 value, HandleAbstractNode node)
{
	hCtx->get()->initVariable(name,value,*node);
}

void updateVariable(HandleAstContext hCtx, char * name, triton::uint64 value)
{
	hCtx->get()->updateVariable(name, value);
}

HandleAbstractNode getVariableNode(HandleAstContext hCtx, char * name)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = hCtx->get()->getVariableNode(name);

	return cc;
}

triton::uint64 getVariableValue(HandleAstContext hCtx, char * varName)
{
	return (uint64) hCtx->get()->getVariableValue(varName);
}

void setRepresentationMode(HandleAstContext hCtx, triton::uint32 mode)
{
	hCtx->get()->setRepresentationMode(mode);
}

triton::uint32 getRepresentationMode(HandleAstContext hCtx)
{
	return hCtx->get()->getRepresentationMode();
}


/*  Ast Node ============================================================================== */

HandleAbstractNode Node_Create(triton::ast::ast_e type, HandleAstContext ctxt)
{
	HandleAbstractNode ss = new std::shared_ptr<triton::ast::AbstractNode>;
	
	return ss;
	
}

void Node_Delete(HandleAbstractNode Handle)
{
	delete Handle;
}

HandleAstContext Node_getContext(HandleAbstractNode Handle)
{
	HandleAstContext cc = new std::shared_ptr<triton::ast::AstContext>;
	*cc = Handle->get()->getContext();

	return cc;
}

triton::ast::ast_e Node_getType(HandleAbstractNode Handle)
{
	return Handle->get()->getType();
}

triton::uint32 Node_getBitvectorSize(HandleAbstractNode Handle)
{
	return Handle->get()->getBitvectorSize();
}

triton::uint64 Node_getBitvectorMask(HandleAbstractNode Handle)
{
	return (uint64)Handle->get()->getBitvectorMask();
}

bool Node_isSigned(HandleAbstractNode Handle)
{
	return Handle->get()->isSigned();
}

bool Node_isSymbolized(HandleAbstractNode Handle)
{
	return Handle->get()->isSymbolized();
}

bool Node_isLogical(HandleAbstractNode Handle)
{
	return Handle->get()->isLogical();
}

bool Node_equalTo(HandleAbstractNode Handle, HandleAbstractNode other)
{
	return Handle->get()->equalTo(*other);
}

triton::uint64 Node_evaluate(HandleAbstractNode Handle)
{
	return (uint64)Handle->get()->evaluate();
}

uint32 Node_getChildren(HandleAbstractNode Handle, HandleAbstractNode * &outArray)
{
	size_t n = Handle->get()->getChildren().size();

	outArray = new HandleAbstractNode[ n ];

	
	for (int i = 0; i < n; i++)
	{
		std::shared_ptr<triton::ast::AbstractNode>* x = new std::shared_ptr<triton::ast::AbstractNode>;
		memcpy(x, &Handle->get()->getChildren()[i], sizeof(std::shared_ptr<triton::ast::AbstractNode>));

		outArray[i] = &*x;
	};

	
	return (uint32)n;
}

uint32   Node_getParents(HandleAbstractNode Handle, HandleAbstractNode * &outArray)
{
	size_t n = Handle->get()->getParents().size();

	outArray = new HandleAbstractNode[n];


	for (int i = 0; i < n; i++)
	{
		std::shared_ptr<triton::ast::AbstractNode>* x = new std::shared_ptr<triton::ast::AbstractNode>;
		memcpy(x, &Handle->get()->getParents()[i], sizeof(std::shared_ptr<triton::ast::AbstractNode>));
		
		outArray[i] = &*x;
	};


	return (uint32)n;
}

void  Node_removeParent(HandleAbstractNode Handle, HandleAbstractNode p)
{
	Handle->get()->removeParent(p->get());
}

void  Node_setParent(HandleAbstractNode Handle, HandleAbstractNode p)
{
	Handle->get()->setParent(p->get());
}

void Node_setParents(HandleAbstractNode Handle, HandleAbstractNode * p, uint32 size)
{
	std::set<AbstractNode*> setAst;

	for (uint32 i = 0; i < size; i++)
	{
		setAst.insert( p[i]->get() );
	};

	Handle->get()->setParent(setAst);
}

void Node_setBitvectorSize(HandleAbstractNode Handle, triton::uint32 size)
{
	Handle->get()->setBitvectorSize(size);
}

void Node_addChild(HandleAbstractNode Handle, HandleAbstractNode child)
{
	Handle->get()->addChild(*child);
}

void Node_setChild(HandleAbstractNode Handle, triton::uint32 index, HandleAbstractNode child)
{
	Handle->get()->setChild(index, *child);
	Handle->get()->init();
}

void Node_str(HandleAbstractNode Handle, char* &sStr)
{
	std::string name = Handle->get()->str();

	sStr = (char *)malloc(sizeof(char) * name.size() + 1u);

	std::vector<char> chars(name.c_str(), name.c_str() + name.size() + 1u);
	strcpy_s(sStr, name.size() + 1, &chars[0]);
	
}

void Node_init(HandleAbstractNode Handle)
{
	Handle->get()->init();
}

HandleAbstractNode Node_unrollAst(HandleAbstractNode node)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = unrollAst(*node);

	return  cc;
}

HandleAbstractNode Node_duplicate(HandleAbstractNode node)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = newInstance(node->get());

	return  cc;
}

void AstToStr(HandleAbstractNode node,char * &sOut)
{
	std::ostringstream stream;
	
	(*node).get()->getContext()->print(stream, (*node).get());

	std::string str = stream.str();
	 
    sOut = (char *)malloc(sizeof(char) * str.size() + 1u);

	std::vector<char> chars(str.c_str(), str.c_str() + str.size() + 1u);
	strcpy_s(sOut, str.size() + 1, &chars[0]);
}

uint32 Node_lookingForNodes(HandleAbstractNode node, HandleAbstractNode *& outArray, triton::ast::ast_e match)
{
	size_t n = lookingForNodes(*node, match).size();

	outArray = new HandleAbstractNode[n];


	for (int i = 0; i < n; i++)
	{
		std::shared_ptr<triton::ast::AbstractNode>* x = new std::shared_ptr<triton::ast::AbstractNode>;
		memcpy(x, &lookingForNodes(*node, match)[i], sizeof(std::shared_ptr<triton::ast::AbstractNode>));

		outArray[i] = &*x;
	};


	return (uint32)n;
}

triton::uint64 Node_hash(HandleAbstractNode node,triton::uint32 deep)
{
	return (uint64)node->get()->hash(deep);
}

triton::uint64 EXPORTCALL NodeInteger_getInteger(HandleAbstractNode node)
{
	if (node->get()->getType() == INTEGER_NODE) {
		auto v = (IntegerNode*)node->get();
		return (uint64)v->getInteger();
	}
	return 0;
}

HandleSharedSymbolicExpression EXPORTCALL NodeRef_getSymbolicExpression(HandleAbstractNode node)
{
	if (node->get()->getType() == REFERENCE_NODE) {
		HandleSharedSymbolicExpression cc = new std::shared_ptr<triton::engines::symbolic::SymbolicExpression>;
		
		auto v = (ReferenceNode*)node->get();
		*cc = v->getSymbolicExpression();

		return cc;
	}
	return NULL;
}

HandleSharedSymbolicVariable EXPORTCALL NodeRef_getSymbolicVariable(HandleAbstractNode node)
{
	if (node->get()->getType() == VARIABLE_NODE) {
		HandleSharedSymbolicVariable cc = new std::shared_ptr<triton::engines::symbolic::SymbolicVariable>;

		auto v = (VariableNode*)node->get();
		*cc = v->getSymbolicVariable();

		return cc;
	}
	return NULL;
}


/*  Solver Model ======================================================================== */

HandleSolverModel SMCreate()
{
	return new triton::engines::solver::SolverModel();
}


HandleSolverModel SMCreateD(HandleSharedSymbolicVariable variable, uint64 value)
{
	return new triton::engines::solver::SolverModel(*variable, value);
}

HandleSolverModel SMCreateFrom(HandleSolverModel other)
{
	return new triton::engines::solver::SolverModel(*other);
}

void SMDelete(HandleSolverModel Handle)
{
	delete Handle;
}

triton::usize SMgetId(HandleSolverModel Handle)
{
	
	return Handle->getId();
}

uint64 SMgetValue(HandleSolverModel Handle)
{
	return (uint64)Handle->getValue();
}

HandleSharedSymbolicVariable SMgetVariable(HandleSolverModel Handle)
{
	HandleSharedSymbolicVariable cc = new triton::engines::symbolic::SharedSymbolicVariable;
	*cc = Handle->getVariable();

	return  cc;
}

/*  PathConstraint ======================================================================== */

HandlePathConstraint PCPathConstraint()
{
	return new triton::engines::symbolic::PathConstraint();
}

HandlePathConstraint PCPathConstraintFrom(HandlePathConstraint other)
{
	return new triton::engines::symbolic::PathConstraint(*other);
}

void PCDelete(HandlePathConstraint Handle)
{
	delete Handle;
}

void PCaddBranchConstraint(HandlePathConstraint Handle, bool taken, triton::uint64 srcdAddr, triton::uint64 dstAddr, HandleAbstractNode pc)
{
	Handle->addBranchConstraint(taken, srcdAddr, dstAddr, *pc);
}

uint32 PCgetBranchConstraints(HandlePathConstraint Handle, PathDat **OutArray)
{
	std::vector<std::tuple<bool, triton::uint64, triton::uint64, triton::ast::SharedAbstractNode>> br = Handle->getBranchConstraints();

	auto n = br.size();
		
	PathDat* a = new PathDat[n];

	auto i = 0;
	for (auto const& value : br) {
		auto dd = std::get<3>(value);

		a[i].taken   = std::get<0>(value);
		a[i].srcAddr = std::get<1>(value);
		a[i].dstAddr = std::get<2>(value);
		
		std::shared_ptr<triton::ast::AbstractNode>* x = new std::shared_ptr<triton::ast::AbstractNode>;
		memcpy(x, &dd, sizeof(std::shared_ptr<triton::ast::AbstractNode>));
		a[i].pc = *&x;

		i++;
	}
	*OutArray = a;

	return (uint32)n;
}

triton::uint64 PCgetTakenAddress(HandlePathConstraint Handle)
{
	return Handle->getTakenAddress();
}

HandleAbstractNode PCgetTakenPathConstraintAst(HandlePathConstraint Handle)
{
	HandleAbstractNode cc = new std::shared_ptr<triton::ast::AbstractNode>;
	*cc = Handle->getTakenPathConstraintAst();

	return cc;
}

bool PCisMultipleBranches(HandlePathConstraint Handle)
{
	return Handle->isMultipleBranches();
}

