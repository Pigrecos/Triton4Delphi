(************ x86.spec *************)
(* GPR 64-bits. *)
ID_REG_X86_RAX,   // rax
ID_REG_X86_RBX,   // rbx
ID_REG_X86_RCX,   // rcx
ID_REG_X86_RDX,   // rdx
ID_REG_X86_RDI,   // rdi
ID_REG_X86_RSI,   // rsi
ID_REG_X86_RBP,   // rbp
ID_REG_X86_RSP,   // rsp
ID_REG_X86_RIP,   // rip

ID_REG_X86_R8,    // r8
ID_REG_X86_R8D,   // r8d
ID_REG_X86_R8W,   // r8w
ID_REG_X86_R8B,   // r8b

ID_REG_X86_R9,    // r9
ID_REG_X86_R9D,   // r9d
ID_REG_X86_R9W,   // r9w
ID_REG_X86_R9B,   // r9b

ID_REG_X86_R10,   // r10
ID_REG_X86_R10D,  // r10d
ID_REG_X86_R10W,  // r10w
ID_REG_X86_R10B,  // r10b

ID_REG_X86_R11,   // r11
ID_REG_X86_R11D,  // r11d
ID_REG_X86_R11W,  // r11w
ID_REG_X86_R11B,  // r11b

ID_REG_X86_R12,    // r12
ID_REG_X86_R12D,   // r12d
ID_REG_X86_R12W,   // r12w
ID_REG_X86_R12B,   // r12b

ID_REG_X86_R13,    // r13
ID_REG_X86_R13D,   // r13d
ID_REG_X86_R13W,   // r13w
ID_REG_X86_R13B,   // r13b
                  
ID_REG_X86_R14,    // r14
ID_REG_X86_R14D,   // r14d
ID_REG_X86_R14W,   // r14w
ID_REG_X86_R14B,   // r14b
                  
ID_REG_X86_R15,    // r15
ID_REG_X86_R15D,   // r15d
ID_REG_X86_R15W,   // r15w
ID_REG_X86_R15B,   // r15b

(* GPR 32-bits *)
ID_REG_X86_EAX,    // eax
ID_REG_X86_AX,     // ax
ID_REG_X86_AH,     // ah
ID_REG_X86_AL,     // al
                   
ID_REG_X86_EBX,    // ebx
ID_REG_X86_BX,     // bx
ID_REG_X86_BH,     // bh
ID_REG_X86_BL,     // bl
                   
ID_REG_X86_ECX,    // ecx
ID_REG_X86_CX,     // cx
ID_REG_X86_CH,     // ch
ID_REG_X86_CL,     // cl
                   
ID_REG_X86_EDX,    // edx
ID_REG_X86_DX,     // dx
ID_REG_X86_DH,     // dh
ID_REG_X86_DL,     // dl
                   
ID_REG_X86_EDI,    // edi
ID_REG_X86_DI,     // di
ID_REG_X86_DIL,    // dil
                   
ID_REG_X86_ESI,    // esi
ID_REG_X86_SI,     // si
ID_REG_X86_SIL,    // sil
                   
ID_REG_X86_EBP,    // ebp
ID_REG_X86_BP,     // bp
ID_REG_X86_BPL,    // bpl
                   
ID_REG_X86_ESP,    // esp
ID_REG_X86_SP,     // sp
ID_REG_X86_SPL,    // spl
                   
ID_REG_X86_EIP,    // eip
ID_REG_X86_IP,     // ip

ID_REG_X86_EFLAGS, // eflags

(* MMX *)
ID_REG_X86_MM0,    // mm0
ID_REG_X86_MM1,    // mm1
ID_REG_X86_MM2,    // mm2
ID_REG_X86_MM3,    // mm3
ID_REG_X86_MM4,    // mm4
ID_REG_X86_MM5,    // mm5
ID_REG_X86_MM6,    // mm6
ID_REG_X86_MM7,    // mm7

(* STX *)
ID_REG_X86_ST0,  // st0
ID_REG_X86_ST1,  // st1
ID_REG_X86_ST2,  // st2
ID_REG_X86_ST3,  // st3
ID_REG_X86_ST4,  // st4
ID_REG_X86_ST5,  // st5
ID_REG_X86_ST6,  // st6
ID_REG_X86_ST7,  // st7

(* FPU *)
ID_REG_X86_FTW, // ftw
ID_REG_X86_FCW, // fcw
ID_REG_X86_FSW, // fsw
ID_REG_X86_FOP, // fop
ID_REG_X86_FCS, // fcs
ID_REG_X86_FDS, // fds
ID_REG_X86_FIP, // fip
ID_REG_X86_FDP, // fdp

(* SSE *)
ID_REG_X86_MXCSR,
ID_REG_X86_MXCSR_MASK,

ID_REG_X86_XMM0,   // xmm0
ID_REG_X86_XMM1,   // xmm1
ID_REG_X86_XMM2,   // xmm2
ID_REG_X86_XMM3,   // xmm3
ID_REG_X86_XMM4,   // xmm4
ID_REG_X86_XMM5,   // xmm5
ID_REG_X86_XMM6,   // xmm6
ID_REG_X86_XMM7,   // xmm7
ID_REG_X86_XMM8,   // xmm8
ID_REG_X86_XMM9,   // xmm9
ID_REG_X86_XMM10,  // xmm10
ID_REG_X86_XMM11,  // xmm11
ID_REG_X86_XMM12,  // xmm12
ID_REG_X86_XMM13,  // xmm13
ID_REG_X86_XMM14,  // xmm14
ID_REG_X86_XMM15,  // xmm15

(* AVX-256 *)
ID_REG_X86_YMM0,   // ymm0
ID_REG_X86_YMM1,   // ymm1
ID_REG_X86_YMM2,   // ymm2
ID_REG_X86_YMM3,   // ymm3
ID_REG_X86_YMM4,   // ymm4
ID_REG_X86_YMM5,   // ymm5
ID_REG_X86_YMM6,   // ymm6
ID_REG_X86_YMM7,   // ymm7
ID_REG_X86_YMM8,   // ymm8
ID_REG_X86_YMM9,   // ymm9
ID_REG_X86_YMM10,  // ymm10
ID_REG_X86_YMM11,  // ymm11
ID_REG_X86_YMM12,  // ymm12
ID_REG_X86_YMM13,  // ymm13
ID_REG_X86_YMM14,  // ymm14
ID_REG_X86_YMM15,  // ymm15

(* AVX-512 *)
ID_REG_X86_ZMM0,   // zmm0
ID_REG_X86_ZMM1,   // zmm1
ID_REG_X86_ZMM2,   // zmm2
ID_REG_X86_ZMM3,   // zmm3
ID_REG_X86_ZMM4,   // zmm4
ID_REG_X86_ZMM5,   // zmm5
ID_REG_X86_ZMM6,   // zmm6
ID_REG_X86_ZMM7,   // zmm7
ID_REG_X86_ZMM8,   // zmm8
ID_REG_X86_ZMM9,   // zmm9
ID_REG_X86_ZMM10,  // zmm10
ID_REG_X86_ZMM11,  // zmm11
ID_REG_X86_ZMM12,  // zmm12
ID_REG_X86_ZMM13,  // zmm13
ID_REG_X86_ZMM14,  // zmm14
ID_REG_X86_ZMM15,  // zmm15
ID_REG_X86_ZMM16,  // zmm16
ID_REG_X86_ZMM17,  // zmm17
ID_REG_X86_ZMM18,  // zmm18
ID_REG_X86_ZMM19,  // zmm19
ID_REG_X86_ZMM20,  // zmm20
ID_REG_X86_ZMM21,  // zmm21
ID_REG_X86_ZMM22,  // zmm22
ID_REG_X86_ZMM23,  // zmm23
ID_REG_X86_ZMM24,  // zmm24
ID_REG_X86_ZMM25,  // zmm25
ID_REG_X86_ZMM26,  // zmm26
ID_REG_X86_ZMM27,  // zmm27
ID_REG_X86_ZMM28,  // zmm28
ID_REG_X86_ZMM29,  // zmm29
ID_REG_X86_ZMM30,  // zmm30
ID_REG_X86_ZMM31,  // zmm31

(* Control *)
ID_REG_X86_CR0,    // cr0
ID_REG_X86_CR1,    // cr1
ID_REG_X86_CR2,    // cr2
ID_REG_X86_CR3,    // cr3
ID_REG_X86_CR4,    // cr4
ID_REG_X86_CR5,    // cr5
ID_REG_X86_CR6,    // cr6
ID_REG_X86_CR7,    // cr7
ID_REG_X86_CR8,    // cr8
ID_REG_X86_CR9,    // cr9
ID_REG_X86_CR10,   // cr10
ID_REG_X86_CR11,   // cr11
ID_REG_X86_CR12,   // cr12
ID_REG_X86_CR13,   // cr13
ID_REG_X86_CR14,   // cr14
ID_REG_X86_CR15,   // cr15

(* Debug *)
ID_REG_X86_DR0,    // dr0
ID_REG_X86_DR1,    // dr1
ID_REG_X86_DR2,    // dr2
ID_REG_X86_DR3,    // dr3
ID_REG_X86_DR6,    // dr6
ID_REG_X86_DR7,    // dr7
				
(* Flags ID used in the Taint and Symbolic Engines *)
ID_REG_X86_AC,   // ac
ID_REG_X86_AF,   // af
ID_REG_X86_CF,   // cf
ID_REG_X86_DF,   // df
ID_REG_X86_ID,   // id
ID_REG_X86_IF,   // if
ID_REG_X86_NT,   // nt
ID_REG_X86_OF,   // of
ID_REG_X86_PF,   // pf
ID_REG_X86_RF,   // rf
ID_REG_X86_SF,   // sf
ID_REG_X86_TF,   // tf
ID_REG_X86_VIF,  // vif
ID_REG_X86_VIP,  // vip
ID_REG_X86_VM,   // vm
ID_REG_X86_ZF,   // zf

(* SSE flags *)
ID_REG_X86_SSE_IE,   // ie (Invalid Operation Flag)
ID_REG_X86_SSE_DE,   // de (Denormal Flag)
ID_REG_X86_SSE_ZE,   // ze (Divide By Zero Flag)
ID_REG_X86_SSE_OE,   // oe (Overflow Flag)
ID_REG_X86_SSE_UE,   // ue (Underflow Flag)
ID_REG_X86_SSE_PE,   // pe (Precision Flag)
ID_REG_X86_SSE_DAZ,  // daz (Invalid Operation Flag)
ID_REG_X86_SSE_IM,   // im (Invalid Operation Mask)
ID_REG_X86_SSE_DM,   // dm (Denormal Mask)
ID_REG_X86_SSE_ZM,   // zm (Divide By Zero Mask)
ID_REG_X86_SSE_OM,   // om (Overflow Mask)
ID_REG_X86_SSE_UM,   // um (Underflow Mask)
ID_REG_X86_SSE_PM,   // pm (Precision Mask)
ID_REG_X86_SSE_RL,   // rl (Round Negative)
ID_REG_X86_SSE_RH,   // rh (Round Positive)
ID_REG_X86_SSE_FZ,   // fz (Flush To Zero)

(* FPU flags *)
ID_REG_X86_FCW_IM,  // im (Invalid Operation Mask)
ID_REG_X86_FCW_DM,  // dm (Denormal Mask)
ID_REG_X86_FCW_ZM,  // zm (Divide By Zero Mask)
ID_REG_X86_FCW_OM,  // om (Overflow Mask)
ID_REG_X86_FCW_UM,  // um (Underflow Mask)
ID_REG_X86_FCW_PM,  // pm (Precision Mask)
ID_REG_X86_FCW_PC,  // pc (Precision Control)
ID_REG_X86_FCW_RC,  // rc (Rounding Control)
ID_REG_X86_FCW_X,   // x  (Infinity Control)

ID_REG_X86_FSW_IE,  // ie (Invalid Operation Mask)
ID_REG_X86_FSW_DE,  // de (Denormal Mask)
ID_REG_X86_FSW_ZE,  // ze (Divide By Zero Mask)
ID_REG_X86_FSW_OE,  // oe (Overflow Mask)
ID_REG_X86_FSW_UE,  // ue (Underflow Mask)
ID_REG_X86_FSW_PE,  // pe (Precision Mask)
ID_REG_X86_FSW_SF,  // sf (Stack Fault)
ID_REG_X86_FSW_ES,  // ef (Error Summary Status)
ID_REG_X86_FSW_C0,  // c0 (Condition Code 0)
ID_REG_X86_FSW_C1,  // c1 (Condition Code 1)
ID_REG_X86_FSW_C2,  // c2 (Condition Code 2)
ID_REG_X86_FSW_TOP, // top (Top of Stack Pointer)
ID_REG_X86_FSW_C3,  // c3 (Condition Code 3)
ID_REG_X86_FSW_B,   // b (FPU Busy)

(* EFER *)
ID_REG_X86_EFER, // efer

ID_REG_X86_EFER_TCE,   // efer_tce
ID_REG_X86_EFER_FFXSR, // efer_ffxsr
ID_REG_X86_EFER_LMSLE, // efer_lmsle
ID_REG_X86_EFER_SVME,  // efer_svme
ID_REG_X86_EFER_NXE,   // efer_nxe
ID_REG_X86_EFER_LMA,   // efer_lma
ID_REG_X86_EFER_LME,   // efer_lme
ID_REG_X86_EFER_SCE,   // efer_sce

(* Segments *)
ID_REG_X86_CS,     // Code Segment
ID_REG_X86_DS,     // Data Segment
ID_REG_X86_ES,     // Extra Segment
ID_REG_X86_FS,     // FS Segment
ID_REG_X86_GS,     // GS Segment
ID_REG_X86_SS,     // Stack Segment

(* Model-Specific Registers (MSR) *)
ID_REG_X86_TSC,     // Time-Stamp Counter

(******** aarch64.spec *********)
// Thirty-one 64-bit general-purpose registers 
ID_REG_AARCH64_X0,  // x0
ID_REG_AARCH64_X1,  // x1
ID_REG_AARCH64_X2,  // x2
ID_REG_AARCH64_X3,  // x3
ID_REG_AARCH64_X4,  // x4
ID_REG_AARCH64_X5,  // x5
ID_REG_AARCH64_X6,  // x6
ID_REG_AARCH64_X7,  // x7
ID_REG_AARCH64_X8,  // x8
ID_REG_AARCH64_X9,  // x9
ID_REG_AARCH64_X10, // x10
ID_REG_AARCH64_X11, // x11
ID_REG_AARCH64_X12, // x12
ID_REG_AARCH64_X13, // x13
ID_REG_AARCH64_X14, // x14
ID_REG_AARCH64_X15, // x15
ID_REG_AARCH64_X16, // x16
ID_REG_AARCH64_X17, // x17
ID_REG_AARCH64_X18, // x18
ID_REG_AARCH64_X19, // x19
ID_REG_AARCH64_X20, // x20
ID_REG_AARCH64_X21, // x21
ID_REG_AARCH64_X22, // x22
ID_REG_AARCH64_X23, // x23
ID_REG_AARCH64_X24, // x24
ID_REG_AARCH64_X25, // x25
ID_REG_AARCH64_X26, // x26
ID_REG_AARCH64_X27, // x27
ID_REG_AARCH64_X28, // x28
ID_REG_AARCH64_X29, // x29
ID_REG_AARCH64_X30, // x30 (LR register)
ID_REG_AARCH64_W0,   // w0
ID_REG_AARCH64_W1,   // w1
ID_REG_AARCH64_W2,   // w2
ID_REG_AARCH64_W3,   // w3
ID_REG_AARCH64_W4,   // w4
ID_REG_AARCH64_W5,   // w5
ID_REG_AARCH64_W6,   // w6
ID_REG_AARCH64_W7,   // w7
ID_REG_AARCH64_W8,   // w8
ID_REG_AARCH64_W9,   // w9
ID_REG_AARCH64_W10, // w10
ID_REG_AARCH64_W11, // w11
ID_REG_AARCH64_W12, // w12
ID_REG_AARCH64_W13, // w13
ID_REG_AARCH64_W14, // w14
ID_REG_AARCH64_W15, // w15
ID_REG_AARCH64_W16, // w16
ID_REG_AARCH64_W17, // w17
ID_REG_AARCH64_W18, // w18
ID_REG_AARCH64_W19, // w19
ID_REG_AARCH64_W20, // w20
ID_REG_AARCH64_W21, // w21
ID_REG_AARCH64_W22, // w22
ID_REG_AARCH64_W23, // w23
ID_REG_AARCH64_W24, // w24
ID_REG_AARCH64_W25, // w25
ID_REG_AARCH64_W26, // w26
ID_REG_AARCH64_W27, // w27
ID_REG_AARCH64_W28, // w28
ID_REG_AARCH64_W29, // w29
ID_REG_AARCH64_W30, // w30

// Flag register
ID_REG_AARCH64_SPSR, // SPSR

// Stack pointer registers
ID_REG_AARCH64_SP,   // SP
ID_REG_AARCH64_WSP,  // WSP

// Program counter register
ID_REG_AARCH64_PC, // PC

// Zero registers
ID_REG_AARCH64_XZR,  // XZR
ID_REG_AARCH64_WZR,  // WZR

// Unique flag registers
ID_REG_AARCH64_C,   // C (Carry)
RID_REG_AARCH64_N,  // N (Negative)
ID_REG_AARCH64_V,   // V (Overflow)
ID_REG_AARCH64_Z,   // Z (Zero)
