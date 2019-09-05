unit UntMain;

interface

(*{$LINK 'remill\Compat.obj'}
{$LINK 'remill\DeadStoreEliminator.obj'}
{$LINK 'remill\Extract.obj'}
{$LINK 'remill\FileSystem.obj'}
{$LINK 'remill\Instruction.obj'}
{$LINK 'remill\IntrinsicTable.obj'}
{$LINK 'remill\Lifter.obj'}

{$LINK 'remill\Name.obj'}
{$LINK 'remill\Optimizer.obj'}
{$LINK 'remill\OS.obj'}
{$LINK 'remill\StateUnfolder.obj'}
{$LINK 'remill\Util.obj'} *)


uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, System.Diagnostics, System.TimeSpan,
  System.Generics.Collections, System.Generics.Defaults, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    pnl1: TPanel;
    pnl2: TPanel;
    mmoLog: TMemo;
    btnStandard_test: TBitBtn;
    btnOp_Pre: TBitBtn;
    btnCoverage: TBitBtn;
    btnTaint: TBitBtn;
    btnslicing: TBitBtn;
    btnIr: TBitBtn;
    btnsimply: TBitBtn;
    btnCallback: TBitBtn;
    procedure btnStandard_testClick(Sender: TObject);
    procedure btnOp_PreClick(Sender: TObject);
    procedure btnCoverageClick(Sender: TObject);
    procedure btnTaintClick(Sender: TObject);
    procedure btnslicingClick(Sender: TObject);
    procedure btnIrClick(Sender: TObject);
    procedure btnsimplyClick(Sender: TObject);
    procedure btnCallbackClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private

    procedure test6;
    procedure test7;
    procedure constraint;
    procedure Test_Conversion_01;
    procedure test_istruzione;
    procedure constraint1;
    procedure test_taint_get_tainted_memory;
    procedure test_taint_get_tainted_registers;
    { Private declarations }
  public
       procedure Log(msg: string);
  end;

var
  Form1: TForm1;



implementation
       uses Triton.Api,
            Triton.Core,
            Triton.Instruction,
            Triton.AstContext,
            Triton.AstNode,
            Triton.SymbolicExpression,
            Triton.SymbolicVariable,
            Triton.SolverModel,
            Triton.Register,
            Triton.BitVector,
            Triton.Immediate,
            Triton.MemoryAccess,
            Triton.OperandWrapper,
            Triton.Define,

            Callback,
            Simplification,
            proving_opaque_predicates,
            code_coverage_crackme_xor,
            forward_tainting,
            backward_slicing,
            ir,
            test_path_constraint;

{$R *.dfm}

procedure TForm1.Log(msg: string);
begin
    mmoLog.Lines.Add(msg)
end;

procedure TForm1.btnStandard_testClick(Sender: TObject);
var
  mem1, mem2 : MemAccess;
  vApi       : TApi;

  rBv              : BitVector  ;
  rImm,rImm1,rImm2 : Immediate ;

  op1,op2,op3,op4  : OpWrapper;

begin
   mmoLog.Lines.Clear;

   test_Path;

 //test_1
     vApi.Create;

     (* Set the arch *)
     vApi.setArchitecture(ARCH_X86_64);


     vApi.setConcreteRegisterValue( vApi.getRegister(ID_REG_X86_RAX), 12345);

     if (vApi.getConcreteRegisterValue( vApi.getRegister(ID_REG_X86_RAX)) <> 12345) then
          Log('Error getConcreteRegisterValue()')
     else
          Log('OK getConcreteRegisterValue()');

     if (vApi.getConcreteRegisterValue(vApi.getRegister(ID_REG_X86_RAX)) <> 12345) then
        Log('Error x8664getConcreteRegisterValue()')
     else
        Log('OK x8664getConcreteRegisterValue()') ;

     vApi.Free ;
 //End test_1

 //test_2
     vApi.Create;

     (* Set the arch *)
     vApi.setArchitecture(ARCH_X86);

     vApi.setConcreteRegisterValue( vApi.getRegister(ID_REG_X86_EAX), 12345);

     if (vApi.getConcreteRegisterValue( vApi.getRegister(ID_REG_X86_EAX)) <> 12345) then
          Log('Error getConcreteRegisterValue()')
     else
          Log('OK getConcreteRegisterValue()');

     if ( vApi.getConcreteRegisterValue(vApi.getRegister(ID_REG_X86_EAX)) <> 12345) then
        Log('Error x86getConcreteRegisterValue()')
     else
        Log('OK x86getConcreteRegisterValue()');

     vApi.Free ;
 //End test_2

 //taint_reg

     Log('taint_reg=============');

     vApi.Create;
     //* Set the arch */
     vApi.setArchitecture(ARCH_X86_64);

     //* Taint the RAX */
     vApi.taintRegister( vApi.getRegister(ID_REG_X86_AH));

     //* Is RDX tainted ? */
     Log(BoolToStr( vApi.isRegisterTainted( vApi.getRegister(ID_REG_X86_RDX)),True)) ;

     //* Spread RAX into RDX */
     vApi.taintAssignmentRegisterRegister( vApi.getRegister(ID_REG_X86_RDX), vApi.getRegister(ID_REG_X86_RAX));

     //* Is RDX tainted ? */
     Log(BoolToStr(vApi.isRegisterTainted( vApi.getRegister(ID_REG_X86_RDX)),True)) ;

     //* Untaint RDX */
     vApi.untaintRegister( vApi.getRegister(ID_REG_X86_RDX));

     //* Is RDX tainted ? */
     Log(BoolToStr(vApi.isRegisterTainted( vApi.getRegister(ID_REG_X86_RDX)),True)) ;

     vApi.Free ;
     Log('end taint_reg=============');
     Log('') ;
 //taint_reg

 // test_3
      Log('test_3=============');
      rBv.Create;

      rBv.SetHigh(31);
      rBv.SetLow(0);


      Log(rBv.ToStr);
      Log('end test_3=============');
      Log('') ;
 //End test_3

 // test_4
      Log('test_4=============');

      rImm.Create;
      rImm.SetValue($ff,1);

      if (rImm.value <> $ff) then
       Log('KO: errore imm '+ rImm.ToStr)
      else
       Log('OK '+ rImm.ToStr);

      rImm.SetValue($1122,1);

      if (rImm.value <> $22) then
       Log('KO: errore imm '+ rImm.ToStr)
      else
       Log('OK '+ rImm.ToStr);

      rImm.SetValue($1122,2);

      if (rImm.value <> $1122) then
       Log('KO: errore imm '+ rImm.ToStr)
      else
       Log('OK '+ rImm.ToStr);

      if (rImm.low <> 0) or (rImm.high <> 15) then
           Log('KO: errore imm '+ rImm.ToStr);


      rImm1.Create(12345, 8);
      rImm2.Create(12345, 8);

      if (rimm1 <> rimm2) then
         Log('KO: errore - imm1 '+ rImm1.ToStr+'" "'+ 'imm2 '+ rImm2.ToStr);
      Log('end test_4=============');
      Log('') ;
 // End test_4

 // test_Implicit COnversion and Pansichar to std::string
      Log('test_Implicit Conversion and Pansichar to std::string=============');

      vApi.Create;
      //* Set the arch */
      vApi.setArchitecture(ARCH_X86_64);

      var pname : AnsiString := 'rdx';
      var hr    : HandleReg;
      var r1    : Registro ;

      hr := REGCreateRegisterS(ID_REG_X86_RDX, PAnsiChar(pname), ID_REG_X86_RDX, 63, 0, False);
      r1 := Registro(hr) ;

      hr := vApi.getRegister(ID_REG_X86_RDX);
      r1 := Registro(hr) ;
      Log(string(r1.name));
      Log('') ;

      r1 := Registro(hr) ;
      Log(r1.ToStr);

      vApi.Free ;
      Log('test_Implicit Conversion and Pansichar to std::string=============');
      Log('') ;
 // End test_Implicit COnversion

 //  test_Memoria
      Log('test Memoria');
      var m1 : MemAccess;
      m1.Create($1000, 1);
      m1.setSegmentRegister(r1);
      m1.setDisplacement(rImm1);
      Log(m1.ToStr);
      Log('End test Memoria');
      Log('') ;
 //  End test_Memoria

      Log('test_5=============');

      rImm1.Create($ff, 1);
      rImm2.Create($ff, 2);

      mem1.Create($1000, 1);
      mem2.Create($1000, 1);

      op1.Create(rImm1);
      op2.Create(rImm2);
      op3.Create(mem1);
      op4.Create(mem2);

      if (op1 = op2) then   Log('test_5: KO ('+ op1.ToStr +' = ' + op2.ToStr +')')
      else                  Log('test_5: OK ('+ op1.ToStr +' = ' + op2.ToStr +')' );

      if op1.Tipo <> op2.Tipo then Log('test_5: KO ('+ IntToStr(Ord(op1.Tipo)) +' = ' + IntToStr(Ord(op2.Tipo)) +')' );

      if op1.imm <> rImm1 then Log('test_5: KO ('+ op1.imm.ToStr +' <> ' + rImm1.ToStr +')' )
      else                     Log('test_5: OK ('+ op1.imm.ToStr +' <> ' + rImm1.ToStr +')' );

      if (op2 < op2) then Log('test_5: KO ('+ op1.ToStr +' < ' + op2.ToStr +')')
      else                Log('test_5: OK ('+ op1.ToStr +' < ' + op2.ToStr +')' );

      op3.SetMemory(mem2);
      if op3.mem <> mem2 then Log('test_5: KO ('+ op1.mem.ToStr +' <> ' + mem2.ToStr +')' )
      else                    Log('test_5: OK ('+ op1.mem.ToStr +' <> ' + mem2.ToStr +')' );

      op1.SetImmediate(rImm2);
      if op1.imm <> rImm2 then Log('test_5: KO ('+ op1.imm.ToStr +' <> ' + rImm2.ToStr +')' )
      else                     Log('test_5: OK ('+ op1.imm.ToStr +' <> ' + rImm2.ToStr +')' );
  Log(' end test_5=============');
  Log('') ;

   Log('test_6=============');
  test6;
   Log(' end test_6=============');
   Log('') ;

   Log(' test_7=============');
  test7;
   Log(' end test_7=============');
   Log('') ;

   Log(' constraint============');
  constraint;
   Log(' end constraint=============');
   Log('') ;

   Log(' constraint============');
  constraint1;
   Log(' end constraint=============');
   Log('') ;


   Log(' Test_Conversion_01============');
  Test_Conversion_01;
   Log(' end Test_Conversion_01=============');
   Log('') ;

   Log(' Test_Istruzione============');
  test_istruzione;
   Log('') ;

   Log(' Test Taint============');
     test_taint_get_tainted_memory;
     test_taint_get_tainted_registers;
   Log(' End Test Taint============');


   Log('=================================') ;

   Log(' ==End Stadard Test==');

end;

procedure TForm1.btnTaintClick(Sender: TObject);
begin
   mmoLog.Clear;
   Log('==test forward_tainting==');
   main_taint;
   Log(' ==End forward_tainting==');
end;

procedure TForm1.test6;
var
  ctx                : TApi;
  inst1,inst2,inst3,
  inst4,inst5,inst6  : Istruzione;
  mem1               : MemAccess;
  rImm1              : Immediate;
begin
  ctx.Create;
	//* Set the arch */
	ctx.setArchitecture(ARCH_X86_64);

  inst1.Create([$48,$89,$D8],3);// mov rax, rbx
  inst2.Create;

  if ctx.processing(inst1) then
    Log(inst1.ToStr);

  inst2.Copy(inst1);
  if inst2.tipo <>  inst1.tipo then Log('[test_6]-inst2.Copy(inst1): KO')
  else                              Log('[test_6]-inst2.Copy(inst1): OK');

  inst3.Create(inst2);
  if inst3.tipo <>  inst2.tipo then Log('[test_6]-inst3.Create(inst2): KO')
  else                              Log('[test_6]-inst3.Create(inst2): OK');


  var op : OpWrapper;
  op.Create(registro( ctx.getRegister(ID_REG_X86_RBX)));
  if not inst3.isReadFrom( op ) then Log('[test_6]-(!isReadFrom(rbx)): KO ')
  else                               Log('[test_6]-(!isReadFrom(rbx)): OK');

  inst3.removeReadRegister( registro( ctx.getRegister(ID_REG_X86_RBX)) );
  if inst3.isReadFrom( op ) then  Log('[test_6] (!isReadFrom(rbx)): KO')
  else                            Log('[test_6] (!isReadFrom(rbx)): OK');

  op.Free;
  op.Create(registro( ctx.getRegister(ID_REG_X86_RAX)));
  if not inst3.isWriteTo( op ) then Log('[test_6]-(!isWriteTo(rax)): KO')
  else                              Log('[test_6]-(!isWriteTo(rax)): OK');

  inst3.removeWrittenRegister( registro( ctx.getRegister(ID_REG_X86_RAX)) );
  if inst3.isWriteTo( op ) then Log('[test_6]  (!isWriteTo(rax)): KO')
  else                          Log('[test_6]  (!isWriteTo(rax)): OK') ;

  if inst3.Symbolized then  Log('test_6: KO (Symbolized)')
  else                      Log('test_6: OK (Symbolized)') ;

  if inst3.tainted then     Log('test_6: KO (Tainted)')
  else                      Log('test_6: OK (Tainted)');

  inst3.setTaint(true);
  if not inst3.Tainted then Log('test_6: KO (setTaint)')
  else                      Log('test_6: OK (setTaint)') ;

  inst4.Create([$48,$8B,$03],3);// // mov rax, [rbx]
  ctx.processing(inst4);

  op.Free;
  mem1.Create(0,8) ;
  op.Create(mem1);
  if not inst4.isReadFrom( op ) then  Log('test_6: KO !isReadFrom(0x0))')
  else                                Log('test_6: OK !isReadFrom(0x0))') ;

  inst4.removeLoadAccess(mem1);
  if inst4.isReadFrom( op ) then  Log('test_6: KO !isReadFrom(0x0))')
  else                            Log('test_6: OK !isReadFrom(0x0))');

  inst5.Create([$48,$89,$18],3);// // mov [rax], rbx
  ctx.processing(inst5);

  if not inst5.isWriteTo( op ) then Log('test_6: KO (!isWriteTo(0x0))')
  else                              Log('test_6: OK (!isWriteTo(0x0))');

  inst5.removeStoreAccess(mem1);
  if inst5.isWriteTo( op ) then Log('test_6: KO (!isWriteTo(0x0))')
  else                          Log('test_6: OK (!isWriteTo(0x0))') ;

  op.Free;
  rImm1.Create(1,1) ;
  op.Create(rImm1);

  inst6.Create([$B0,$01],2); // mov al, 1
  ctx.processing(inst6);

  if not inst6.isReadFrom( op ) then Log('test_6: KO !isReadFrom(0x0))')
  else                               Log('test_6: OK !isReadFrom(0x0))');

  inst6.removeReadImmediate(rImm1);
  if inst6.isReadFrom( op ) then Log('test_6: KO !isReadFrom(0x0))')
  else                           Log('test_6: OK !isReadFrom(0x0))');

  ctx.free;

end;

procedure TForm1.test7;
var
  ctx : TApi;
begin
    ctx.Create;
    ctx.setArchitecture(ARCH_X86);

    var inst0 : Istruzione ;  inst0.Create( [$37], 1);             // aaa
    var inst1 : Istruzione ;  inst1.Create( [$d5,$0a], 2);         // aad
    var inst2 : Istruzione ;  inst2.Create( [$d5,$08], 2);         // aad 8
    var inst3 : Istruzione ;  inst3.Create( [$d4,$08], 2);         // aam
    var inst4 : Istruzione ;  inst4.Create( [$d4,$08], 2);         // aam 8
    var inst5 : Istruzione ;  inst5.Create( [$3f], 1);             // aas
    var inst6 : Istruzione ;  inst6.Create( [$0f,$06], 2);         // clts
    var inst7 : Istruzione ;  inst7.Create( [$fa], 1);             // cli
    var inst8 : Istruzione ;  inst8.Create( [$0f,$a2], 2);         // cpuid
    var inst10 : Istruzione ;  inst10.Create([$0f,$08], 2);        // invd
    var inst11 : Istruzione ; inst11.Create([$0f,$01,$38], 3);     // invlpg [eax]
    var inst12 : Istruzione ; inst12.Create([$0f,$ae,$38], 3);     // clflush [eax]
    var inst13 : Istruzione ; inst13.Create([$98], 1);             // cwde
    var inst14 : Istruzione ; inst14.Create([$0f,$ae,$10], 3);     // ldmxcsr [eax]
    var inst15 : Istruzione ; inst15.Create([$0f,$ae,$e8], 3);     // lfence
    var inst16 : Istruzione ; inst16.Create([$0f,$ae,$f0], 3);     // mfence
    var inst17 : Istruzione ; inst17.Create([$f2,$0f,$d6,$c9], 4); // movdq2q mm1, xmm1
    var inst18 : Istruzione ; inst18.Create([$66,$0f,$e7,$08], 4); // movntdq [eax], xmm1
    var inst19 : Istruzione ; inst19.Create([$0f,$c3,$18], 3);     // movnti [eax], ebx
    var inst20 : Istruzione ; inst20.Create([$66,$0f,$2b,$08], 4); // movntpd [eax], xmm1
    var inst21 : Istruzione ; inst21.Create([$0f,$2b,$08], 3);     // movntps [eax], xmm1
    var inst22 : Istruzione ; inst22.Create([$0f,$e7,$08], 3);     // movntq [eax], mm1
    var inst23 : Istruzione ; inst23.Create([$0f,$e7,$08], 3);     // movntq [eax], mm1
    var inst24 : Istruzione ; inst24.Create([$f3,$0f,$d6,$ca], 4); // movq2dq xmm1, mm2
    var inst25 : Istruzione ; inst25.Create([$66,$0f,$d6,$08], 4); // movq [eax], xmm1
    var inst26 : Istruzione ; inst26.Create([$66,$0f,$db,$ca], 4); // pand xmm1, xmm2
    var inst27 : Istruzione ; inst27.Create([$66,$0f,$df,$ca], 4); // pandn xmm1, xmm2
    var inst28 : Istruzione ; inst28.Create([$f3,$90], 2);         // pause
    var inst29 : Istruzione ; inst29.Create([$61], 1);             // popal
    var inst30 : Istruzione ; inst30.Create([$9d], 1);             // popfd
    var inst31 : Istruzione ; inst31.Create([$66,$0f,$eb,$ca], 4); // por xmm1, xmm2
    var inst32 : Istruzione ; inst32.Create([$0f,$18,$00], 3);     // prefetchnta [eax]
    var inst33 : Istruzione ; inst33.Create([$0f,$18,$08], 3);     // prefetcht0 [eax]
    var inst34 : Istruzione ; inst34.Create([$0f,$18,$10], 3);     // prefetcht1 [eax]
    var inst35 : Istruzione ; inst35.Create([$0f,$18,$18], 3);     // prefetcht2 [eax]
    var inst36 : Istruzione ; inst36.Create([$0f,$0d,$08], 3);     // prefetchw [eax]
    var inst37 : Istruzione ; inst37.Create([$0f,$70,$ca,$08], 4); // pshufw mm1, mm2, 8
    var inst38 : Istruzione ; inst38.Create([$60], 1);             // pushal
    var inst39 : Istruzione ; inst39.Create([$9c], 1);             // pushfd
    var inst40 : Istruzione ; inst40.Create([$0f,$31], 2);         // rdtsc
    var inst41 : Istruzione ; inst41.Create([$0f,$ae,$f8], 3);     // sfence
    var inst42 : Istruzione ; inst42.Create([$fb], 1);             // sti
    var inst43 : Istruzione ; inst43.Create([$0f,$ae,$18], 3);     // stmxcsr [eax]
    var inst44 : Istruzione ; inst44.Create([$9b], 1);             // wait
    var inst45 : Istruzione ; inst45.Create([$0f,$09], 2);         // wbinvd
    try
      ctx.Processing(inst0);
      ctx.Processing(inst1);
      ctx.Processing(inst2);
      ctx.Processing(inst3);
      ctx.Processing(inst4);
      ctx.Processing(inst5);
      ctx.Processing(inst6);
      ctx.Processing(inst7);
      ctx.Processing(inst8);
      ctx.Processing(inst10);
      ctx.Processing(inst11);
      ctx.Processing(inst12);
      ctx.Processing(inst13);
      ctx.Processing(inst14);
      ctx.Processing(inst15);
      ctx.Processing(inst16);
      ctx.Processing(inst17);
      ctx.Processing(inst18);
      ctx.Processing(inst19);
      ctx.Processing(inst20);
      ctx.Processing(inst21);
      ctx.Processing(inst22);
      ctx.Processing(inst23);
      ctx.Processing(inst24);
      ctx.Processing(inst25);
      ctx.Processing(inst26);
      ctx.Processing(inst27);
      ctx.Processing(inst28);
      ctx.Processing(inst29);
      ctx.Processing(inst30);
      ctx.Processing(inst31);
      ctx.Processing(inst32);
      ctx.Processing(inst33);
      ctx.Processing(inst34);
      ctx.Processing(inst35);
      ctx.Processing(inst36);
      ctx.Processing(inst37);
      ctx.Processing(inst38);
      ctx.Processing(inst39);
      ctx.Processing(inst40);
      ctx.Processing(inst41);
      ctx.Processing(inst42);
      ctx.Processing(inst43);
      ctx.Processing(inst44);
      ctx.Processing(inst45);
    except
      raise Exception.Create('Error During process instruction');
    end;
    Log('test7 OK');
end;

type
  op = record
     addr : UInt32;
     inst : array of Byte;
     size : UInt32;
  end;


procedure TForm1.btnIrClick(Sender: TObject);
begin
    mmoLog.Clear;
    Log('==test Ir==');
    main_Ir ;
    Log(' ==End test Ir==');
end;

procedure TForm1.btnOp_PreClick(Sender: TObject);
begin
    mmoLog.Clear;
    Log('==test opaque predicates==');
    test_trace(trace_1);
    test_trace(trace_2);
    test_trace(trace_3);
    test_trace(trace_4);
    test_trace(trace_5) ;
    Log(' ==End opaque predicates==');
end;

procedure TForm1.btnsimplyClick(Sender: TObject);
begin
    mmoLog.Clear;
    Log('==test Simplification==');
    main_Simplification;
    Log(' ==End Simplification==');
end;

procedure TForm1.btnslicingClick(Sender: TObject);
begin
    mmoLog.Clear;
    Log('==test backward slicing==');
    main_slicing;
    Log(' ==End backward slicing==');
end;

procedure TForm1.btnCallbackClick(Sender: TObject);
begin
    mmoLog.Clear;
    Log('==test Callback==');
    main_Callback;
    Log(' ==End Callback==');
end;

procedure TForm1.btnCoverageClick(Sender: TObject);
begin
    mmoLog.Clear;
    Log('==test code_coverage_crackme_xor==');
    main ;
    Log(' ==End code_coverage_crackme_xor==');
end;

procedure TForm1.constraint;
const trace : array[0..1] of op  = (
                                    (addr:$400017; inst : [$48,$35,$44,$33,$22,$11]; Size:6), //* xor rax, 0x11223344 */
                                    (addr:$0;      inst :[];                         Size:0)
                                   );

var
 api       : TApi;
 inst      : Istruzione;
 raxSymS   : symbolicExp;
 raxFullAst,
 constraint: AbstractNode;
 Ctx       : AstContext;
 n         : TPair<UInt32,SolverModel>;
 model     : TDictionary<UInt32,SolverModel>;

begin
   api.Create;
  (* Set the arch *)
  api.setArchitecture(ARCH_X86_64);
  (* Build an instruction *)
  inst.Create;
  (* Setup opcode *)
  inst.setOpcode(topcode(trace[0].inst), trace[0].size);

  (* Define RAX as symbolic variable *)
  api.convertRegisterToSymbolicVariable( api.getRegister(ID_REG_X86_RAX) );

  (* Process everything *)
  api.processing(inst);

  (* Get the RAX symbolic ID *)
  raxSymS :=  api.getSymbolicRegister( api.getRegister(ID_REG_X86_RAX) ) ;
  Log('Exp. simbolica: '+raxSymS.ToStr);

  (* Get the RAX full AST *)
  raxFullAst := raxSymS.Ast.unrollAst;

  log('test - raxFullAst.Childrens[0] '+ raxFullAst.Childrens[0].str );
  log('test - raxFullAst.Childrens[1] '+ raxFullAst.Childrens[1].ToStr );

  //* Display RAX's AST*/
  log('RAX expr: '+ raxFullAst.ToStr);

  //* Get the context to create and ast constraint*/
  Ctx := api.getAstContext ;

  (* Modify RAX's AST to build the constraint *)
  constraint := Ctx.equal(raxFullAst, Ctx.bv(0, raxFullAst.BitvectorSize ));

  //* Display RAX's AST*/
  log('constraint:  '+ constraint.ToStr);

  (* Ask a model *)
  model := api.getModel(constraint);

  (* Display all symbolic variable value contained in the model *)
  log('Model:');
  for n in model do
  begin
    log('  - Variable id  : ' + IntToStr(n.Key ));
    log('  - Variable name: ' + string( n.Value.Variable.Name)) ;
    log('  - Value        : ' + IntToHex( n.Value.Value,8));
  end;


end;

procedure TForm1.constraint1;
const trace : array[0..2] of op  = (
                                    (addr:$400017; inst:[$83,$f8,$04]; Size:3 ), (* cmp eax, 4 *)
                                    (addr:$400017; inst:[$7E,$12];     Size:2 ), (* jle    0x14  *)
                                    (addr:$0;      inst :[];           Size:0)
                                   );

var
 i         : Integer;
 api       : TApi;
 inst      : Istruzione;
 rip       : AbstractNode;
 whip      : AbstractNode;
 constraint: AbstractNode;
 Ctx       : AstContext;
 n         : TPair<UInt32,SolverModel>;
 models    : TList< TDictionary<UInt32,SolverModel> >;

begin
   api.Create;
  (* Set the arch *)
  api.setArchitecture(ARCH_X86_64);

  (* Make eax symbolic *)
  api.convertRegisterToSymbolicVariable( api.getRegister(ID_REG_X86_EAX),'eax' );

  (* # cmp eax, 4 *)
  inst.Create;
  inst.setOpcode(topcode(trace[0].inst));
  api.processing(inst);

  (* # # jle    0x14 *)
  inst.Create;
  inst.setOpcode(topcode(trace[1].inst));
  api.processing(inst);

  (* #  Build rip *)
  rip := api.getSymbolicRegister( api.getRegister(ID_REG_X86_RIP) ).Ast.unrollAst;

  //* Display rip's AST*/
  log('rip expr: '+ rip.ToStr);

  //* Get the context to create and ast constraint*/
  Ctx := api.getAstContext ;

  whip := Ctx.variable( SymbolicVar (api.newSymbolicVariable(64,'whip') ) );
  (* Modify RAX's AST to build the constraint *)
  constraint := ctx.assert_( Ctx.equal(rip, whip) );

  //* Display RAX's AST*/
  log('constraint:  '+ constraint.ToStr);

  (* Ask a model *)
  models := api.getModels(constraint,12);

  (* Display all symbolic variable value contained in the model *)
  log('Model:');
  for i := 0 to models.Count - 1 do
  begin
      for n in models[i] do
      begin
          log('Solver : '+n.Value.ToStr);
        log('  - Variable id  : ' + IntToStr(n.Key ));
        log('  - Variable name: ' + string( n.Value.Variable.Name)+' comment: ' + string( n.Value.Variable.Comment) );
        log('  - Value        : ' + IntToHex( n.Value.Value,8));
      end;
  end;

end;

procedure TForm1.FormShow(Sender: TObject);
var
  pA : string;
begin
    pA := ExtractFilePath( Application.ExeName);

    SetCurrentDir(pA) ;
end;

procedure TForm1.Test_Conversion_01;
var
  api       : TApi;
  inst      : Istruzione;
  j         : Integer;
  g         : TArray<Registro>;
  ass       : TArray<AbstractNode>;
  tt        : TDictionary<Registro,AbstractNode> ;
  node      : AbstractNode;
  astCtxt   : AstContext;
begin
    api.Create;
    (* Set the arch *)
    api.setArchitecture(ARCH_X86_64);
    (* Build an instruction *)
    api.setAstRepresentationMode(ord(PYTHON_REPRESENTATION));

    (* Setup opcode *)
    inst.Create;
    inst.setOpcode([$48,01,$D8],3);
    inst.setAddress($00400000);

    api.setConcreteRegisterValue( Api.getRegister(ID_REG_X86_RAX), $1122334455667788);
    api.setConcreteRegisterValue( Api.getRegister(ID_REG_X86_RBX), $8877665544332211);

    (* Process everything *)
    api.processing(inst);

    log('test symbolicExpressions: ');
    inst.symbolicExpressions  ;
    for j := 0 to inst.symbolicExpressions.Count -1 do
        log( inst.symbolicExpressions.Items[j].ToStr );


    tt := inst.writtenRegisters;

    g   := tt.Keys.ToArray ;

    ass := tt.Values.ToArray ;

    for j := 0 to High(g) do  log('registro: '+g[j].ToStr);

    for j := 0 to High(ass) do log('Node: '+ass[j].ToStr);

    api.free ;
    //
    api.Create;
    (* Set the arch *)
    api.setArchitecture(ARCH_X86_64);
    astCtxt := api.getAstContext;

    log('test AstNode: ');
    node := astCtxt.bvadd(astCtxt.bv(1, 8), astCtxt.bvxor(astCtxt.bv(10, 8), astCtxt.bv(20, 8)));

    log('Node: '+node.ToStr);
    log('Node: '+node.Str);

    var a,b,c : AbstractNode;
    a := astCtxt.bv(1, 8);
    b := astCtxt.bv(2, 8) ;
    c := astCtxt.bvor( astCtxt.bvand(a, astCtxt.bvnot(b) ),  astCtxt.bvand(astCtxt.bvnot(a), b)) ;
    log('Node: '+c.ToStr);

    api.Free ;
end;

procedure TForm1.test_istruzione;
const trace : array[0..5] of op  = (
                                    (addr:$400000; inst : [$48,$8b,$05,$b8,$13,$00,$00]; Size:7), //* mov        rax, QWORD PTR [rip+0x13b8] */
                                    (addr:$400007; inst : [$48,$8d,$34,$c3]; Size:4),              //* lea        rsi, [rbx+rax*8] */
                                    (addr:$40000B; inst : [$67,$48,$8D,$74,$C3,$0A]; Size:6),      //* lea        rsi, [ebx+eax*8+0xa] */
                                    (addr:$400011; inst : [$66,$0F,$D7,$D1]; Size:4),              //* pmovmskb   edx, xmm1 */
                                    (addr:$400015; inst : [$89,$d0]; Size:2),                      //* mov        eax, edx */
                                    (addr:$400017; inst : [$80,$f4,$99]; Size:3)                   //* xor        ah, 0x99 */
                                   );

var
 api       : TApi;
 i,j       : Integer;
 inst      : Istruzione;

begin
   api.Create;
  (* Set the arch *)
  api.setArchitecture(ARCH_X86_64);
  (* Build an instruction *)
  for i := 0  to High(trace) do
  begin
      inst.Create;
      (* Setup opcode *)
      inst.setOpcode(topcode(trace[i].inst));

      inst.setAddress(trace[i].addr);

      if not api.processing(inst) then
         log('Fail an instruction' );

      Log(inst.ToStr) ;

      for j := 0 to Length(inst.operands) - 1 do
      begin
          Log('    '+inst.operands[j].ToStr);

          if inst.operands[j].Tipo = OP_MEM then
          begin
               Log('        base  : '+ inst.operands[j].mem.baseReg.ToStr);
               Log('        index : '+ inst.operands[j].mem.indexReg.ToStr);
               Log('        disp  : '+ inst.operands[j].mem.displacement.ToStr);
               Log('        scale : '+ inst.operands[j].mem.scale.ToStr);
          end;

      end;
      Log('');

  end;

end;


procedure TForm1.test_taint_get_tainted_registers;
var
 Triton : TApi;
 r      : TArray<Registro>;
 function  assertTrue(v:Registro; regs: TArray<Registro>):Boolean;
 var
   FoundIndex : Integer;
   Comparison : IComparer<Registro>;
 begin
     Comparison := TDelegatedComparer<Registro>.Create(
              function(const Left, Right: Registro): Integer
              begin
                Result := Ord(Left.id)-ord(Right.id);
              end);

     Result :=  TArray.BinarySearch<Registro>(regs,v,FoundIndex,Comparison)
 end;

begin
        //Get tainted registers//
        Triton.Create;
        Triton.setArchitecture(ARCH_X86_64);

        r := Triton.getTaintedRegisters;
        assert(Length(r) = 0);

        Triton.taintRegister( Triton.getRegister(ID_REG_X86_EAX)) ;
        Triton.taintRegister( Triton.getRegister(ID_REG_X86_AX) ) ;
        Triton.taintRegister( Triton.getRegister(ID_REG_X86_RBX)) ;
        Triton.taintRegister( Triton.getRegister(ID_REG_X86_CL) ) ;
        Triton.taintRegister( Triton.getRegister(ID_REG_X86_DI) ) ;

        r := Triton.getTaintedRegisters;

        assert( assertTrue(Triton.getRegister(ID_REG_X86_RAX), r),'Failed rax');
        assert( assertTrue(Triton.getRegister(ID_REG_X86_RBX), r),'Failed rbx') ;
        assert( assertTrue(Triton.getRegister(ID_REG_X86_RCX), r),'Failed rcx') ;
        assert( assertTrue(Triton.getRegister(ID_REG_X86_RDI), r),'Failed rdi') ;
end;

procedure TForm1.test_taint_get_tainted_memory;
var
 Triton : TApi;
 uAdd   : TArray<UInt64>;
 m      : MemAccess;

 function  assertTrue(v:UInt64; mems: TArray<UInt64>):Boolean;
 var
   FoundIndex : Integer;
 begin
     Result :=  TArray.BinarySearch<UInt64>(mems,v,FoundIndex)
 end;

begin
        //Get tainted memory//
        Triton.Create;
        Triton.setArchitecture(ARCH_X86_64);

        uAdd := Triton.getTaintedMemory;
        assert(Length(uAdd) = 0);

        Triton.taintMemory($1000);
        Triton.taintMemory($2000);
        Triton.taintMemory($3000);

        m.Create($4000, 4);
        Triton.taintMemory(m);

        uAdd := Triton.getTaintedMemory;

        assert( assertTrue($1000, uAdd)= True,'Failed $1000');
        assert( assertTrue($2000, uAdd)= True,'Failed $2000');
        assert( assertTrue($3000, uAdd)= True,'Failed $3000');
        assert( assertTrue($4000, uAdd)= True,'Failed $4000');
        assert( assertTrue($4001, uAdd)= True,'Failed $4001');
        assert( assertTrue($4002, uAdd)= True,'Failed $4002');
        assert( assertTrue($4003, uAdd)= True,'Failed $4003');
        assert( assertTrue($5000, uAdd)= False,'Failed $4004');
end;

end.
