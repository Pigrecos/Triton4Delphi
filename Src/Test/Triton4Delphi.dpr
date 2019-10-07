program Triton4Delphi;

uses
  Vcl.Forms,
  UntMain in 'UntMain.pas' {Form1},
  Triton.Core in '..\Triton.Core.pas',
  Triton.Instruction in '..\Triton.Instruction.pas',
  Triton.AstContext in '..\Triton.AstContext.pas',
  Triton.Define in '..\Triton.Define.pas',
  Triton.Register in '..\Triton.Register.pas',
  Triton.BitVector in '..\Triton.BitVector.pas',
  Triton.Immediate in '..\Triton.Immediate.pas',
  Triton.MemoryAccess in '..\Triton.MemoryAccess.pas',
  Triton.OperandWrapper in '..\Triton.OperandWrapper.pas',
  Triton.SymbolicExpression in '..\Triton.SymbolicExpression.pas',
  Triton.Api in '..\Triton.Api.pas',
  Triton.AstNode in '..\Triton.AstNode.pas',
  Triton.SymbolicVariable in '..\Triton.SymbolicVariable.pas',
  Triton.SolverModel in '..\Triton.SolverModel.pas',
  Triton.pathConstraint in '..\Triton.pathConstraint.pas',
  proving_opaque_predicates in 'proving_opaque_predicates.pas',
  code_coverage_crackme_xor in 'code_coverage_crackme_xor.pas',
  forward_tainting in 'forward_tainting.pas',
  backward_slicing in 'backward_slicing.pas',
  Ir in 'Ir.pas',
  test_path_constraint in 'test_path_constraint.pas',
  Callback in 'Callback.pas',
  Vcl.Themes,
  Vcl.Styles,
  Simplification in 'Simplification.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
