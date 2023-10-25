unit Simplification;

interface
      uses Winapi.Windows, Winapi.Messages, System.SysUtils,System.Generics.Collections;
(*
## Output:
##
##  $ ./simplification.py
##  Expr:  (bvxor (_ bv1 8) (_ bv1 8))
##  Simp:  (_ bv0 8)
##
##  Expr:  (bvor (bvand (_ bv1 8) (bvnot (_ bv2 8))) (bvand (bvnot (_ bv1 8)) (_ bv2 8)))
##  Simp:  (bvxor (_ bv1 8) (_ bv2 8))
##
##  Expr:  (bvor (bvand (bvnot (_ bv2 8)) (_ bv1 8)) (bvand (bvnot (_ bv1 8)) (_ bv2 8)))
##  Simp:  (bvxor (_ bv1 8) (_ bv2 8))
##
##  Expr:  (bvor (bvand (bvnot (_ bv2 8)) (_ bv1 8)) (bvand (_ bv2 8) (bvnot (_ bv1 8))))
##  Simp:  (bvxor (_ bv1 8) (_ bv2 8))
##
##  Expr:  (bvor (bvand (_ bv2 8) (bvnot (_ bv1 8))) (bvand (bvnot (_ bv2 8)) (_ bv1 8)))
##  Simp:  (bvxor (_ bv2 8) (_ bv1 8))
##
*)


        procedure main_Simplification;

implementation
       uses Triton.Api,
            Triton.Core,
            Triton.Instruction,
            Triton.AstContext,
            Triton.AstNode,
            Triton.SymbolicExpression,
            Triton.SymbolicVariable,
            Triton.pathConstraint,
            Triton.SolverModel,
            Triton.Register,
            Triton.BitVector,
            Triton.Immediate,
            Triton.MemoryAccess,
            Triton.OperandWrapper,
            Triton.Define,
            UntMain;

var
 Triton : TTritonCtx;

// a ^ a -> a = 0
function xor1(aApi : HandleContext; sNode: HandleAbstractNode): HandleAbstractNode;cdecl;
var
  node : AbstractNode;
begin
    node := AbstractNode(snode);

    if node.Tipo = BVXOR_NODE then
       if node.Childrens[0].equalTo(node.Childrens[1]) then
       begin
          Exit(  AstContext(node.Context).bv(0,node.BitvectorSize) ) ;
       end;

    Result := node;
end;

// ((a & ~b) | (~a & b)) -> (a ^ b)
function xor2(aApi : HandleContext; sNode: HandleAbstractNode): HandleAbstractNode;cdecl;
var
  node   : AbstractNode;
  c1,c2  : AbstractNode;
  c1_not,
  c2_not,
  c1_nonNot,
  c2_nonNot : AbstractNode;

  function getNot(vNode: AbstractNode): AbstractNode;
  var
   a,b : AbstractNode;
  begin
        a := vNode.Childrens[0];
        b := vNode.Childrens[1];
        if (a.Tipo = BVNOT_NODE) and (b.Tipo <> BVNOT_NODE ) then
            Exit( a );
        if (b.Tipo = BVNOT_NODE) and (a.Tipo <> BVNOT_NODE) then
            Exit( b );
        Result := nil ;
  end;

  function getNonNot(vNode: AbstractNode): AbstractNode;
  var
   a,b : AbstractNode;
  begin
        a := vNode.Childrens[0];
        b := vNode.Childrens[1];
        if (a.Tipo <> BVNOT_NODE) and (b.Tipo = BVNOT_NODE) then
            Exit( a );
        if (b.Tipo <> BVNOT_NODE) and (a.Tipo = BVNOT_NODE) then
            Exit( b );
        Result := nil ;
  end;
begin
    node := AbstractNode(snode);


    if node.Tipo = BVOR_NODE then
    begin
        c1 := node.Childrens[0];
        c2 := node.Childrens[1];
        if (c1.Tipo = BVAND_NODE) and (c2.Tipo = BVAND_NODE) then
        begin
            c1_not := getNot(c1);
            c2_not := getNot(c2);

            c1_nonNot := getNonNot(c1);
            c2_nonNot := getNonNot(c2);

            if (c1_not.equalTo(not c2_nonNot)) and (c2_not.equalTo(not c1_nonNot)) then
                Exit(c1_nonNot xor c2_nonNot);
        end;
     end;

    Result := node ;
end;



procedure main_Simplification;
var
  astCtxt : AstContext;
  a,b,c   : AbstractNode;
begin
    Triton.Create;
    // Set arch to init engines
    Triton.setArchitecture(ARCH_X86_64);

    // Record simplifications
    Triton.addCallback(@xor1, SYMBOLIC_SIMPLIFICATION) ;
    Triton.addCallback(@xor2, SYMBOLIC_SIMPLIFICATION) ;

    astCtxt := Triton.getAstContext;

    a := astCtxt.bv(1, 8);
    b := astCtxt.bv(2, 8);

    // Example 1
    c := a xor a ;
    Form1.Log('Expr: ' + c.ToStr);
    c := Triton.simplify(c) ;
    Form1.Log('Simp: '+ c.ToStr);

    Form1.Log('');

    // Example 2 - forme A
    c := (a and not b) or (not a and b);
    Form1.Log('Expr: ' + c.ToStr);
    c := Triton.simplify(c) ;
    Form1.Log('Simp: '+ c.ToStr);

    Form1.Log('');

    // Example 2 - forme B
    c := (not b and a) or (not a and b);
    Form1.Log('Expr: ' + c.ToStr);
    c := Triton.simplify(c) ;
    Form1.Log('Simp: '+ c.ToStr);

    Form1.Log('');

    // Example 2 - forme C
    c := (not b and a) or (b and not a);
    Form1.Log('Expr: ' + c.ToStr);
    c := Triton.simplify(c);
    Form1.Log('Simp: '+ c.ToStr);

    Form1.Log('');

    // Example 2 - forme D
    c := (b and not a) or (not b and a) ;
    Form1.Log('Expr: ' + c.ToStr);
    c := Triton.simplify(c);
    Form1.Log('Simp: '+ c.ToStr);

    Triton.Free;
end;

end.
