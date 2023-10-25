unit BasicBlock;

(*
## VMProtect's junk code sample from https://back.engineering/17/05/2021/
##
## [Original basic block] -----------------------------------------------
## 0x140004149: rcl di, cl
## 0x14000414c: pop rax
## 0x14000414d: shld r11w, bx, 1
## 0x140004153: pop r11
## 0x140004155: and dh, 0xca
## 0x140004158: not di
## 0x14000415b: pop rdi
## 0x14000415c: rol r9w, 0xc
## 0x140004161: stc
## 0x140004162: pop r8
## 0x140004164: cmc
## 0x140004165: clc
## 0x140004166: shl r9w, 0xb
## 0x14000416b: pop rdx
## 0x14000416c: cmp cx, 0xd2eb
## 0x140004171: bt rcx, rsi
## 0x140004175: pop r9
## 0x140004177: and r10w, sp
## 0x14000417b: rcl r10d, 0x10
## 0x14000417f: pop r10
## 0x140004181: btc cx, 0xc
## 0x140004186: bswap r12
## 0x140004189: cmp rax, -0x38828b69
## 0x14000418f: pop r12
## 0x140004191: rol cx, cl
## 0x140004194: cmc
## 0x140004195: btr bp, 1
## 0x14000419a: sar r14w, cl
## 0x14000419e: pop rbp
## 0x14000419f: sub r14w, si
## 0x1400041a3: or si, si
## 0x1400041a6: add esi, eax
## 0x1400041a8: xadd si, cx
## 0x1400041ac: popfq
## 0x1400041ad: setg cl
## 0x1400041b0: setle cl
## 0x1400041b3: movsx r14, al
## 0x1400041b7: pop rcx
## 0x1400041b8: not ecx
## 0x1400041ba: pop rcx
## 0x1400041bb: lea r13, [rax - 0x36d7e613]
## 0x1400041c2: not si
## 0x1400041c5: pop r14
## 0x1400041c7: not si
## 0x1400041ca: movsx r13w, dl
## 0x1400041cf: mov r13d, 0xb7486bb2
## 0x1400041d5: pop rsi
## 0x1400041d6: mov r13w, 0x44ca
## 0x1400041db: lea r13, [rbx + 0x14631131]
## 0x1400041e2: bswap r13d
## 0x1400041e5: pop r13
## 0x1400041e7: ret
## [End of original basic block] ----------------------------------------
##
## [Simplified basic block] ---------------------------------------------
## 0x140004149: pop rax
## 0x14000414a: pop r11
## 0x14000414c: pop rdi
## 0x14000414d: pop r8
## 0x14000414f: pop rdx
## 0x140004150: pop r9
## 0x140004152: pop r10
## 0x140004154: pop r12
## 0x140004156: pop rbp
## 0x140004157: popfq
## 0x140004158: pop rcx
## 0x140004159: pop rcx
## 0x14000415a: pop r14
## 0x14000415c: pop rsi
## 0x14000415d: pop r13
## 0x14000415f: ret
## [End of simplified basic block] --------------------------------------
##
*)

interface
      uses Winapi.Windows, Winapi.Messages, System.SysUtils,System.Generics.Collections,
           Triton.BasicBlock,
           triton.Instruction;


var
  BBlock : TBasicBlock;

implementation

initialization
var
  Instruction : Istruzione;
begin
    var Ainstructions : TArray<Istruzione> := [
    Instruction.CreateFrom([$66,$d3,$d7]),                    //# rcl     di, cl
    Instruction.CreateFrom([$58]),                            //# pop     rax
    Instruction.CreateFrom([$66,$41,$0f,$a4,$db,$01]),        //# shld    r11w, bx, 1
    Instruction.CreateFrom([$41,$5b]),                        //# pop     r11
    Instruction.CreateFrom([$80,$e6,$ca]),                    //# and     dh, 0CAh
    Instruction.CreateFrom([$66,$f7,$d7]),                    //# not     di
    Instruction.CreateFrom([$5f]),                            //# pop     rdi
    Instruction.CreateFrom([$66,$41,$c1,$c1,$0c]),            //# rol     r9w, 0Ch
    Instruction.CreateFrom([$f9]),                            //# stc
    Instruction.CreateFrom([$41,$58]),                        //# pop     r8
    Instruction.CreateFrom([$f5]),                            //# cmc
    Instruction.CreateFrom([$f8]),                            //# clc
    Instruction.CreateFrom([$66,$41,$c1,$e1,$0b]),            //# shl     r9w, 0Bh
    Instruction.CreateFrom([$5a]),                            //# pop     rdx
    Instruction.CreateFrom([$66,$81,$f9,$eb,$d2]),            //# cmp     cx, 0D2EBh
    Instruction.CreateFrom([$48,$0f,$a3,$f1]),                //# bt      rcx, rsi
    Instruction.CreateFrom([$41,$59]),                        //# pop     r9
    Instruction.CreateFrom([$66,$41,$21,$e2]),                //# and     r10w, sp
    Instruction.CreateFrom([$41,$c1,$d2,$10]),                //# rcl     r10d, 10h
    Instruction.CreateFrom([$41,$5a]),                        //# pop     r10
    Instruction.CreateFrom([$66,$0f,$ba,$f9,$0c]),            //# btc     cx, 0Ch
    Instruction.CreateFrom([$49,$0f,$cc]),                    //# bswap   r12
    Instruction.CreateFrom([$48,$3d,$97,$74,$7d,$c7]),        //# cmp     rax, 0FFFFFFFFC77D7497h
    Instruction.CreateFrom([$41,$5c]),                        //# pop     r12
    Instruction.CreateFrom([$66,$d3,$c1]),                    //# rol     cx, cl
    Instruction.CreateFrom([$f5]),                            //# cmc
    Instruction.CreateFrom([$66,$0f,$ba,$f5,$01]),            //# btr     bp, 1
    Instruction.CreateFrom([$66,$41,$d3,$fe]),                //# sar     r14w, cl
    Instruction.CreateFrom([$5d]),                            //# pop     rbp
    Instruction.CreateFrom([$66,$41,$29,$f6]),                //# sub     r14w, si
    Instruction.CreateFrom([$66,$09,$f6]),                    //# or      si, si
    Instruction.CreateFrom([$01,$c6]),                        //# add     esi, eax
    Instruction.CreateFrom([$66,$0f,$c1,$ce]),                //# xadd    si, cx
    Instruction.CreateFrom([$9d]),                            //# popfq
    Instruction.CreateFrom([$0f,$9f,$c1]),                    //# setnle  cl
    Instruction.CreateFrom([$0f,$9e,$c1]),                    //# setle   cl
    Instruction.CreateFrom([$4c,$0f,$be,$f0]),                //# movsx   r14, al
    Instruction.CreateFrom([$59]),                            //# pop     rcx
    Instruction.CreateFrom([$f7,$d1]),                        //# not     ecx
    Instruction.CreateFrom([$59]),                            //# pop     rcx
    Instruction.CreateFrom([$4c,$8d,$a8,$ed,$19,$28,$c9]),    //# lea     r13, [rax-36D7E613h]
    Instruction.CreateFrom([$66,$f7,$d6]),                    //# not     si
    Instruction.CreateFrom([$41,$5e]),                        //# pop     r14
    Instruction.CreateFrom([$66,$f7,$d6]),                    //# not     si
    Instruction.CreateFrom([$66,$44,$0f,$be,$ea]),            //# movsx   r13w, dl
    Instruction.CreateFrom([$41,$bd,$b2,$6b,$48,$b7]),        //# mov     r13d, 0B7486BB2h
    Instruction.CreateFrom([$5e]),                            //# pop     rsi
    Instruction.CreateFrom([$66,$41,$bd,$ca,$44]),            //# mov     r13w, 44CAh
    Instruction.CreateFrom([$4c,$8d,$ab,$31,$11,$63,$14]),    //# lea     r13, [rbx+14631131h]
    Instruction.CreateFrom([$41,$0f,$cd]),                    //# bswap   r13d
    Instruction.CreateFrom([$41,$5d]),                        //# pop     r13
    Instruction.CreateFrom([$c3])                             //# ret
    ];

    BBlock.Create(Ainstructions)  ;

end;

finalization
   BBlock.Free;

end.
