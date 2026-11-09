"""v4.5: page 3 gets a growth-vs-book strip (s2_grow1-4) and a strategy-alignment tag per theme (Theme_View T:W, not scored)."""
import openpyxl, copy, sys
from openpyxl.styles import Font, Alignment
F='Arial'; B=Font(name=F,size=10); BL=Font(name=F,size=10,color='0000FF'); GN=Font(name=F,size=10,color='008000'); BD=Font(name=F,size=10,bold=True); NT=Font(name=F,size=9,italic=True,color='595959')
def hdr(ws,src,c):
    c.font=copy.copy(src.font); c.fill=copy.copy(src.fill); c.alignment=Alignment(wrap_text=True,vertical='center'); c.border=copy.copy(src.border)
def patch45(wb):
    H=wb['Scoring']['A4']; tv=wb['Theme_View']
    if tv['T4'].value: return
    for col,h,w in (('T','In client’s stated priorities? (Yes/No)',14),('U','Alignment Evidence_IDs',18),('V','Slide tag (formula)',30),('W','Alignment check',14)):
        tv[f'{col}4']=h; hdr(tv,H,tv[f'{col}4']); tv.column_dimensions[col].width=w
    tv['T3']='Strategy alignment (Prompt 5b): Yes only if the client names the theme as a priority in its own strategy or position documents, not merely as eligible under its financing framework. Shown on page 3; not used in scores.'; tv['T3'].font=NT
    for r in range(5,10):
        tv[f'T{r}'].font=BL; tv[f'U{r}'].font=BL
        tv[f'V{r}']=f'=IF(T{r}="Yes","✓ In "&Inputs!$B$13&"’s stated priorities","")'; tv[f'V{r}'].font=GN
        tv[f'W{r}']=f'=IF(T{r}="","Missing",IF(T{r}="No","OK",IF(ISNUMBER(MATCH(TRIM(LEFT(U{r},FIND(",",U{r}&",")-1)),Evidence_Log!$A:$A,0)),"OK","Check evidence")))'
    s=wb['Slide_Content']; last=max(r for r in range(5,200) if s.cell(r,1).value)
    tr=next(r for r in range(5,last+1) if isinstance(s.cell(r,8).value,str) and s.cell(r,8).value.startswith('='))
    keys=[]
    for n in range(1,5):
        keys+=[(f's2_grow{n}_head',f'Growth sector {n} name (from the national strategy), ≤32 characters',32),(f's2_grow{n}_text',f'Growth sector {n}: client exposure (£, % of book) and nature dependency, ≤82 characters',82)]
    rows={}
    for k,g,lim in keys:
        last+=1; r=last; rows[k]=r
        s.cell(r,1).value=k; s.cell(r,1).font=BD; s.cell(r,4).value=g; s.cell(r,4).font=NT; s.cell(r,5).value=lim
        for col,src in (('B','K'),('C','L')):
            look=f'INDEX($'+src+'$5:$'+src+'$120,MATCH($A'+str(r)+',$J$5:$J$120,0))'
            s[f'{col}{r}']=f'=IFERROR(IF({look}="","",{look}),"")'; s[f'{col}{r}'].font=GN
        s.cell(r,6).value=f'=LEN(B{r})'; s.cell(r,7).value=f'=IF(B{r}="","Missing",IF(F{r}>E{r},"Too long: "&(F{r}-E{r})&" over","OK"))'
        s.cell(r,8).value=s.cell(tr,8).value.replace(f'C{tr}',f'C{r}').replace(f'B{tr}',f'B{r}')
    d=wb['Deck_Tokens']; r=max(x for x in range(5,300) if d.cell(x,1).value)+1
    for n in range(1,6):
        d.cell(r,1).value='{{'+f'r{n}_align'+'}}'; d.cell(r,1).font=BD
        d.cell(r,2).value=f'=INDEX(Theme_View!$V$5:$V$9,MATCH({n},Theme_View!$K$5:$K$9,0))&""'; d.cell(r,2).font=GN; r+=1
    for k,_,_ in keys:
        d.cell(r,1).value='{{'+k+'}}'; d.cell(r,1).font=BD; d.cell(r,2).value=f'=IF(Slide_Content!$B${rows[k]}="","",Slide_Content!$B${rows[k]})'; d.cell(r,2).font=GN; r+=1
    q=wb['QA_Log']; r=max(x for x in range(5,100) if q.cell(x,1).value)+1
    q.cell(r,1).value=r-4; q.cell(r,2).value='Strategy alignment set for all 5 themes, with evidence for every Yes'
    q.cell(r,3).value='=IF(COUNTIF(Theme_View!W5:W9,"OK")=5,"Pass",5-COUNTIF(Theme_View!W5:W9,"OK")&" themes missing or unevidenced")'
    gl=wb['Glossary']; g=next(x for x in range(30,60) if not gl.cell(x,1).value)
    for c,v in enumerate(['DEF-ALIGN','In the client’s stated priorities','Definition','Theme named as a priority in the client’s own strategy or position documents (not merely eligible under its financing framework). Shown on page 3; not used in scores.','SRC-METH','Section 6'],1):
        gl.cell(g,c).value=v; gl.cell(g,c).font=BL
    rd=wb['README']; rd.cell(1,1).value='Nature Opportunity Master Template (v4.5)'; x=rd.max_row+1
    rd.cell(x,1).value='v4.5 changes'; rd.cell(x,2).value='Page 3: growth-vs-book strip (s2_grow1–4) and a strategy-alignment tag per theme (Theme_View T:W, Prompt 5b; not scored). QA check 23.'
if __name__=='__main__':
    wb=openpyxl.load_workbook(sys.argv[1]); patch45(wb); wb.save(sys.argv[2]); print('v4.5',sys.argv[2])
