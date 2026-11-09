"""Upgrade a v4 Nature Opportunity workbook to v4.1 (paste areas, Growth_Sectors, page 2 context band)."""
import openpyxl, copy, sys
from openpyxl.styles import Font, Alignment
from openpyxl.formatting.rule import FormulaRule
from openpyxl.styles import PatternFill
F='Arial'; B=Font(name=F,size=10); BL=Font(name=F,size=10,color='0000FF'); GN=Font(name=F,size=10,color='008000'); BD=Font(name=F,size=10,bold=True); NT=Font(name=F,size=9,italic=True,color='595959')
WR=Alignment(wrap_text=True,vertical='top'); RED=PatternFill('solid',fgColor='F8CBAD')
def hdr_like(ws,src,cell):
    c=ws[cell]; c.font=copy.copy(src.font); c.fill=copy.copy(src.fill); c.alignment=Alignment(wrap_text=True,vertical='center'); c.border=copy.copy(src.border)
def put(ws,r,c,v,f=B,fmt=None):
    x=ws.cell(r,c); x.value=v; x.font=f; x.alignment=WR
    if fmt: x.number_format=fmt
    return x
def patch(wb):
    H=wb['Scoring']['A4']
    # 1. Scoring: paste area W:AC, judgement columns become lookups (paste Prompt 5 output in one block)
    sc=wb['Scoring']
    ph=['Paste: SubTheme_ID','C1','C2','C4','Evidence_IDs','Rationale','Client exposure (as disclosed)']
    for j,h in enumerate(ph,23):
        sc.cell(4,j).value=h; hdr_like(sc,H,sc.cell(4,j).coordinate); sc.column_dimensions[openpyxl.utils.get_column_letter(j)].width=[14,6,6,6,24,60,30][j-23]
    sc['W3']='Paste the Prompt 5 table here (columns W:AC, any row order). Columns E, F, H, P, Q, R look it up by SubTheme_ID.'; sc['W3'].font=NT
    for r in range(5,20):
        for col,src in (('E','X'),('F','Y'),('H','Z'),('P','AA'),('Q','AB'),('R','AC')):
            look=f'INDEX(${src}$5:${src}$19,MATCH($B{r},$W$5:$W$19,0))'
            sc[f'{col}{r}']=f'=IFERROR(IF({look}="","",{look}),"")'; sc[f'{col}{r}'].font=GN
        for col in ('W','X','Y','Z','AA','AB','AC'): sc[f'{col}{r}'].font=BL
    sc['A2']='Paste the Prompt 5 output into the paste area W:AC. Everything else calculates. Trace ID SC-<SubTheme_ID> is what claims cite.'
    # 2. Slide_Content: paste area J:L, B and C look up by key
    s=wb['Slide_Content']
    last=max(r for r in range(5,200) if s.cell(r,1).value)
    nr=last+1
    put(s,nr,1,'s1_ctx_why',BD); put(s,nr,4,'Context band, third block: why it matters for the client’s sector, quoted (e.g. finance gap, role of private finance)',NT)
    put(s,nr,5,150,B,'0'); put(s,nr,6,f'=LEN(B{nr})',B,'0')
    put(s,nr,7,f'=IF(B{nr}="","Missing",IF(F{nr}>E{nr},"Too long: "&(F{nr}-E{nr})&" over","OK"))')
    tr=next(r for r in range(5,last+1) if isinstance(s.cell(r,8).value,str) and s.cell(r,8).value.startswith('='))
    put(s,nr,8,s.cell(tr,8).value.replace(f'C{tr}',f'C{nr}').replace(f'B{tr}',f'B{nr}'))
    last=nr
    for j,h in enumerate(['Paste: Key','Value','Claim_IDs'],10):
        s.cell(4,j).value=h; hdr_like(s,H,s.cell(4,j).coordinate)
    s.column_dimensions['J'].width=18; s.column_dimensions['K'].width=70; s.column_dimensions['L'].width=22
    s['J3']='Paste the Prompt 7 Slide_Content table here (columns J:L, any row order). Columns B and C look it up by key.'; s['J3'].font=NT
    for r in range(5,last+1):
        for col,src in (('B','K'),('C','L')):
            look=f'INDEX($'+src+'$5:$'+src+'$120,MATCH($A'+str(r)+',$J$5:$J$120,0))'
            s[f'{col}{r}']=f'=IFERROR(IF({look}="","",{look}),"")'; s[f'{col}{r}'].font=GN
    for r in range(5,121):
        for col in ('J','K','L'): s[f'{col}{r}'].font=BL; s[f'{col}{r}'].alignment=WR
    s.conditional_formatting.add(f'G5:H{last}',FormulaRule(formula=['OR(LEFT(G5,3)="Too",G5="Missing",H5="No trace",H5="Unknown claim ID")'],fill=RED))
    s['A2']='Paste the Prompt 7 Slide_Content output into J:L. Every value except structural keys needs Claim_IDs that exist in Claims_Register. Numbers Excel calculates are not typed here – see Deck_Tokens.'
    ctx_row=last
    # 3. Deck_Tokens: context band tokens
    d=wb['Deck_Tokens']; r=max(x for x in range(5,300) if d.cell(x,1).value)+1
    lab='Inputs!$B$14'; sym='Inputs!$B$16'
    toks=[('s1_ctx_what',f'=IF(OR(Inputs!$B$8="",Inputs!$B$8="N/A"),"Each country sets out its nature plan in an NBSAP (National Biodiversity Strategy and Action Plan). The "&{lab}&" has not yet published one.","Each country sets out its nature plan in an NBSAP (National Biodiversity Strategy and Action Plan). For the "&{lab}&" this is the "&Inputs!$B$8&".")'),
          ('s1_ctx_measured',f'="We mapped the "&TEXT(Calculations!$C$5,"0")&" "&{lab}&" commitments with a stated amount (c."&{sym}&TEXT(Calculations!$C$6,"0")&"bn, indicative) in EY’s NBSAP sizing tool to 5 themes."'),
          ('s1_ctx_why',f'=IF(Slide_Content!$B${ctx_row}="","",Slide_Content!$B${ctx_row})')]
    for k,f in toks: put(d,r,1,'{{'+k+'}}',BD); put(d,r,2,f,GN); r+=1
    for x in range(5,r):
        if d.cell(x,1).value=='{{s1_left_sub}}': d.cell(x,2).value=f'="Value of "&{lab}&" NBSAP commitments by theme ("&{sym}&"bn, share of total; indicative)"'
    # 4. Growth_Sectors tab (Prompt 2b)
    g=wb.create_sheet('Growth_Sectors',index=wb.sheetnames.index('NBSAP_Mapping')+1)
    g['A1']='Growth sectors and their nature dependencies (Prompt 2b)'; g['A1'].font=Font(name=F,size=14,bold=True)
    g['A2']='From official national strategies only (growth or industrial strategy, sector plans, national water / infrastructure plans; no media). Every sector and every dependency must cite Evidence_IDs from Evidence_Log. Links to themes and to the client book are EY view.'; g['A2'].font=NT
    gh=['Growth_ID','Growth sector (as named in the source)','Priority Evidence_IDs','Nature dependency (as stated in the source)','Dependency Evidence_IDs','Linked themes (EY view)','Client exposure link (Evidence_IDs or "Not split")','Note','Check']
    for j,(h,w) in enumerate(zip(gh,[9,32,16,50,16,14,26,40,14]),1):
        g.cell(4,j).value=h; hdr_like(g,H,g.cell(4,j).coordinate); g.column_dimensions[openpyxl.utils.get_column_letter(j)].width=w
    for rr in range(5,61):
        first=lambda c: f'TRIM(LEFT({c}{rr},FIND(",",{c}{rr}&",")-1))'
        put(g,rr,9,f'=IF(A{rr}="","",IF(AND(ISNUMBER(MATCH({first("C")},Evidence_Log!$A:$A,0)),ISNUMBER(MATCH({first("E")},Evidence_Log!$A:$A,0))),"OK","Check evidence IDs"))')
    g.freeze_panes='A5'
    # 5. Notes and QA
    wb['Evidence_Log']['A2']=wb['Evidence_Log']['A2'].value.replace('| External driver.','| External driver | Growth priority.')
    wb['Calculations']['A29']='Client-specific calculations (add below; formulas only). Look figures up by Evidence_ID, e.g. =VLOOKUP("EV-040",Client_Exposure!$A:$E,5,FALSE)/VLOOKUP("EV-067",Client_Exposure!$A:$E,5,FALSE)'
    q=wb['QA_Log']; r=max(x for x in range(5,100) if q.cell(x,1).value)+1
    for a,b in [('Prompt 5 output pasted for all 15 sub-themes','=IF(COUNTA(Scoring!W5:W19)=15,"Pass",15-COUNTA(Scoring!W5:W19)&" sub-themes not pasted")'),
                ('Growth_Sectors rows cite evidence that exists','=IF(COUNTIF(Growth_Sectors!I5:I60,"Check*")=0,"Pass","Fix Growth_Sectors evidence IDs")')]:
        put(q,r,1,r-4); put(q,r,2,a); put(q,r,3,b); r+=1
    so=wb['Sources']
    for x in range(5,20):
        if so.cell(x,1).value=='SRC-METH': so.cell(x,2).value='Nature Opportunity Assessment – Methodology and Specifications (v4.1)'
    rd=wb['README']
    for x in range(1,rd.max_row+1):
        if rd.cell(x,1).value=='Nature Opportunity Master Template (v4)': rd.cell(x,1).value='Nature Opportunity Master Template (v4.1)'
        if rd.cell(x,1).value=='Tabs': rd.cell(x,2).value=rd.cell(x,2).value.replace('NBSAP_Mapping |','NBSAP_Mapping | Growth_Sectors |')
    put(rd,rd.max_row+1,1,'v4.1 changes',BD); put(rd,rd.max_row,2,'Paste areas: Scoring W:AC (Prompt 5) and Slide_Content J:L (Prompt 7), looked up by ID so row order does not matter. Growth_Sectors tab (Prompt 2b). Page 2 context band (s1_ctx_what and s1_ctx_measured written by Excel; s1_ctx_why by the agent). Client calculations look up by Evidence_ID.')
    return ctx_row
if __name__=='__main__':
    wb=openpyxl.load_workbook(sys.argv[1]); patch(wb); wb.save(sys.argv[2]); print('patched',sys.argv[2])
