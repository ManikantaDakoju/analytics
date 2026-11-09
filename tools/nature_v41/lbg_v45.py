"""v4.5 LBG example: strategy-alignment tags and the growth-vs-book strip on page 3, all traced."""
import openpyxl, sys
from openpyxl.styles import Font
from patch_v45 import patch45
import verify_quotes as V
SRC, OUT = sys.argv[1], sys.argv[2]
wb=openpyxl.load_workbook(SRC); patch45(wb); BL=Font(name='Arial',size=10,color='0000FF')
def setrow(ws,r,vals,c0=1):
    for j,v in enumerate(vals,c0): x=ws.cell(r,j); x.value=v; x.font=BL
def nextrow(ws,col=1,start=5):
    r=start
    while ws.cell(r,col).value not in (None,''): r+=1
    return r
# evidence: LBG's stated priorities
q='nature-based solutions across three critical priorities: cooling, flood preparedness and drought resilience'
assert V.check('SRC-02',3,q)=='exact'
E=wb['Evidence_Log']; r=nextrow(E); setrow(E,r,['EV-115','SRC-02','PDF p.3','',q,'Commitment','Value chain / portfolio','LBG’s three stated resilience priorities (Built to Last).','Script check vs PDF text: exact (09/11/2026)'])
M=wb['NBSAP_Mapping']; m=nextrow(M)
for j,v in [(1,'EV-115'),(3,'Client position'),(4,'DM01'),(6,'W03'),(7,'Direct'),(8,'Stated priorities: flood preparedness (DM01), drought resilience (W03)')]: M.cell(m,j).value=v; M.cell(m,j).font=BL
# strategy alignment (strict: stated priorities only)
tv=wb['Theme_View']
for x in range(5,10):
    tid=tv.cell(x,1).value
    tv.cell(x,20).value='Yes' if tid in ('DM','W') else 'No'; tv.cell(x,21).value='EV-115' if tid in ('DM','W') else ''
    for c in (20,21): tv.cell(x,c).font=BL
# calculation: financial, business and other services share
C=wb['Calculations']; c=nextrow(C,1,30)
setrow(C,c,['CALC-58','Financial, business and other services as % of Group exposure','=VLOOKUP("EV-099",Client_Exposure!$A:$E,5,FALSE)/VLOOKUP("EV-067",Client_Exposure!$A:$E,5,FALSE)','%','75,615 ÷ 645,058','EV-099, EV-067','SRC-04'])
C.cell(c,3).number_format='0.000'
# glossary
Gl=wb['Glossary']; g=nextrow(Gl,1,60)
setrow(Gl,g,['ABB-AI','AI','Abbreviation','Artificial Intelligence','SRC-08','p.6 ("data centres and Artificial Intelligence")'])
# slide text (paste area J:L)
s=wb['Slide_Content']; p=nextrow(s,10)
TXT=[('s2_grow1_head','Financial and business services','C-130'),('s2_grow1_text','£75.6bn (11.7% of LBG’s book); low water dependency (0.23–0.29)','C-131, C-132, C-133'),
('s2_grow2_head','Advanced manufacturing','C-134'),('s2_grow2_text','£13.1bn (2.0%); water dependency 0.36–0.55; growth needs water','C-135, C-136, C-137, C-138'),
('s2_grow3_head','Clean energy','C-139'),('s2_grow3_text','Energy and water supply £12.1bn (1.9%); water dependency 0.49','C-140, C-141, C-142'),
('s2_grow4_head','Data centres and AI','C-143'),('s2_grow4_text','Not split in LBG’s book; need cooling water, land and power','C-144, C-145')]
for k,v,cid in TXT: setrow(s,p,[k,v,cid],10); p+=1
rows={s.cell(x,10).value:x for x in range(5,121) if s.cell(x,10).value}
x=rows['s2_talk']; t=s.cell(x,11).value
t=t[:t.index(' Capital deployment:')]+(' The strip at the bottom answers “where could LBG deploy capital?”: the government’s priority growth sectors against LBG’s book. LBG’s biggest overlap is financial and business services, which depend little on water; the water-dependent growth sectors (manufacturing, clean energy, data centres) are small or not split in its disclosures. The tags show that flood and water are also LBG’s own stated priorities. Capital also needs capability: lending into sectors such as data centres needs teams that understand them.')
s.cell(x,11).value=t; assert len(t)<=1000,len(t)
# claims
CR=wb['Claims_Register']; cr=max(x for x in range(5,450) if CR.cell(x,1).value)+1
CL=[('C-130','s2','s2_grow1_head','Financial and business services','Quoted fact','EV-091','EV-099','Combines two IS-8 sectors (Financial Services; Professional and Business Services) to match LBG’s disclosed sector “Financial, business and other services”'),
('C-131','s2','s2_grow1_text','£75.6bn','Quoted fact','EV-099','','£75,615m ÷ 1,000, 1 d.p.'),
('C-132','s2','s2_grow1_text','11.7%','Calculated','CALC-58','',''),
('C-133','s2','s2_grow1_text','low water dependency (0.23–0.29)','Quoted fact','EV-112','EV-113','ENCORE water supply: banking and capital markets 0.23; professional, business and social activities 0.29. "Low" = below the other strip sectors (EY wording).'),
('C-134','s2','s2_grow2_head','Advanced manufacturing','Quoted fact','EV-091','',''),
('C-135','s2','s2_grow2_text','£13.1bn','Quoted fact','EV-045','','£13,095m ÷ 1,000, 1 d.p. (LBG sector: manufacturing)'),
('C-136','s2','s2_grow2_text','2.0%','Calculated','CALC-56','',''),
('C-137','s2','s2_grow2_text','water dependency 0.36–0.55','Quoted fact','EV-114','EV-110','ENCORE water supply: automotive 0.36; chemical and materials industry 0.55 (EY mapping of advanced manufacturing)'),
('C-138','s2','s2_grow2_text','growth needs water','Quoted fact','EV-095','','Paraphrase, no quotation marks: "so that industrial and residential developments are not held back due to pressures on water availability"'),
('C-139','s2','s2_grow3_head','Clean energy','Quoted fact','EV-091','EV-093','IS-8 sector: Clean Energy Industries'),
('C-140','s2','s2_grow3_text','Energy and water supply £12.1bn','Quoted fact','EV-038','','£12,084m ÷ 1,000, 1 d.p.'),
('C-141','s2','s2_grow3_text','1.9%','Calculated','CALC-54','',''),
('C-142','s2','s2_grow3_text','water dependency 0.49','Quoted fact','EV-109','','ENCORE water supply: electricity 0.49'),
('C-143','s2','s2_grow4_head','Data centres and AI','Quoted fact','EV-079','EV-087, ABB-AI','Named as a growth priority by the Environment Agency (EV-079) and the AI Opportunities Action Plan (EV-087)'),
('C-144','s2','s2_grow4_text','Not split in LBG’s book','Absence check','SRC-04','EV-100','Data sheet tab 18 has no data-centre line; nearest disclosed sector is postal and telecommunications (EV-100). SRC-01, 02, 05 searched.'),
('C-145','s2','s2_grow4_text','need cooling water, land and power','Quoted fact','EV-085','EV-088','Paraphrase of "a large amount of water for cooling processes" (EV-085) and "access to land and power" (EV-088)'),
('C-146','s2','auto:r1-r5 align','✓ In LBG’s stated priorities (flood, coastal and adaptation; water)','Definition','DEF-ALIGN','EV-115','Built to Last names flood preparedness and drought resilience among LBG’s three critical priorities; other themes not named as LBG priorities in the documents reviewed')]
for row in CL: setrow(CR,cr,list(row)); cr+=1
wb.save(OUT); print('saved',OUT)
