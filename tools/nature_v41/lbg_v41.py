"""Build the LBG v4.1 worked example: upgrade the v4 workbook and add Prompt 2b evidence, growth sectors, context band and revised claims."""
import openpyxl, sys
from openpyxl.styles import Font
from patch_master_v41 import patch
from verify_quotes import check
SRC, OUT = sys.argv[1], sys.argv[2]
wb=openpyxl.load_workbook(SRC)
BL=Font(name='Arial',size=10,color='0000FF')
def setrow(ws,r,vals,c0=1):
    for j,v in enumerate(vals,c0): x=ws.cell(r,j); x.value=v; x.font=BL
def nextrow(ws,col=1,start=5):
    r=start
    while ws.cell(r,col).value not in (None,''): r+=1
    return r
# capture typed judgements and slide text before the patch turns those columns into lookups
sc=wb['Scoring']; judg=[[sc.cell(r,c).value for c in (2,5,6,8,16,17,18)] for r in range(5,20)]
s=wb['Slide_Content']; text={s.cell(r,1).value:(s.cell(r,2).value,s.cell(r,3).value) for r in range(5,120) if s.cell(r,1).value}
patch(wb)
for i,row in enumerate(judg,5): setrow(sc,i,row,23)
# ---- Sources
S=wb['Sources']; r=nextrow(S)
setrow(S,r,['SRC-07','AI Opportunities Action Plan (CP 1241)','HM Government (Department for Science, Innovation and Technology)','2025','National strategy','United_Kingdom_ai_opportunities_action_plan.pdf','09/11/2026','printed = PDF − 1','Y','Prompt 2b'])
setrow(S,r+1,['SRC-08','The National Framework for Water Resources 2025: Summary Document (updated April 2026)','Environment Agency','2026','National plan','United_Kingdom_2025_EA_National_Framework_Water_Resources_summary.pdf','09/11/2026','printed = PDF page','Y','Prompt 2b'])
# ---- Evidence (Prompt 2b), each verified word for word
NEW=[('EV-078','SRC-08',6,'Housing growth is a top priority, alongside moving to Net Zero and decarbonised industry, greater national food security','UD03'),
('EV-079','SRC-08',6,'the UK as a world leader in data centres and Artificial Intelligence. All of these require water','W03'),
('EV-080','SRC-08',6,'Without a sufficient and sustainable water supply, growth cannot happen.','W03'),
('EV-081','SRC-08',3,'we could have a 5 billion litre a day shortfall by 2055 for public water supplies alone','W03'),
('EV-082','SRC-08',7,'There could be an additional 8 million people in England by 2055','W03'),
('EV-083','SRC-08',14,'Non PWS sectors alone will account for an additional demand of 1090 Ml/d by the 2050s','W01'),
('EV-084','SRC-08',14,'Additional pressure will result from other sectors of use, particularly from the energy, food and data centre sectors.','W01'),
('EV-085','SRC-08',24,'Data centres can require a large amount of water for cooling processes','W01'),
('EV-086','SRC-08',7,'A programme of £8 billion has been secured through the latest round of water company planning','W03'),
('EV-087','SRC-07',9,'Establish ‘AI Growth Zones’ (AIGZs) to facilitate the accelerated build out of AI data centres.','UD03'),
('EV-088','SRC-07',9,'As AI infrastructure providers seek access to land and power','UD03'),
('EV-089','SRC-07',5,'Our transformative planning reforms will make it easier to build the data centres that are the engines of the AI age.','UD03')]
E=wb['Evidence_Log']; M=wb['NBSAP_Mapping']; r=nextrow(E); m=nextrow(M)
for ev,src,pdfp,q,st in NEW:
    assert check(src,pdfp,q)=='exact',ev
    printed=f'p.{pdfp-1}' if src=='SRC-07' else f'p.{pdfp}'
    setrow(E,r,[ev,src,printed,pdfp,q,'Growth priority','External driver','Prompt 2b: national growth priority or its water / land need.','Script check vs PDF text: exact (09/11/2026)']); r+=1
    for j,v in [(1,ev),(3,'External driver'),(4,st),(7,'Indirect'),(8,'Prompt 2b (growth sector evidence)')]: M.cell(m,j).value=v; M.cell(m,j).font=BL
    m+=1
# ---- Growth sectors
G=wb['Growth_Sectors']
gs=[('GS-01','Housing growth','EV-078','All of these require water; growth cannot happen without sufficient water supply','EV-079, EV-080','W, UD, DM','EV-076, EV-040','Housebuilders (EV-076) and mortgages (EV-040) are the closest LBG links. EY view.'),
('GS-02','Net Zero and decarbonised industry','EV-078','All of these require water (incl. water for energy)','EV-079, EV-084','W','EV-038','Energy and water supply exposure. EY view.'),
('GS-03','National food security','EV-078','All of these require water; food sector named as an additional source of water demand','EV-079, EV-084','W, AFF','EV-075','Agriculture, forestry and fishing exposure. EY view.'),
('GS-04','Data centres and Artificial Intelligence','EV-079, EV-087, EV-089','Large amount of water for cooling; land and power','EV-085, EV-088','W, UD','Not split','LBG exposure tables do not split data centres; eligible in LBG’s framework (EV-063). EY view.')]
for i,row in enumerate(gs,5): setrow(G,i,row)
# ---- Calculations by Evidence_ID lookup
C=wb['Calculations']; L=lambda ev: f'VLOOKUP("{ev}",Client_Exposure!$A:$E,5,FALSE)'
for r in range(30,40):
    cid=C.cell(r,1).value
    f={'CALC-51':f'={L("EV-067")}/1000','CALC-52':f'={L("EV-040")}/{L("EV-067")}','CALC-53':f'={L("EV-068")}/{L("EV-067")}','CALC-54':f'={L("EV-038")}/{L("EV-067")}',
       'CALC-55':f'={L("EV-075")}/{L("EV-067")}','CALC-56':f'={L("EV-045")}/{L("EV-067")}','CALC-57':f'={L("EV-039")}/{L("EV-067")}'}.get(cid)
    if f: C.cell(r,3).value=f
# ---- Slide text (paste area J:L): v4 text plus v4.1 changes
text.update({
 's1_tile3_stat':('5bn','C-025'),
 's1_tile3_text':('litres a day public water shortfall by 2055 without action (Environment Agency)','C-025, C-026'),
 's1_tile4_text':('Housing, net zero, food and data centres all require water (Environment Agency)','C-027'),
 's1_ctx_why':('LBG’s own report puts the UK nature finance gap at £56bn and says private finance has a vital role in closing it for nature-based solutions.','C-001, C-117, C-118'),
 's1_footnote':('Sources: EY NBSAP Nature Sizing workbook (UK commitments; indicative; EY classification review); Environment Agency, National Framework for Water Resources 2025, pp.3, 6; LBG Built to Last (2026), PDF pp.7, 10, 11; LBG Working with nature (2024) pp.3, 7, 13. Full trace in speaker notes.',''),
 's1_talk':('Quick context first: every country has a national nature plan; in the UK it is the Environmental Improvement Plan 2025, and EY tracks the funded commitments behind it. Of c.£60bn, 69% goes to water and flood. Then the direction of travel: the Environment Agency says housing, net zero, food and data centres all require water, with a 5bn-litre-a-day shortfall by 2055 without action. Close: this is a growth market for banks, not only a risk topic.',''),
 's2_talk':(text['s2_talk'][0]+' Capital also needs capability: lending into growth sectors such as data centres needs teams that understand them.',''),
})
s=wb['Slide_Content']
for i,(k,(v,c)) in enumerate(text.items(),5): setrow(s,i,[k,v,c],10)
# ---- Claims: replace tile 3 claims, update tile 4 and title support, add context band claims
CR=wb['Claims_Register']; rows={CR.cell(r,1).value:r for r in range(5,450) if CR.cell(r,1).value}
def setclaim(cid,vals):
    r=rows.get(cid) or nextrow(CR); rows[cid]=r; setrow(CR,r,[cid]+vals)
setclaim('C-025',['s1','s1_tile3_stat','5bn litres a day public water shortfall by 2055 without action','Quoted fact','EV-081','','"5 billion litre a day shortfall by 2055 for public water supplies" (without continued action)'])
setclaim('C-026',['s1','s1_tile3_text','Environment Agency','Quoted fact','SRC-08','','Publisher of the National Framework for Water Resources 2025'])
setclaim('C-027',['s1','s1_tile4_text','Housing, net zero, food and data centres all require water','Quoted fact','EV-079','EV-078','Paraphrase, no quotation marks: sectors named in EV-078 and EV-079; source wording "All of these require water" (EV-079)'])
setclaim('C-011',['s1','s1_title','growth will add more','EY view','EV-080','EV-078, EV-079, EV-081, EV-061','Government says growth needs water (EV-080) and projects a shortfall (EV-081); infrastructure pipeline (EV-061)'])
setclaim('C-020',['s1','s1_sub_r','Growth, drought and regulation all point the same way','EY view','EV-080','EV-019, EV-081, EV-070','Summary of the four tiles and the rules line'])
setclaim('C-100',['s1','auto:s1_left_sub','Value of UK NBSAP commitments by theme (indicative)','Definition','DEF-COMMIT','CALC-02, ABB-NBSAP','Chart A subtitle; values in C-101'])
setclaim('C-110',['s1','auto:s1_ctx_what','NBSAP (National Biodiversity Strategy and Action Plan)','Abbreviation','ABB-NBSAP','',''])
setclaim('C-111',['s1','auto:s1_ctx_what','For the UK this is the Environmental Improvement Plan 2025','Quoted fact','SRC-EY','','EY workbook, Countries tab (GBR, NBSAP document)'])
setclaim('C-112',['s1','auto:s1_ctx_measured','34 UK commitments with a stated amount','Calculated','CALC-01','DEF-COMMIT',''])
setclaim('C-113',['s1','auto:s1_ctx_measured','c.£60bn, indicative','Calculated','CALC-02','','£60.0bn, shown as c.£60bn'])
setclaim('C-114',['s1','auto:s1_ctx_measured','EY’s NBSAP sizing tool … to 5 themes','Definition','DEF-THEME','ABB-EY, ABB-NBSAP',''])
setclaim('C-117',['s1','s1_ctx_why','UK nature finance gap at £56bn','Quoted fact','EV-008','','"the finance gap to meet the UK’s nature related outcomes in the UK is £56 billion"'])
setclaim('C-118',['s1','s1_ctx_why','private finance has a vital role in closing it for nature-based solutions','Quoted fact','EV-013','','"Private financial institutions have a vital role to play in closing the finance gap for NbS."'])
wb['Inputs']['B11']='09/11/2026'
wb.save(OUT); print('saved',OUT)
