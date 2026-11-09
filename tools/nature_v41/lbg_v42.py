"""v4.2: add the UK Industrial Strategy (SRC-09) to the LBG example: IS-8 growth sectors, evidence, exposure links, page 2 'So what' and talk tracks."""
import openpyxl, sys
from openpyxl.styles import Font
import verify_quotes as V
V.PDF['SRC-09']=sys.argv[3]
SRC, OUT = sys.argv[1], sys.argv[2]
wb=openpyxl.load_workbook(SRC); BL=Font(name='Arial',size=10,color='0000FF')
def setrow(ws,r,vals,c0=1):
    for j,v in enumerate(vals,c0): x=ws.cell(r,j); x.value=v; x.font=BL
def nextrow(ws,col=1,start=5):
    r=start
    while ws.cell(r,col).value not in (None,''): r+=1
    return r
from patch_master_v41 import patch42
if __name__=='__main__' and len(sys.argv)==3: pass
patch42(wb)
# ---- Source
S=wb['Sources']; r=nextrow(S)
setrow(S,r,['SRC-09','The UK’s Modern Industrial Strategy (print-ready version)','HM Government (Department for Business and Trade)','2025','National strategy',
 'https://assets.publishing.service.gov.uk/media/69256e16367485ea116a56de/industrial_strategy_policy_paper.pdf','09/11/2026','printed = PDF page','Y',
 'Downloaded from the GOV.UK collection "The UK’s Modern Industrial Strategy 2025"; 35,377,105 bytes; SHA-256 65232644a45bf9a4f3925eedbfb42e3ffa2b6f581a12cdde45a120fc3fc0d67b. Not stored in the repo (35MB).'])
# ---- Evidence (verified word for word)
NEW=[('EV-090',115,'the whole of government is backing eight sectors which represent 32% of the economy','XC'),
('EV-091',115,'Advanced Manufacturing, Clean Energy Industries, Creative Industries, Defence, Digital and Technologies, Financial Services, Life Sciences, and Professional and Business Services','XC'),
('EV-092',20,'they will add an extra £152 billion in GVA to the economy annually, and create 1.1 million new jobs by 2035','XC'),
('EV-093',20,'Business investment in frontier Clean Energy Industries will at least double to over £30 billion.','XC'),
('EV-094',20,'The UK’s Advanced Manufacturing sector will achieve a near doubling of business investment to £39 billion per year.','XC'),
('EV-095',110,'so that industrial and residential developments are not held back due to pressures on water availability','W03'),
('EV-096',82,'Increasing certainty for gigafactories, laboratories, and data centres – crucial commercial opportunities in the IS-8','UD03'),
('EV-097',12,'£4 billion additional capital for the IS-8','XC'),
('EV-098',20,'The Professional and Business Services sector will double business investment to £65 billion.','XC'),
('EV-101',115,'we must remain alive to the risks to growth posed by climate change and nature degradation','DM03'),
('EV-102',82,'implementing a Nature Restoration Fund which will mean that in many cases they can make a single payment','UD03')]
E=wb['Evidence_Log']; M=wb['NBSAP_Mapping']; r=nextrow(E); m=nextrow(M)
for ev,p,q,st in NEW:
    assert V.check('SRC-09',p,q)=='exact',ev
    setrow(E,r,[ev,'SRC-09',f'p.{p}',p,q,'Growth priority','External driver','Prompt 3b: UK Industrial Strategy.','Script check vs PDF text: exact (09/11/2026)']); r+=1
    for j,v in [(1,ev),(3,'External driver'),(4,st),(7,'Indirect'),(8,'Prompt 3b (Industrial Strategy)')]: M.cell(m,j).value=v; M.cell(m,j).font=BL
    m+=1
# exposure rows for IS-8 links (data sheet tab 18)
for ev,row,ce in [('EV-099',['Financial, business and other services','Total','40,221 | 35,394 | 75,615 | 0.4% | 0.3%'],('Financial, business and other services','Total',75615)),
                  ('EV-100',['Postal and telecommunications','Total','3,177 | 3,909 | 7,086 | 0.0% | 0.0%'],('Postal and telecommunications','Total',7086))]:
    setrow(E,r,[ev,'SRC-04','Tab 18 – Environmental risk','',' | '.join(row),'Exposure','Value chain / portfolio',f'{ce[0]} total exposure £{ce[2]:,}m.','Figure checked vs data sheet (09/11/2026)']); r+=1
    for j,v in [(1,ev),(3,'Client exposure'),(4,'XC'),(7,'Indirect'),(8,'Exposure for IS-8 link (EY view)')]: M.cell(m,j).value=v; M.cell(m,j).font=BL
    m+=1
    CE=wb['Client_Exposure']; c=nextrow(CE); setrow(CE,c,[ev,'SRC-04',ce[0],ce[1],ce[2],'GBP','No','Sector total','—','Tab 18']); CE.cell(c,5).number_format='#,##0'
# ---- Growth sectors: IS-8 (dependency only where a source states it)
NONE='None stated in sources reviewed'
gs=[('GS-05','Advanced Manufacturing','EV-091, EV-094','Industrial developments must not be held back by water availability (new reservoirs, Oxford–Cambridge)','EV-095','W','EV-045','Manufacturing exposure £13.1bn. EY view.'),
('GS-06','Clean Energy Industries','EV-091, EV-093','Energy sector named as a source of additional water demand','EV-084','W','EV-038','Energy and water supply exposure £12.1bn. EY view.'),
('GS-07','Creative Industries','EV-091','—',NONE,'—','Not split','No nature need stated in the Industrial Strategy, EA framework or AI plan.'),
('GS-08','Defence','EV-091','—',NONE,'—','Not split','No nature need stated in the sources reviewed.'),
('GS-09','Digital and Technologies','EV-091, EV-096','Data centres: large amount of water for cooling; access to land and power','EV-085, EV-088','W, UD','EV-100','Nearest disclosed LBG sector: postal and telecommunications £7.1bn; data centres not split. EY view.'),
('GS-10','Financial Services','EV-091','—',NONE,'—','EV-099','LBG’s own sector; no nature need stated.'),
('GS-11','Life Sciences','EV-091, EV-096','—',NONE,'—','Not split','Laboratories named for planning (EV-096) but no water or land need stated.'),
('GS-12','Professional and Business Services','EV-091, EV-098','—',NONE,'—','EV-099','No nature need stated.')]
G=wb['Growth_Sectors']; r=nextrow(G)
for row in gs: setrow(G,r,list(row)); r+=1
# ---- Slide text (paste area J:L)
s=wb['Slide_Content']; rows={s.cell(x,10).value:x for x in range(5,121) if s.cell(x,10).value}
def settext(k,v,c=None):
    x=rows[k]; s.cell(x,11).value=v; s.cell(x,11).font=BL
    if c is not None: s.cell(x,12).value=c
settext('s1_sowhat','Government backs 8 growth sectors (32% of the economy) and says growth cannot happen without water. Financing water and flood resilience is how banks share in that growth.','C-033, C-034, C-119')
settext('s1_footnote','Sources: EY NBSAP Nature Sizing workbook (UK commitments; indicative; EY classification review); HM Government, Modern Industrial Strategy (2025) p.115; Environment Agency, National Framework for Water Resources 2025, pp.3, 6; LBG Built to Last (2026), PDF pp.7, 10, 11; LBG Working with nature (2024) pp.3, 7, 13.','')
settext('s1_talk','Quick context first: every country has a national nature plan; in the UK it is the Environmental Improvement Plan 2025, and EY tracks the funded commitments behind it. Of c.£60bn, 69% goes to water and flood. Then the direction of travel: the Industrial Strategy backs eight sectors, 32% of the economy, and calls nature degradation a risk to growth; it plans new reservoirs so development is not held back by water. The Environment Agency goes further: growth cannot happen without water. Close: water and flood resilience is a growth market for banks.','')
settext('s2_talk',s.cell(rows['s2_talk'],11).value.split(' Capital also needs capability')[0]+' Capital deployment: the Growth_Sectors tab sets the eight priority sectors against LBG’s book. The largest overlap is financial and business services; the water-hungry growth sectors (clean energy, manufacturing, data centres) are small or not split. Capital also needs capability: lending into sectors such as data centres needs teams that understand them.','')
for k in ('s1_sowhat','s1_footnote','s1_talk','s2_talk'):
    lim=next(s.cell(x,5).value for x in range(5,120) if s.cell(x,1).value==k); v=s.cell(rows[k],11).value
    assert len(v)<=lim,(k,len(v),lim)
# ---- Claims
CR=wb['Claims_Register']; crow={CR.cell(x,1).value:x for x in range(5,450) if CR.cell(x,1).value}
def setclaim(cid,vals):
    x=crow.get(cid) or nextrow(CR); crow[cid]=x; setrow(CR,x,[cid]+vals)
setclaim('C-033',['s1','s1_sowhat','Government backs 8 growth sectors (32% of the economy)','Quoted fact','EV-090','EV-091','"the whole of government is backing eight sectors which represent 32% of the economy" (Industrial Strategy p.115)'])
setclaim('C-034',['s1','s1_sowhat','says growth cannot happen without water','Quoted fact','EV-080','','Environment Agency (a government body): "Without a sufficient and sustainable water supply, growth cannot happen."'])
setclaim('C-119',['s1','s1_sowhat','Financing water and flood resilience is how banks share in that growth','EY view','EV-080','EV-095, EV-032, CALC-TOP2-S','Growth needs water (EV-080, EV-095); water and flood take 69% of UK NBSAP money (CALC-TOP2-S); LBG cites a $100–130bn adaptation financing opportunity for banks (EV-032)'])
wb.save(OUT); print('saved',OUT)
