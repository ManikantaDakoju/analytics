"""v4.3: add ENCORE Insights Country Dashboard v3.0 (SRC-10) to the LBG example: UK headline dependency, sector water dependencies, Growth_Sectors, context band."""
import openpyxl, sys, csv, io, zipfile
from openpyxl.styles import Font
SRC, OUT, ZIP = sys.argv[1], sys.argv[2], sys.argv[3]
z=zipfile.ZipFile(ZIP)
rd=lambda f: list(csv.reader(io.TextIOWrapper(z.open(f),encoding='utf-8-sig',errors='replace')))
hf=rd('country_headline_figures.csv'); H=hf[0]; GB=next(r for r in hf if r[0]=='GB')
es=rd('ecosystem_services_domestic.csv'); cols=[i for i,c in enumerate(es[0]) if c=='GB']; sect=[es[1][i] for i in cols]
ES={row[0]:{sect[k]:row[i] for k,i in enumerate(cols)} for row in es[2:]}
SERV=['Water supply','Water flow regulation','Flood mitigation services']
wb=openpyxl.load_workbook(SRC); BL=Font(name='Arial',size=10,color='0000FF')
def setrow(ws,r,vals,c0=1):
    for j,v in enumerate(vals,c0): x=ws.cell(r,j); x.value=v; x.font=BL
def nextrow(ws,col=1,start=5):
    r=start
    while ws.cell(r,col).value not in (None,''): r+=1
    return r
# ---- Source (licence requires the citation below)
CIT='Global Canopy (2026). ENCORE Insights: Country Dashboard. Part of Horizon Europe Nature3B project. [On-line], [November 2026], Oxford, UK. Available at countries.encorenature.org. DOI: https://zenodo.org/records/17803534'
S=wb['Sources']; r=nextrow(S)
setrow(S,r,['SRC-10',CIT+' (data download v3.0)','Global Canopy (Horizon Europe Nature3B project)','2026','Dataset (CSV)','ENCORE_Insights_Country_Dashboard_Data_v3.0.zip','09/11/2026','files and rows (no pages)','Y','CC BY-SA 4.0. Required citation: '+CIT+'. Values are modelled sector averages (0–1 dependency scores; GVA in million euros).'])
# ---- Evidence straight from the CSV rows
E=wb['Evidence_Log']; M=wb['NBSAP_Mapping']; r=nextrow(E); m=nextrow(M)
evs=[]
def add(ev,where,quote,summary,st):
    global r,m
    setrow(E,r,[ev,'SRC-10',where,'',quote,'Growth priority','External driver',summary,'Extracted by script from the ENCORE CSV (09/11/2026)']); r+=1
    for j,v in [(1,ev),(3,'External driver'),(4,st),(7,'Indirect'),(8,'Prompt 3b (ENCORE dependency data)')]: M.cell(m,j).value=v; M.cell(m,j).font=BL
    m+=1; evs.append(ev)
add('EV-103','country_headline_figures.csv, row GB',f'GB | {H[2]} {GB[2]} | {H[3]} {GB[3]}','51.38% of UK GVA (domestic production) is highly dependent on nature (€1,216,552m).','XC')
add('EV-104','country_headline_figures.csv, row GB',f'GB | {H[17]} {GB[17]} | {H[18]} {GB[18]}','UK jobs in highly nature-dependent production: 20,634.9 thousand (62%).','XC')
SECT=[('EV-105','Construction','UD03'),('EV-106','Real estate','DM01'),('EV-107','Agriculture','AFF02'),('EV-108','Food beverages and tobacco','AFF02'),('EV-109','Electricity','W01'),
      ('EV-110','Chemical and materials industry','W01'),('EV-111','Information technology','W01'),('EV-112','Banking and capital markets','XC'),('EV-113','Professional, business and social activities','XC'),('EV-114','Automotive','W01')]
ESV={}
for ev,sec,st in SECT:
    vals=[ES[s][sec] for s in SERV]; ESV[ev]=(sec,vals)
    add(ev,f'ecosystem_services_domestic.csv, column GB / {sec}',f'GB | {sec} | Water supply {vals[0]} | Water flow regulation {vals[1]} | Flood mitigation services {vals[2]}',f'{sec}: mean weighted domestic dependency (0–1) on water supply, water flow regulation and flood mitigation.',st)
def esv(ev): sec,v=ESV[ev]; return f'ENCORE (modelled, 0–1): {sec} – water supply {v[0]}, water flow {v[1]}, flood mitigation {v[2]}'
# ---- Growth_Sectors: add ENCORE dependency evidence (modelled, labelled as such)
G=wb['Growth_Sectors']; rows={G.cell(x,1).value:x for x in range(5,61) if G.cell(x,1).value}
upd={'GS-01':'EV-105','GS-02':'EV-109','GS-03':'EV-107','GS-04':'EV-111','GS-05':'EV-110','GS-06':'EV-109','GS-09':'EV-111','GS-10':'EV-112','GS-11':'EV-110','GS-12':'EV-113'}
mapnote={'GS-11':'ENCORE sector used: Chemical and materials industry (EY mapping: life sciences manufacturing sits in chemicals in ENCORE/EXIOBASE).',
         'GS-09':'ENCORE averages the whole IT sector (water supply 0.27); the Environment Agency singles out data centres for cooling water (EV-085). Both shown.',
         'GS-04':'ENCORE averages the whole IT sector; the EA singles out data centres for cooling water (EV-085).',
         'GS-10':'ENCORE sector: Banking and capital markets. Low modelled water dependency.','GS-12':'ENCORE sector: Professional, business and social activities.'}
for gid,ev in upd.items():
    x=rows[gid]; d=G.cell(x,4).value; e=G.cell(x,5).value
    if e and e.startswith('None'):
        G.cell(x,4).value=esv(ev); G.cell(x,5).value=ev
    else:
        G.cell(x,4).value=f'{d}; {esv(ev)}'; G.cell(x,5).value=f'{e}, {ev}'
    G.cell(x,8).value=((G.cell(x,8).value or '')+' '+mapnote.get(gid,'ENCORE values are modelled sector averages, not stated requirements.')).strip()
    for c in (4,5,8): G.cell(x,c).font=BL
# ---- Glossary
Gl=wb['Glossary']; g=nextrow(Gl,1,60)
setrow(Gl,g,['DEF-OUTPUT','Economic output highly dependent on nature','Definition','Share of UK gross value added (GVA) from domestic production in activities with a high nature-dependency rating, as modelled by ENCORE Insights (Global Canopy)','SRC-10','country_headline_figures.csv; ReadMe.txt'])
setrow(Gl,g+1,['ABB-ENC2','ENCORE Insights','Abbreviation','ENCORE Insights: Country Dashboard by Global Canopy (ENCORE = Exploring Natural Capital Opportunities, Risks and Exposure)','SRC-10','ReadMe.txt'])
# ---- Slide text: context band "Why it matters"
s=wb['Slide_Content']; srow={s.cell(x,10).value:x for x in range(5,121) if s.cell(x,10).value}
def settext(k,v,c=None):
    x=srow[k]; s.cell(x,11).value=v; s.cell(x,11).font=BL
    if c is not None: s.cell(x,12).value=c
settext('s1_ctx_why','Over half (51%) of UK economic output depends highly on nature (ENCORE). LBG puts the UK nature finance gap at £56bn.','C-120, C-121, C-001, C-117')
fn='Sources: EY NBSAP Nature Sizing workbook (indicative); Modern Industrial Strategy (2025) p.115; Environment Agency, National Framework for Water Resources 2025, pp.3, 6; Global Canopy, ENCORE Insights Country Dashboard (2026); LBG Built to Last (2026), PDF pp.7, 10, 11; LBG Working with nature (2024) pp.7, 13.'
settext('s1_footnote',fn,'')
lim={s.cell(x,1).value:s.cell(x,5).value for x in range(5,120) if s.cell(x,1).value}
for k in ('s1_ctx_why','s1_footnote'): assert len(s.cell(srow[k],11).value)<=lim[k],(k,len(s.cell(srow[k],11).value),lim[k])
t=s.cell(srow['s2_talk'],11).value
s.cell(srow['s2_talk'],11).value=t.replace(' Capital also needs capability',' ENCORE shows the water and flood dependency of LBG’s larger nature-linked sectors: construction (flood mitigation 0.59, water supply 0.48), agriculture (water supply 0.70) and food (0.67), against banking at 0.23. Capital also needs capability')
assert len(s.cell(srow['s2_talk'],11).value)<=lim['s2_talk']
# ---- Claims
CR=wb['Claims_Register']; crow={CR.cell(x,1).value:x for x in range(5,450) if CR.cell(x,1).value}
def setclaim(cid,vals):
    x=crow.get(cid) or nextrow(CR); crow[cid]=x; setrow(CR,x,[cid]+vals)
setclaim('C-120',['s1','s1_ctx_why','Over half (51%) of UK economic output depends highly on nature','Quoted fact','EV-103','DEF-OUTPUT','ENCORE "GVA % High Dep" for GB = 51.38, shown as 51%'])
setclaim('C-121',['s1','s1_ctx_why','ENCORE','Abbreviation','ABB-ENCORE','ABB-ENC2, SRC-10','Full licence citation in Sources (SRC-10)'])
r118=crow.get('C-118')
if r118:
    for c in range(1,15): CR.cell(r118,c).value=None
    # remove the empty row gap by moving later rows up is not needed: Claims_Register checks ignore blank rows
wb.save(OUT); print('saved',OUT,'new evidence',len(evs))
